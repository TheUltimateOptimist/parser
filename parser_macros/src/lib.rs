#![feature(proc_macro_span)]
extern crate proc_macro;
use core::panic;
use std::str::FromStr;
use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro::TokenTree;
use quote::quote;
use syn::{
    self, parse::Parser, parse_macro_input, punctuated::Punctuated,
    token::Comma, Expr, FnArg, Ident, ItemFn, MetaNameValue, Path, Token, Type,
};

#[proc_macro]
pub fn register(item: TokenStream) -> TokenStream {
    let mut source_builder = SourceBuilder::new();
    let mut next = Next::Name;
    let mut name: Option<TokenTree> = None;
    for token in item {
        let token_str = token.to_string();
        match next {
            Next::Name => name = Some(token),
            Next::Value => {
                if let Some(name_token) = name {
                    source_builder.push(RawCommand::Leaf { col_number: name_token.span().start().column as i32 - 1, name: name_token.to_string(), definition: token.to_string()});
                    name = None;
                }
            },
            Next::NameOrColon => {
                if !token_str.eq(":") {
                    let tree = name.unwrap();
                    source_builder.push(RawCommand::Node { col_number: tree.span().start().column as i32 - 1, name: tree.to_string()});
                    name = Some(token);
                }
            },
        }
        next = next.next(token_str);
    }
    if let Some(tree) = name {
        source_builder.push(RawCommand::Node { col_number: tree.span().start().column as i32 - 1, name: tree.to_string()});
    }
    source_builder.finish();
    //println!("{}", &source_builder.source);
    TokenStream::from_str(&source_builder.source).unwrap()
}

enum Next {
    Name,
    Value,
    NameOrColon,
}

impl Next {
    fn next(&self, value: String) -> Next {
        match self {
            Next::Name => Next::NameOrColon,
            Next::Value => Next::Name,
            Next::NameOrColon => if value.eq(":") {Next::Value} else {Next::NameOrColon}
        }
    }
}

#[proc_macro_attribute]
pub fn command(attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    let parsed_optionals = Punctuated::<MetaNameValue, Token![,]>::parse_terminated
        .parse(attr)
        .unwrap();
    let mut optional_names = Vec::new();
    let mut optional_values = Vec::new();
    parsed_optionals.iter().for_each(|x| {
        optional_names.push(&x.path);
        optional_values.push(&x.value);
    });
    println!("{}", parsed_optionals.iter().len());
    let mut param_names = Vec::new();
    let mut param_types = Vec::new();
    for input in func.sig.inputs.iter() {
        if let syn::FnArg::Typed(pat_type) = input {
            if let syn::Pat::Ident(ident) = &*pat_type.pat {
                param_names.push(&ident.ident);
                param_types.push(&pat_type.ty);
            }
        }
    }
    let mut optional_types = Vec::new();
    optional_names.iter().enumerate().for_each(|(_, x)| {
        if let Some((i, _)) = param_names
            .iter()
            .enumerate()
            .find(|(_, y)| y.to_string() == quote!(#x).to_string())
        {
            optional_types.push(&param_types[i]);
        } else {
            panic!("invalid option name")
        }
        if optional_names
            .iter()
            .filter(|y| quote!(#y).to_string() == quote!(#x).to_string())
            .count()
            > 1
        {
            panic!("duplicated option");
        }
    });
    let mut final_param_names = Vec::new();
    let mut final_param_types = Vec::new();
    param_names.iter().enumerate().for_each(|(index, x)| {
        if !optional_names
            .iter()
            .map(|y| quote!(#y).to_string())
            .collect::<Vec<String>>()
            .contains(&x.to_string())
        {
            final_param_names.push(x);
            final_param_types.push(&param_types[index]);
        }
    });
    let params_code = params(final_param_names, final_param_types);
    let final_params_code = Punctuated::<Expr, Token![,]>::parse_terminated
        .parse(params_code)
        .unwrap();
    let optionals_stream = optionals(optional_names, optional_types, optional_values);
    let optionals_code = Punctuated::<Expr, Token![,]>::parse_terminated
        .parse(optionals_stream)
        .unwrap();
    let command_name = &func.sig.ident;
    let body = &func.block;
    let inputs = &func.sig.inputs;
    let callings_stream = callings(inputs);
    let callings_code = Punctuated::<Expr, Token![,]>::parse_terminated
        .parse(callings_stream)
        .unwrap();
    let tokens = quote! {
        fn #command_name() -> Command<'static> {
            fn private__executor(#inputs)#body
            Command {
                params : vec![#final_params_code],
                optionals: vec![#optionals_code],
                execute: |arguments| {
                    private__executor(#callings_code);
                }
            }
        }
    };
    println!("{}", tokens.to_string());
    TokenStream::from(tokens)
}

fn params(names: Vec<&&Ident>, types: Vec<&&Box<Type>>) -> TokenStream {
    let streams = names
        .iter()
        .enumerate()
        .map(|(index, x)| -> TokenStream {
            let typ = &types[index];
            let name = x.to_string();
            quote!(Parameter::#typ(#name)).into()
        })
        .collect::<Vec<TokenStream>>();
    let mut puncts = Punctuated::<Expr, Token![,]>::new();
    for stream in streams {
        puncts.push(parse_macro_input!(stream as Expr));
    }
    quote! {#puncts}.into()
}

fn optionals(names: Vec<&Path>, types: Vec<&&Box<Type>>, defaults: Vec<&Expr>) -> TokenStream {
    let streams = names
        .iter()
        .enumerate()
        .map(|(index, x)| -> TokenStream {
            let typ = types[index];
            let default = defaults[index];
            let name = quote!(#x).to_string();
            quote!(Optional::#typ(#name, #default)).into()
        })
        .collect::<Vec<TokenStream>>();
    let mut puncts = Punctuated::<Expr, Token![,]>::new();
    for stream in streams {
        puncts.push(parse_macro_input!(stream as Expr));
    }
    quote! {#puncts}.into()
}

fn callings(inputs: &Punctuated<FnArg, Comma>) -> TokenStream {
    let streams = inputs
        .iter()
        .enumerate()
        .map(|(index, _)| -> TokenStream { quote!((&arguments[#index]).into()).into() })
        .collect::<Vec<TokenStream>>();
    let mut puncts = Punctuated::<Expr, Token![,]>::new();
    for stream in streams {
        puncts.push(parse_macro_input!(stream as Expr));
    }
    quote! {#puncts}.into()
}

struct StackEntry {
    col_number: i32,
    has_child: bool,
}

impl StackEntry {
    fn new(col_number: i32) -> StackEntry {
        StackEntry { col_number: col_number, has_child: false}
    }
}

struct SourceBuilder {
    start_col: Option<i32>,
    source: String,
    col_stack: Vec<StackEntry>,
    names: HashMap<i32, Vec<String>>,
}


enum RawCommand {
    Leaf {col_number: i32, name: String, definition: String },
    Node {col_number: i32, name: String },
}

impl RawCommand {
    fn col_number(&self) -> i32 {
        match self {
            RawCommand::Leaf { col_number, name: _, definition: _ } => *col_number,
            RawCommand::Node { col_number, name: _ } => *col_number,
        }
    }

    fn name(&self) -> &String {
        match self {
            RawCommand::Leaf { col_number: _, name, definition: _ } => name,
            RawCommand::Node { col_number: _, name } => name,
        }
    }
}

impl SourceBuilder {
    fn new() -> Self {
        SourceBuilder {
            start_col: None,
            source: String::new(),
            col_stack: vec![],
            names: HashMap::new(),
            
        }
    }

    fn push(&mut self, command: RawCommand) {
        if self.start_col == None {
            self.start_col = Some(command.col_number());
            //self.source.push_str("fn command_tree() -> Vec<CommandNode<'static>>{return vec![")
            self.source.push_str("fn parse(input: &str) {let tree = vec![")
        }
        println!("start_col: {}", self.start_col.unwrap());
        validate_format(self.start_col.unwrap(), self.col_stack.last(), &command);
        match self.names.get_mut(&command.col_number()) {
            Some(siblings) => {
                if siblings.contains(command.name()) {
                    panic!("duplicate command name");
                }
                siblings.push(command.name().to_string());
            },
            None => {self.names.insert(command.col_number(), vec![command.name().to_string()]); ()},
        }
        self._pop_string(command.col_number());
        self._push_string(&command);
    }

    fn _push_string(&mut self, command: &RawCommand) {
        match command {
            RawCommand::Leaf { col_number: _, name, definition } => {
                self.col_stack.iter_mut().for_each(|x| x.has_child = true);
                self.source.push_str(&format!("CommandNode::Leaf{{name: {name}, command: {definition}()}},"))
            },
            RawCommand::Node { col_number, name } => {
                self.source.push_str(&format!("CommandNode::Node{{name: {name}, children: vec!["));
                self.col_stack.push(StackEntry::new(*col_number));
            },
        }
    }

    fn _pop_string(&mut self, child_col: i32) {
        while let Some(parent) = self.col_stack.last() {
            if parent.col_number >= child_col  {
                if !parent.has_child {
                    panic!("command has no final executor");
                }
                self.col_stack.pop();
                self.source.push_str("]},")
            }
            else {
                break;
            }
        }
    }

    fn finish(&mut self) {
        self._pop_string(self.start_col.unwrap());
        self.source.push_str("]; parse_with_tree(tree, input);}")
    }
}

fn validate_format(start_col: i32, parent: Option<&StackEntry>, command: &RawCommand) {
    if command.col_number() % 4 != 0 {
        panic!("invalid format -> col_number not dividable by four");
    }
    if command.col_number() < start_col {
        panic!("invalid format -> col_number smaller than start");
    }
    match parent {
        Some(stack_entry) => match command {
            RawCommand::Leaf{ col_number: child_col, name: _, definition: _ } => {
                if *child_col != start_col && *child_col != stack_entry.col_number + 4 {
                    panic!("invalid format -> leaf col number should be equal to start_col or parent_col + 4")
                }
            },
            RawCommand::Node { col_number: child_col, name: _ } => {
                if *child_col < start_col || *child_col > stack_entry.col_number + 4   {
                    panic!("invalid format -> node col number should be between start_col and parent_col + 4")
                }
            },
        },
        None => {
            if command.col_number() != start_col {
                panic!("invalid format -> col_number should be equal to start");
            }
        },
    }
}
