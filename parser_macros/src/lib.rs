extern crate proc_macro;
extern crate proc_macro2;
use core::panic;
use std::collections::HashMap;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro::TokenTree;
use quote::quote;
use syn::{
    self, parse::Parser, parse_macro_input, punctuated::Punctuated, token::Comma, Expr, FnArg,
    Ident, ItemFn, MetaNameValue, Path, Token, Type,
};

#[proc_macro]
pub fn register(item: TokenStream) -> TokenStream {
    let mut source_builder = SourceBuilder::new();
    let mut next = Next::Name;
    let mut name: Option<TokenTree> = None;
    let mut col_numbers: Vec<i32> = vec![0];
    for token in item {
        let token_str = token.to_string();
        if token_str == "-" {
            *col_numbers.last_mut().unwrap()+=1;
            continue;
        }
        match next {
            Next::Name => {
                name = Some(token);
                col_numbers.push(0);
            },
            Next::Value => {
                if let Some(name_token) = name {
                    source_builder.push(RawCommand::Leaf {
                        col_number: col_numbers.remove(0),
                        name: name_token.to_string(),
                        definition: token.to_string(),
                    });
                    name = None;
                }
            }
            Next::NameOrColon => {
                if !token_str.eq(":") {
                    let tree = name.unwrap();
                    source_builder.push(RawCommand::Node {
                        col_number: col_numbers.remove(0),
                        name: tree.to_string(),
                    });
                    name = Some(token);
                    col_numbers.push(0);
                }
            }
        }
        next = next.next(token_str);
    }
    if let Some(tree) = name {
        source_builder.push(RawCommand::Node {
            col_number: col_numbers.remove(0),
            name: tree.to_string(),
        });
    }
    source_builder.finish();
    //println!("{}", &source_builder.source);
    TokenStream::from_str(&source_builder.source).unwrap()
    // for token in item {
    //     println!("{}", token.to_string());
    // }
    // TokenStream::from_str("const one: i32 = 3;").unwrap()
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
            Next::NameOrColon => {
                if value.eq(":") {
                    Next::Value
                } else {
                    Next::NameOrColon
                }
            }
        }
    }
}

fn command_basis(attr: TokenStream, item: TokenStream, multiple: bool, result: bool) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    let mut param_names = Vec::new();
    let mut param_types = Vec::new();
    for (index, input) in func.sig.inputs.iter().enumerate() {
        if index == func.sig.inputs.len() - 1 {
            continue;
        }
        if let syn::FnArg::Typed(pat_type) = input {
            if let syn::Pat::Ident(ident) = &*pat_type.pat {
                param_names.push(&ident.ident);
                param_types.push(&pat_type.ty);
            }
        }
    }
    let parsed_optionals = Punctuated::<MetaNameValue, Token![,]>::parse_terminated
        .parse(attr)
        .unwrap();
    let mut optional_names = Vec::new();
    let mut optional_values = Vec::new();
    let mut optional_types = Vec::new();
    param_names.iter().enumerate().for_each(|(i, x)| {
        let matching_optionals = parsed_optionals
            .iter()
            .filter(|y| {
                let name = &y.path;
                let name_string = quote!(#name).to_string();
                name_string == x.to_string()
            })
            .collect::<Vec<&MetaNameValue>>();
        if matching_optionals.len() > 1 {
            panic!("duplicated option");
        }
        if matching_optionals.len() == 1 {
            optional_names.push(&matching_optionals[0].path);
            optional_values.push(&matching_optionals[0].value);
            optional_types.push(&param_types[i]);
        }
    });
    if optional_names.len() < parsed_optionals.len() {
        panic!("invalid option name");
    }
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
    let state_type = match inputs.last().expect("State needs to be added as a parameter") {
        FnArg::Receiver(_) => panic!("wrong FnARg"),
        FnArg::Typed(the_type) => &the_type.ty,
    };
    let type_name = format!("type_{}", command_name.to_string());
    let type_name: proc_macro2::TokenStream = type_name.parse().unwrap();
    let tokens = match (multiple, result) {
        (true, false) => quote! {
            #[allow(non_camel_case_types)]
            type #type_name = #state_type;
            fn #command_name() -> parser::Command<'static, #state_type> {
                async fn user__executor(#inputs) -> Vec<parser::Output> #body
                async fn to__async__vec(arguments: Vec<String>, state: #state_type) -> Result<Vec<parser::Output>, parser::ParseError>{
                    let output = user__executor(#callings_code);
                    return Ok(output.await);
                }
                fn final__executor(arguments: Vec<String>, state: #state_type) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {
                    let output = to__async__vec(arguments, state);
                    Box::pin(output)
                }
                parser::Command {
                    params : vec![#final_params_code],
                    optionals: vec![#optionals_code],
                    execute: final__executor,
                }
            }
        },
        (false, false) => quote! {
            #[allow(non_camel_case_types)]
            type #type_name = #state_type;
            fn #command_name() -> parser::Command<'static, #state_type> {
                async fn user__executor(#inputs) -> parser::Output #body
                async fn to__async__vec(arguments: Vec<String>, state: #state_type) -> Result<Vec<parser::Output>, parser::ParseError>{
                    let output = user__executor(#callings_code);
                    return Ok(vec![output.await]);
                }
                fn final__executor(arguments: Vec<String>, state: #state_type) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {
                    let output = to__async__vec(arguments, state);
                    Box::pin(output)
                }
                parser::Command {
                    params : vec![#final_params_code],
                    optionals: vec![#optionals_code],
                    execute: final__executor,
                }
            }
        },
        (true, true) => quote! {
            #[allow(non_camel_case_types)]
            type #type_name = #state_type;
            fn #command_name() -> parser::Command<'static, #state_type> {
                async fn user__executor(#inputs) -> Result<Vec<parser::Output>, Box<dyn std::error::Error>> #body
                async fn to__async__vec(arguments: Vec<String>, state: #state_type) -> Result<Vec<parser::Output>, parser::ParseError>{
                    let output = user__executor(#callings_code).await;
                    match output {
                        Ok(value) => Ok(value),
                        Err(err) => Ok(vec![parser::Output::Error(err.to_string())]),
                    }
                }
                fn final__executor(arguments: Vec<String>, state: #state_type) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {
                    let output = to__async__vec(arguments, state);
                    Box::pin(output)
                }
                parser::Command {
                    params : vec![#final_params_code],
                    optionals: vec![#optionals_code],
                    execute: final__executor,
                }
            }
        },
        (false, true) => quote! {
            #[allow(non_camel_case_types)]
            type #type_name = #state_type;
            fn #command_name() -> parser::Command<'static, #state_type> {
                async fn user__executor(#inputs) -> Result<parser::Output, Box<dyn std::error::Error>> #body
                async fn to__async__vec(arguments: Vec<String>, state: #state_type) -> Result<Vec<parser::Output>, parser::ParseError>{
                    let output = user__executor(#callings_code).await;
                    match output {
                        Ok(value) => Ok(vec![value]),
                        Err(err) => Ok(vec![parser::Output::Error(err.to_string())]),
                    }
                }
                fn final__executor(arguments: Vec<String>, state: #state_type) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {
                    let output = to__async__vec(arguments, state);
                    Box::pin(output)
                }
                parser::Command {
                    params : vec![#final_params_code],
                    optionals: vec![#optionals_code],
                    execute: final__executor,
                }
            }
        },
    };
    println!("{}", tokens.to_string());
    TokenStream::from(tokens)
}

#[proc_macro_attribute]
pub fn command(attr: TokenStream, item: TokenStream) -> TokenStream {
    command_basis(attr, item, false, false)
}

#[proc_macro_attribute]
pub fn command_n(attr: TokenStream, item: TokenStream) -> TokenStream {
    command_basis(attr, item, true, false)
}

#[proc_macro_attribute]
pub fn command_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    command_basis(attr, item, false, true)
}

#[proc_macro_attribute]
pub fn command_n_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    command_basis(attr, item, true, true)
}

fn params(names: Vec<&&Ident>, types: Vec<&&Box<Type>>) -> TokenStream {
    let streams = names
        .iter()
        .enumerate()
        .map(|(index, x)| -> TokenStream {
            let typ = &types[index];
            let name = x.to_string();
            quote!(parser::Parameter{name: #name, datatype: parser::DataType::#typ}).into()
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
            quote!(parser::Optional{name: #name, default: #default.to_string(), datatype: parser::DataType::#typ})
                .into()
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
        .map(|(index, _)| -> TokenStream {
            if index == inputs.len() - 1 {
                quote!(state).into()
            }
            else {
                quote!((&arguments[#index]).parse().map_err(|err| parser::ParseError::MissmatchedTypes)?).into()
            }
        })
        .collect::<Vec<TokenStream>>();
    let mut puncts = Punctuated::<Expr, Token![,]>::new();
    for stream in streams {
        puncts.push(parse_macro_input!(stream as Expr));
    }
    quote! {#puncts}.into()
}

#[derive(Debug)]
struct StackEntry {
    col_number: i32,
    has_child: bool,
}

impl StackEntry {
    fn new(col_number: i32) -> StackEntry {
        StackEntry {
            col_number: col_number,
            has_child: false,
        }
    }
}

struct SourceBuilder {
    start_col: Option<i32>,
    source: String,
    col_stack: Vec<StackEntry>,
    names: HashMap<i32, Vec<String>>,
}

#[derive(Debug)]
enum RawCommand {
    Leaf {
        col_number: i32,
        name: String,
        definition: String,
    },
    Node {
        col_number: i32,
        name: String,
    },
}

impl RawCommand {
    fn col_number(&self) -> i32 {
        match self {
            RawCommand::Leaf {
                col_number,
                name: _,
                definition: _,
            } => *col_number,
            RawCommand::Node {
                col_number,
                name: _,
            } => *col_number,
        }
    }

    fn name(&self) -> &String {
        match self {
            RawCommand::Leaf {
                col_number: _,
                name,
                definition: _,
            } => name,
            RawCommand::Node {
                col_number: _,
                name,
            } => name,
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
            let full_name = command.name();
            let name = &full_name[1..full_name.len() - 1];
            self.source
                .push_str(&format!("type CustomState = type_{name}; fn parse(input: &str, state: CustomState) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {{let tree = vec!["))
        }
        println!("start_col: {}", self.start_col.unwrap());
        validate_format(self.start_col.unwrap(), self.col_stack.last(), &command);
        match self.names.get_mut(&command.col_number()) {
            Some(siblings) => {
                if siblings.contains(command.name()) {
                    panic!("duplicate command name");
                }
                siblings.push(command.name().to_string());
            }
            None => {
                self.names
                    .insert(command.col_number(), vec![command.name().to_string()]);
                ()
            }
        }
        self._pop_string(command.col_number());
        self._push_string(&command);
    }

    fn _push_string(&mut self, command: &RawCommand) {
        match command {
            RawCommand::Leaf {
                col_number: _,
                name,
                definition,
            } => {
                self.col_stack.iter_mut().for_each(|x| x.has_child = true);
                self.source.push_str(&format!(
                    "parser::CommandNode::Leaf{{name: {name}, command: {definition}()}},"
                ))
            }
            RawCommand::Node { col_number, name } => {
                self.source
                    .push_str(&format!("parser::CommandNode::Node{{name: {name}, children: vec!["));
                self.col_stack.push(StackEntry::new(*col_number));
            }
        }
    }

    fn _pop_string(&mut self, child_col: i32) {
        while let Some(parent) = self.col_stack.last() {
            if parent.col_number >= child_col {
                if !parent.has_child {
                    panic!("command has no final executor");
                }
                self.col_stack.pop();
                self.source.push_str("]},")
            } else {
                break;
            }
        }
    }

    fn finish(&mut self) {
        self._pop_string(self.start_col.unwrap());
        self.source
            .push_str("]; return parser::parse_with_tree(tree, input, state);}");
        println!("--------------------------------------------------------------");
        println!("{}", self.source);
    }
}

fn validate_format(start_col: i32, parent: Option<&StackEntry>, command: &RawCommand) {
    println!("{:?}", command);
    println!("{:?}", parent);
    if command.col_number() % 4 != 0 {
        panic!("invalid format -> col_number not dividable by four");
    }
    if command.col_number() < start_col {
        panic!("invalid format -> col_number smaller than start");
    }
    match parent {
        Some(stack_entry) => match command {
            RawCommand::Leaf {
                col_number: child_col,
                name: _,
                definition: _,
            } => {
                if *child_col != start_col && *child_col != stack_entry.col_number + 4 {
                    panic!("invalid format -> leaf col number should be equal to start_col or parent_col + 4")
                }
            }
            RawCommand::Node {
                col_number: child_col,
                name: _,
            } => {
                if *child_col < start_col || *child_col > stack_entry.col_number + 4 {
                    panic!("invalid format -> node col number should be between start_col and parent_col + 4")
                }
            }
        },
        None => {
            if command.col_number() != start_col {
                panic!("invalid format -> col_number should be equal to start");
            }
        }
    }
}