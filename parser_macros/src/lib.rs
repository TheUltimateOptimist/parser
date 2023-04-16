extern crate proc_macro;
extern crate proc_macro2;
use core::panic;
use std::str::FromStr;
use std::vec;

use proc_macro::TokenStream;
use proc_macro::TokenTree;
use quote::quote;
use syn::{
    self, parse::Parser, parse_macro_input, punctuated::Punctuated, token::Comma, Expr, FnArg,
    Ident, ItemFn, MetaNameValue, Path, Token, Type,
};

#[proc_macro]
pub fn register(item: TokenStream) -> TokenStream {
    let mut next = Next::Name;
    let mut name: Option<TokenTree> = None;
    let mut col_numbers: Vec<i32> = vec![0];
    let mut raw_command = RawCommand::Node { col_number: 0, name: "".to_owned(), children: vec![]};
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
                    raw_command.insert(RawCommand::Leaf {
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
                    raw_command.insert(RawCommand::Node {
                        col_number: col_numbers.remove(0),
                        name: tree.to_string(),
                        children: vec![],
                    });
                    name = Some(token);
                    col_numbers.push(0);
                }
            }
        }
        next = next.next(token_str);
    }
    if let Some(tree) = name {
        raw_command.insert(RawCommand::Node {
            col_number: col_numbers.remove(0),
            name: tree.to_string(),
            children: vec![],
        });
    }
    TokenStream::from_str(&raw_command.build()).unwrap()
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
            let name = name.replace("_", "-");
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
            let name = name.replace("_", "-");
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
enum RawCommand {
    Leaf {
        col_number: i32,
        name: String,
        definition: String,
    },
    Node {
        col_number: i32,
        name: String,
        children: Vec<RawCommand>,
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
                children: _,
            } => *col_number,
        }
    }

    fn _children(&self) -> Option<&Vec<RawCommand>> {
        match self {
            RawCommand::Leaf { col_number: _, name: _, definition: _ } => None,
            RawCommand::Node { col_number: _, name: _, children } => Some(children),
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
                children: _,
            } => name,
        }
    }

    fn insert(&mut self, command: RawCommand) {
        let parent_col = self.col_number();
        let child_col = command.col_number();
        if child_col % 4 != 0 {
            panic!("The number of dashes has to be dividable by four");
        }
        if child_col - 4 < parent_col {
            panic!("child column number - 1 can not be smaller than parent column number")
        }
        match self {
            RawCommand::Leaf { col_number: _, name: _, definition: _ } => {
                panic!("Can not insert a new command below a leaf command");
            },
            RawCommand::Node { col_number: _, name: _, children } => {
                if child_col - 4 == parent_col {
                    children.iter().for_each(|x| {
                        if x.name() == command.name() {
                            panic!("Duplicate Command names");
                        }
                    });
                    children.push(command);
                }
                else {
                    children.last_mut().unwrap().insert(command);
                }
            },
        };
    }

    fn build(&self) -> String {
        let name = self._children().unwrap().first().unwrap().name();
        println!("length: {}", name.len());
        let name_without_quotes = &name[1..name.len() - 1];
        format!("type CustomState = type_{}; fn parse(input: &str, state: CustomState) -> Pin<Box<dyn Future<Output = Result<Vec<parser::Output>, parser::ParseError>> + core::marker::Send>> {{let tree = vec![{}]; return parser::parse_with_tree(tree, input, state);}}",
            name_without_quotes, self._children().unwrap().iter().map(|x| x._build()).collect::<Vec<String>>().join(", ")
        )
    }

    fn _build(&self) -> String {
        match self {
            RawCommand::Leaf { col_number: _, name, definition } => format!("parser::CommandNode::Leaf{{name: {name}, command: {definition}()}}"),
            RawCommand::Node { col_number: _, name, children } => {
                format!("parser::CommandNode::Node{{name: {name}, children: vec![{}]}}", children.iter().map(|x| x._build()).collect::<Vec<String>>().join(", "))
            },
        }
    }
}