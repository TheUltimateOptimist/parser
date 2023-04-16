use serde::Serialize;
use std::{error::Error, fmt, future::{Future}, pin::Pin};

#[derive(Serialize)]
pub struct TreeNode {
    pub name: Option<String>,
    pub children: Option<Vec<TreeNode>>,
}

#[derive(Serialize)]
pub struct Table {
    pub title: String,
    pub data: Vec<Vec<String>>,
}

#[derive(Serialize)]
pub struct PomodoroTimer {
    pub duration: i32, //in seconds
    pub pause: i32,    //in seconds
    pub topic_id: i32,
    pub topic_name: String,
}

pub enum Output {
    Empty,
    Error(String),
    Text(String),
    Table(Table),
    Tree(TreeNode),
    PomodoroTimer(PomodoroTimer),
    Logout,
    Url(String),
}

impl Serialize for Output {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Serializable<T> {
            code: i32,
            content: T,
        }
        match self {
            Output::Empty => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: "",
            }),
            Output::Error(message) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: message,
            }),
            Output::Text(text) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: text,
            }),
            Output::Table(table) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: table,
            }),
            Output::Tree(tree_node) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: tree_node,
            }),
            Output::PomodoroTimer(timer) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: timer,
            }),
            Output::Logout => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: "",
            }),
            Output::Url(url) => serializer.serialize_some(&Serializable {
                code: self.code(),
                content: url,
            }),
        }
    }
}

impl Output {
    pub fn code(&self) -> i32 {
        match self {
            Output::Empty => 200,
            Output::Error(_) => 201,
            Output::Text(_) => 202,
            Output::Table(_) => 203,
            Output::Tree(_) => 204,
            Output::PomodoroTimer(_) => 205,
            Output::Logout => 206,
            Output::Url(_) => 207,
        }
    }
}

pub trait ToJson {
    fn to_json(&self) -> serde_json::Value;
}

impl ToJson for Output {
    fn to_json(&self) -> serde_json::Value {
        fn _to_json<T: Serialize>(output: &Output, content: T) -> serde_json::Value {
            serde_json::json!({"code": output.code(), "content": content})
        }
        match self {
            Output::Empty => _to_json(self, ""),
            Output::Error(message) => _to_json(self, message),
            Output::Text(text) => _to_json(self, text),
            Output::Table(table) => _to_json(self, table),
            Output::Tree(tree_node) => _to_json(self, tree_node),
            Output::PomodoroTimer(timer) => _to_json(self, timer),
            Output::Logout => _to_json(self, ""),
            Output::Url(url) => _to_json(self, url),
        }
    }
}

impl ToJson for Vec<Output> {
    fn to_json(&self) -> serde_json::Value {
        serde_json::json!(self.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>())
    }
}

#[derive(Debug)]
pub enum ParseError {
    MissingParams,
    MissingOptionalValue,
    DuplicatedOptional,
    UnrecognizedTokens(Vec<String>),
    MissmatchedTypes,
    InvalidCommand(String),
    WhiteSpaceExpected,
    NoCommand,
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::MissingParams => {
                write!(f, "You have not provided all required parameters.")
            }
            ParseError::MissingOptionalValue => write!(
                f,
                "You have not provided the value for all entered optionals."
            ),
            ParseError::DuplicatedOptional => {
                write!(f, "You have provided the same optional more than once.")
            }
            ParseError::UnrecognizedTokens(tokens) => write!(
                f,
                "The following tokens can not be recognized: {}",
                tokens
                    .iter()
                    .map(|x| format!("\"{x}\""))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            ParseError::MissmatchedTypes => write!(f, "You provided a value with the wrong Type."),
            ParseError::InvalidCommand(token) => write!(
                f,
                "No command or subcommand exists for the token: \"{token}\""
            ),
            ParseError::WhiteSpaceExpected => write!(
                f,
                "You provided a String as a value without a space after it."
            ),
            ParseError::NoCommand => write!(f, "No command could be found for the given input."),
        }
    }
}

pub fn parse_with_tree<S>(tree: Vec<CommandNode<'_, S>>, input: &str, state: S) -> Pin<Box<dyn Future<Output = Result<Vec<Output>, ParseError>> + core::marker::Send>> {
    let tokens = match tokenize(input) {
        Ok(tokens) => tokens,
        Err(err) => return Box::pin(std::future::ready(Err(err))),
    };
    let mut remaining_tree = &tree;
    for (index, token) in tokens.iter().enumerate() {
        if index == 0 && token == "help" {
            let name_tree = TreeNode { 
                name: None,
                children: Some(tree.iter().map(|x| x.name_tree()).collect()),
            };
            let title = Output::Text("All commands:".to_owned());
            return Box::pin(std::future::ready(Ok(vec![title, Output::Tree(name_tree)])));
        }
        let matching_node = remaining_tree.iter().find(|x| x.name_is(token));
        match matching_node {
            Some(CommandNode::Leaf { name: _, command }) => {
                return command.execute(tokens.into_iter().skip(index + 1).collect(), state);
            }
            Some(CommandNode::Node { name: _, children }) => remaining_tree = children,
            None => return Box::pin(std::future::ready(Err(ParseError::InvalidCommand(token.to_string())))),
        }
    }
    return Box::pin(std::future::ready(Err(ParseError::NoCommand)));
}

#[derive(Debug)]
pub enum CommandNode<'a, S> {
    Leaf {
        name: &'a str,
        command: Command<'a, S>,
    },
    Node {
        name: &'a str,
        children: Vec<CommandNode<'a, S>>,
    },
}

fn get_description(parameters: &Vec<Parameter>, optionals: &Vec<Optional>) -> Option<Vec<TreeNode>> {
    if parameters.is_empty() && optionals.is_empty() {
        return None;
    }
    let mut nodes: Vec<TreeNode> = vec![];
    for parameter in parameters {
        let name = format!("{}: {}", parameter.name, parameter.datatype.to_string());
        nodes.push(TreeNode { name: Some(name), children: None});
    }
    for optional in optionals {
        let name = format!("--{}: {} = {}", optional.name, optional.datatype.to_string(), optional.default);
        nodes.push(TreeNode { name: Some(name), children: None });
    }
    Some(nodes)
}

impl<S> CommandNode<'_, S> {
    fn name_is(&self, other: &str) -> bool {
        match self {
            CommandNode::Leaf { name, command: _ } => *name == other,
            CommandNode::Node { name, children: _ } => *name == other,
        }
    }

    fn name_tree(&self) -> TreeNode {
        match self {
            CommandNode::Leaf { name, command } => {
                TreeNode {
                name: Some(name.to_string()),
                children: get_description(command.get_params(), command.get_optionals())
            }},
            CommandNode::Node { name, children } => TreeNode {
                name: Some(name.to_string()),
                children: Some(children.iter().map(|x| x.name_tree()).collect())
            },
        }
    }
}



#[derive(Debug)]
pub struct Command<'a, S> {
    pub params: Vec<Parameter<'a>>,
    pub optionals: Vec<Optional<'a>>,
    pub execute: fn(Vec<String>, S) -> Pin<Box<dyn Future<Output = Result<Vec<Output>, ParseError>> + core::marker::Send>>,
}

impl<S> Command<'_, S> {
    fn get_params(&self) -> &Vec<Parameter> {
        return &self.params;
    }

    fn get_optionals(&self) -> &Vec<Optional> {
        return &self.optionals;
    }
    fn _extract_arguments(&self, tokens: Vec<String>) -> Result<Vec<String>, ParseError> {
        println!("tokens: {:?}", tokens);
        let mut arguments: Vec<String> = Vec::new();
        println!("params: {:?}", self.params);
        for (index, _) in self.params.iter().enumerate() {
            arguments.push(
                tokens
                    .get(index)
                    .ok_or(ParseError::MissingParams)?
                    .to_string(),
            )
        }
        let mut optional_tokens = tokens
            .iter()
            .skip(self.params.len())
            .collect::<Vec<&String>>()
            .clone();
        println!("optionals: {:?}", self.optionals);
        for optional in &self.optionals {
            let pairs = optional_tokens
                .iter()
                .enumerate()
                .filter(|(_, x)| optional.name_is(x))
                .map(|(i, x)| (i, *x))
                .collect::<Vec<(usize, &String)>>();
            if pairs.len() > 1 {
                return Err(ParseError::DuplicatedOptional);
            };
            let argument = match pairs.get(0) {
                Some((index, _)) => match optional.datatype {
                    DataType::bool => {
                        optional_tokens.remove(*index);
                        "true".to_owned()
                    }
                    _ => {
                        let value = *optional_tokens
                            .get(index + 1)
                            .ok_or(ParseError::MissingOptionalValue)?;
                        optional_tokens.remove(index + 1);
                        optional_tokens.remove(*index);
                        value.to_string()
                    }
                },
                None => optional.default.to_owned(),
            };
            arguments.push(argument);
        }
        if !optional_tokens.is_empty() {
            let owned: Vec<String> = optional_tokens.iter().map(|x| x.to_string()).collect();
            return Err(ParseError::UnrecognizedTokens(owned));
        }
        return Ok(arguments);
    }

    fn execute(&self, tokens: Vec<String>, state: S) -> Pin<Box<dyn Future<Output = Result<Vec<Output>, ParseError>> + core::marker::Send>> {
        let arguments = match self._extract_arguments(tokens){
            Ok(arguments) => arguments,
            Err(err) => return Box::pin(std::future::ready(Err(err))),
        };
        (self.execute)(arguments, state)
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum DataType {
    u8,
    u16,
    u32,
    u64,
    u128,
    i8,
    i16,
    i32,
    i64,
    i128,
    f32,
    f64,
    String,
    bool,
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::u8 => write!(f, "u8"),
            DataType::u16 => write!(f, "u16"),
            DataType::u32 => write!(f, "u32"),
            DataType::u64 => write!(f, "u64"),
            DataType::u128 => write!(f, "u128"),
            DataType::i8 => write!(f, "i8"),
            DataType::i16 => write!(f, "i16"),
            DataType::i32 => write!(f, "i32"),
            DataType::i64 => write!(f, "i64"),
            DataType::i128 => write!(f, "i128"),
            DataType::f32 => write!(f, "f32"),
            DataType::f64 => write!(f, "f64"),
            DataType::String => write!(f, "String"),
            DataType::bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug)]
pub struct Parameter<'a> {
    pub name: &'a str,
    pub datatype: DataType,
}

#[derive(Debug)]
pub struct Optional<'a> {
    pub name: &'a str,
    pub default: String,
    pub datatype: DataType,
}

impl Optional<'_> {
    fn name_is(&self, other: &str) -> bool {
        return format!("--{}", self.name) == other;
    }
}

pub fn tokenize(input: &str) -> Result<Vec<String>, ParseError> {
    let mut parts: Vec<String> = vec![];
    let mut single_quote = false;
    let mut double_quote = false;
    let mut whitespace_expected = false;
    let mut word = String::new();
    for char in input.trim().chars() {
        if char == ' ' {
            if single_quote || double_quote {
                word.push(char);
                continue;
            }
            if word.len() > 0 {
                parts.push(word.clone());
                word = "".to_string();
            }
            whitespace_expected = false;
            continue;
        }
        if whitespace_expected {
            return Err(ParseError::WhiteSpaceExpected);
        }
        if char == '"' {
            if single_quote {
                word.push(char);
                continue;
            }
            if double_quote {
                parts.push(word.clone());
                word = "".to_string();
                double_quote = false;
                whitespace_expected = true;
                continue;
            }
            double_quote = true;
            continue;
        }
        if char == '\'' {
            if double_quote {
                word.push(char);
                continue;
            }
            if single_quote {
                single_quote = false;
                whitespace_expected = true;
                parts.push(word.clone());
                word = "".to_string();
                continue;
            }
            single_quote = true;
            continue;
        }
        word.push(char);
    }
    if word.len() > 0 {
        parts.push(word.clone());
    }
    return Ok(parts);
}