use std::{error::Error, fmt, str::FromStr};

#[derive(Debug)]
pub enum ParseError {
    MissingParams,
    MissingOptionalValue,
    DuplicatedOptional,
    UnrecognizedTokens(Vec<String>),
    MissmatchedTypes,
    InvalidCommand(String),
    WhiteSpaceExpected,
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::MissingParams => {
                write!(f, "You have not provided all required parameters.")
            }
            ParseError::MissingOptionalValue => write!(f, "You have not provided the value for all entered optionals."),
            ParseError::DuplicatedOptional => write!(f, "You have provided the same optional more than once."),
            ParseError::UnrecognizedTokens(tokens) => write!(f, "The following tokens can not be recognized: {}",
            tokens.iter().map(|x| format!("\"{x}\"")).collect::<Vec<String>>().join(", ")
            ),
            ParseError::MissmatchedTypes => write!(f, "You provided a value with the wrong Type."),
            ParseError::InvalidCommand(token) => write!(f, "No command or subcommand exists for the token: \"{token}\""), 
            ParseError::WhiteSpaceExpected => write!(f, "You provided a String as a value without a space after it.")
        }
    }
}

pub fn parse_with_tree(tree: Vec<CommandNode>, input: &str) -> Result<(), ParseError>{
    let tokens = tokenize(input)?;
    let mut remaining_tree = &tree;
    for (index, token) in tokens.iter().enumerate() {
        let matching_node = remaining_tree.iter().find(|x| x.name_is(token));
        match matching_node {
            Some(CommandNode::Leaf { name: _, command }) => {
                command.execute(tokens.iter().skip(index + 1).collect())?
            }
            Some(CommandNode::Node { name: _, children }) => remaining_tree = children,
            None => return Err(ParseError::InvalidCommand(token.to_string())),
        }
    }
    Ok(())
}

#[derive(Debug)]
pub enum CommandNode<'a> {
    Leaf {
        name: &'a str,
        command: Command<'a>,
    },
    Node {
        name: &'a str,
        children: Vec<CommandNode<'a>>,
    },
}

impl CommandNode<'_> {
    fn name_is(&self, other: &str) -> bool {
        match self {
            CommandNode::Leaf { name, command: _ } => *name == other,
            CommandNode::Node { name, children: _ } => *name == other,
        }
    }
}

#[derive(Debug)]
pub struct Command<'a> {
    pub params: Vec<Parameter<'a>>,
    pub optionals: Vec<Optional<'a>>,
    pub execute: fn(Vec<DataType>),
}

impl Command<'_> {
    fn _extract_data_types(&self, tokens: Vec<&String>) -> Result<Vec<DataType>, ParseError> {
        println!("tokens: {:?}", tokens);
        let mut data_types = Vec::new();
        println!("params: {:?}", self.params);
        for (index, param) in self.params.iter().enumerate() {
            data_types.push(
                DataType::from_param(
                param,
                tokens.get(index).ok_or(ParseError::MissingParams)?,
                )?
            )
        }
        let mut optional_tokens = tokens
            .iter()
            .skip(self.params.len())
            .collect::<Vec<&&String>>()
            .clone();
        println!("optionals: {:?}", self.optionals);
        for optional in &self.optionals {
            let pairs = optional_tokens
                .iter()
                .enumerate()
                .filter(|(_, x)| optional.name_is(x))
                .map(|(i, x)| (i, *x))
                .collect::<Vec<(usize, &&String)>>();
            if pairs.len() > 1 {
                return Err(ParseError::DuplicatedOptional);
            };
            let data_type = match pairs.get(0) {
                Some((index, _)) => match optional {
                    Optional::bool(_) => {
                        optional_tokens.remove(*index);
                        DataType::bool(true)
                    }
                    _ => {
                        let value = *optional_tokens
                            .get(index + 1)
                            .ok_or(ParseError::MissingOptionalValue)?;
                        optional_tokens.remove(index + 1);
                        optional_tokens.remove(*index);
                        DataType::from_optional(optional, Some(value))?
                    }
                },
                None => DataType::from_optional(optional, None)?,
            };
            data_types.push(data_type);
        }
        if !optional_tokens.is_empty() {
            let owned: Vec<String> = optional_tokens.iter().map(|x| x.to_string()).collect();
            return Err(ParseError::UnrecognizedTokens(owned));
        }
        return Ok(data_types);
    }

    fn execute(&self, tokens: Vec<&String>) -> Result<(), ParseError> {
        let data_types = self._extract_data_types(tokens)?;
        (self.execute)(data_types);
        Ok(())
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Optional<'a> {
    u8(&'a str, u8),
    u16(&'a str, u16),
    u32(&'a str, u32),
    u64(&'a str, u64),
    u128(&'a str, u128),
    i8(&'a str, i8),
    i16(&'a str, i16),
    i32(&'a str, i32),
    i64(&'a str, i64),
    i128(&'a str, i128),
    f32(&'a str, f32),
    f64(&'a str, f64),
    String(&'a str, &'a str),
    bool(&'a str),
}

impl Optional<'_> {
    fn name_is(&self, token: &str) -> bool {
        match self {
            Optional::u8(name, _) => format!("--{}", name) == token,
            Optional::u16(name, _) => format!("--{}", name) == token,
            Optional::u32(name, _) => format!("--{}", name) == token,
            Optional::u64(name, _) => format!("--{}", name) == token,
            Optional::u128(name, _) => format!("--{}", name) == token,
            Optional::i8(name, _) => format!("--{}", name) == token,
            Optional::i16(name, _) => format!("--{}", name) == token,
            Optional::i32(name, _) => format!("--{}", name) == token,
            Optional::i64(name, _) => format!("--{}", name) == token,
            Optional::i128(name, _) => format!("--{}", name) == token,
            Optional::f32(name, _) => format!("--{}", name) == token,
            Optional::f64(name, _) => format!("--{}", name) == token,
            Optional::String(name, _) => format!("--{}", name) == token,
            Optional::bool(name) => format!("--{}", name) == token,
        }
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Parameter<'a> {
    u8(&'a str),
    u16(&'a str),
    u32(&'a str),
    u64(&'a str),
    u128(&'a str),
    i8(&'a str),
    i16(&'a str),
    i32(&'a str),
    i64(&'a str),
    i128(&'a str),
    f32(&'a str),
    f64(&'a str),
    String(&'a str),
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum DataType {
    u8(u8),
    u16(u16),
    u32(u32),
    u64(u64),
    u128(u128),
    i8(i8),
    i16(i16),
    i32(i32),
    i64(i64),
    i128(i128),
    f32(f32),
    f64(f64),
    String(String),
    bool(bool),
}

impl DataType {
    fn _parse_input<T: FromStr>(s: &str) -> Result<T, ParseError> {
        s.parse::<T>().map_err(|_| ParseError::MissmatchedTypes)
    }
    fn from_param(param: &Parameter, input: &str) -> Result<DataType, ParseError> {
        match param {
            Parameter::u8(_) => Ok(DataType::u8(Self::_parse_input(input)?)),
            Parameter::u16(_) => Ok(DataType::u16(Self::_parse_input(input)?)),
            Parameter::u32(_) => Ok(DataType::u32(Self::_parse_input(input)?)),
            Parameter::u64(_) => Ok(DataType::u64(Self::_parse_input(input)?)),
            Parameter::u128(_) => Ok(DataType::u128(Self::_parse_input(input)?)),
            Parameter::i8(_) => Ok(DataType::i8(Self::_parse_input(input)?)),
            Parameter::i16(_) => Ok(DataType::i16(Self::_parse_input(input)?)),
            Parameter::i32(_) => Ok(DataType::i32(Self::_parse_input(input)?)),
            Parameter::i64(_) => Ok(DataType::i64(Self::_parse_input(input)?)),
            Parameter::i128(_) => Ok(DataType::i128(Self::_parse_input(input)?)),
            Parameter::f32(_) => Ok(DataType::f32(Self::_parse_input(input)?)),
            Parameter::f64(_) => Ok(DataType::f64(Self::_parse_input(input)?)),
            Parameter::String(_) => Ok(DataType::String(input.to_string())),
        }
    }

    fn from_optional(optional: &Optional, input: Option<&str>) -> Result<DataType, ParseError> {
        match optional {
            Optional::u8(_, default) => match input {
                Some(value) => Ok(DataType::u8(Self::_parse_input(value)?)),
                None => Ok(DataType::u8(*default)),
            },
            Optional::u16(_, default) => match input {
                Some(value) => Ok(DataType::u16(Self::_parse_input(value)?)),
                None => Ok(DataType::u16(*default)),
            },
            Optional::u32(_, default) => match input {
                Some(value) => Ok(DataType::u32(Self::_parse_input(value)?)),
                None => Ok(DataType::u32(*default)),
            },
            Optional::u64(_, default) => match input {
                Some(value) => Ok(DataType::u64(Self::_parse_input(value)?)),
                None => Ok(DataType::u64(*default)),
            },
            Optional::u128(_, default) => match input {
                Some(value) => Ok(DataType::u128(Self::_parse_input(value)?)),
                None => Ok(DataType::u128(*default)),
            },
            Optional::i8(_, default) => match input {
                Some(value) => Ok(DataType::i8(Self::_parse_input(value)?)),
                None => Ok(DataType::i8(*default)),
            },
            Optional::i16(_, default) => match input {
                Some(value) => Ok(DataType::i16(Self::_parse_input(value)?)),
                None => Ok(DataType::i16(*default)),
            },
            Optional::i32(_, default) => match input {
                Some(value) => Ok(DataType::i32(Self::_parse_input(value)?)),
                None => Ok(DataType::i32(*default)),
            },
            Optional::i64(_, default) => match input {
                Some(value) => Ok(DataType::i64(Self::_parse_input(value)?)),
                None => Ok(DataType::i64(*default)),
            },
            Optional::i128(_, default) => match input {
                Some(value) => Ok(DataType::i128(Self::_parse_input(value)?)),
                None => Ok(DataType::i128(*default)),
            },
            Optional::f32(_, default) => match input {
                Some(value) => Ok(DataType::f32(Self::_parse_input(value)?)),
                None => Ok(DataType::f32(*default)),
            },
            Optional::f64(_, default) => match input {
                Some(value) => Ok(DataType::f64(Self::_parse_input(value)?)),
                None => Ok(DataType::f64(*default)),
            },
            Optional::String(_, default) => match input {
                Some(value) => Ok(DataType::String(value.to_string())),
                None => Ok(DataType::String(default.to_string())),
            },
            Optional::bool(_) => match input {
                Some(value) => Ok(DataType::bool(Self::_parse_input(value)?)),
                None => Ok(DataType::bool(false)),
            },
        }
    }
}

impl From<&DataType> for u8 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u8(v) => *v,
            _ => panic!("Error: Tried to convert to u8 from a non-u8 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for u16 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u16(v) => *v,
            _ => panic!("Error: Tried to convert to u16 from a non-u16 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for u32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u32(v) => *v,
            _ => panic!("Error: Tried to convert to u32 from a non-u32 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for u64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u64(v) => *v,
            _ => panic!("Error: Tried to convert to u64 from a non-u64 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for u128 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u128(v) => *v,
            _ => panic!("Error: Tried to convert to u128 from a non-u128 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for i8 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i8(v) => *v,
            _ => panic!("Error: Tried to convert to i8 from a non-i8 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for i16 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i16(v) => *v,
            _ => panic!("Error: Tried to convert to i16 from a non-i16 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for i32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i32(v) => *v,
            _ => panic!("Error: Tried to convert to i32 from a non-i32 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for i64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i64(v) => *v,
            _ => panic!("Error: Tried to convert to i64 from a non-i64 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for i128 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i128(v) => *v,
            _ => panic!("Error: Tried to convert to i128 from a non-i128 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for f32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f32(v) => *v,
            _ => panic!("Error: Tried to convert to f32 from a non-f32 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for f64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f64(v) => *v,
            _ => panic!("Error: Tried to convert to f64 from a non-f64 DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for String {
    fn from(value: &DataType) -> Self {
        println!("datatype: {:?}", value);
        match value {
            DataType::String(v) => (*v.clone()).to_string(),
            _ => panic!("Error: Tried to convert to String from a non-String DataType. This implies an error in the source code. It should never happen."),
        }
    }
}

impl From<&DataType> for bool {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::bool(v) => *v,
            _ => panic!("Error: Tried to convert to bool from a non-bool DataType. This implies an error in the source code. It should never happen."),
        }
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
