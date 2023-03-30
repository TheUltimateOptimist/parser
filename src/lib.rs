pub fn parse_with_tree(tree: Vec<CommandNode>, input: &str) {
    let tokens = tokenize(input);
        let mut node = &CommandNode::Node {
            name: "root",
            children: tree,
        };
        for (index, token) in tokens.iter().enumerate() {
            match node {
                CommandNode::Leaf { name, command } => {
                    if name == token {
                        command.execute(tokens.iter().skip(index).collect());
                    } else {
                        panic!("could not find matching command for leaf");
                    }
                }
                CommandNode::Node { name: _, children } => {
                    let matching_node = children.iter().find(|x| x.name_is(token));
                    match matching_node {
                        Some(new_node) => node = new_node,
                        None => panic!("could not find matching command"),
                    }
                }
            }
        }
}

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

pub struct Command<'a> {
    pub params: Vec<Parameter<'a>>,
    pub optionals: Vec<Optional<'a>>,
    pub execute: fn(Vec<DataType>),
}

impl Command<'_> {
    fn _extract_data_types(&self, tokens: Vec<&String>) -> Vec<DataType> {
        let mut data_types = Vec::new();
        self.params.iter().enumerate().for_each(|(index, x)| {
            data_types.push(DataType::from_param(
                x,
                tokens
                    .get(index)
                    .expect("not all parameters have been specified"),
            ))
        });
        let mut optional_tokens = tokens
            .iter()
            .skip(self.params.len())
            .collect::<Vec<&&String>>()
            .clone();
        self.optionals.iter().for_each(|optional| {
            let pair = optional_tokens
                .iter()
                .enumerate()
                .find(|(_, x)| optional.name_is(x));
            let data_type = match pair {
                Some((index, _)) => match optional {
                    Optional::bool(_) => {
                        optional_tokens.remove(index);
                        DataType::from_optional(optional, None)
                    }
                    _ => {
                        let value = optional_tokens
                            .get(index + 1)
                            .expect("value has to be specified for optional")
                            .clone();
                        optional_tokens.remove(index + 1);
                        optional_tokens.remove(index);
                        DataType::from_optional(optional, Some(value))
                    }
                },
                None => DataType::from_optional(optional, None),
            };
            data_types.push(data_type);
        });
        return data_types;
    }

    fn execute(&self, tokens: Vec<&String>) {
        let data_types = self._extract_data_types(tokens);
        (self.execute)(data_types);
    }
}

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
    fn from_param(param: &Parameter, input: &str) -> DataType {
        let message = "parameter conversion failed";
        match param {
            Parameter::u8(_) => DataType::u8(input.parse().expect(message)),
            Parameter::u16(_) => DataType::u16(input.parse().expect(message)),
            Parameter::u32(_) => DataType::u32(input.parse().expect(message)),
            Parameter::u64(_) => DataType::u64(input.parse().expect(message)),
            Parameter::u128(_) => DataType::u128(input.parse().expect(message)),
            Parameter::i8(_) => DataType::i8(input.parse().expect(message)),
            Parameter::i16(_) => DataType::i16(input.parse().expect(message)),
            Parameter::i32(_) => DataType::i32(input.parse().expect(message)),
            Parameter::i64(_) => DataType::i64(input.parse().expect(message)),
            Parameter::i128(_) => DataType::i128(input.parse().expect(message)),
            Parameter::f32(_) => DataType::f32(input.parse().expect(message)),
            Parameter::f64(_) => DataType::f64(input.parse().expect(message)),
            Parameter::String(_) => DataType::String(input.parse().expect(message)),
        }
    }

    fn from_optional(optional: &Optional, input: Option<&str>) -> DataType {
        let message = "optional conversion failed";
        match optional {
            Optional::u8(_, default) => match input {
                Some(value) => DataType::u8(value.parse().expect(message)),
                None => DataType::u8(*default),
            },
            Optional::u16(_, default) => match input {
                Some(value) => DataType::u16(value.parse().expect(message)),
                None => DataType::u16(*default),
            },
            Optional::u32(_, default) => match input {
                Some(value) => DataType::u32(value.parse().expect(message)),
                None => DataType::u32(*default),
            },
            Optional::u64(_, default) => match input {
                Some(value) => DataType::u64(value.parse().expect(message)),
                None => DataType::u64(*default),
            },
            Optional::u128(_, default) => match input {
                Some(value) => DataType::u128(value.parse().expect(message)),
                None => DataType::u128(*default),
            },
            Optional::i8(_, default) => match input {
                Some(value) => DataType::i8(value.parse().expect(message)),
                None => DataType::i8(*default),
            },
            Optional::i16(_, default) => match input {
                Some(value) => DataType::i16(value.parse().expect(message)),
                None => DataType::i16(*default),
            },
            Optional::i32(_, default) => match input {
                Some(value) => DataType::i32(value.parse().expect(message)),
                None => DataType::i32(*default),
            },
            Optional::i64(_, default) => match input {
                Some(value) => DataType::i64(value.parse().expect(message)),
                None => DataType::i64(*default),
            },
            Optional::i128(_, default) => match input {
                Some(value) => DataType::i128(value.parse().expect(message)),
                None => DataType::i128(*default),
            },
            Optional::f32(_, default) => match input {
                Some(value) => DataType::f32(value.parse().expect(message)),
                None => DataType::f32(*default),
            },
            Optional::f64(_, default) => match input {
                Some(value) => DataType::f64(value.parse().expect(message)),
                None => DataType::f64(*default),
            },
            Optional::String(_, default) => match input {
                Some(value) => DataType::String(value.parse().expect(message)),
                None => DataType::String(default.to_string()),
            },
            Optional::bool(_) => match input {
                Some(value) => DataType::bool(value.parse().expect(message)),
                None => DataType::bool(false),
            },
        }
    }
}

impl From<&DataType> for u8 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u8(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u16 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u16(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u128 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u128(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i8 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i8(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i16 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i16(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i128 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i128(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for f32 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for f64 {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for String {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::String(v) => (*v.clone()).to_string(),
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for bool {
    fn from(value: &DataType) -> Self {
        match value {
            DataType::bool(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

pub fn tokenize(input: &str) -> Vec<String> {
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
            panic!("expected whitespace");
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
    return parts;
}
