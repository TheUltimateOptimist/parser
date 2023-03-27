pub enum CommandNode<'a> {
    Leaf{name: &'a str, command: Command<'a>},
    Node{name: &'a str, children: Vec<CommandNode<'a>>},
}

pub struct Command<'a> {
    pub params: Vec<Parameter<'a>>,
    pub optionals: Vec<Optional<'a>>,
    pub execute: fn(Vec<DataType>),
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
    string(String),
    boolean(bool)
}

impl From<&DataType> for u8{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u8(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u16{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u16(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u32{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u64{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for u128{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::u128(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i8{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i8(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i16{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i16(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i32{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i64{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for i128{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::i128(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for f32{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f32(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for f64{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::f64(v) => *v,
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for String{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::string(v) => (*v.clone()).to_string(),
            _ => panic!("invalid"),
        }
    }
}

impl From<&DataType> for bool{
    fn from(value: &DataType) -> Self {
        match value {
            DataType::boolean(v) => *v,
            _ => panic!("invalid"),
        }
    }
}


pub fn tokenize(input: String) -> Vec<String>{
    let mut parts: Vec<String> = vec![];
    let mut single_quote = false;
    let mut double_quote = false;
    let mut whitespace_expected = false;
    let mut word = String::new();
    for char in input.trim().chars(){
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



