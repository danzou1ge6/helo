use crate::byte_code;

pub struct StrChunk(String);

impl StrChunk {
    pub fn new() -> Self {
        Self(String::new())
    }
    pub fn push(&mut self, s: &str) -> Option<byte_code::StrAddr> {
        let addr = self.0.len();
        self.0.push_str(s);
        self.0.push('\0');
        u32::try_from(addr).map(|x| byte_code::StrAddr(x)).ok()
    }
}

pub struct Symbols {
    f: Vec<(byte_code::FunctionAddr, byte_code::StrAddr)>,
}

impl Symbols {
    pub fn new(function_names: Vec<(byte_code::FunctionAddr, byte_code::StrAddr)>) -> Self {
        Self { f: function_names }
    }
}

pub struct Executable {
    chunk: byte_code::Chunk,
    entry: byte_code::FunctionAddr,
    str_chunk: StrChunk,
    symbols: Symbols,
}

impl Executable {
    pub fn new(
        chunk: byte_code::Chunk,
        entry: byte_code::FunctionAddr,
        str_chunk: StrChunk,
        symbols: Symbols,
    ) -> Self {
        Self {
            chunk,
            entry,
            str_chunk,
            symbols,
        }
    }
}
