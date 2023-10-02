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

pub struct Symbols {}

pub struct Executable {
    chunk: byte_code::Chunk,
    str_chunk: String,
    symbols: Symbols,
}
