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
    pub fn read(&self, addr: byte_code::StrAddr) -> &str {
        let addr = addr.0 as usize;
        let len = self.0[addr..].find('\0').unwrap();
        &self.0[addr..addr + len]
    }
}

#[derive(Debug)]
pub struct Symbols {
    f: Vec<(byte_code::Addr, byte_code::StrAddr)>,
}

impl Symbols {
    pub fn new(function_names: Vec<(byte_code::Addr, byte_code::StrAddr)>) -> Self {
        Self { f: function_names }
    }
    pub fn find(&self, addr: byte_code::Addr) -> byte_code::StrAddr {
        self.f.iter().find(|(f_addr, _)| *f_addr == addr).unwrap().1
    }
    pub fn try_find(&self, addr: byte_code::Addr) -> Option<byte_code::StrAddr> {
        self.f
            .iter()
            .find(|(f_addr, _)| *f_addr == addr)
            .map(|(_, name_addr)| name_addr)
            .copied()
    }
}

pub struct Executable {
    pub(crate) chunk: byte_code::Chunk,
    pub(crate) entry: byte_code::Addr,
    pub(crate) str_chunk: StrChunk,
    pub(crate) symbols: Symbols,
}

impl Executable {
    pub fn new(
        chunk: byte_code::Chunk,
        entry: byte_code::Addr,
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
