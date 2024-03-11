use crate::byte_code::{self, ToBytes};
use std::io;
use std::io::{Read, Write};

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
    pub fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        for (c, s) in self.f.iter() {
            w.write_all(&c.0.to_le_bytes())?;
            w.write_all(&s.0.to_le_bytes())?;
        }
        Ok(())
    }
    pub fn read_from(r: &mut impl Read, len: u32) -> io::Result<Self> {
        let mut buf = [0; 4];
        let mut f = Vec::new();

        for _ in 0..len {
            r.read_exact(&mut buf)?;
            let c = u32::from_le_bytes(buf);
            r.read_exact(&mut buf)?;
            let s = u32::from_le_bytes(buf);
            f.push((byte_code::Addr(c), byte_code::StrAddr(s)));
        }

        Ok(Self { f })
    }
}

pub struct Executable {
    pub(crate) chunk: byte_code::Chunk,
    pub(crate) entry: byte_code::Addr,
    pub(crate) str_chunk: StrChunk,
    pub(crate) symbols: Symbols,
}

struct Header {
    /// Size of code chunk in bytes
    chunk_len: u32,
    /// Entry address of code in bytes
    entry: u32,
    /// Size of string data in bytes
    str_len: u32,
    /// Size of symbol table in entries
    symbol_table_len: u32,
}

impl Header {
    pub fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_all(&self.chunk_len.to_bytes())?;
        w.write_all(&self.entry.to_bytes())?;
        w.write_all(&self.str_len.to_bytes())?;
        w.write_all(&self.symbol_table_len.to_bytes())
    }
    pub fn read_from(r: &mut impl Read) -> io::Result<Self> {
        let mut buf = [0; 4];
        r.read_exact(&mut buf)?;
        let chunk_len = u32::from_le_bytes(buf);
        r.read_exact(&mut buf)?;
        let entry = u32::from_le_bytes(buf);
        r.read_exact(&mut buf)?;
        let str_len = u32::from_le_bytes(buf);
        r.read_exact(&mut buf)?;
        let symbol_table_len = u32::from_le_bytes(buf);

        Ok(Self {
            chunk_len,
            entry,
            str_len,
            symbol_table_len,
        })
    }
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
    fn header(&self) -> Header {
        // None of the field can overflow u32, as chunk size is limited to u32
        Header {
            chunk_len: self.chunk.len() as u32,
            entry: self.entry.0,
            str_len: self.str_chunk.0.len() as u32,
            symbol_table_len: self.symbols.f.len() as u32,
        }
    }
    pub fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.header().write_to(w)?;
        w.write_all(&self.chunk.code)?;
        w.write_all(self.str_chunk.0.as_bytes())?;
        self.symbols.write_to(w)
    }
    pub fn read_from(r: &mut impl Read) -> io::Result<Self> {
        let header = Header::read_from(r)?;

        let mut code = vec![0; header.chunk_len as usize];
        r.read_exact(&mut code[0..header.chunk_len as usize])?;
        let chunk = byte_code::Chunk { code };

        let mut string = vec![0; header.str_len as usize];
        r.read_exact(&mut string[0..header.str_len as usize])?;
        let str_chunk = String::from_utf8_lossy(&string).to_string();
        let str_chunk = StrChunk(str_chunk);

        let symbols = Symbols::read_from(r, header.symbol_table_len)?;

        Ok(Self {
            chunk,
            entry: byte_code::Addr(header.entry),
            str_chunk,
            symbols,
        })
    }
}
