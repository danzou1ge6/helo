use crate::{byte_code, executable, mem};
use executable::Executable;

struct CallFrame {
    reg_offset: usize,
}

pub fn vm(exe: &Executable) -> Result<(), ()> {
    unimplemented!()
}
