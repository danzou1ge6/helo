use crate::vm::VmState;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum RunTimeError_<T> {
    #[error("Out of Menory")]
    OutOfMemory(T),
    #[error("{}", .0)]
    Panic(Panic, T),
    #[error("Bad Op Code in Program: {}", .0)]
    BadOpCode(u8, T),
    #[error("Zero Division")]
    ZeroDivision(T),
    #[error("Exponent Out of Range: Should be a u32, got {}", .0)]
    IntExponentOutOfRange(i64, T),
    #[error("Exponent Out of Range: Should be a i32, got {}", .0)]
    FloatExponentOutOfRange(i64, T),
    #[error("Called `string_head` or `string_tail` on an empty string")]
    EmptyString(T),
    #[error("IO Error of {:?}", .0)]
    Io(std::io::Error, T),
    #[error("Hang")]
    Hang(T),
}

impl From<std::io::Error> for RunTimeError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value, ())
    }
}

#[derive(Error, Debug)]
#[error("Panic at characters {}-{} in file {}: {}", self.file, self.span.0, self.span.1, self.msg)]
pub struct Panic {
    pub file: String,
    pub span: (usize, usize),
    pub msg: String,
}

pub type RunTimeError = RunTimeError_<()>;
pub type Exception = RunTimeError_<VmState>;

impl RunTimeError {
    pub fn to_exception(self, vm_state: VmState) -> Exception {
        use RunTimeError_::*;
        match self {
            OutOfMemory(_) => OutOfMemory(vm_state),
            Panic(p, _) => Panic(p, vm_state),
            BadOpCode(c, ..) => BadOpCode(c, vm_state),
            ZeroDivision(_) => ZeroDivision(vm_state),
            IntExponentOutOfRange(x, _) => IntExponentOutOfRange(x, vm_state),
            FloatExponentOutOfRange(x, _) => FloatExponentOutOfRange(x, vm_state),
            EmptyString(_) => EmptyString(vm_state),
            Io(e, _) => Io(e, vm_state),
            Hang(_) => Hang(vm_state),
        }
    }
}
