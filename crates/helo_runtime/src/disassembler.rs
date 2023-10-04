use crate::builtins::Builtins;
use crate::{byte_code, executable};

use tabled::{Table, Tabled};

#[derive(Tabled)]
struct Row {
    #[tabled(display_with = "hex_ref")]
    addr: u32,
    op_code: String,
    to: String,
    args: String,
    extra: String,
}

struct RowIter<'c> {
    exe: &'c executable::Executable,
    ip: u32,
}

impl<'c> RowIter<'c> {
    fn new(exe: &'c executable::Executable) -> Self {
        Self { exe, ip: 0 }
    }
}

fn hex(x: u32) -> String {
    format!("{:#010x}", x)
}
fn hex_ref(x: &u32) -> String {
    format!("{:#010x}", x)
}

struct RegisterArray(Vec<byte_code::RegisterId>);

impl<const N: usize> From<[byte_code::RegisterId; N]> for RegisterArray {
    fn from(value: [byte_code::RegisterId; N]) -> Self {
        Self(value.into_iter().collect())
    }
}

impl std::fmt::Display for RegisterArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for register_id in self.0.iter() {
            write!(f, "{},", register_id)?;
        }
        write!(f, "]")
    }
}

const STR_TRUNC_LEN: usize = 128;

pub fn trunc_str(s: &str) -> &str {
    if s.len() > STR_TRUNC_LEN {
        &s[0..STR_TRUNC_LEN]
    } else {
        s
    }
}

impl<'c> Iterator for RowIter<'c> {
    type Item = Vec<Row>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.ip as usize == self.exe.chunk.len() {
            return None;
        }

        let reader = self.exe.chunk.fetch(self.ip);
        let (reader, op_code) = reader.read::<byte_code::OpCode, _>();

        let es = || String::new();
        let mk_row = |to: byte_code::RegisterId, args, extra| Row {
            addr: self.ip as u32,
            op_code: op_code.to_string(),
            to: to.to_string(),
            args,
            extra,
        };
        let mk_row1 = |args, extra| Row {
            addr: self.ip as u32,
            op_code: op_code.to_string(),
            to: es(),
            args,
            extra,
        };
        let mk_info_row = |addr, args, extra| Row {
            addr,
            op_code: es(),
            to: es(),
            args,
            extra,
        };

        macro_rules! arm_apply {
            ($f:ident, $rows:ident) => {{
                let (ret, callee, args) = reader.$f();
                $rows.push(mk_row(
                    ret,
                    format!("{}, {}", callee, RegisterArray::from(args)),
                    es(),
                ));
            }};
        }

        macro_rules! arm_apply_with_name {
            ($f:ident, $rows:ident) => {{
                let (ret, callee, args) = reader.$f();
                let name = self.exe.str_chunk.read(self.exe.symbols.find(callee));
                $rows.push(mk_row(
                    ret,
                    format!("{}, {}", callee, RegisterArray::from(args)),
                    name.to_string(),
                ));
            }};
        }

        macro_rules! arm_call_builtin {
            ($f:ident, $rows:ident) => {{
                let (ret, callee, args) = reader.$f();
                let name = Builtins::name_by_id(callee);
                $rows.push(mk_row(
                    ret,
                    format!("{}, {}", callee, RegisterArray::from(args)),
                    name.to_string(),
                ));
            }};
        }

        macro_rules! arm_apply_many {
            ($f:ident, $rows:ident) => {{
                let (ret, callee, cnt) = reader.$f();
                let registers = (0..cnt)
                    .map(|i| {
                        self.exe
                            .chunk
                            .fetch(self.ip + 8 + i as u32)
                            .read::<byte_code::RegisterId, _>()
                            .1
                    })
                    .collect::<Vec<_>>();
                $rows.push(mk_row(
                    ret,
                    format!("{}, {}", callee, cnt),
                    RegisterArray(registers).to_string(),
                ));
                self.ip += (cnt.div_ceil(8) * 8) as u32;
            }};
        }

        macro_rules! arm_apply_many_with_name {
            ($f:ident, $rows:ident) => {{
                let (ret, callee, cnt) = reader.$f();
                let name = self.exe.str_chunk.read(self.exe.symbols.find(callee));
                let registers = (0..cnt)
                    .map(|i| {
                        self.exe
                            .chunk
                            .fetch(self.ip + 8 + i as u32)
                            .read::<byte_code::RegisterId, _>()
                            .1
                    })
                    .collect::<Vec<_>>();
                $rows.push(mk_row(
                    ret,
                    format!("{}, {}", callee, cnt),
                    format!("{}, {}", name, RegisterArray(registers)),
                ));
                self.ip += (cnt.div_ceil(8) * 8) as u32;
            }};
        }

        macro_rules! arm_push {
            ($f:ident, $rows:ident) => {{
                let (to, args) = reader.$f();
                $rows.push(mk_row1(
                    format!("{}, {}", to, RegisterArray::from(args)),
                    es(),
                ));
            }};
        }

        let mut rows = if let Some(f_name_addr) = self.exe.symbols.try_find(self.ip.into()) {
            let f_name = self.exe.str_chunk.read(f_name_addr).to_string();
            vec![mk_info_row(
                self.ip,
                "== FUNCTION BEGIN ==".to_string(),
                f_name,
            )]
        } else {
            Vec::new()
        };

        use byte_code::OpCode::*;
        match op_code {
            JUMP_TABLE => {
                let (r, size) = reader.jump_table();
                rows.push(mk_row1(r.to_string(), es()));
                for i in 0..size {
                    let (_, delta) = self
                        .exe
                        .chunk
                        .fetch(self.ip + 8 + (i * 2) as u32)
                        .read::<byte_code::JumpDistance, _>();
                    rows.push(mk_info_row(
                        self.ip + 8 + (i * 2) as u32,
                        delta.to_string(),
                        hex(self.ip + delta as u32),
                    ));
                }
                self.ip += ((size * 2).div_ceil(8) * 8) as u32;
            }
            JUMP_IF => {
                let (r, branch) = reader.jump_if();
                rows.push(mk_row1(
                    format!("{}, {}", r, branch),
                    hex(branch as u32 + self.ip),
                ));
            }
            JUMP_IF_EQ_BOOL => {
                let (r, value, branch) = reader.jump_if_eq_bool();
                rows.push(mk_row1(
                    format!("{}, {}, {}", r, value, branch),
                    hex(branch as u32 + self.ip),
                ));
            }
            JUMP_IF_EQ_I32 => {
                let (r, value, branch) = reader.jump_if_eq_i32();
                rows.push(mk_row1(
                    format!("{}, {}, {}", r, value, branch),
                    hex(branch as u32 + self.ip),
                ));
            }
            JUMP_IF_EQ_I64 => {
                let (r, branch) = reader.jump_if_eq_i64();
                let (_, value) = self.exe.chunk.fetch(self.ip + 8).read::<i64, _>();
                rows.push(mk_row1(
                    format!("{}, {}, {}", r, value, branch),
                    hex(branch as u32 + self.ip),
                ));
                self.ip += 8;
            }
            JUMP_IF_EQ_STR => {
                let (r, value, branch) = reader.jump_if_eq_str();
                let s = trunc_str(self.exe.str_chunk.read(value));
                rows.push(mk_row1(
                    format!("{}, {}, {}", r, value, branch),
                    format!("{}, {}", s, hex(branch as u32 + self.ip)),
                ));
            }
            JUMP => {
                let branch = reader.jump();
                rows.push(mk_row1(es(), hex(self.ip + branch as u32)));
            }
            APPLY1 => arm_apply!(apply1, rows),
            APPLY2 => arm_apply!(apply2, rows),
            APPLY3 => arm_apply!(apply3, rows),
            APPLY4 => arm_apply!(apply4, rows),
            APPLY5 => arm_apply!(apply5, rows),
            APPLY_MANY => arm_apply_many!(apply_many, rows),
            CALL1 => arm_apply_with_name!(call1, rows),
            CALL2 => arm_apply_with_name!(call2, rows),
            CALL_MANY => arm_apply_many_with_name!(call_many, rows),
            TAIL_CALL_U1 => arm_apply_with_name!(tail_call_u1, rows),
            TAIL_CALL_U2 => arm_apply_with_name!(tail_call_u2, rows),
            TAIL_CALL_U_MANY => arm_apply_many_with_name!(tail_call_u_many, rows),
            TAIL_CALL1 => arm_apply!(tail_call1, rows),
            TAIL_CALL2 => arm_apply!(tail_call2, rows),
            TAIL_CALL3 => arm_apply!(tail_call3, rows),
            TAIL_CALL4 => arm_apply!(tail_call4, rows),
            TAIL_CALL5 => arm_apply!(tail_call5, rows),
            TAIL_CALL_MANY => arm_apply_many!(tail_call_many, rows),
            CALL_BUILTIN1 => arm_call_builtin!(call_builtin1, rows),
            CALL_BUILTIN2 => arm_call_builtin!(call_builtin2, rows),
            CALL_BUILTIN3 => arm_call_builtin!(call_builtin3, rows),
            CALL_BUILTIN4 => arm_call_builtin!(call_builtin4, rows),
            INT32 => {
                let (to, value) = reader.int32();
                rows.push(mk_row(to, value.to_string(), es()))
            }
            BOOL => {
                let (to, value) = reader.bool();
                rows.push(mk_row(to, value.to_string(), es()))
            }
            STR => {
                let (to, addr) = reader.str();
                let s = trunc_str(self.exe.str_chunk.read(addr));
                rows.push(mk_row(to, addr.to_string(), s.to_string()))
            }
            INT64 => {
                let to = reader.int64();
                let (_, value) = self.exe.chunk.fetch(self.ip + 8).read::<i64, _>();
                let r = rows.push(mk_row(to, value.to_string(), es()));
                self.ip += 8;
                r
            }
            FLOAT => {
                let to = reader.float();
                let (_, value) = self.exe.chunk.fetch(self.ip + 8).read::<f64, _>();
                let r = rows.push(mk_row(to, value.to_string(), es()));
                self.ip += 8;
                r
            }
            PUSH1 => arm_push!(push1, rows),
            PUSH2 => arm_push!(push2, rows),
            PUSH3 => arm_push!(push3, rows),
            PUSH4 => arm_push!(push4, rows),
            PUSH5 => arm_push!(push5, rows),
            PUSH6 => arm_push!(push6, rows),
            FUNCTION => {
                let (to, addr) = reader.function();
                let name = self.exe.str_chunk.read(self.exe.symbols.find(addr));
                rows.push(mk_row(to, addr.to_string(), name.to_string()))
            }
            BUILTIN => {
                let (to, id) = reader.builtin();
                let name = Builtins::name_by_id(id);
                rows.push(mk_row(to, id.to_string(), name.to_string()))
            }
            FIELD => {
                let (to, from, n) = reader.field();
                rows.push(mk_row(to, format!("{}. {}", from, n), es()))
            }
            TAGGED1 => arm_apply!(tagged1, rows),
            TAGGED2 => arm_apply!(tagged2, rows),
            TAGGED3 => arm_apply!(tagged3, rows),
            TAGGED4 => arm_apply!(tagged4, rows),
            TAGGED5 => arm_apply!(tagged5, rows),
            TAGGED => {
                let (to, tag) = reader.tagged();
                rows.push(mk_row(to, tag.to_string(), es()))
            }
            MOV => {
                let (to, from) = reader.mov();
                rows.push(mk_row(to, from.to_string(), es()))
            }
            RET => {
                let r = reader.ret();
                rows.push(mk_row1(r.to_string(), es()))
            }
            PANIC => {
                let addr = reader.panic();
                let s = trunc_str(self.exe.str_chunk.read(addr));
                rows.push(mk_row1(addr.to_string(), s.to_string()))
            }
            UNKNOWN(code) => rows.push(mk_row1(code.to_string(), es())),
        };
        self.ip += 8;
        Some(rows)
    }
}

pub fn disassemble(exe: &executable::Executable) -> Table {
    let rows = RowIter::new(exe).flatten();
    let mut table = Table::new(rows);

    use tabled::settings::Style;
    table.with(Style::blank());
    table
}
