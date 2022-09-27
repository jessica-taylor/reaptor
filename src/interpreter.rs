use std::collections::BTreeMap;
use std::default::Default;
use std::rc::Rc;
use std::cell::RefCell;

use serde::{Deserialize, Serialize};

use crate::assembly::{Counts, VMWordLValue, VMPtrLValue, VMWordRValue, VMPtrRValue, VMStatement, VMProcedure, VMModule, VMLibrary, WordUnOp, WordBinOp};


#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Serialize, Deserialize)]
struct WordIValue(u64);

#[derive(Clone)]
enum PtrIValue {
    Null,
    Fun(usize, usize),
    Rc(Rc<RefCell<IValues>>),
}

impl Default for PtrIValue {
    fn default() -> Self {
        PtrIValue::Null
    }
}

impl PartialEq for PtrIValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PtrIValue::Null, PtrIValue::Null) => true,
            (PtrIValue::Fun(a, b), PtrIValue::Fun(c, d)) => a == c && b == d,
            (PtrIValue::Rc(a), PtrIValue::Rc(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for PtrIValue {}

#[derive(Default, Clone, Eq, PartialEq)]
struct IValues {
    words: Vec<WordIValue>,
    ptrs: Vec<PtrIValue>,
}

impl IValues {
    fn new(counts: &Counts) -> Self {
        IValues {
            words: vec![WordIValue::default(); counts.words],
            ptrs: vec![PtrIValue::default(); counts.ptrs],
        }
    }
}

enum WordIRef<'a> {
    Direct(&'a mut WordIValue),
    Rc(Rc<RefCell<IValues>>, usize),
}

impl<'a> WordIRef<'a> {
    fn get(&self) -> WordIValue {
        match self {
            WordIRef::Direct(x) => **x,
            WordIRef::Rc(x, y) => x.borrow().words[*y],
        }
    }
    fn set(&mut self, value: WordIValue) {
        match self {
            WordIRef::Direct(x) => **x = value,
            WordIRef::Rc(x, y) => x.borrow_mut().words[*y] = value,
        }
    }
}

enum PtrIRef<'a> {
    Direct(&'a mut PtrIValue),
    Rc(Rc<RefCell<IValues>>, usize),
}

impl<'a> PtrIRef<'a> {
    fn get(&self) -> PtrIValue {
        match self {
            PtrIRef::Direct(x) => (*x).clone(),
            PtrIRef::Rc(x, y) => x.borrow().ptrs[*y].clone(),
        }
    }
    fn set(&mut self, value: PtrIValue) {
        match self {
            PtrIRef::Direct(x) => **x = value,
            PtrIRef::Rc(x, y) => x.borrow_mut().ptrs[*y] = value,
        }
    }
}

fn eval_word_unop(op: WordUnOp, val: WordIValue) -> WordIValue {
    match op {
        WordUnOp::Neg => WordIValue(-(val.0 as i64) as u64),
        WordUnOp::Not => WordIValue(!val.0),
    }
}

fn bool_to_word(b: bool) -> WordIValue {
    WordIValue((b as u64) * u64::MAX)
}

fn eval_word_binop(op: WordBinOp, lhs: WordIValue, rhs: WordIValue) -> Result<WordIValue, String> {
    Ok(match op {
        WordBinOp::Add(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_add(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_add(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_add(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_add(rhs.0))
                }
            }
        }
        WordBinOp::Sub(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_sub(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_sub(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_sub(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_sub(rhs.0))
                }
            }
        }
        WordBinOp::Mul(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_mul(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_mul(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_mul(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_mul(rhs.0))
                }
            }
        }
        WordBinOp::Div(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_div(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_div(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_div(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_div(rhs.0))
                }
            }
        }
        WordBinOp::Mod(signed, checked) => {
            if signed {
                if checked {
                    WordIValue((lhs.0 as i64).checked_rem(rhs.0 as i64).ok_or("overflow".to_string())? as u64)
                } else {
                    WordIValue((lhs.0 as i64).wrapping_rem(rhs.0 as i64) as u64)
                }
            } else {
                if checked {
                    WordIValue(lhs.0.checked_rem(rhs.0).ok_or("overflow".to_string())?)
                } else {
                    WordIValue(lhs.0.wrapping_rem(rhs.0))
                }
            }
        }
        WordBinOp::And => WordIValue(lhs.0 & rhs.0),
        WordBinOp::Or => WordIValue(lhs.0 | rhs.0),
        WordBinOp::Xor => WordIValue(lhs.0 ^ rhs.0),
        WordBinOp::Shl => WordIValue(lhs.0 << rhs.0),
        WordBinOp::Shr => WordIValue(lhs.0 >> rhs.0),
        WordBinOp::Eq => bool_to_word(lhs.0 == rhs.0),
        WordBinOp::Ne => bool_to_word(lhs.0 != rhs.0),
        WordBinOp::Lt(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) < (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 < rhs.0)
            }
        }
        WordBinOp::Le(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) <= (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 <= rhs.0)
            }
        }
        WordBinOp::Gt(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) > (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 > rhs.0)
            }
        }
        WordBinOp::Ge(signed) => {
            if signed {
                bool_to_word((lhs.0 as i64) >= (rhs.0 as i64))
            } else {
                bool_to_word(lhs.0 >= rhs.0)
            }
        }
    })
}


struct ProcedureInterpreter<'a> {
    procedure: &'a VMProcedure<usize>,
    locals: IValues,
    block_frames: Vec<(&'a Vec<VMStatement<usize>>, usize)>,
    returns: Option<IValues>,
}

struct ModuleInterpreter<'a> {
    module: &'a VMModule<usize>,
    stack: Vec<ProcedureInterpreter<'a>>,
    globals: IValues,
}

struct LibraryInterpreter<'a> {
    library: &'a VMLibrary<usize>,
    modules: Vec<ModuleInterpreter<'a>>,
}

impl<'a> ProcedureInterpreter<'a> {
    fn new(procedure: &'a VMProcedure<usize>) -> Self {
        Self {
            procedure,
            locals: IValues::new(&procedure.local_counts),
            block_frames: vec![(&procedure.statements, 0)],
            returns: None
        }
    }
    fn eval_word_rvalue(&mut self, lib: &LibraryInterpreter<'a>, module: &mut ModuleInterpreter<'a>, rvalue: &VMWordRValue) -> Result<WordIValue, String> {
        match rvalue {
            VMWordRValue::Const(v) => Ok(WordIValue(*v)),
            VMWordRValue::Copy(lval) => Ok(self.eval_word_lvalue(lib, module, lval)?.get()),
            VMWordRValue::PtrTag(lval) => {
                let ptr = self.eval_ptr_lvalue(lib, module, lval)?.get();
                Ok(match ptr {
                    PtrIValue::Null => WordIValue(0),
                    PtrIValue::Fun(_, _) => WordIValue(1),
                    PtrIValue::Rc(_) => WordIValue(2),
                })
            },
            VMWordRValue::PtrLengthWord(lval) =>  {
                let ptr = self.eval_ptr_lvalue(lib, module, lval)?.get();
                Ok(match ptr {
                    PtrIValue::Null => WordIValue(0),
                    PtrIValue::Fun(_, _) => WordIValue(0),
                    PtrIValue::Rc(rc) => WordIValue(rc.borrow().words.len() as u64),
                })
            },
            VMWordRValue::PtrLengthPtr(lval) => {
                let ptr = self.eval_ptr_lvalue(lib, module, lval)?.get();
                Ok(match ptr {
                    PtrIValue::Null => WordIValue(0),
                    PtrIValue::Fun(_, _) => WordIValue(0),
                    PtrIValue::Rc(rc) => WordIValue(rc.borrow().ptrs.len() as u64),
                })
            },
            VMWordRValue::UnOp(op, rhs) => {
                let rhs = self.eval_word_rvalue(lib, module, rhs)?;
                Ok(eval_word_unop(*op, rhs))
            },
            VMWordRValue::BinOp(op, lhs, rhs) => {
                let lhs = self.eval_word_rvalue(lib, module, lhs)?;
                let rhs = self.eval_word_rvalue(lib, module, rhs)?;
                eval_word_binop(*op, lhs, rhs)
            },
        }
    }
    fn eval_ptr_rvalue(&mut self, lib: &LibraryInterpreter<'a>, module: &mut ModuleInterpreter<'a>, rvalue: &VMPtrRValue<usize>) -> Result<PtrIValue, String> {
        match rvalue {
            VMPtrRValue::Null => Ok(PtrIValue::Null),
            VMPtrRValue::Copy(lval) => Ok(self.eval_ptr_lvalue(lib, module, lval)?.get()),
            VMPtrRValue::FunPtr(module_idx, procedure_idx) => {
                if *module_idx >= lib.modules.len() {
                    return Err("invalid module index".to_string());
                }
                if *procedure_idx >= lib.modules[*module_idx].module.procedures.len() {
                    return Err("invalid procedure index".to_string());
                }
                Ok(PtrIValue::Fun(*module_idx, *procedure_idx))
            },
            VMPtrRValue::Alloc(len_w, len_p) => {
                let WordIValue(len_w) = self.eval_word_rvalue(lib, module, len_w)?;
                let WordIValue(len_p) = self.eval_word_rvalue(lib, module, len_p)?;
                let rc = Rc::new(RefCell::new(IValues::new(&Counts {
                    words: len_w as usize,
                    ptrs: len_p as usize
                })));
                Ok(PtrIValue::Rc(rc))
            },
        }
    }
    fn eval_word_lvalue<'b>(&'b mut self, lib: &'b LibraryInterpreter<'a>, module: &'b mut ModuleInterpreter<'a>, lvalue: &VMWordLValue) -> Result<WordIRef<'b>, String> {
        match lvalue {
            VMWordLValue::Local(idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                Ok(WordIRef::Direct(&mut self.locals.words[ix as usize]))
            },
            VMWordLValue::Global(idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                Ok(WordIRef::Direct(&mut module.globals.words[ix as usize]))
            },
            VMWordLValue::Index(lval, idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                let ptr = self.eval_ptr_lvalue(lib, module, lval)?.get();
                match ptr {
                    PtrIValue::Null => Err("null pointer".to_string()),
                    PtrIValue::Fun(_, _) => Err("function pointer".to_string()),
                    PtrIValue::Rc(rc) => Ok(WordIRef::Rc(rc.clone(), ix as usize))
                }
            },
        }
    }
    fn eval_ptr_lvalue<'b>(&'b mut self, lib: &'b LibraryInterpreter<'a>, module: &'b mut ModuleInterpreter<'a>, lvalue: &VMPtrLValue) -> Result<PtrIRef<'b>, String> {
        match lvalue {
            VMPtrLValue::Local(idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                Ok(PtrIRef::Direct(&mut self.locals.ptrs[ix as usize]))
            },
            VMPtrLValue::Global(idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                Ok(PtrIRef::Direct(&mut module.globals.ptrs[ix as usize]))
            },
            VMPtrLValue::Index(lval, idx) => {
                let WordIValue(ix) = self.eval_word_rvalue(lib, module, idx)?;
                let ptr = self.eval_ptr_lvalue(lib, module, lval)?.get();
                match ptr {
                    PtrIValue::Null => Err("null pointer".to_string()),
                    PtrIValue::Fun(_, _) => Err("function pointer".to_string()),
                    PtrIValue::Rc(rc) => Ok(PtrIRef::Rc(rc.clone(), ix as usize))
                }
            },
        }
    }
    fn eval_statement(&mut self, lib: &LibraryInterpreter<'a>, module: &mut ModuleInterpreter<'a>, stmt: &'a VMStatement<usize>) -> Result<(), String> {
        match stmt {
            VMStatement::SetWord(lval, rval) => {
                let rval = self.eval_word_rvalue(lib, module, rval)?;
                let mut lval = self.eval_word_lvalue(lib, module, lval)?;
                lval.set(rval);
            },
            VMStatement::SetPtr(lval, rval) => {
                let rval = self.eval_ptr_rvalue(lib, module, rval)?;
                let mut lval = self.eval_ptr_lvalue(lib, module, lval)?;
                lval.set(rval);
            },
            VMStatement::Call(mod_id, fun_id, args, rets) => {
                // TODO
            },
            VMStatement::CallPtr(fun_ptr, args, rets) => {
                // TODO
            },
            VMStatement::Return(rets) => {
                let mut returns = IValues {
                    words: Vec::new(),
                    ptrs: Vec::new(),
                };
                for word_rvalue in &rets.words {
                    returns.words.push(self.eval_word_rvalue(lib, module, word_rvalue)?);
                }
                for ptr_rvalue in &rets.ptrs {
                    returns.ptrs.push(self.eval_ptr_rvalue(lib, module, ptr_rvalue)?);
                }
                self.returns = Some(returns);
            },
            VMStatement::If(cond, stmt) => {
                let WordIValue(cond) = self.eval_word_rvalue(lib, module, cond)?;
                if cond != 0 {
                    self.eval_statement(lib, module, stmt)?;
                }
            },
            VMStatement::Block(stmts) => {
                self.block_frames.push((stmts, 0));
            },
            VMStatement::Continue(usize) => {
                self.block_frames.last_mut().unwrap().1 = 0;
            },
            VMStatement::Break(usize) => {
                if self.block_frames.len() == 1 {
                    return Err("break outside loop".to_string());
                }
                self.block_frames.pop();
            },
        }
        Ok(())
    }
}
