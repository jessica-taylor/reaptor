use std::collections::BTreeMap;
use std::ops;

use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
pub enum VMType {
    Word,
    Ptr,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
pub enum VMPtrType {
    Fun,
    Null,
    Rc,
    Framework
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
pub struct VMTypeIndexed<T> {
    pub words: T,
    pub ptrs: T,
}

impl<T> ops::Index<VMType> for VMTypeIndexed<T> {
    type Output = T;

    fn index(&self, index: VMType) -> &Self::Output {
        match index {
            VMType::Word => &self.words,
            VMType::Ptr => &self.ptrs
        }
    }
}

impl<T> ops::IndexMut<VMType> for VMTypeIndexed<T> {
    fn index_mut(&mut self, index: VMType) -> &mut Self::Output {
        match index {
            VMType::Word => &mut self.words,
            VMType::Ptr => &mut self.ptrs
        }
    }
}

pub type Counts = VMTypeIndexed<usize>;

impl Counts {
    pub fn zero() -> Counts {
        Counts { words: 0, ptrs: 0 }
    }
    pub fn max(self, other: Counts) -> Counts {
        Counts { words: self.words.max(other.words), ptrs: self.ptrs.max(other.ptrs) }
    }
    pub fn min(self, other: Counts) -> Counts {
        Counts { words: self.words.min(other.words), ptrs: self.ptrs.min(other.ptrs) }
    }

}

impl ops::Add for Counts {
    type Output = Counts;

    fn add(self, other: Counts) -> Counts {
        Counts {
            words: self.words + other.words,
            ptrs: self.ptrs + other.ptrs
        }
    }
}

impl ops::Sub for Counts {
    type Output = Counts;

    fn sub(self, other: Counts) -> Counts {
        Counts {
            words: self.words - other.words,
            ptrs: self.ptrs - other.ptrs
        }
    }
}

type Vars = VMTypeIndexed<Vec<usize>>;

impl Vars {
    pub fn count(&self) -> Counts {
        Counts {
            words: self.words.len(),
            ptrs: self.ptrs.len(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub struct Locals(Vars);

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WordUnOp {
    Neg,
    Not,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WordBinOp {
    Add(bool, bool), // signed, checked
    Sub(bool, bool),
    Mul(bool, bool),
    Div(bool, bool),
    Mod(bool, bool),
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt(bool), // signed
    Le(bool),
    Gt(bool),
    Ge(bool),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub enum VMStatement<V> {
    DelWord,
    DelPtr,
    PopWord(usize),
    PopPtr(usize),
    PushWord(usize),
    PushPtr(usize),
    SwapPtr(usize),
    Null,
    FunPtr(V, V),
    ConstWord(u64),
    WordUn(WordUnOp),
    WordBin(WordBinOp),
    PtrTag,
    PtrLengthWord,
    PtrLengthPtr,
    AllocPtr,
    GetWordAt,
    GetPtrAt,
    SetWordAt,
    SetPtrAt,
    SwapPtrAt,
    Call(V, V),
    CallPtr(usize, usize, usize, usize),
    If(Vec<VMStatement<V>>, Vec<VMStatement<V>>),
    While(Vec<VMStatement<V>>, Vec<VMStatement<V>>),
                                                  
    // loop? continue? break? return?
}

pub trait VMProcedureTyper<V> {
    fn args_rets(&self, module: &V, proc: &V) -> Result<(Counts, Counts), String>;
}

pub trait VMVarMapper<V1, V2> {
    fn map_module(&self, module: &V1) -> Result<V2, String>;
    fn map_procedure(&self, module: &V1, proc: &V1) -> Result<V2, String>;
}

impl<V> VMStatement<V> {
    pub fn stack_type(&self, typer: &impl VMProcedureTyper<V>) -> Result<(Counts, Counts), String> {
        // counts of args, counts of rets
        Ok(match self {
            Self::DelWord => (Counts { words: 1, ptrs: 0 }, Counts::zero()),
            Self::DelPtr => (Counts { words: 0, ptrs: 1 }, Counts::zero()),
            Self::PopWord(_) => (Counts { words: 1, ptrs: 0 }, Counts::zero()),
            Self::PopPtr(_) => (Counts { words: 0, ptrs: 1 }, Counts::zero()),
            Self::PushWord(_) => (Counts::zero(), Counts { words: 1, ptrs: 0 }),
            Self::PushPtr(_) => (Counts::zero(), Counts { words: 0, ptrs: 1 }),
            Self::SwapPtr(_) => (Counts { words: 0, ptrs: 1 }, Counts { words: 0, ptrs: 1 }),
            Self::Null => (Counts::zero(), Counts { words: 0, ptrs: 1 }),
            Self::FunPtr(_, _) => (Counts::zero(), Counts { words: 0, ptrs: 1 }),
            Self::ConstWord(_) => (Counts::zero(), Counts { words: 1, ptrs: 0 }),
            Self::WordUn(_) => (Counts { words: 1, ptrs: 0 }, Counts { words: 1, ptrs: 0 }),
            Self::WordBin(_) => (Counts { words: 2, ptrs: 0 }, Counts { words: 1, ptrs: 0 }),
            Self::PtrTag => (Counts { words: 0, ptrs: 1 }, Counts { words: 1, ptrs: 1 }),
            Self::PtrLengthWord => (Counts { words: 0, ptrs: 1 }, Counts { words: 1, ptrs: 1 }),
            Self::PtrLengthPtr => (Counts { words: 0, ptrs: 1 }, Counts { words: 1, ptrs: 1 }),
            Self::AllocPtr => (Counts { words: 2, ptrs: 0 }, Counts { words: 0, ptrs: 1 }),
            Self::GetWordAt => (Counts { words: 1, ptrs: 1 }, Counts { words: 1, ptrs: 1 }),
            Self::GetPtrAt => (Counts { words: 1, ptrs: 1 }, Counts { words: 0, ptrs: 2 }),
            Self::SetWordAt => (Counts { words: 2, ptrs: 1 }, Counts { words: 0, ptrs: 1 }),
            Self::SetPtrAt => (Counts { words: 1, ptrs: 2 }, Counts { words: 0, ptrs: 1 }),
            Self::SwapPtrAt => (Counts { words: 1, ptrs: 2 }, Counts { words: 0, ptrs: 2 }),
            Self::Call(module, proc) => typer.args_rets(module, proc)?,
            Self::CallPtr(a, b, c, d) => (Counts { words: *a, ptrs: *b }, Counts { words: *c, ptrs: *d }),
            Self::If(thn, els) => {
                let (thn_arg, thn_ret) = Self::total_stack_type(thn, typer)?;
                let (els_arg, els_ret) = Self::total_stack_type(els, typer)?;
                if thn_ret - thn_arg != els_ret - els_arg {
                    return Err("if branches have different stack types".to_string());
                }
                (thn_arg.max(els_arg), thn_ret.max(els_ret))
            }
            Self::While(cond, body) => {
                let (cond_arg, cond_ret) = Self::total_stack_type(cond, typer)?;
                let (body_arg, body_ret) = Self::total_stack_type(body, typer)?;
                if cond_ret - cond_arg != (Counts { words: 1, ptrs: 0 }) {
                    return Err("while condition does not push a word".to_string());
                }
                if body_ret - body_arg != (Counts { words: 0, ptrs: 0 }) {
                    return Err("while body modifies the stack".to_string());
                }
                (cond_arg.max(body_arg), cond_arg.max(body_arg))
            }
        })
    }
    pub fn total_stack_type(stmts: &Vec<VMStatement<V>>, typer: &impl VMProcedureTyper<V>) -> Result<(Counts, Counts), String> {
        let mut net_stack = Counts::zero();
        let mut min_stack = Counts::zero();
        for stmt in stmts {
            let (pop, push) = stmt.stack_type(typer)?;
            let popped = net_stack - pop;
            net_stack = popped + push;
            min_stack = min_stack.min(popped);
        }
        Ok((Counts::zero() - min_stack, min_stack + net_stack))
    }
    pub fn map<V2>(&self, mapper: &impl VMVarMapper<V, V2>) -> Result<VMStatement<V2>, String> {
        Ok(match self {
            Self::DelWord => VMStatement::DelWord,
            Self::DelPtr => VMStatement::DelPtr,
            Self::PopWord(i) => VMStatement::PopWord(*i),
            Self::PopPtr(i) => VMStatement::PopPtr(*i),
            Self::PushWord(i) => VMStatement::PushWord(*i),
            Self::PushPtr(i) => VMStatement::PushPtr(*i),
            Self::SwapPtr(i) => VMStatement::SwapPtr(*i),
            Self::Null => VMStatement::Null,
            Self::FunPtr(module, proc) => {
                let m = mapper.map_module(module)?;
                let p = mapper.map_procedure(module, proc)?;
                VMStatement::FunPtr(m, p)
            },
            Self::ConstWord(w) => VMStatement::ConstWord(*w),
            Self::WordUn(op) => VMStatement::WordUn(*op),
            Self::WordBin(op) => VMStatement::WordBin(*op),
            Self::PtrTag => VMStatement::PtrTag,
            Self::PtrLengthWord => VMStatement::PtrLengthWord,
            Self::PtrLengthPtr => VMStatement::PtrLengthPtr,
            Self::AllocPtr => VMStatement::AllocPtr,
            Self::GetWordAt => VMStatement::GetWordAt,
            Self::GetPtrAt => VMStatement::GetPtrAt,
            Self::SetWordAt => VMStatement::SetWordAt,
            Self::SetPtrAt => VMStatement::SetPtrAt,
            Self::SwapPtrAt => VMStatement::SwapPtrAt,
            Self::Call(module, proc) => {
                let m = mapper.map_module(module)?;
                let p = mapper.map_procedure(module, proc)?;
                VMStatement::Call(m, p)
            },
            Self::CallPtr(a, b, c, d) => VMStatement::CallPtr(*a, *b, *c, *d),
            Self::If(thn, els) => VMStatement::If(Self::multi_map(thn, mapper)?, Self::multi_map(els, mapper)?),
            Self::While(cond, body) => VMStatement::While(Self::multi_map(cond, mapper)?, Self::multi_map(body, mapper)?),
        })
    }
    pub fn multi_map<V2>(stmts: &Vec<VMStatement<V>>, mapper: &impl VMVarMapper<V, V2>) -> Result<Vec<VMStatement<V2>>, String> {
        let mut result = Vec::new();
        for stmt in stmts {
            result.push(stmt.map(mapper)?);
        }
        Ok(result)
    }
    pub fn max_locals(&self) -> Counts {
        match self {
            Self::PopWord(i) => Counts { words: *i, ptrs: 0 },
            Self::PopPtr(i) => Counts { words: 0, ptrs: *i },
            Self::PushWord(i) => Counts { words: *i, ptrs: 0 },
            Self::PushPtr(i) => Counts { words: 0, ptrs: *i },
            Self::SwapPtr(i) => Counts { words: 0, ptrs: *i },
            Self::If(thn, els) => Self::multi_max_locals(thn).max(Self::multi_max_locals(els)),
            _ => Counts::zero(),
        }
    }
    pub fn multi_max_locals(stmts: &Vec<VMStatement<V>>) -> Counts {
        let mut max = Counts::zero();
        for stmt in stmts {
            max = max.max(stmt.max_locals());
        }
        max
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub struct VMProcedure<V> {
    pub param_counts: Counts,
    pub local_counts: Counts,
    pub return_counts: Counts,
    pub statements: Vec<VMStatement<V>>,
}

impl<V> VMProcedure<V> {
    // TODO check stack counts? local counts?
    pub fn map<V2>(&self, mapper: &impl VMVarMapper<V, V2>) -> Result<VMProcedure<V2>, String> {
        Ok(VMProcedure {
            param_counts: self.param_counts,
            local_counts: self.local_counts,
            return_counts: self.return_counts,
            statements: VMStatement::multi_map(&self.statements, mapper)?,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub struct VMModule<V: Ord> {
    pub procedures: BTreeMap<V, VMProcedure<V>>,
}

impl<V: Ord> VMModule<V> {
    fn map<V2: Ord>(self, mod_name: &V, mapper: &impl VMVarMapper<V, V2>) -> Result<VMModule<V2>, String> {
        let mut procedures = BTreeMap::new();
        for (name, proc) in self.procedures {
            procedures.insert(mapper.map_procedure(&mod_name, &name)?, proc.map(mapper)?);
        }
        Ok(VMModule {
            procedures
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
pub struct VMLibrary<V: Ord> {
    pub modules: BTreeMap<V, VMModule<V>>,
    pub exports: BTreeMap<String, (V, V)>,
}

impl<V: Ord> VMLibrary<V> {
    fn map<V2: Ord>(self, mapper: &impl VMVarMapper<V, V2>) -> Result<VMLibrary<V2>, String> {
        let mut modules = BTreeMap::new();
        for (mod_name, module) in self.modules {
            modules.insert(mapper.map_module(&mod_name)?, module.map(&mod_name, mapper)?);
        }
        let mut exports = BTreeMap::new();
        for (k, v) in self.exports {
            let mod_id = mapper.map_module(&v.0)?;
            let proc_id = mapper.map_procedure(&v.0, &v.1)?;
            exports.insert(k, (mod_id, proc_id));
        }
        Ok(VMLibrary { modules, exports })
    }
}

impl<V: Ord> VMProcedureTyper<V> for VMLibrary<V> {
    fn args_rets(&self, module: &V, proc: &V) -> Result<(Counts, Counts), String> {
        let module = self.modules.get(module).ok_or(format!("module not found"))?;
        let proc = module.procedures.get(proc).ok_or(format!("procedure not found"))?;
        Ok((proc.param_counts, proc.return_counts))
    }
}

// struct VarModuleManager {
//     procedures: BTreeMap<String, usize>,
// }
// 
// struct VarLibraryManager {
//     modules: BTreeMap<String, usize>,
//     module_managers: Vec<VarModuleManager>,
// }
// 
// fn add_index_to_map(map: &mut BTreeMap<String, usize>, name: &str) -> usize {
//     let index = map.len();
//     if map.insert(name.to_string(), index).is_some() {
//         panic!("duplicate variable name");
//     }
//     index
// }
// 
// impl VarModuleManager {
//     fn new() -> Self {
//         VarModuleManager {
//             procedures: BTreeMap::new(),
//         }
//     }
//     fn add_procedure(&mut self, name: &str) -> usize {
//         add_index_to_map(&mut self.procedures, name)
//     }
// }
// 
// impl VarLibraryManager {
//     fn new() -> Self {
//         VarLibraryManager {
//             modules: BTreeMap::new(),
//             module_managers: Vec::new(),
//         }
//     }
//     fn add_module(&mut self, name: &str) -> &mut VarModuleManager {
//         add_index_to_map(&mut self.modules, &name);
//         self.module_managers.push(VarModuleManager::new());
//         self.module_managers.last_mut().unwrap()
//     }
// }
// 
// impl VarMapper<String, usize> for VarLibraryManager {
//     fn get_module(&self, module: &String) -> Result<usize, String> {
//         self.modules.get(module).cloned().ok_or(format!("unknown module {}", module))
//     }
//     fn get_procedure(&self, mod_id: &usize, procedure: &String) -> Result<usize, String> {
//         let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
//         mod_man.procedures.get(procedure).cloned().ok_or(format!("unknown procedure {}", procedure))
//     }
// }
// 
// pub fn translate_library_vars(lib: VMLibrary<String>) -> Result<VMLibrary<usize>, String> {
//     let mut lib_manager = VarLibraryManager::new();
//     for module in &lib.modules {
//         let mut mod_manager = lib_manager.add_module(&module.name);
//         for proc in &module.procedures {
//             mod_manager.add_procedure(&proc.name);
//         }
//     }
//     lib.map(&lib_manager)
// }
