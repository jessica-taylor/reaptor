use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

trait VarMapper<V1, V2> {
    fn get_global_word(&self, word: &V1) -> Result<V2, String>;
    fn get_global_ptr(&self, ptr: &V1) -> Result<V2, String>;
    fn get_local_word(&self, word: &V1) -> Result<V2, String>;
    fn get_local_ptr(&self, ptr: &V1) -> Result<V2, String>;
    fn get_module(&self, module: &V1) -> Result<V2, String>;
    fn get_procedure(&self, module: &V1, procedure: &V1) -> Result<V2, String>;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
enum VMType {
    Word,
    Ptr,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
enum VMPtrType {
    Fun,
    Static,
    Box,
    Rc
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone, Copy)]
struct Counts {
    words: usize,
    ptrs: usize
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct Locals<V> {
    words: Vec<V>,
    ptrs: Vec<V>,
}

impl<V> Locals<V> {
    fn count(&self) -> Counts {
        Counts {
            words: self.words.len(),
            ptrs: self.ptrs.len(),
        }
    }

    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<Locals<V2>, String> {
        let mut words = Vec::new();
        let mut ptrs = Vec::new();
        for word in &self.words {
            words.push(mapper.get_local_word(word)?);
        }
        for ptr in &self.ptrs {
            ptrs.push(mapper.get_local_ptr(ptr)?);
        }
        Ok(Locals {
            words: words,
            ptrs: ptrs,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordLValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
}

impl<V> VMWordLValue<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMWordLValue<V2>, String> {
        Ok(match self {
            VMWordLValue::Local(v) => VMWordLValue::Local(mapper.get_local_word(v)?),
            VMWordLValue::Global(v) => VMWordLValue::Global(mapper.get_global_word(v)?),
            VMWordLValue::Index(ptr, index) => VMWordLValue::Index(Box::new(ptr.map(mapper)?), Box::new(index.map(mapper)?)),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMPtrLValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
}

impl<V> VMPtrLValue<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMPtrLValue<V2>, String> {
        Ok(match self {
            VMPtrLValue::Local(v) => VMPtrLValue::Local(mapper.get_local_ptr(v)?),
            VMPtrLValue::Global(v) => VMPtrLValue::Global(mapper.get_global_ptr(v)?),
            VMPtrLValue::Index(ptr, index) => VMPtrLValue::Index(Box::new(ptr.map(mapper)?), Box::new(index.map(mapper)?)),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordRValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
    PtrTag(Box<VMPtrLValue<V>>),
    PtrLengthWord(Box<VMPtrLValue<V>>),
    PtrLengthPtr(Box<VMPtrLValue<V>>),
}

impl<V> VMWordRValue<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMWordRValue<V2>, String> {
        Ok(match self {
            VMWordRValue::Local(v) => VMWordRValue::Local(mapper.get_local_word(v)?),
            VMWordRValue::Global(v) => VMWordRValue::Global(mapper.get_global_word(v)?),
            VMWordRValue::Index(ptr, index) => VMWordRValue::Index(Box::new(ptr.map(mapper)?), Box::new(index.map(mapper)?)),
            VMWordRValue::PtrTag(ptr) => VMWordRValue::PtrTag(Box::new(ptr.map(mapper)?)),
            VMWordRValue::PtrLengthWord(ptr) => VMWordRValue::PtrLengthWord(Box::new(ptr.map(mapper)?)),
            VMWordRValue::PtrLengthPtr(ptr) => VMWordRValue::PtrLengthPtr(Box::new(ptr.map(mapper)?)),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMPtrRValue<V> {
    Copy(Box<VMPtrLValue<V>>),
    CloneRc(Box<VMPtrLValue<V>>),
    Closure(V, V, Locals<Box<VMPtrRValue<V>>>), // module, function, curries
}

impl<V> VMPtrRValue<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMPtrRValue<V2>, String> {
        Ok(match self {
            VMPtrRValue::Copy(ptr) => VMPtrRValue::Copy(Box::new(ptr.map(mapper)?)),
            VMPtrRValue::CloneRc(ptr) => VMPtrRValue::CloneRc(Box::new(ptr.map(mapper)?)),
            VMPtrRValue::Closure(module, function, curries) => {
                let mut cur_words = Vec::new();
                let mut cur_ptrs = Vec::new();
                for cur in &curries.words {
                    cur_words.push(Box::new(cur.map(mapper)?));
                }
                for cur in &curries.ptrs {
                    cur_ptrs.push(Box::new(cur.map(mapper)?));
                }
                VMPtrRValue::Closure(
                    mapper.get_module(module)?,
                    mapper.get_procedure(module, function)?,
                    Locals {
                        words: cur_words,
                        ptrs: cur_ptrs,
                    })
            }
        })
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMStatement<V> {
    SetWord(VMWordLValue<V>, VMWordRValue<V>),
    SetPtr(VMPtrLValue<V>, VMPtrRValue<V>),
    SwapWord(VMWordLValue<V>, VMWordLValue<V>),
    SwapPtr(VMWordLValue<V>, VMPtrLValue<V>),
    Alloc(VMPtrLValue<V>, VMWordRValue<V>),
    Call(V, V, Locals<V>, Locals<V>), // module, function, args, returns
    CallPtr(VMPtrLValue<V>, Locals<V>, Locals<V>),
    Return(Locals<V>)
}

impl<V> VMStatement<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMStatement<V2>, String> {
        Ok(match self {
            VMStatement::SetWord(lvalue, rvalue) => VMStatement::SetWord(lvalue.map(mapper)?, rvalue.map(mapper)?),
            VMStatement::SetPtr(lvalue, rvalue) => VMStatement::SetPtr(lvalue.map(mapper)?, rvalue.map(mapper)?),
            VMStatement::SwapWord(lvalue1, lvalue2) => VMStatement::SwapWord(lvalue1.map(mapper)?, lvalue2.map(mapper)?),
            VMStatement::SwapPtr(lvalue1, lvalue2) => VMStatement::SwapPtr(lvalue1.map(mapper)?, lvalue2.map(mapper)?),
            VMStatement::Alloc(lvalue, rvalue) => VMStatement::Alloc(lvalue.map(mapper)?, rvalue.map(mapper)?),
            VMStatement::Call(module, function, args, returns) => VMStatement::Call(
                mapper.get_module(module)?,
                mapper.get_procedure(module, function)?,
                args.map(mapper)?,
                returns.map(mapper)?),
            VMStatement::CallPtr(ptr, args, returns) => VMStatement::CallPtr(
                ptr.map(mapper)?,
                args.map(mapper)?,
                returns.map(mapper)?),
            VMStatement::Return(returns) => VMStatement::Return(returns.map(mapper)?),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMProcedure<V> {
    params: Locals<V>,
    local_counts: Counts,
    return_counts: Counts, // must be <= local_counts
    statements: Vec<VMStatement<V>>,
}

impl<V> VMProcedure<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMProcedure<V2>, String> {
        let mut statements = Vec::new();
        for stmt in &self.statements {
            statements.push(stmt.map(mapper)?);
        }
        Ok(VMProcedure {
            params: self.params.map(mapper)?,
            local_counts: self.local_counts,
            return_counts: self.return_counts,
            statements,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMModule<V> {
    globals: Counts,
    procedures: Vec<VMProcedure<V>>,
}

impl<V> VMModule<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMModule<V2>, String> {
        let mut procedures = Vec::new();
        for proc in &self.procedures {
            procedures.push(proc.map(mapper)?);
        }
        Ok(VMModule {
            globals: self.globals,
            procedures,
        })
    }
}

struct VarProcedureManager {
    local_words: BTreeMap<String, usize>,
    local_ptrs: BTreeMap<String, usize>,
    params: Locals<usize>,
}

struct VarModuleManager {
    global_words: BTreeMap<String, usize>,
    global_ptrs: BTreeMap<String, usize>,
    procedures: BTreeMap<String, usize>,
    procedure_managers: Vec<VarProcedureManager>,
}

struct VarLibraryManager {
    modules: BTreeMap<String, usize>,
    module_managers: Vec<VarModuleManager>,
}

fn add_index_to_map(map: &mut BTreeMap<String, usize>, name: &str) -> usize {
    let index = map.len();
    if map.insert(name.to_string(), index).is_some() {
        panic!("duplicate variable name");
    }
    index
}

impl VarProcedureManager {
    fn new() -> Self {
        VarProcedureManager {
            local_words: BTreeMap::new(),
            local_ptrs: BTreeMap::new(),
            params: Locals {
                words: Vec::new(),
                ptrs: Vec::new(),
            },
        }
    }
    fn add_local_word(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.local_words, name)
    }
    fn add_local_ptr(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.local_ptrs, name)
    }
    fn set_params(&mut self, params: Locals<String>) {

    }
}

impl VarModuleManager {
    fn new() -> Self {
        VarModuleManager {
            global_words: BTreeMap::new(),
            global_ptrs: BTreeMap::new(),
            procedures: BTreeMap::new(),
            procedure_managers: Vec::new(),
        }
    }
    fn add_global_word(&mut self, name: String) -> usize {
        add_index_to_map(&mut self.global_words, &name)
    }
    fn add_global_ptr(&mut self, name: String) -> usize {
        add_index_to_map(&mut self.global_ptrs, &name)
    }
    fn add_procedure(&mut self, name: String) -> usize {
        add_index_to_map(&mut self.procedures, &name)
    }
}
