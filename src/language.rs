use std::collections::BTreeMap;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

trait VarMapper<V1, V2> {
    fn get_module(&self, module: &V1) -> Result<V2, String>;

    fn get_procedure(&self, mod_id: &V2, procedure: &V1) -> Result<V2, String>;
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
struct Vars {
    words: Vec<usize>,
    ptrs: Vec<usize>,
}

impl Vars {
    fn count(&self) -> Counts {
        Counts {
            words: self.words.len(),
            ptrs: self.ptrs.len(),
        }
    }

    fn is_ascending(&self) -> bool {
        for i in 0..self.words.len() {
            if self.words[i] != i {
                return false;
            }
        }
        for i in 0..self.ptrs.len() {
            if self.ptrs[i] != i {
                return false;
            }
        }
        return true;
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct Locals(Vars);

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct Globals(Vars);

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordLValue {
    Local(usize),
    Global(usize),
    Index(Box<VMPtrLValue>, Box<VMWordRValue>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMPtrLValue {
    Local(usize),
    Global(usize),
    Index(Box<VMPtrLValue>, Box<VMWordRValue>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordRValue {
    Copy(Box<VMPtrLValue>),
    PtrTag(Box<VMPtrLValue>),
    PtrLengthWord(Box<VMPtrLValue>),
    PtrLengthPtr(Box<VMPtrLValue>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMPtrRValue<V> {
    Copy(Box<VMPtrLValue>),
    CloneRc(Box<VMPtrLValue>),
    FunPtr(V, V), // module, function
}

impl<V> VMPtrRValue<V> {
    fn map<V2>(self, mapper: &dyn VarMapper<V, V2>) -> Result<VMPtrRValue<V2>, String> {
        Ok(match self {
            VMPtrRValue::Copy(ptr) => VMPtrRValue::Copy(ptr),
            VMPtrRValue::CloneRc(ptr) => VMPtrRValue::CloneRc(ptr),
            VMPtrRValue::FunPtr(module, function) => {
                let fun_mod_id = mapper.get_module(&module)?;
                let fun_proc_id = mapper.get_procedure(&fun_mod_id, &function)?;
                VMPtrRValue::FunPtr(fun_mod_id, fun_proc_id)
            }
        })
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMStatement<V> {
    SetWord(VMWordLValue, VMWordRValue),
    SetPtr(VMPtrLValue, VMPtrRValue<V>),
    SwapWord(VMWordLValue, VMWordLValue),
    SwapPtr(VMWordLValue, VMPtrLValue),
    Alloc(VMPtrLValue, VMWordRValue),
    Call(V, V, Locals, Locals), // module, function, args, returns
    CallPtr(VMPtrLValue, Locals, Locals),
    Return(Locals)
}

impl<V> VMStatement<V> {
    fn map<V2>(self, mapper: &dyn VarMapper<V, V2>) -> Result<VMStatement<V2>, String> {
        Ok(match self {
            VMStatement::SetPtr(lvalue, rvalue) => VMStatement::SetPtr(lvalue, rvalue.map(mapper)?),
            VMStatement::Call(module, function, args, returns) => {
                let fun_module = mapper.get_module(&module)?;
                let fun_proc = mapper.get_procedure(&fun_module, &function)?;
                VMStatement::Call(
                    fun_module,
                    fun_proc,
                    args,
                    returns)
            },
            VMStatement::SetWord(a, b) => VMStatement::SetWord(a, b),
            VMStatement::SwapWord(a, b) => VMStatement::SwapWord(a, b),
            VMStatement::SwapPtr(a, b) => VMStatement::SwapPtr(a, b),
            VMStatement::Alloc(a, b) => VMStatement::Alloc(a, b),
            VMStatement::CallPtr(a, b, c) => VMStatement::CallPtr(a, b, c),
            VMStatement::Return(a) => VMStatement::Return(a),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMProcedure<V> {
    name: V,
    local_counts: Counts,
    param_counts: Counts, // must be <= local_counts
    return_counts: Counts, // must be <= local_counts
    statements: Vec<VMStatement<V>>,
}

impl<V> VMProcedure<V> {
    fn map<V2>(self, mod_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMProcedure<V2>, String> {
        let mut statements = Vec::new();
        for stmt in self.statements {
            statements.push(stmt.map(mapper)?);
        }
        Ok(VMProcedure {
            name: mapper.get_procedure(mod_id, &self.name)?,
            local_counts: self.local_counts,
            param_counts: self.param_counts,
            return_counts: self.return_counts,
            statements,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMModule<V> {
    name: V,
    global_counts: Counts,
    procedures: Vec<VMProcedure<V>>,
}

impl<V> VMModule<V> {
    fn map<V2>(self, mapper: &dyn VarMapper<V, V2>) -> Result<VMModule<V2>, String> {
        let mod_id = mapper.get_module(&self.name)?;
        let mut procedures = Vec::new();
        for proc in self.procedures {
            procedures.push(proc.map(&mod_id, mapper)?);
        }
        Ok(VMModule {
            name: mod_id,
            global_counts: self.global_counts,
            procedures,
        })
    }
}

impl VMModule<usize> {
    fn procedures_ascending(&self) -> bool {
        for i in 0..self.procedures.len() {
            if self.procedures[i].name != i {
                return false;
            }
        }
        true
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMLibrary<V> {
    modules: Vec<VMModule<V>>,
    exports: BTreeMap<String, (V, V)>,
}

impl<V> VMLibrary<V> {
    fn map<V2>(self, mapper: &dyn VarMapper<V, V2>) -> Result<VMLibrary<V2>, String> {
        let mut modules = Vec::new();
        for module in self.modules {
            modules.push(module.map(mapper)?);
        }
        let mut exports = BTreeMap::new();
        for (k, v) in self.exports {
            let mod_id = mapper.get_module(&v.0)?;
            let proc_id = mapper.get_procedure(&mod_id, &v.1)?;
            exports.insert(k, (mod_id, proc_id));
        }
        Ok(VMLibrary { modules, exports })
    }
}

impl VMLibrary<usize> {
    fn modules_ascending(&self) -> bool {
        for i in 0..self.modules.len() {
            if self.modules[i].name != i {
                return false;
            }
        }
        true
    }
}

struct VarModuleManager {
    procedures: BTreeMap<String, usize>,
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

impl VarModuleManager {
    fn new() -> Self {
        VarModuleManager {
            procedures: BTreeMap::new(),
        }
    }
    fn add_procedure(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.procedures, name)
    }
}

impl VarLibraryManager {
    fn new() -> Self {
        VarLibraryManager {
            modules: BTreeMap::new(),
            module_managers: Vec::new(),
        }
    }
    fn add_module(&mut self, name: &str) -> &mut VarModuleManager {
        add_index_to_map(&mut self.modules, &name);
        self.module_managers.push(VarModuleManager::new());
        self.module_managers.last_mut().unwrap()
    }
}

impl VarMapper<String, usize> for VarLibraryManager {
    fn get_module(&self, module: &String) -> Result<usize, String> {
        self.modules.get(module).cloned().ok_or(format!("unknown module {}", module))
    }
    fn get_procedure(&self, mod_id: &usize, procedure: &String) -> Result<usize, String> {
        let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
        mod_man.procedures.get(procedure).cloned().ok_or(format!("unknown procedure {}", procedure))
    }
}

fn translate_library_vars(lib: VMLibrary<String>) -> Result<VMLibrary<usize>, String> {
    let mut lib_manager = VarLibraryManager::new();
    for module in &lib.modules {
        let mut mod_manager = lib_manager.add_module(&module.name);
        for proc in &module.procedures {
            mod_manager.add_procedure(&proc.name);
        }
    }
    lib.map(&lib_manager)
}
