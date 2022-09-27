use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

trait VarMapper<V1, V2> {
    fn get_module(&self, module: &V1) -> Result<V2, String>;

    fn get_procedure(&self, mod_id: &V2, procedure: &V1) -> Result<V2, String>;
    fn get_global_word(&self, mod_id: &V2, word: &V1) -> Result<V2, String>;
    fn get_global_ptr(&self, mod_id: &V2, ptr: &V1) -> Result<V2, String>;

    fn get_local_word(&self, mod_id: &V2, proc_id: &V2, word: &V1) -> Result<V2, String>;
    fn get_local_ptr(&self, mod_id: &V2, proc_id: &V2, ptr: &V1) -> Result<V2, String>;
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
struct Vars<V> {
    words: Vec<V>,
    ptrs: Vec<V>,
}

impl<V> Vars<V> {
    fn count(&self) -> Counts {
        Counts {
            words: self.words.len(),
            ptrs: self.ptrs.len(),
        }
    }
}

impl Vars<usize> {
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
struct Locals<V>(Vars<V>);

impl<V> Locals<V> {
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<Locals<V2>, String> {
        let mut words = Vec::new();
        let mut ptrs = Vec::new();
        for word in &self.0.words {
            words.push(mapper.get_local_word(mod_id, proc_id, word)?);
        }
        for ptr in &self.0.ptrs {
            ptrs.push(mapper.get_local_ptr(mod_id, proc_id, ptr)?);
        }
        Ok(Locals(Vars {
            words: words,
            ptrs: ptrs,
        }))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct Globals<V>(Vars<V>);

impl<V> Globals<V> {
    fn map<V2>(&self, mod_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<Globals<V2>, String> {
        let mut words = Vec::new();
        let mut ptrs = Vec::new();
        for word in &self.0.words {
            words.push(mapper.get_global_word(mod_id, word)?);
        }
        for ptr in &self.0.ptrs {
            ptrs.push(mapper.get_global_ptr(mod_id, ptr)?);
        }
        Ok(Globals(Vars {
            words: words,
            ptrs: ptrs,
        }))
    }
}



#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordLValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
}

impl<V> VMWordLValue<V> {
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMWordLValue<V2>, String> {
        Ok(match self {
            VMWordLValue::Local(v) => VMWordLValue::Local(mapper.get_local_word(mod_id, proc_id, v)?),
            VMWordLValue::Global(v) => VMWordLValue::Global(mapper.get_global_word(mod_id, v)?),
            VMWordLValue::Index(ptr, index) => VMWordLValue::Index(Box::new(ptr.map(mod_id, proc_id, mapper)?), Box::new(index.map(mod_id, proc_id, mapper)?)),
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
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMPtrLValue<V2>, String> {
        Ok(match self {
            VMPtrLValue::Local(v) => VMPtrLValue::Local(mapper.get_local_ptr(mod_id, proc_id, v)?),
            VMPtrLValue::Global(v) => VMPtrLValue::Global(mapper.get_global_ptr(mod_id, v)?),
            VMPtrLValue::Index(ptr, index) => VMPtrLValue::Index(Box::new(ptr.map(mod_id, proc_id, mapper)?), Box::new(index.map(mod_id, proc_id, mapper)?)),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
enum VMWordRValue<V> {
    Copy(Box<VMPtrLValue<V>>),
    PtrTag(Box<VMPtrLValue<V>>),
    PtrLengthWord(Box<VMPtrLValue<V>>),
    PtrLengthPtr(Box<VMPtrLValue<V>>),
}

impl<V> VMWordRValue<V> {
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMWordRValue<V2>, String> {
        Ok(match self {
            VMWordRValue::Copy(ptr) => VMWordRValue::Copy(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
            VMWordRValue::PtrTag(ptr) => VMWordRValue::PtrTag(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
            VMWordRValue::PtrLengthWord(ptr) => VMWordRValue::PtrLengthWord(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
            VMWordRValue::PtrLengthPtr(ptr) => VMWordRValue::PtrLengthPtr(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
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
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMPtrRValue<V2>, String> {
        Ok(match self {
            VMPtrRValue::Copy(ptr) => VMPtrRValue::Copy(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
            VMPtrRValue::CloneRc(ptr) => VMPtrRValue::CloneRc(Box::new(ptr.map(mod_id, proc_id, mapper)?)),
            VMPtrRValue::Closure(module, function, curries) => {
                let mut cur_words = Vec::new();
                let mut cur_ptrs = Vec::new();
                for cur in &curries.0.words {
                    cur_words.push(Box::new(cur.map(mod_id, proc_id, mapper)?));
                }
                for cur in &curries.0.ptrs {
                    cur_ptrs.push(Box::new(cur.map(mod_id, proc_id, mapper)?));
                }
                let fun_mod_id = mapper.get_module(module)?;
                let fun_proc_id = mapper.get_procedure(&fun_mod_id, function)?;
                VMPtrRValue::Closure(
                    fun_mod_id,
                    fun_proc_id,
                    Locals(Vars {
                        words: cur_words,
                        ptrs: cur_ptrs,
                    }))
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
    fn map<V2>(&self, mod_id: &V2, proc_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMStatement<V2>, String> {
        Ok(match self {
            VMStatement::SetWord(lvalue, rvalue) => VMStatement::SetWord(lvalue.map(mod_id, proc_id, mapper)?, rvalue.map(mod_id, proc_id, mapper)?),
            VMStatement::SetPtr(lvalue, rvalue) => VMStatement::SetPtr(lvalue.map(mod_id, proc_id, mapper)?, rvalue.map(mod_id, proc_id, mapper)?),
            VMStatement::SwapWord(lvalue1, lvalue2) => VMStatement::SwapWord(lvalue1.map(mod_id, proc_id, mapper)?, lvalue2.map(mod_id, proc_id, mapper)?),
            VMStatement::SwapPtr(lvalue1, lvalue2) => VMStatement::SwapPtr(lvalue1.map(mod_id, proc_id, mapper)?, lvalue2.map(mod_id, proc_id, mapper)?),
            VMStatement::Alloc(lvalue, rvalue) => VMStatement::Alloc(lvalue.map(mod_id, proc_id, mapper)?, rvalue.map(mod_id, proc_id, mapper)?),
            VMStatement::Call(module, function, args, returns) => {
                let fun_module = mapper.get_module(module)?;
                let fun_proc = mapper.get_procedure(&fun_module, function)?;
                VMStatement::Call(
                    fun_module,
                    fun_proc,
                    args.map(mod_id, proc_id, mapper)?,
                    returns.map(mod_id, proc_id, mapper)?)
            },
            VMStatement::CallPtr(ptr, args, returns) => VMStatement::CallPtr(
                ptr.map(mod_id, proc_id, mapper)?,
                args.map(mod_id, proc_id, mapper)?,
                returns.map(mod_id, proc_id, mapper)?),
            VMStatement::Return(returns) => VMStatement::Return(returns.map(mod_id, proc_id, mapper)?),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMProcedure<V> {
    name: V,
    locals: Locals<V>,
    params: Locals<V>,
    return_counts: Counts, // must be <= local_counts
    statements: Vec<VMStatement<V>>,
}

impl<V> VMProcedure<V> {
    fn map<V2>(&self, mod_id: &V2, mapper: &dyn VarMapper<V, V2>) -> Result<VMProcedure<V2>, String> {
        let proc_id = mapper.get_procedure(mod_id, &self.name)?;
        let mut statements = Vec::new();
        for stmt in &self.statements {
            statements.push(stmt.map(mod_id, &proc_id, mapper)?);
        }
        let locals = self.locals.map(mod_id, &proc_id, mapper)?;
        let params = self.params.map(mod_id, &proc_id, mapper)?;
        Ok(VMProcedure {
            name: proc_id,
            locals,
            params,
            return_counts: self.return_counts,
            statements,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Clone)]
struct VMModule<V> {
    name: V,
    globals: Globals<V>,
    procedures: Vec<VMProcedure<V>>,
}

impl<V> VMModule<V> {
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMModule<V2>, String> {
        let mod_id = mapper.get_module(&self.name)?;
        let mut procedures = Vec::new();
        for proc in &self.procedures {
            procedures.push(proc.map(&mod_id, mapper)?);
        }
        let globals = self.globals.map(&mod_id, mapper)?;
        Ok(VMModule {
            name: mod_id,
            globals,
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
    fn map<V2>(&self, mapper: &dyn VarMapper<V, V2>) -> Result<VMLibrary<V2>, String> {
        let mut modules = Vec::new();
        for module in &self.modules {
            modules.push(module.map(mapper)?);
        }
        let mut exports = BTreeMap::new();
        for (k, v) in &self.exports {
            let mod_id = mapper.get_module(&v.0)?;
            let proc_id = mapper.get_procedure(&mod_id, &v.1)?;
            exports.insert(k.clone(), (mod_id, proc_id));
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

struct VarProcedureManager {
    local_words: BTreeMap<String, usize>,
    local_ptrs: BTreeMap<String, usize>,
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
        }
    }
    fn add_local_word(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.local_words, name)
    }
    fn add_local_ptr(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.local_ptrs, name)
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
    fn add_global_word(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.global_words, name)
    }
    fn add_global_ptr(&mut self, name: &str) -> usize {
        add_index_to_map(&mut self.global_ptrs, name)
    }
    fn add_procedure(&mut self, name: &str) -> &mut VarProcedureManager {
        add_index_to_map(&mut self.procedures, name);
        self.procedure_managers.push(VarProcedureManager::new());
        self.procedure_managers.last_mut().unwrap()
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
    fn get_global_word(&self, mod_id: &usize, word: &String) -> Result<usize, String> {
        let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
        mod_man.global_words.get(word).cloned().ok_or(format!("unknown global word {}", word))
    }
    fn get_global_ptr(&self, mod_id: &usize, ptr: &String) -> Result<usize, String> {
        let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
        mod_man.global_ptrs.get(ptr).cloned().ok_or(format!("unknown global ptr {}", ptr))
    }
    fn get_local_word(&self, mod_id: &usize, proc_id: &usize, word: &String) -> Result<usize, String> {
        let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
        let proc_man = mod_man.procedure_managers.get(*proc_id).ok_or(format!("bad procedure index"))?;
        proc_man.local_words.get(word).cloned().ok_or(format!("unknown local word {}", word))
    }
    fn get_local_ptr(&self, mod_id: &usize, proc_id: &usize, ptr: &String) -> Result<usize, String> {
        let mod_man = self.module_managers.get(*mod_id).ok_or(format!("bad module index"))?;
        let proc_man = mod_man.procedure_managers.get(*proc_id).ok_or(format!("bad procedure index"))?;
        proc_man.local_ptrs.get(ptr).cloned().ok_or(format!("unknown local ptr {}", ptr))
    }
}

fn translate_library_vars(lib: &VMLibrary<String>) -> Result<VMLibrary<usize>, String> {
    let mut lib_manager = VarLibraryManager::new();
    for module in &lib.modules {
        let mut mod_manager = lib_manager.add_module(&module.name);
        for global_word in &module.globals.0.words {
            mod_manager.add_global_word(global_word);
        }
        for global_ptr in &module.globals.0.ptrs {
            mod_manager.add_global_ptr(global_ptr);
        }
        for proc in &module.procedures {
            let mut proc_manager = mod_manager.add_procedure(&proc.name);
            for local_word in &proc.locals.0.words {
                proc_manager.add_local_word(local_word);
            }
            for local_ptr in &proc.locals.0.ptrs {
                proc_manager.add_local_ptr(local_ptr);
            }
        }
    }
    lib.map(&lib_manager)
}
