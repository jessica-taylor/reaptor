
enum VMType {
    Word,
    Ptr,
}

enum VMPtrType {
    Fun,
    Static,
    Box,
    Rc
}

enum VMWordLValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
}

enum VMPtrLValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
}

enum VMWordRValue<V> {
    Local(V),
    Global(V),
    Index(Box<VMPtrLValue<V>>, Box<VMWordRValue<V>>),
    PtrTag(Box<VMPtrLValue<V>>),
    PtrLengthWord(Box<VMPtrLValue<V>>),
    PtrLengthPtr(Box<VMPtrLValue<V>>),
}

enum VMPtrRValue<V> {
    Copy(VMPtrLValue<V>),
    CloneRc(VMPtrLValue<V>),
}

struct Counts {
    words: usize,
    ptrs: usize
}

struct Locals<V> {
    words: Vec<V>,
    ptrs: Vec<V>,
}

enum VMStatement<V> {
    SetWord(VMWordLValue<V>, VMWordRValue<V>),
    SetPtr(VMPtrLValue<V>, VMPtrRValue<V>),
    SwapWord(VMWordLValue<V>, VMWordLValue<V>),
    SwapPtr(VMWordLValue<V>, VMPtrLValue<V>),
    Alloc(VMPtrLValue<V>, VMWordRValue<V>),
    Call(V, V, Locals<V>, Locals<V>), // module, function, args, returns
    Closure(V, V, Locals<V>), // module, function, curries
    CallPtr(VMPtrLValue<V>, Locals<V>, Locals<V>),
    Return(Locals<V>)
}

struct VMProcedure<V> {
    param_counts: Counts,
    local_counts: Counts, // must be >= param_counts
    return_counts: Counts, // must be <= local_counts
    statements: Vec<VMStatement<V>>,
}

struct VMModule<V> {
    globals: Counts,
    procedures: Vec<VMProcedure<V>>,
}
