
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

enum VMWordLValue {
    Local(usize),
    Index(Box<VMPtrLValue>, Box<VMWordRValue>),
}

enum VMPtrLValue {
    Local(usize),
    Index(Box<VMPtrLValue>, Box<VMWordRValue>),
}

enum VMWordRValue {
    Local(usize),
    Index(Box<VMPtrLValue>, Box<VMWordRValue>),
    PtrTag(Box<VMPtrLValue>),
    PtrLengthWord(Box<VMPtrLValue>),
    PtrLengthPtr(Box<VMPtrLValue>),
}

enum VMPtrRValue {
    Copy(VMPtrLValue),
    CloneRc(VMPtrLValue),
}

struct Counts {
    n_words: usize,
    n_ptrs: usize
}

struct Locals {
    words: Vec<usize>,
    ptrs: Vec<usize>,
}

enum VMStatement {
    SetWord(VMWordLValue, VMWordRValue),
    SetPtr(VMPtrLValue, VMPtrRValue),
    SwapWord(VMWordLValue, VMWordLValue),
    SwapPtr(VMWordLValue, VMPtrLValue),
    Alloc(VMPtrLValue, VMWordRValue),
    Call(usize, Locals, Locals),
    CallPtr(VMPtrLValue, Locals, Locals),
    Return(Locals)
}

struct VMProcedure {
    param_counts: Counts,
    local_counts: Counts, // must be >= param_counts
    return_counts: Counts, // must be <= local_counts
    statements: Vec<VMStatement>,
}
