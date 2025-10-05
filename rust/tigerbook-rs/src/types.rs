use crate::symbol::{Interner, Env, Symbol};

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Unique { id: u32 }

pub struct UniqueGen { next: u32 }

impl UniqueGen {
    pub fn new() -> UniqueGen { UniqueGen {next : 0} }

    pub fn fresh(&mut self) -> Unique {
        let id = self.next;
        self.next += 1;
        Unique {id}
    }
}

enum Ty {
    Record(Vec<(Symbol, Ty)>, Unique),
    Nil,
    Int,
    String,
    Array(Box<Ty>, Unique)
}

