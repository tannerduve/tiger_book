use std::collections::HashMap;

use im::OrdMap;
use std::cmp::Ordering;
use std::hash::Hash;

#[derive(Hash, Debug, PartialEq, PartialOrd, Clone, Eq)]
pub struct Symbol {
    name : String,
    id : u32
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Interner {
    next : u32,
    dict : HashMap<String, u32>
}

impl Symbol {
    pub fn name(&self) -> &str { &self.name }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering { self.id.cmp(&other.id) }
}

impl Interner {
    pub fn symbol(&mut self, name : &str) -> Symbol {
        let id = *self.dict.entry(name.to_string())
            .or_insert_with(|| { let id = self.next; self.next += 1; id});
        Symbol {name : name.to_string(), id : id}
    }
}

pub struct Env<T>(OrdMap<Symbol, T>);

impl<T> Env<T> {
    pub fn empty() -> Self { Self(OrdMap::new()) }
    pub fn look(&self, k: Symbol) -> Option<&T> { self.0.get(&k) }
}

impl <T : Clone> Env<T> { pub fn insert(&self, k: Symbol, v: T) -> Self { Self(self.0.update(k, v)) } }



