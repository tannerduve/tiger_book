type Id = String;

#[derive(Debug, Clone)]
pub enum Binop { Plus, Minus, Times, Div }

#[derive(Debug, Clone)]
pub enum Stm {
    CompoundStm(Box<Stm>, Box<Stm>),
    AssignStm(Id, Exp),
    PrintStm(Vec<Exp>),
}

#[derive(Debug, Clone)]
pub enum Exp {
    IdExp(Id),
    NumExp(i32),
    OpExp(Box<Exp>, Binop, Box<Exp>),
    EseqExp(Box<Stm>, Box<Exp>),
}

pub fn maxargs(s: &Stm) -> i32 {
    match s {
        Stm::CompoundStm(s1, s2) => maxargs(s1).max(maxargs(s2)),
        Stm::AssignStm(_, e)     => maxargsexp(e),
        Stm::PrintStm(v)         => v.iter().map(maxargsexp).max().unwrap_or(0),
    }
}

pub fn maxargsexp(e: &Exp) -> i32 {
    match e {
        Exp::OpExp(e1, _, e2) => maxargsexp(e1).max(maxargsexp(e2)),
        Exp::EseqExp(s, e1)   => maxargs(s).max(maxargsexp(e1)),
        _ => 0,
    }
}

/* ---------- Interpreter ---------- */

type Table = Vec<(Id, i32)>;

pub fn lookup(t: &Table, k: &str) -> i32 {
    t.iter()
        .find(|(key, _)| key == k)
        .map(|(_, v)| *v)
        .expect("unbound id")
}

pub fn update_table(mut t: Table, k: Id, v: i32) -> Table {
    t.push((k, v));
    t
}

/// Evaluate an expression, returning its value and the (possibly) updated table.
pub fn interp_exp(e: &Exp, t: Table) -> (i32, Table) {
    match e {
        Exp::IdExp(s)    => (lookup(&t, s), t),
        Exp::NumExp(n)   => (*n, t),
        Exp::OpExp(e1, op, e2) => {
            let (i1, t1) = interp_exp(e1, t);
            let (i2, t2) = interp_exp(e2, t1);
            let i3 = match op {
                Binop::Plus  => i1 + i2,
                Binop::Minus => i1 - i2,
                Binop::Times => i1 * i2,
                Binop::Div   => i1 / i2,
            };
            (i3, t2)
        }
        Exp::EseqExp(s1, e1) => {
            let t1 = interp(s1, t);
            interp_exp(e1, t1)
        }
    }
}

/// Interpret a statement, returning the new table. Printing is a side effect.
pub fn interp(s: &Stm, t: Table) -> Table {
    match s {
        Stm::CompoundStm(s1, s2) => {
            let t1 = interp(s1, t);
            interp(s2, t1)
        }
        Stm::AssignStm(id, e) => {
            let (n, t1) = interp_exp(e, t);
            update_table(t1, id.clone(), n)
        }
        Stm::PrintStm(v) => {
            let mut t_curr = t;
            for (i, e) in v.iter().enumerate() {
                let (n, t_next) = interp_exp(e, t_curr);
                if i + 1 == v.len() { println!("{n}"); } else { print!("{n} "); }
                t_curr = t_next;
            }
            t_curr
        }
    }
}

type Key = String;

#[derive(Clone, Debug)]
pub enum BinTree {
    Leaf,
    Tree(Box<BinTree>, Key, Box<BinTree>)
}

pub fn insert(k : &Key, t : &BinTree) -> BinTree {
    match t {
        BinTree::Leaf => BinTree::Tree(Box::new(BinTree::Leaf), k.to_string(), Box::new(BinTree::Leaf)),
        BinTree::Tree(lt, x, rt) => {
            if k < &x {
                BinTree::Tree(Box::new(insert(k, &lt)), x.to_string(), rt.clone())
            } else if k > x {
                BinTree::Tree(lt.clone(), x.to_string(), Box::new(insert(k, &rt)))
            } else {
                t.clone()
            }
        }
    }
}