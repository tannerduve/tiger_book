mod intro;
mod ast;
mod symbol; 
mod types;
use crate::intro::{Exp, Stm, BinTree, Binop, interp};

fn main() {
        let prog = Stm::CompoundStm(
        Box::new(Stm::AssignStm(
            "a".to_string(),
            Exp::OpExp(
                Box::new(Exp::NumExp(5)),
                Binop::Plus,
                Box::new(Exp::NumExp(3)),
            ),
        )),
        Box::new(Stm::CompoundStm(
            Box::new(Stm::AssignStm(
                "b".to_string(),
                Exp::EseqExp(
                    Box::new(Stm::PrintStm(vec![
                        Exp::IdExp("a".to_string()),
                        Exp::OpExp(
                            Box::new(Exp::IdExp("a".to_string())),
                            Binop::Minus,
                            Box::new(Exp::NumExp(1)),
                        ),
                    ])),
                    Box::new(Exp::OpExp(
                        Box::new(Exp::NumExp(1)),
                        Binop::Times,
                        Box::new(Exp::IdExp("a".to_string())),
                    )),
                ),
            )),
            Box::new(Stm::PrintStm(vec![Exp::IdExp("b".to_string())])),
        )),
    );
    let v = vec![];
    interp(&prog, v);
}
