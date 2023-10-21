use std::collections::HashMap;

use crate::ast;

pub type OpPriority = u32;

pub struct Precedence(pub OpPriority, pub OpPriority);

impl Precedence {
    pub fn left(&self) -> OpPriority {
        self.0
    }
    pub fn right(&self) -> OpPriority {
        self.1
    }
}

pub struct PrecedenceTable<'s>(HashMap<&'s str, Precedence>);

impl<'s> PrecedenceTable<'s> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, k: &'s str, v: Precedence) -> Option<Precedence> {
        self.0.insert(k, v)
    }

    pub fn get(&self, k: &str) -> Option<&Precedence> {
        self.0.get(k)
    }

    pub fn priority_left(&self, id: &str) -> OpPriority {
        self.get(id).map(|p| p.left()).map_or_else(|| 0, |x| x)
    }
    pub fn priority_right(&self, id: &str) -> OpPriority {
        self.get(id).map(|p| p.right()).map_or_else(|| 0, |x| x)
    }
}

type ResolutionEntry<'s> = (&'s str, ast::LocalId);

#[derive(Default)]
pub struct FunctionResolutionEnv<'s> {
    locals: Vec<ResolutionEntry<'s>>,
    scope_cnt: Vec<usize>,
    id_cnt: ast::LocalId,
}

impl<'s> FunctionResolutionEnv<'s> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            scope_cnt: vec![0],
            id_cnt: 0.into(),
        }
    }
    fn local_cnt(&self) -> usize {
        self.id_cnt.into()
    }
}

pub enum ResolutionEnv<'s> {
    Normal(FunctionResolutionEnv<'s>),
    InClosure {
        parent: FunctionResolutionEnv<'s>,
        current: FunctionResolutionEnv<'s>,
        captures: Vec<ast::LocalId>,
        caputures_meta: Vec<ast::Meta>,
        this_name: &'s str,
    },
}

impl<'s> ResolutionEnv<'s> {
    fn current(&mut self) -> &mut FunctionResolutionEnv<'s> {
        match self {
            ResolutionEnv::Normal(current) => current,
            ResolutionEnv::InClosure { current, .. } => current,
        }
    }
    fn pop(&mut self, cnt: usize) {
        let new_len = self.current().locals.len() - cnt;
        self.current()
            .locals
            .resize_with(new_len, || unreachable!());
    }
    fn pop_scope(&mut self) {
        let pop_cnt = self.current().scope_cnt.pop().unwrap();
        self.pop(pop_cnt);
    }
}

pub struct Resolver<'s>(ResolutionEnv<'s>);

impl<'s> Resolver<'s> {
    pub fn resolve(&mut self, name: &str, meta: &ast::Meta) -> Option<ast::Expr<'s>> {
        match self {
            Self(ResolutionEnv::Normal(f_res), ..) => f_res
                .locals
                .iter()
                .rfind(|(n, _)| *n == name)
                .map(|(_, id)| ast::Expr::new_untyped(ast::ExprNode::Local(*id), meta.clone())),

            // See documentation on [`ast::Closure_`] for how locals are laid out
            Self(
                ResolutionEnv::InClosure {
                    parent,
                    current,
                    captures,
                    caputures_meta,
                    this_name,
                },
                ..,
            ) => {
                current
                    .locals
                    .iter()
                    .rfind(|(n, _)| *n == name)
                    .map_or_else(
                        // Identifier not found as local, look in parent namespace, that is the function body surrounding the closure
                        || {
                            parent.locals.iter().rfind(|(n, _)| *n == name).map_or(
                                None,
                                |(_, id)| {
                                    // Found in parent namespace
                                    if let Some(i) = captures.iter().position(|i| *i == *id) {
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured(i.into(), *this_name == name),
                                            meta.clone(),
                                        ))
                                    } else {
                                        captures.push(*id);
                                        caputures_meta.push(meta.clone());
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured(
                                                (captures.len() - 1).into(),
                                                *this_name == name,
                                            ),
                                            meta.clone(),
                                        ))
                                    }
                                },
                            )
                        },
                        // Local
                        |(_, id)| {
                            Some(ast::Expr::new_untyped(
                                ast::ExprNode::Local(*id),
                                meta.clone(),
                            ))
                        },
                    )
            }
        }
    }
    pub fn define_local(&mut self, name: &'s str) -> ast::LocalId {
        let id = self.0.current().id_cnt;
        self.0.current().locals.push((name, id));
        *self.0.current().scope_cnt.last_mut().unwrap() += 1;
        self.0.current().id_cnt = id + 1;
        id
    }

    // Returns local count of the function, along with result of `f`
    pub fn with_function_scope<R>(&mut self, f: impl FnOnce(&mut Resolver<'s>) -> R) -> (usize, R) {
        self.0 = ResolutionEnv::Normal(FunctionResolutionEnv::new());
        let r = f(self);
        (self.0.current().local_cnt(), r)
    }

    /// All variables defined in `f` will be popped upon exit
    pub fn with_scope<R>(&mut self, f: impl FnOnce(&mut Resolver<'s>) -> R) -> R {
        self.0.current().scope_cnt.push(0);
        let r = f(self);

        self.0.pop_scope();
        r
    }

    /// Set current status to "in-closure", that is , identifiers are resolved further in surrunding namespace
    /// if they are not found in local namespace.
    /// Return (captures, number of locals, result of `f`)
    pub fn with_closure_scope<R>(
        &mut self,
        this_name: &'s str,
        f: impl FnOnce(&mut Resolver<'s>) -> R,
    ) -> (Vec<ast::LocalId>, Vec<ast::Meta>, usize, R) {
        let parent = match &mut self.0 {
            ResolutionEnv::Normal(p) => std::mem::take(p),
            _ => unreachable!(),
        };
        self.0 = ResolutionEnv::InClosure {
            parent,
            current: FunctionResolutionEnv::new(),
            captures: Vec::new(),
            caputures_meta: Vec::new(),
            this_name,
        };

        let r = f(self);

        let (parent, captures, captures_meta, locals_cnt) = match &mut self.0 {
            ResolutionEnv::InClosure {
                parent,
                captures,
                current,
                caputures_meta,
                ..
            } => (
                std::mem::take(parent),
                std::mem::take(captures),
                std::mem::take(caputures_meta),
                current.local_cnt(),
            ),
            _ => unreachable!(),
        };
        self.0 = ResolutionEnv::Normal(parent);
        (captures, captures_meta, locals_cnt, r)
    }
    pub fn new() -> Self {
        Self(ResolutionEnv::Normal(FunctionResolutionEnv::new()))
    }
}
