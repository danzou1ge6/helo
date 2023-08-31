use std::collections::HashMap;

use crate::ast;

#[derive(Debug, Clone)]
pub enum Slot<'s> {
    Value(ast::Type<'s>),
    Empty,
}

#[derive(Debug, Clone)]
pub struct UnionFind<'s> {
    /// NOTE First few elements on `heap` are reserved for locals
    heap: Vec<Slot<'s>>,
}

impl<'s> UnionFind<'s> {
    pub fn new() -> Self {
        UnionFind { heap: vec![] }
    }

    fn get(&self, addr: ast::TypeVarId) -> &Slot<'s> {
        &self.heap[addr.0]
    }

    fn get_mut(&mut self, addr: ast::TypeVarId) -> &mut Slot<'s> {
        &mut self.heap[addr.0]
    }

    pub fn find(&mut self, addr: ast::TypeVarId) -> ast::TypeVarId {
        let same_as = match &self.get(addr) {
            Slot::Value(ast::Type {
                node: ast::TypeNode::Var(var_id),
                ..
            }) => *var_id,
            _ => return addr,
        };
        let root = self.find(same_as);
        match self.get_mut(addr) {
            Slot::Value(ast::Type {
                node: ast::TypeNode::Var(var_id),
                ..
            }) => *var_id = root,
            _ => unreachable!(),
        };
        root
    }

    pub fn new_slot(&mut self) -> ast::TypeVarId {
        let addr = self.heap.len();
        self.heap.push(Slot::Empty);
        addr.into()
    }

    pub fn new_slots(&mut self, cnt: usize) -> ast::TypeVarId {
        let addr = self.heap.len();
        self.heap.resize_with(addr + cnt, || Slot::Empty);
        addr.into()
    }
}

pub struct Inferer<'s> {
    uf: UnionFind<'s>,
    function_name: String
}

use crate::errors;

impl<'s> std::fmt::Debug for Inferer<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, slot) in self.uf.heap.iter().enumerate() {
            match slot {
                Slot::Empty => write!(f, "{}: Empty\n", i)?,
                Slot::Value(v) => write!(f, "{}: {}\n", i, v)?,
            };
        }
        write!(f, "")
    }
}

impl<'s> Inferer<'s> {
    pub fn new(f_name: String) -> Self {
        Self {
            function_name: f_name,
            uf: UnionFind::new(),
        }
    }

    pub fn function_name(&self) -> &str {
        &self.function_name
    }

    fn update_var_no_rollback(
        &mut self,
        id: ast::TypeVarId,
        type_: &ast::Type<'s>,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        let root = self.uf.find(id);
        
        match self.uf.get(root).clone() {
            Slot::Empty => match type_ {
                ast::Type {
                    node: ast::TypeNode::Var(var_id),
                    meta,
                } => {
                    let root1 = self.uf.find(*var_id);
                    if root != root1 {
                        *self.uf.get_mut(root) = Slot::Value(ast::Type {
                            node: ast::TypeNode::Var(root1),
                            meta: meta.clone(),
                        })
                    }
                }
                other => *self.uf.get_mut(root) = Slot::Value(other.clone()),
            },
            Slot::Value(old_value) => self.unify_no_rollback(&old_value, &type_, meta)?,
        };
        
        Ok(())
    }

    pub fn update_var(
        &mut self,
        id: ast::TypeVarId,
        type_: &ast::Type<'s>,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        let uf_backup = self.uf.clone();
        self.update_var_no_rollback(id, type_, meta).map_err(|e| {
            self.uf = uf_backup;
            e
        })
    }

    pub fn alloc_var(&mut self) -> ast::TypeVarId {
        self.uf.new_slot()
    }

    pub fn alloc_vars(&mut self, var_cnt: usize) -> impl Iterator<Item = ast::TypeVarId> {
        let offset = self.uf.new_slots(var_cnt).0;
        (offset..offset + var_cnt).map(|x| ast::TypeVarId(x))
    }

    pub fn alloc_type_vars<'a>(
        &mut self,
        var_cnt: usize,
        meta_i: &'a (dyn Fn(usize) -> ast::Meta),
    ) -> impl Iterator<Item = ast::Type<'static>> + 'a {
        self.alloc_vars(var_cnt).enumerate().map(|(i, id)| ast::Type {
            node: ast::TypeNode::Var(id),
            meta: meta_i(i),
        })
    }

    fn unify_list_no_rollback<'a, 'b, TA, TB>(
        &mut self,
        a: TA,
        b: TB,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure>
    where
        TA: Iterator<Item = &'a ast::Type<'s>>,
        's: 'a,
        TB: Iterator<Item = &'b ast::Type<'s>>,
        's: 'b,
    {
        for (a, b) in a.zip(b) {
            self.unify_no_rollback(a, b, meta)?;
        }
        Ok(())
    }

    pub fn unify_list<'a, 'b, TA, TB>(
        &mut self,
        a: TA,
        b: TB,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure>
    where
        TA: Iterator<Item = &'a ast::Type<'s>>,
        's: 'a,
        TB: Iterator<Item = &'b ast::Type<'s>>,
        's: 'b,
    {
        let uf_backup = self.uf.clone();
        self.unify_list_no_rollback(a, b, meta).map_err(|e| {
            self.uf = uf_backup;
            e
        })
    }

    fn unify_no_rollback(
        &mut self,
        a: &ast::Type<'s>,
        b: &ast::Type<'s>,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        use ast::TypeNode::*;
        match (&a.node, &b.node) {
            (Unit, Unit) => Ok(()),
            (Var(a_id), _) => Ok(self.update_var_no_rollback(*a_id, b, meta)?),
            (_, Var(b_id)) => Ok(self.update_var_no_rollback(*b_id, a, meta)?),
            (Never, _) => Ok(()),
            (_, Never) => Ok(()),
            (Tuple(a_list, ..), Tuple(b_list, ..)) => {
                Ok(self.unify_list_no_rollback(a_list.iter(), b_list.iter(), meta)?)
            }
            (Generic(a_template, a_args), Generic(b_template, b_args))
                if a_template == b_template =>
            {
                Ok(self.unify_list(a_args.iter(), b_args.iter(), meta)?)
            }
            (
                Callable(ast::CallableType {
                    params: a_params,
                    ret: a_ret,
                }),
                Callable(ast::CallableType {
                    params: b_params,
                    ret: b_ret,
                }),
            ) => {
                self.unify_list_no_rollback(a_params.iter(), b_params.iter(), meta)?;
                self.unify_no_rollback(a_ret, b_ret, meta)?;
                Ok(())
            }
            (Primitive(pa), Primitive(pb)) if pa == pb => Ok(()),
            _ => Err(errors::UnificationFailure::new(a, b, meta)),
        }
    }

    pub fn unify(
        &mut self,
        a: &ast::Type<'s>,
        b: &ast::Type<'s>,
        ctx_meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        let uf_backup = self.uf.clone();
        self.unify_no_rollback(a, b, ctx_meta).map_err(|e| {
            self.uf = uf_backup;
            e
        })
    }

    /// During type inference of function call, new type-vars may be introduced by the
    /// function template. Corresponding slots must be created in uf set, and those
    /// type-vars need to be renamed.
    pub fn rename_callable_type_vars(
        &mut self,
        type_: &ast::CallableType<'s>,
        var_cnt: usize,
    ) -> ast::CallableType<'s> {
        let offset = self.uf.new_slots(var_cnt);
        type_.apply(
            &|t| matches!(t.node, ast::TypeNode::Var(_)),
            &mut |t| match t.node {
                ast::TypeNode::Var(v) => ast::Type {
                    node: ast::TypeNode::Var(v + offset),
                    meta: t.meta.clone(),
                },
                _ => unreachable!(),
            },
        )
    }

    pub fn resolve_var(&mut self, id: ast::TypeVarId, meta: &ast::Meta) -> ast::Type<'s> {
        let root_var_id = self.uf.find(id);
        match &self.uf.get(root_var_id).clone() {
            Slot::Value(v) => {
                let (t, _) = self.resolve(&v);
                t
            }
            Slot::Empty => ast::Type::new_var(root_var_id, meta.clone()),
        }
    }

    /// Replace all variables in `type_`. Returns `(_, true)` if still some variables exist
    pub fn resolve(&mut self, type_: &ast::Type<'s>) -> (ast::Type<'s>, bool) {
        use ast::TypeNode::*;
        // During traversing of the tree, some [`Var`] node resolves to another variable, then the resolution
        // is not clean
        let mut not_clean = false;
        (
            type_.apply(&|t| matches!(t.node, Var(_)), &mut |t| match t.node {
                Var(v) => {
                    let root_var_id = self.uf.find(v);
                    match &self.uf.get(root_var_id).clone() {
                        Slot::Value(v) => {
                            let (t, nc) = self.resolve(&v);
                            not_clean |= nc;
                            t
                        }
                        Slot::Empty => {
                            not_clean = true;
                            ast::Type::new_var(root_var_id, t.meta.clone())
                        }
                    }
                }
                _ => unreachable!(),
            }),
            not_clean,
        )
    }

    fn discretization_walk(
        &mut self,
        type_: &ast::Type<'s>,
        map: &mut HashMap<ast::TypeVarId, ast::TypeVarId>,
        counter: &mut usize,
    ) {
        use ast::TypeNode::*;
        match &type_.node {
            Var(v) => {
                let root_var_id = self.uf.find(*v);
                match &self.uf.get(root_var_id).clone() {
                    Slot::Value(v) => {
                        self.discretization_walk(v, map, counter);
                    }
                    Slot::Empty => {
                        map.entry(root_var_id).or_insert_with(|| {
                            let x = *counter;
                            *counter += 1;
                            ast::TypeVarId::from(x)
                        });
                    }
                }
            }
            Callable(ca) => {
                ca.params
                    .iter()
                    .for_each(|t| self.discretization_walk(t, map, counter));
                self.discretization_walk(&ca.ret, map, counter);
            }
            Tuple(v) => {
                v.iter()
                    .for_each(|t| self.discretization_walk(t, map, counter));
            }
            Generic(_, args) => args
                .iter()
                .for_each(|a| self.discretization_walk(a, map, counter)),
            _ => {}
        }
    }

    /// Walk `type_` and for each variable that hasn't been deternimed, map them to a continuous range
    /// of integers, starting from zero
    pub fn discretization(
        &mut self,
        type_: &ast::Type<'s>,
    ) -> (HashMap<ast::TypeVarId, ast::TypeVarId>, usize) {
        let mut map = HashMap::new();
        let mut counter = 0;

        self.discretization_walk(type_, &mut map, &mut counter);
        (map, counter)
    }

    /// Same as `discretization`, but for [`ast::CallableType`]
    pub fn discretization_callable(
        &mut self,
        type_: &ast::CallableType<'s>,
    ) -> (HashMap<ast::TypeVarId, ast::TypeVarId>, usize) {
        let mut map = HashMap::new();
        let mut counter = 0;

        type_
            .params
            .iter()
            .for_each(|type_| self.discretization_walk(type_, &mut map, &mut counter));
        self.discretization_walk(&type_.ret, &mut map, &mut counter);
        (map, counter)
    }

    pub fn new_slots(&mut self, cnt: usize) -> ast::TypeVarId {
        self.uf.new_slots(cnt)
    }
}
