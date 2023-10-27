use std::collections::HashMap;

use crate::ast;

#[derive(Debug, Clone)]
/// Represents a type variable
pub enum Slot<'s> {
    /// The type variable is bound to some other type expression
    Value(ast::Type<'s>),
    /// The type variable is undetermined
    Empty,
}

#[derive(Debug, Clone)]
/// A heap of type variables
pub struct UnionFind<'s> {
    heap: imbl::Vector<Slot<'s>>,
}

impl<'s> UnionFind<'s> {
    pub fn new() -> Self {
        UnionFind {
            heap: imbl::Vector::new(),
        }
    }

    fn get(&self, addr: ast::TypeVarId) -> &Slot<'s> {
        &self.heap[addr.0]
    }

    fn get_mut(&mut self, addr: ast::TypeVarId) -> &mut Slot<'s> {
        &mut self.heap[addr.0]
    }

    /// Find the root of a class of equivalent type variables. NOTE that path compression is applied
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

    /// Allocate a new empty type variale on heap
    pub fn new_slot(&mut self) -> ast::TypeVarId {
        let addr = self.heap.len();
        self.heap.push_back(Slot::Empty);
        addr.into()
    }

    /// Allocate a continuous series of empty type variables on heap, returning the index of the first one
    pub fn new_slots(&mut self, cnt: usize) -> ast::TypeVarId {
        let addr = self.heap.len();
        (0..addr + cnt).for_each(|_| {
            self.heap.push_back(Slot::Empty);
        });
        addr.into()
    }
}

/// The type inferer. Inference are performed seperatedly for each function.
/// Unfortunately, only self-recursion is supported. If function A refers to function B, and function B refers indirectly or
/// directly to function A, then at least one of them must be type annotated by the programmer.
///
/// All methods suffixed with `no_rollback` won't rollback to previous state upon unification failure.
/// Currently, rollback upon unfication failure is implemented simply by cloning the underlying union-find array,
/// which is less then efficient.
pub struct Inferer<'s> {
    /// THe underlying union-find set
    uf: UnionFind<'s>,
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
    pub fn new() -> Self {
        Self {
            uf: UnionFind::new(),
        }
    }

    /// WARNING The underlying type variable must be empty upon calling this function, or the algorithm will go wrong
    /// Write `type` to root of a equivalent class root `root`. If `type_` is also refering to a type variable, execute
    /// root-merge on them
    fn fill_empty_slot(&mut self, root: ast::TypeVarId, type_: &ast::Type<'s>) {
        match type_ {
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
        }
    }

    /// Unify type variable `id` wit `type_`
    fn update_var_no_rollback(
        &mut self,
        id: ast::TypeVarId,
        type_: &ast::Type<'s>,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        let root = self.uf.find(id);
        match self.uf.get(root).clone() {
            Slot::Empty => self.fill_empty_slot(root, type_),
            Slot::Value(old_value) => self.unify_no_rollback(&old_value, &type_, meta)?,
        };
        Ok(())
    }

    /// Unify type variable `id` with `type_`
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

    /// Allocate `var_cnt` of empty type variables and returns an Iterator yielding them.
    /// NOTE that the caller has to provide the metas for those new types
    pub fn alloc_type_vars<'a>(
        &mut self,
        var_cnt: usize,
        meta_i: &'a (dyn Fn(usize) -> ast::Meta),
    ) -> impl Iterator<Item = ast::Type<'static>> + 'a {
        self.alloc_vars(var_cnt)
            .enumerate()
            .map(|(i, id)| ast::Type {
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
    /// Unify type expressions `a` with `b`
    fn unify_no_rollback(
        &mut self,
        a: &ast::Type<'s>,
        b: &ast::Type<'s>,
        meta: &ast::Meta,
    ) -> Result<(), errors::UnificationFailure> {
        use ast::TypeNode::*;
        let r = match (&a.node, &b.node) {
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
                Ok(self.unify_list_no_rollback(a_args.iter(), b_args.iter(), meta)?)
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
            _ => Err(errors::UnificationFailure::new(a, b, meta, false)),
        };
        r
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
    pub fn rename_type_vars<T: ast::TypeApply<'s>>(&mut self, type_: &T, var_cnt: usize) -> T {
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

    /// Repeatingly replace all non-empty type variables with the type expression until they hold, until
    /// no variable is non-empty
    pub fn resolve_var_descdent(
        &mut self,
        id: ast::TypeVarId,
        meta: &ast::Meta,
        var_stack: &mut Vec<ast::TypeVarId>,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        let root_var_id = self.uf.find(id);
        match &self.uf.get(root_var_id).clone() {
            Slot::Value(v) => {
                if var_stack.iter().rfind(|x| **x == root_var_id).is_some() {
                    return Err(errors::InfiniteType::new(id, meta, v));
                }
                var_stack.push(root_var_id);
                self.resolve_descdent(&v, var_stack)
            }
            Slot::Empty => Ok(ast::Type::new_var(root_var_id, meta.clone())),
        }
    }

    pub fn resolve_var(
        &mut self,
        id: ast::TypeVarId,
        meta: &ast::Meta,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        self.resolve_var_descdent(id, meta, &mut Vec::new())
    }

    /// Replace all variables in `type_`.
    fn resolve_descdent(
        &mut self,
        type_: &ast::Type<'s>,
        var_stack: &mut Vec<ast::TypeVarId>,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        use ast::TypeNode::*;
        // During traversing of the tree, some [`Var`] node resolves to another variable, then the resolution
        // is not clean
        type_.apply_result(&|t| matches!(t.node, Var(_)), &mut |t| match t.node {
            Var(v) => self.resolve_var_descdent(v, &t.meta, var_stack),
            _ => unreachable!(),
        })
    }

    pub fn resolve(
        &mut self,
        type_: &ast::Type<'s>,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        self.resolve_descdent(type_, &mut Vec::new())
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
    /// of integers, starting from zero.
    ///
    /// For example, `[1, 4, [1] -> 3] -> 3` is mapped to `[0, 1, [0] -> 2] -> 2` by the returned HashMap
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
    pub fn discretization_function(
        &mut self,
        type_: &ast::FunctionType<'s>,
    ) -> (HashMap<ast::TypeVarId, ast::TypeVarId>, usize) {
        let mut map = HashMap::new();
        let mut counter = 0;

        type_
            .params
            .iter()
            .for_each(|type_| self.discretization_walk(type_, &mut map, &mut counter));
        type_
            .captures
            .iter()
            .for_each(|type_| self.discretization_walk(type_, &mut map, &mut counter));
        self.discretization_walk(&type_.ret, &mut map, &mut counter);
        (map, counter)
    }

    pub fn new_slots(&mut self, cnt: usize) -> ast::TypeVarId {
        self.uf.new_slots(cnt)
    }

    pub fn instantiate_wildcard(&mut self, type_: &ast::Type<'s>) -> ast::Type<'s> {
        use ast::TypeApply;
        type_.apply(&|t| matches!(&t.node, ast::TypeNode::WildCard), &mut |_| {
            ast::Type::new_var(self.alloc_var(), type_.meta.clone())
        })
    }
}
