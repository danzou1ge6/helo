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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeVarStore<'s>(Vec<ast::Type<'s>>);

impl<'s> TypeVarStore<'s> {
    pub fn clone_vec(&self) -> Vec<ast::Type<'s>> {
        self.0.clone()
    }
    pub fn empty() -> Self {
        Self(Vec::new())
    }
    pub fn as_slice(&self) -> &[ast::Type<'s>] {
        &self.0
    }
}

impl<'s> std::ops::Index<ast::TypeVarId> for TypeVarStore<'s> {
    type Output = ast::Type<'s>;
    fn index(&self, index: ast::TypeVarId) -> &Self::Output {
        &self.0[index.0]
    }
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

    /// Find without path compression
    pub fn find_immutable(&self, addr: ast::TypeVarId) -> ast::TypeVarId {
        let same_as = match &self.get(addr) {
            Slot::Value(ast::Type {
                node: ast::TypeNode::Var(var_id),
            }) => *var_id,
            _ => return addr,
        };
        self.find_immutable(same_as)
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
        (addr..addr + cnt).for_each(|_| {
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
#[derive(Clone)]
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

    pub fn export_var_store(&self) -> TypeVarStore<'s> {
        TypeVarStore(
            self.uf
                .heap
                .iter()
                .map(|x| match x {
                    Slot::Value(v) => v.clone(),
                    Slot::Empty => panic!("cannot export var store: some type-var undetermined"),
                })
                .collect(),
        )
    }

    /// WARNING The underlying type variable must be empty upon calling this function, or the algorithm will go wrong
    /// Write `type` to root of a equivalent class root `root`. If `type_` is also refering to a type variable, execute
    /// root-merge on them
    fn fill_empty_slot(&mut self, root: ast::TypeVarId, type_: &ast::Type<'s>) {
        match type_ {
            ast::Type {
                node: ast::TypeNode::Var(var_id),
            } => {
                let root1 = self.uf.find(*var_id);
                if root != root1 {
                    *self.uf.get_mut(root) = Slot::Value(ast::Type {
                        node: ast::TypeNode::Var(root1),
                    })
                }
            }
            other => *self.uf.get_mut(root) = Slot::Value(other.clone()),
        }
    }

    /// Unify type variable `id` wit `type_`.
    /// `Lock` indicates whether free variables in type var `id` can be modified
    /// (Including `id` itself)
    fn update_var_no_rollback_left_lock<const LOCK: bool>(
        &mut self,
        id: ast::TypeVarId,
        type_: &ast::Type<'s>,
    ) -> Result<(), ()> {
        let root = self.uf.find(id);
        if let ast::TypeNode::Var(type_var) = type_.node {
            if self.uf.find(type_var) == root {
                return Ok(());
            }
        }
        match self.uf.get(root).clone() {
            Slot::Empty if !LOCK => self.fill_empty_slot(root, type_),
            Slot::Empty => return Err(()),
            Slot::Value(old_value) => self.unify_no_rollback_lock::<LOCK>(&old_value, &type_)?,
        };
        Ok(())
    }

    /// Similar to `update_var_no_rollback_left_lock`, but `Lock` indicates
    /// whether free variables in `type_` can be modified
    fn update_var_no_rollback_right_lock<const LOCK: bool>(
        &mut self,
        id: ast::TypeVarId,
        type_: &ast::Type<'s>,
    ) -> Result<(), ()> {
        let root = self.uf.find(id);
        if let ast::TypeNode::Var(type_var) = type_.node {
            if self.uf.find(type_var) == root {
                return Ok(());
            }
        }
        match self.uf.get(root).clone() {
            Slot::Empty => self.fill_empty_slot(root, type_),
            Slot::Value(old_value) => self.unify_no_rollback_lock::<LOCK>(&type_, &old_value)?,
        };
        Ok(())
    }

    /// Unify type variable `id` with `type_`
    pub fn update_var(&mut self, id: ast::TypeVarId, type_: &ast::Type<'s>) -> Result<(), ()> {
        let uf_backup = self.uf.clone();
        self.update_var_no_rollback_left_lock::<false>(id, type_)
            .map_err(|_| {
                self.uf = uf_backup;
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
    ) -> impl Iterator<Item = ast::Type<'static>> + 'a {
        self.alloc_vars(var_cnt).map(|id| ast::Type {
            node: ast::TypeNode::Var(id),
        })
    }

    pub fn unify_list_no_rollback_lock<'a, 'a1, 'b, 'b1, const LOCK: bool>(
        &mut self,
        a: impl Iterator<Item = &'a ast::Type<'s>>,
        b: impl Iterator<Item = &'b ast::Type<'s>>,
    ) -> Result<(), ()>
    where
        's: 'a + 'b + 'a1 + 'b1,
    {
        for (a, b) in a.zip(b) {
            self.unify_no_rollback_lock::<LOCK>(a, b)?;
        }
        Ok(())
    }

    pub fn unify_list<'a, 'b>(
        &mut self,
        a: impl Iterator<Item = &'a ast::Type<'s>>,
        b: impl Iterator<Item = &'b ast::Type<'s>>,
    ) -> Result<(), usize>
    where
        's: 'a + 'b,
    {
        self.unify_list_lock::<false>(a, b)
    }

    pub fn unify_list_lock<'a, 'b, const LOCK: bool>(
        &mut self,
        a: impl Iterator<Item = &'a ast::Type<'s>>,
        b: impl Iterator<Item = &'b ast::Type<'s>>,
    ) -> Result<(), usize>
    where
        's: 'a + 'b,
    {
        let uf_backup = self.uf.clone();
        for (i, (a, b)) in a.zip(b).enumerate() {
            if let Err(..) = self.unify_no_rollback_lock::<LOCK>(a, b) {
                self.uf = uf_backup;
                return Err(i);
            }
        }
        Ok(())
    }

    pub fn unify_callable_no_rallback_lock<const LOCK: bool>(
        &mut self,
        a_params: &[ast::Type<'s>],
        a_ret: &ast::Type<'s>,
        b_params: &[ast::Type<'s>],
        b_ret: &ast::Type<'s>,
    ) -> Result<(), ()> {
        if a_params.len() != b_params.len() {
            return Err(());
        }

        self.unify_list_no_rollback_lock::<LOCK>(a_params.iter(), b_params.iter())?;
        self.unify_no_rollback_lock::<LOCK>(a_ret, b_ret)
    }

    /// Unify type expressions `a` with `b`.
    /// `Lock` in all methods ending with `_lock` determines whether free variables
    /// in `a` can be modified
    pub fn unify_no_rollback_lock<const LOCK: bool>(
        &mut self,
        a: &ast::Type<'s>,
        b: &ast::Type<'s>,
    ) -> Result<(), ()> {
        use ast::TypeNode::*;
        let r =
            match (&a.node, &b.node) {
                (Unit, Unit) => Ok(()),
                (Var(a_id), _) => Ok(self.update_var_no_rollback_left_lock::<LOCK>(*a_id, b)?),
                (_, Var(b_id)) => Ok(self.update_var_no_rollback_right_lock::<LOCK>(*b_id, a)?),
                (Never, _) => Ok(()),
                (_, Never) => Ok(()),
                (Tuple(a_list, ..), Tuple(b_list, ..)) => Ok(self
                    .unify_list_no_rollback_lock::<LOCK>(a_list.iter(), b_list.iter())
                    .map_err(|_| ())?),
                (Generic(a_template, a_args), Generic(b_template, b_args))
                    if a_template == b_template =>
                {
                    Ok(self
                        .unify_list_no_rollback_lock::<LOCK>(a_args.iter(), b_args.iter())
                        .map_err(|_| ())?)
                }
                (Callable(a), Callable(b)) => self
                    .unify_callable_no_rallback_lock::<LOCK>(&a.params, &a.ret, &b.params, &b.ret),
                (ImpureCallable(a), ImpureCallable(b)) => self
                    .unify_callable_no_rallback_lock::<LOCK>(&a.params, &a.ret, &b.params, &b.ret),
                (Primitive(pa), Primitive(pb)) if pa == pb => Ok(()),
                _ => Err(()),
            };
        r
    }

    pub fn unify(&mut self, a: &ast::Type<'s>, b: &ast::Type<'s>) -> Result<(), ()> {
        let uf_backup = self.uf.clone();
        self.unify_no_rollback_lock::<false>(a, b).map_err(|_| {
            self.uf = uf_backup;
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
                },
                _ => unreachable!(),
            },
        )
    }

    /// Repeatingly replace all non-empty type variables with the type expression until they hold, until
    /// no variable is non-empty
    fn resolve_var_descdent(
        &self,
        id: ast::TypeVarId,
        meta: &ast::Meta,
        var_stack: &mut Vec<ast::TypeVarId>,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        let root_var_id = self.uf.find_immutable(id);
        match &self.uf.get(root_var_id).clone() {
            Slot::Value(v) => {
                if var_stack.iter().rfind(|x| **x == root_var_id).is_some() {
                    return Err(errors::InfiniteType::new(id, meta, v));
                }
                var_stack.push(root_var_id);
                let r = self.resolve_descdent(v, meta, var_stack)?;
                var_stack.pop().unwrap();
                Ok(r)
            }
            Slot::Empty => Ok(ast::Type::new_var(root_var_id)),
        }
    }

    pub fn resolve_var(
        &self,
        id: ast::TypeVarId,
        meta: &ast::Meta,
    ) -> Result<ast::Type<'s>, errors::InfiniteType> {
        self.resolve_var_descdent(id, meta, &mut Vec::new())
    }

    /// Replace all variables in `type_`.
    fn resolve_descdent<T: ast::TypeApplyResult<'s>>(
        &self,
        type_: &T,
        meta: &ast::Meta,
        var_stack: &mut Vec<ast::TypeVarId>,
    ) -> Result<T, errors::InfiniteType> {
        use ast::TypeNode::*;
        // During traversing of the tree, some [`Var`] node resolves to another variable, then the resolution
        // is not clean
        type_.apply_result(&|t| matches!(t.node, Var(_)), &mut |t| match t.node {
            Var(v) => self.resolve_var_descdent(v, meta, var_stack),
            _ => unreachable!(),
        })
    }

    pub fn resolve<T: ast::TypeApplyResult<'s>>(
        &self,
        type_: &T,
        meta: &ast::Meta,
    ) -> Result<T, errors::InfiniteType> {
        self.resolve_descdent(type_, meta, &mut Vec::new())
    }

    /// Walk `type_` and for each variable that hasn't been deternimed, map them to a continuous range
    /// of integers, starting from zero.
    ///
    /// For example, `[1, 4, [1] -> 3] -> 3` is mapped to `[0, 1, [0] -> 2] -> 2` by the returned HashMap
    pub fn discretization<T>(
        &mut self,
        type_: &T,
    ) -> (HashMap<ast::TypeVarId, ast::TypeVarId>, usize)
    where
        T: ast::TypeWalk<'s>,
    {
        let mut map = HashMap::new();
        let mut counter = 0;

        fn walk_f<'s>(
            inferer: &mut Inferer<'s>,
            map: &mut HashMap<ast::TypeVarId, ast::TypeVarId>,
            counter: &mut usize,
            type_: &ast::Type<'s>,
        ) {
            use ast::TypeNode::*;
            match &type_.node {
                Var(v) => {
                    let root_var_id = inferer.uf.find(*v);
                    match &inferer.uf.get(root_var_id).clone() {
                        Slot::Value(v) => {
                            walk_f(inferer, map, counter, v);
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
                _ => {}
            };
        }

        type_.walk(&mut |type_| walk_f(self, &mut map, &mut counter, type_));
        (map, counter)
    }

    pub fn discretization_applied<T>(&mut self, type_: &T) -> (T, usize)
    where
        T: ast::TypeWalk<'s> + ast::TypeApply<'s>,
    {
        let (map, var_cnt) = self.discretization(type_);
        (
            type_.substitute_vars_with_nodes(&|id| ast::TypeNode::Var(map[&id])),
            var_cnt,
        )
    }

    pub fn new_slots(&mut self, cnt: usize) -> ast::TypeVarId {
        self.uf.new_slots(cnt)
    }

    pub fn instantiate_wildcard(&mut self, type_: &ast::Type<'s>) -> ast::Type<'s> {
        use ast::TypeApply;
        type_.apply(&|t| matches!(&t.node, ast::TypeNode::WildCard), &mut |_| {
            ast::Type::new_var(self.alloc_var())
        })
    }
}
