use std::collections::HashSet;

use crate::ast::{self, Relation, RelationName, Trie};
use crate::errors::InfiniteType;
use crate::inferer::Inferer;
use ast::{Constrain, InstanceId, InstanceIdTable};

pub struct Assumptions<'s>(HashSet<Constrain<'s>>);

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error<'s> {
    #[error("Fail to prove required constrain")]
    Fail,
    #[error(transparent)]
    Resolve(#[from] InfiniteType),
    #[error("Too many hits for matching instances")]
    TooManyHit(Vec<InstanceId<'s>>),
}

impl<'s> Assumptions<'s> {
    pub fn contains(
        &self,
        c: &Constrain<'s>,
        inferer: &mut Inferer<'s>,
    ) -> Result<bool, InfiniteType> {
        for assump in self.0.iter() {
            let assump_resolved = inferer.resolve(assump, &assump.meta)?;
            let c_resolved = inferer.resolve(c, &c.meta)?;
            if c_resolved == assump_resolved {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn empty() -> Self {
        Self(HashSet::new())
    }

    pub fn which_instance(
        &self,
        mut inferer: Inferer<'s>,
        c: &Constrain<'s>,
        instances: &InstanceIdTable<'s, ast::Instance<'s>>,
        relations: &Trie<RelationName<'s>, Relation<'s>, &'s str>,
    ) -> Result<(Option<InstanceId<'s>>, Inferer<'s>), Error> {
        if self.contains(c, &mut inferer)? {
            return Ok((None, inferer));
        }

        let matches = instances
            .of_rel(&c.rel_name)
            .unwrap_or(&[])
            .iter()
            .enumerate()
            .filter_map(|(i, ins)| {
                let mut inferer1 = inferer.clone();

                let ins = inferer1.rename_type_vars_free(ins, ins.var_cnt);
                let rel = relations.get(&c.rel_name)?;

                // free variables in non-dependent arguments in `c` are not allowed to
                // be modified
                if let Ok(()) = inferer1.unify_list_lock::<true>(
                    c.args
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| !rel.dependent.contains(i))
                        .map(|(_, t)| t),
                    ins.rel
                        .args
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| !rel.dependent.contains(i))
                        .map(|(_, t)| t),
                ) {
                    // unify dependent arguments
                    inferer1
                        .unify_list(
                            c.args
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| rel.dependent.contains(i))
                                .map(|(_, t)| t),
                            ins.rel
                                .args
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| rel.dependent.contains(i))
                                .map(|(_, t)| t),
                        )
                        .map(|_| {
                            // check hypothesises leading to `c`
                            let r = ins.constrains.iter().try_fold(inferer1, |inferer1, c| {
                                self.which_instance(inferer1.clone(), c, instances, relations)
                                    .map(|(_, inferer)| inferer)
                            });

                            if let Ok(inferer1) = r {
                                Some((
                                    InstanceId {
                                        rel_name: c.rel_name.clone(),
                                        n: i,
                                    },
                                    inferer1,
                                ))
                            } else {
                                None
                            }
                        })
                        .unwrap_or(None)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if matches.len() == 0 {
            Err(Error::Fail)
        } else if matches.len() == 1 {
            let mut matches = matches;
            let (ins, inf) = matches.pop().unwrap();
            Ok((Some(ins), inf))
        } else {
            Err(Error::TooManyHit(
                matches.into_iter().map(|(ins, _)| ins).collect(),
            ))
        }
    }

    pub fn which_instance_no_dependent(
        &self,
        inferer: Inferer<'s>,
        c: &Constrain<'s>,
        instances: &InstanceIdTable<'s, ast::Instance<'s>>,
        relations: &Trie<RelationName<'s>, Relation<'s>, &'s str>,
    ) -> Result<(InstanceId<'s>, Inferer<'s>), Error> {
        let matches = instances
            .of_rel(&c.rel_name)
            .unwrap_or(&[])
            .iter()
            .enumerate()
            .filter_map(|(i, ins)| {
                let mut inferer1 = inferer.clone();

                let ins = inferer1.rename_type_vars_free(ins, ins.var_cnt);

                if let Ok(()) = inferer1.unify_list_lock::<true>(c.args.iter(), ins.rel.args.iter())
                {
                    // check hypothesises leading to `c`
                    let r = ins.constrains.iter().try_fold(inferer1, |inferer1, c| {
                        self.which_instance(inferer1.clone(), c, instances, relations)
                            .map(|(_, inferer)| inferer)
                    });

                    if let Ok(inferer1) = r {
                        Some((
                            InstanceId {
                                rel_name: c.rel_name.clone(),
                                n: i,
                            },
                            inferer1,
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if matches.len() == 0 {
            Err(Error::Fail)
        } else if matches.len() == 1 {
            let mut matches = matches;
            let (ins, inf) = matches.pop().unwrap();
            Ok((ins, inf))
        } else {
            Err(Error::TooManyHit(
                matches.into_iter().map(|(ins, _)| ins).collect(),
            ))
        }
    }
}

impl<'s> FromIterator<Constrain<'s>> for Assumptions<'s> {
    fn from_iter<T: IntoIterator<Item = Constrain<'s>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
