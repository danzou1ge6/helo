use crate::{ast, errors, parse, parse::tast};
use errors::ManyError;
use std::ffi::OsStr;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone)]
pub struct SourceFile {
    pub src: Arc<String>,
    pub file_path: Arc<String>,
    pub file_name: String,
}

#[derive(Clone)]
pub enum SourceTreeNode {
    File(SourceFile),
    Directory(String, Vec<SourceTreeNode>),
}

#[derive(Clone)]
pub struct SourceTree {
    nodes: Vec<SourceTreeNode>,
    root_file_name: String,
    root_dir: String,
    module_root: String,
}

impl SourceFile {
    pub fn parse_into<'s, 'a: 's>(
        &'a self,
        symbols: &mut tast::Symbols<'s>,
        path: ast::Path<'s>,
    ) -> Result<(), errors::ParseError> {
        parse::parse_ast(
            &self.src,
            self.src.clone(),
            self.file_path.clone(),
            path,
            symbols,
        )
    }

    /// Assumes `fp` is a good source file
    pub fn new(fp: PathBuf) -> Result<Self, Error> {
        let fp_str = fp.to_str().unwrap();
        let mut f = File::open(fp_str)?;
        let mut src = String::new();
        f.read_to_string(&mut src)?;
        Ok(SourceFile {
            src: Arc::new(src),
            file_path: Arc::new(fp_str.to_string()),
            file_name: fp.file_stem().unwrap().to_str().unwrap().to_string(),
        })
    }
}

use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("SourceTree creation failure")]
pub enum Error {
    Io(#[from] std::io::Error),
    #[error("{} is not valid utf-8 name", .0.display())]
    NonUtf8FileName(PathBuf),
    #[error("{} is neither file nor directory. What can it be?", .0.display())]
    NotFileNorDir(PathBuf),
    #[error("{} is not a file", .0.display())]
    NotFile(PathBuf),
}

impl SourceTreeNode {
    pub fn name(&self) -> &str {
        match self {
            Self::File(f) => &f.file_name,
            Self::Directory(n, _) => &n,
        }
    }
    pub fn walk<'s, 'a: 's>(
        &'a self,
        symbols: &mut tast::Symbols<'s>,
        parent_path: &ast::Path<'s>,
        e: &mut ManyError,
    ) {
        match self {
            Self::File(f) => {
                let p = if f.file_name == "mod" {
                    parent_path.clone()
                } else {
                    parent_path.clone().pushed(&f.file_name)
                };
                f.parse_into(symbols, p).unwrap_or_else(|err| {
                    e.push(err);
                })
            }
            Self::Directory(n, nodes) => {
                let path = parent_path.clone().pushed(&n);
                for node in nodes {
                    node.walk(symbols, &path, e);
                }
            }
        };
    }

    pub fn new_file(fp: PathBuf) -> Result<SourceTreeNode, Error> {
        Ok(SourceTreeNode::File(SourceFile::new(fp)?))
    }

    /// assumes fp exists and is good directory name
    pub fn new_dir(fp: PathBuf) -> Result<Option<SourceTreeNode>, Error> {
        let items = dir_items(fp.clone())?;

        if items.len() == 0 {
            return Ok(None);
        }

        let dir_name = fp
            .file_name()
            .unwrap()
            .to_str()
            .ok_or_else(|| Error::NonUtf8FileName(fp.clone()))?;
        Ok(Some(Self::Directory(dir_name.to_string(), items)))
    }

    pub fn search<'a>(&self, mut p: impl Iterator<Item = &'a OsStr>) -> Option<SourceFile> {
        match self {
            Self::File(f) => match p.next() {
                None => Some(f.clone()),
                Some(_) => None,
            },
            Self::Directory(_, children) => match p.next() {
                None => None,
                Some(name) => search_dir(name, p, &children),
            },
        }
    }

    pub fn insert(&mut self, p: ast::PathIter<'_>, file: SourceFile) -> Result<(), ()> {
        match self {
            SourceTreeNode::File(..) => Err(()),
            SourceTreeNode::Directory(_, items) => dir_insert(items, p, file),
        }
    }
}

fn search_dir<'a>(
    name: &OsStr,
    p: impl Iterator<Item = &'a OsStr>,
    children: &[SourceTreeNode],
) -> Option<SourceFile> {
    let name = name.to_str()?.trim_end_matches(".helo");
    let node = children.iter().find(|n| n.name() == name)?;
    node.search(p)
}

fn dir_items(fp: PathBuf) -> Result<Vec<SourceTreeNode>, Error> {
    let mut items = Vec::new();
    for item in fp
        .as_path()
        .read_dir()?
        .filter_map(|x| x.ok())
        .map(|item| item.path())
    {
        if let Some(ext) = item.extension() {
            let ext = ext
                .to_str()
                .ok_or_else(|| Error::NonUtf8FileName(item.clone()))?;
            if ext == "helo" {
                items.push(SourceTreeNode::new_file(item)?);
            }
        } else if item.is_dir() {
            if let Some(sub_dir) = SourceTreeNode::new_dir(item)? {
                items.push(sub_dir);
            }
        }
    }
    Ok(items)
}

/// Insert `file` in directory `parent` relative to `nodes`
fn dir_insert(
    nodes: &mut Vec<SourceTreeNode>,
    mut parent: ast::PathIter<'_>,
    file: SourceFile,
) -> Result<(), ()> {
    if let Some(head) = parent.next() {
        if let Some(node) = nodes.iter_mut().find(|n| n.name() == head) {
            node.insert(parent, file)
        } else {
            let mut new_dir_node = SourceTreeNode::Directory(head.to_string(), Vec::new());
            new_dir_node.insert(parent, file)?;
            nodes.push(new_dir_node);
            Ok(())
        }
    } else {
        if nodes.iter().find(|n| n.name() == file.file_name).is_some() {
            Err(())
        } else {
            nodes.push(SourceTreeNode::File(file));
            Ok(())
        }
    }
}

impl SourceTree {
    pub fn walk<'s, 'a: 's>(&'a self, symbols: &mut tast::Symbols<'s>, e: &mut ManyError) {
        // If root file is called "mod", then its sibling files are its children modules
        if self.root_file_name == "mod" {
            let path = ast::Path::new([&self.root_dir]);
            for node in self.nodes.iter() {
                node.walk(symbols, &path, e);
            }
        // Otherwise, root module's children resides in a direcotry with the same name
        } else {
            let root = ast::Path::new([&self.root_file_name]);
            for node in self.nodes.iter() {
                match node {
                    SourceTreeNode::File(f) if f.file_name == self.root_file_name => {
                        f.parse_into(symbols, root.clone())
                            .unwrap_or_else(|err| e.push(err));
                    }
                    SourceTreeNode::Directory(n, nodes) if n == &self.root_file_name => {
                        for node in nodes {
                            node.walk(symbols, &root, e);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Create a file tree, root of which is parent of `fp`
    pub fn new(fp: PathBuf) -> Result<Self, Error> {
        if fp.is_file() {
            let root_file_name = fp
                .file_stem()
                .expect("Why the hell is the file called `..`")
                .to_str()
                .ok_or_else(|| Error::NonUtf8FileName(fp.clone()))?;

            let parent = fp.parent().unwrap();
            let items = dir_items(parent.to_path_buf())?;
            let parent_dir_name = parent
                .to_str()
                .ok_or_else(|| Error::NonUtf8FileName(fp.clone()))?
                .to_string();
            Ok(Self {
                nodes: items,
                root_file_name: root_file_name.to_string(),
                root_dir: parent_dir_name.clone(),
                module_root: if root_file_name == "mod" {
                    parent_dir_name
                } else {
                    root_file_name.to_string()
                },
            })
        } else {
            Err(Error::NotFile(fp))
        }
    }

    pub fn new_empty() -> Self {
        Self {
            nodes: Vec::new(),
            root_file_name: String::new(),
            root_dir: String::new(),
            module_root: String::new(),
        }
    }

    pub fn insert(&mut self, parent: ast::PathIter<'_>, file: SourceFile) -> Result<(), ()> {
        dir_insert(&mut self.nodes, parent, file)
    }

    pub fn search(&self, p: &str) -> Option<SourceFile> {
        let p = PathBuf::from(p);
        let rel_p = p.strip_prefix(self.root_dir.clone()).ok()?;
        let mut p = rel_p.iter();
        let name = p.next()?;
        search_dir(name, p, &self.nodes)
    }

    pub fn set_root_file_name(&mut self, name: String) {
        self.root_file_name = name
    }

    pub fn module_root(&self) -> &str {
        &self.module_root
    }
}
