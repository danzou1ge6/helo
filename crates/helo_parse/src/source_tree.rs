use crate::{ast, errors, parse, parse::tast};
use errors::ManyError;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;

pub struct SourceFile {
    src: Arc<String>,
    file_path: Arc<String>,
    file_name: String,
}

pub enum SourceTreeNode {
    File(SourceFile),
    Directory(String, Vec<SourceTreeNode>),
}

pub struct SourceTree {
    nodes: Vec<SourceTreeNode>,
    root_file_name: String,
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

impl SourceTree {
    pub fn walk<'s, 'a: 's>(&'a self, symbols: &mut tast::Symbols<'s>, e: &mut ManyError) {
        // If root file is called "mod", then its sibling files are its children modules
        if self.root_file_name == "mod" {
            let path = ast::Path::new([]);
            for node in self.nodes.iter() {
                node.walk(symbols, &path, e);
            }
        // Otherwise, root module's children resides in a direcotry with the same name
        } else {
            let root = ast::Path::new([]);
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
            Ok(Self {
                nodes: items,
                root_file_name: root_file_name.to_string(),
            })
        } else {
            Err(Error::NotFile(fp))
        }
    }
}
