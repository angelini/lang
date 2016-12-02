use ast::{Type, Value};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::result;

#[derive(Debug)]
pub enum Error {
    AscendFromRoot,
    KeyNotFound(String),
    LocalBindingAlreadyExists(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::AscendFromRoot => write!(f, "Tried to ascend from root"),
            Error::KeyNotFound(ref key) => write!(f, "Key not found {}", key),
            Error::LocalBindingAlreadyExists(ref key) => {
                write!(f, "Local binding already exists {}", key)
            }
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
struct Node<T> {
    id: usize,
    env: HashMap<String, T>,
    deps: HashSet<String>,
    parent: Option<usize>,
}

impl<T: Clone> Node<T> {
    pub fn new(id: usize, parent: Option<usize>) -> Node<T> {
        Node {
            id: id,
            env: HashMap::new(),
            deps: HashSet::new(),
            parent: parent,
        }
    }
}

#[derive(Debug)]
pub struct TypeScope {
    nodes: Vec<Node<Type>>,
    index: usize,
}

impl TypeScope {
    pub fn new() -> Self {
        let mut nodes = vec![];
        nodes.push(Node::new(0, None));
        Self {
            nodes: nodes,
            index: 0,
        }
    }

    pub fn descend(&mut self) {
        self.index += 1;
        self.nodes.push(Node::new(self.index, Some(self.index - 1)));
    }

    pub fn ascend(&mut self) -> Result<()> {
        let node = self.nodes.remove(self.index);
        self.index = match node.parent {
            Some(parent) => parent,
            None => return Err(Error::AscendFromRoot),
        };
        Ok(())
    }

    pub fn insert(&mut self, key: String, val: Type) -> Result<()> {
        let mut node = self.nodes.get_mut(self.index).unwrap();
        if node.env.contains_key(&key) {
            return Err(Error::LocalBindingAlreadyExists(key));
        }
        node.env.insert(key, val);
        Ok(())
    }

    pub fn get(&self, key: &str) -> Option<Type> {
        let mut id_opt = Some(self.index);
        while let Some(id) = id_opt {
            let node = self.nodes.get(id).unwrap();
            if node.env.contains_key(key) {
                return Some(node.env.get(key).unwrap().clone());
            }
            id_opt = node.parent;
        }
        None
    }

    pub fn contains_key(&self, key: &str) -> bool {
        let mut id_opt = Some(self.index);
        while let Some(id) = id_opt {
            let node = self.nodes.get(id).unwrap();
            if node.env.contains_key(key) {
                return true;
            }
            id_opt = node.parent;
        }
        false
    }
}

#[derive(Debug)]
pub struct ValueScope {
    nodes: HashMap<usize, Node<Rc<Value>>>,
    curr_id: usize,
    next_id: usize,
}

impl ValueScope {
    pub fn new() -> Self {
        let mut nodes = HashMap::new();
        nodes.insert(0, Node::new(0, None));
        Self {
            nodes: nodes,
            curr_id: 0,
            next_id: 1,
        }
    }

    pub fn descend(&mut self) {
        let node = Node::new(self.next_id, Some(self.curr_id));
        self.curr_id = self.next_id;
        self.next_id += 1;
        self.nodes.insert(self.curr_id, node);
    }

    pub fn descend_from(&mut self, fn_key: &str) -> usize {
        let current = self.curr_id;
        let from = self.find_leaf_id(fn_key);
        let node = Node::new(self.next_id, Some(from));
        self.curr_id = self.next_id;
        self.next_id += 1;
        self.nodes.insert(self.curr_id, node);
        current
    }

    #[allow(unused_assignments)]
    pub fn ascend(&mut self) -> Result<()> {
        let mut parent = None;
        let mut to_remove = vec![];
        {
            let node = self.nodes.get(&self.curr_id).unwrap();
            for val in node.env.values() {
                if let Value::Fn(box (ref fn_key, _, _)) = **val {
                    to_remove.push((fn_key.clone(), self.lineage(node.parent)))
                }
            }
            parent = node.parent;
        }

        for &(ref fn_key, ref ids) in &to_remove {
            for id in ids {
                let mut remove = false;
                {
                    let mut node = self.nodes.get_mut(id).unwrap();
                    node.deps.remove(fn_key);
                    remove = node.deps.is_empty();
                }
                if remove {
                    self.nodes.remove(id).unwrap();
                }
            }
        }

        if self.nodes.contains_key(&self.curr_id) &&
           self.nodes.get(&self.curr_id).unwrap().deps.is_empty() {
            self.nodes.remove(&self.curr_id);
        }

        self.curr_id = match parent {
            Some(parent) => parent,
            None => return Err(Error::AscendFromRoot),
        };
        Ok(())
    }

    pub fn jump_to(&mut self, id: usize) {
        self.curr_id = id;
    }

    pub fn insert_local(&mut self, key: String, val: Rc<Value>) -> Result<()> {
        let mut node = self.nodes.get_mut(&self.curr_id).unwrap();
        if node.env.contains_key(&key) {
            return Err(Error::LocalBindingAlreadyExists(key));
        }
        node.env.insert(key, val);
        Ok(())
    }

    pub fn update(&mut self, key: String, val: Rc<Value>) -> Result<()> {
        let mut id_opt = Some(self.curr_id);
        while let Some(id) = id_opt {
            let node = self.nodes.get_mut(&id).unwrap();
            if node.env.contains_key(&key) {
                node.env.insert(key, val);
                return Ok(());
            }
            id_opt = node.parent;
        }
        Err(Error::KeyNotFound(key))
    }

    pub fn get(&self, key: &str) -> Option<Rc<Value>> {
        let mut id_opt = Some(self.curr_id);
        while let Some(id) = id_opt {
            let node = self.nodes.get(&id).unwrap();
            if node.env.contains_key(key) {
                return Some(node.env.get(key).unwrap().clone());
            }
            id_opt = node.parent;
        }
        None
    }

    pub fn contains_key(&self, key: &str) -> bool {
        let mut id_opt = Some(self.curr_id);
        while let Some(id) = id_opt {
            let node = self.nodes.get(&id).unwrap();
            if node.env.contains_key(key) {
                return true;
            }
            id_opt = node.parent;
        }
        false
    }

    pub fn tag_self_and_parents(&mut self, fn_key: &str) {
        let mut id_opt = Some(self.curr_id);
        while let Some(id) = id_opt {
            let node = self.nodes.get_mut(&id).unwrap();
            node.deps.insert(fn_key.to_string());
            id_opt = node.parent;
        }
    }

    fn lineage(&self, mut id_opt: Option<usize>) -> Vec<usize> {
        let mut results = vec![];
        while let Some(id) = id_opt {
            let node = self.nodes.get(&id).unwrap();
            results.push(id);
            id_opt = node.parent;
        }
        results.reverse();
        results
    }

    fn find_leaf_id(&self, fn_key: &str) -> usize {
        let mut key_present = HashSet::new();
        let mut has_child = HashSet::new();

        for (id, node) in &self.nodes {
            if node.deps.contains(fn_key) {
                key_present.insert(*id);
                if let Some(parent) = node.parent {
                    has_child.insert(parent);
                }
            }
        }

        let leaf = key_present.difference(&has_child).collect::<Vec<&usize>>();
        assert!(leaf.len() == 1, "More than 1 leaf found");
        *leaf[0]
    }
}
