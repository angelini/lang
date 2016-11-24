use ast::Value;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug)]
struct Node {
    id: usize,
    env: HashMap<String, Rc<Value>>,
    deps: HashSet<String>,
    parent: Option<usize>,
}

impl Node {
    pub fn new(id: usize, parent: Option<usize>) -> Node {
        Node {
            id: id,
            env: HashMap::new(),
            deps: HashSet::new(),
            parent: parent,
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    nodes: HashMap<usize, Node>,
    curr_id: usize,
    next_id: usize,
}

impl Scope {
    pub fn new() -> Scope {
        let mut nodes = HashMap::new();
        nodes.insert(0, Node::new(0, None));
        Scope {
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
    pub fn ascend(&mut self) {
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

        for &(ref fn_key, ref ids) in to_remove.iter() {
            for id in ids {
                let mut remove = false;
                {
                    let mut node = self.nodes.get_mut(&id).unwrap();
                    node.deps.remove(fn_key);
                    remove = node.deps.is_empty();
                }
                if remove {
                    self.nodes.remove(&id).unwrap();
                }
            }
        }

        if self.nodes.contains_key(&self.curr_id) {
            if self.nodes.get(&self.curr_id).unwrap().deps.is_empty() {
                self.nodes.remove(&self.curr_id);
            }
        }

        self.curr_id = match parent {
            Some(parent) => parent,
            None => panic!("Ascending from root"),
        };
    }

    pub fn jump_to(&mut self, id: usize) {
        self.curr_id = id;
    }

    pub fn insert(&mut self, key: String, val: Rc<Value>) {
        let mut id_opt = Some(self.curr_id);
        while let Some(id) = id_opt {
            let node = self.nodes.get_mut(&id).unwrap();
            if node.env.contains_key(&key) {
                node.env.insert(key, val);
                return;
            }
            id_opt = node.parent;
        }
        self.nodes.get_mut(&self.curr_id).unwrap().env.insert(key, val);
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

        for (id, node) in self.nodes.iter() {
            if node.deps.contains(fn_key) {
                key_present.insert(*id);
                if let Some(parent) = node.parent {
                    has_child.insert(parent);
                }
            }
        }

        let leaf = key_present.difference(&has_child).collect::<Vec<&usize>>();
        assert!(leaf.len() == 1, "More than 1 leaf found");
        leaf[0].clone()
    }
}
