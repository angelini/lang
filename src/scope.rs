use ast::Value;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Scope {
    levels: Vec<HashMap<String, Value>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { levels: vec![HashMap::new()] }
    }

    pub fn descend(&mut self) {
        self.levels.push(HashMap::new())
    }

    pub fn ascend(&mut self) {
        self.levels.pop();
    }

    pub fn insert(&mut self, key: String, val: Value) {
        for level in self.levels.iter_mut().rev() {
            if level.contains_key(&key) {
                level.insert(key, val);
                return;
            }
        }
        let last = self.levels.len() - 1;
        self.levels.get_mut(last).unwrap().insert(key, val);
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        for level in self.levels.iter().rev() {
            if level.contains_key(key) {
                return level.get(key);
            }
        }
        None
    }

    pub fn contains_key(&self, key: &str) -> bool {
        for level in self.levels.iter().rev() {
            if level.contains_key(key) {
                return true;
            }
        }
        false
    }
}
