use crate::lexer::Lexer;

#[allow(dead_code)]
pub enum Module<'a> {
    File {
        name: String,
        lexer: Lexer<'a>,
        parsed: bool,
    },
    Dir {
        name: String,
        modules: Vec<Module<'a>>,
        parsed: bool,
    },
}

#[allow(dead_code)]
impl<'a> Module<'a> {
    pub fn find(&mut self, name: &str) -> Option<&mut Module<'a>> {
        match self {
            Module::File { name: mod_name, .. } if mod_name == name => Some(self),
            Module::File { .. } => None,
            Module::Dir { modules, .. } => {
                for module in modules {
                    if let Some(module) = module.find(name) {
                        return Some(module);
                    }
                }
                None
            }
        }
    }

    pub fn parsed(&self) -> bool {
        match self {
            Module::File { parsed, .. } => *parsed,
            Module::Dir { parsed, .. } => *parsed,
        }
    }

    pub fn set_parsed(&mut self) {
        match self {
            Module::File { parsed, .. } => *parsed = true,
            Module::Dir { parsed, .. } => *parsed = true,
        }
    }

    pub fn is_file(&self) -> bool {
        match self {
            Module::File { .. } => true,
            Module::Dir { .. } => false,
        }
    }

    pub fn is_dir(&self) -> bool {
        match self {
            Module::File { .. } => false,
            Module::Dir { .. } => true,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Module::File { name, .. } => name,
            Module::Dir { name, .. } => name,
        }
    }

    pub fn modules(&self) -> &Vec<Module<'a>> {
        match self {
            Module::File { .. } => panic!("Not a directory"),
            Module::Dir { modules, .. } => modules,
        }
    }

    pub fn modules_mut(&mut self) -> &mut Vec<Module<'a>> {
        match self {
            Module::File { .. } => panic!("Not a directory"),
            Module::Dir { modules, .. } => modules,
        }
    }

    pub fn lexer(&self) -> &Lexer<'a> {
        match self {
            Module::File { lexer, .. } => lexer,
            Module::Dir { .. } => panic!("Not a file"),
        }
    }
}
