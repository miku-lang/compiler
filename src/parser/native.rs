use super::{ast::MikuType, typedast::Type};
use crate::parser::parse_song_statement;

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub name: String,
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct NativeVariable {
    pub name: String,
    pub typed: Type,
}

#[derive(Debug, Clone)]
pub struct NativeModule {
    pub name: String,
    pub functions: Vec<NativeFunction>,
    pub variables: Vec<NativeVariable>,
}

impl NativeModule {
    pub fn new(filename: &str) -> Self {
        let content = std::fs::read_to_string(format!("{filename}.album"))
            .expect(&format!("File {filename}.album not found"));

        let mut functions = vec![];
        let mut variables = vec![];
        for line in content.lines() {
            match parse_song_statement(line) {
                super::ast::SongStatementKind::MelodyDeclaration {
                    verse: _,
                    name,
                    parameters,
                    return_type,
                    body: _,
                } => {
                    let types = parameters
                        .iter()
                        .map(|p| miku_type_to_type(&p.miku_type))
                        .collect::<Vec<_>>();
                    let return_type = miku_type_to_type(&return_type);
                    functions.push(NativeFunction {
                        name,
                        parameters: types,
                        return_type,
                    });
                }
                super::ast::SongStatementKind::VariableDeclaration {
                    name,
                    miku_type,
                    initializer,
                } => {
                    assert!(miku_type.is_some());
                    assert!(initializer.is_none());
                    let Some(mtype) = miku_type else { todo!() };

                    variables.push(NativeVariable {
                        name: name.clone(),
                        typed: miku_type_to_type(&mtype),
                    });
                }
            }
        }

        Self {
            name: filename.to_string(),
            functions,
            variables,
        }
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.functions.iter().find(|p| p.name == name).is_some()
    }

    pub fn get_variable(&self, name: &str) -> Option<&NativeVariable> {
        self.variables.iter().find(|p| p.name == name)
    }
}

fn miku_type_to_type(miku_type: &MikuType) -> Type {
    match miku_type {
        MikuType::Note => Type::Int,
        MikuType::Pitch => Type::Float,
        MikuType::Lyric => Type::String,
        MikuType::Beat => Type::Boolean,
        MikuType::Track => todo!(),
        MikuType::Harmony => todo!(),
        MikuType::Void => Type::Void,
        MikuType::Identifier(name) => Type::Struct(name.clone()),
    }
}
