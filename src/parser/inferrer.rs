use crate::parser::ast::MikuType;
use std::collections::BTreeMap;
use std::collections::HashMap;

use super::ast;
use super::typedast::Function as AstFunction;
use super::typedast::Variable as AstVariable;
use super::typedast::*;

#[derive(Debug, Clone)]
pub struct Inferrer {
    pub title: String,
    pub covers: Option<String>,
    scopes: Vec<HashMap<String, Type>>,
    pub structs: HashMap<BTreeMap<String, Type>, String>,
    functions: Vec<Function>,
    variables: Vec<Variable>,
    pub typed_functions: Vec<AstFunction>,
    pub typed_variables: Vec<AstVariable>,
    pub modules: Vec<Inferrer>,
}

impl Inferrer {
    pub fn new(program: ast::Program) -> Self {
        let mut modules = vec![];

        for r in &program.imports {
            match &r.import_type {
                ast::RemixType::Module { module, alias: _ } => {
                    let code = super::parse(&format!("{module}.miku"));
                    modules.push(Self::new(code));
                }
                ast::RemixType::Selective {
                    items: _,
                    module: _,
                } => todo!(),
                ast::RemixType::Wildcard { module: _ } => todo!(),
            }
        }

        let mut functions = vec![];
        let mut variables = vec![];
        for stmt in &program.song.statements {
            match stmt {
                ast::SongStatementKind::MelodyDeclaration {
                    verse,
                    name,
                    parameters,
                    return_type,
                    body,
                } => {
                    functions.push(Function {
                        verse: *verse,
                        class: program.song.name.clone(),
                        name: name.to_string(),
                        parameters: parameters.to_vec(),
                        return_type: return_type.clone(),
                        body: body.to_vec(),
                    });
                }
                ast::SongStatementKind::VariableDeclaration {
                    name,
                    miku_type,
                    initializer,
                } => {
                    variables.push(Variable {
                        name: name.to_string(),
                        miku_type: miku_type.clone(),
                        initializer: initializer.clone(),
                    });
                }
            }
        }

        Self {
            title: program.song.name.clone(),
            covers: program.song.parent.clone(),
            scopes: vec![],
            structs: HashMap::new(),
            functions,
            variables,
            typed_functions: vec![],
            typed_variables: vec![],
            modules,
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn load_typed_variables(&mut self) {
        for var in self.variables.clone() {
            let ast_var = self.infer_variable(&var.name, &var.miku_type, &var.initializer);
            self.typed_variables.push(ast_var);
        }
    }

    pub fn infer_main(&mut self) {
        self.infer_function(
            &self.title.clone(),
            "main",
            vec![Type::Array(Box::new(Type::String))],
            false,
        );
    }

    pub fn infer_function(
        &mut self,
        title: &str,
        name: &str,
        parameters: Vec<Type>,
        is_method: bool,
    ) -> Type {
        if self.variables.len() != self.typed_variables.len() {
            self.load_typed_variables();
        }

        if title != self.title {
            if let Some(module) = self.find_module(title) {
                module.push();

                let ret = module.infer_function(title, name, parameters, is_method);

                module.pop();

                return ret;
            } else {
                panic!("Unknown module {title}.");
            }
        }

        let mut func = None;
        for f in &self.functions {
            if f.name == name
                && f.verse == !is_method
                && check_parameters_compatibility(&parameters, &f.parameters)
            {
                assert_eq!(func, None);
                func = Some(f);
            }
        }

        let Some(fun) = func else {
            panic!(
                "{} {name}({}) doesn't exist.",
                if is_method { "Melody" } else { "Verse" },
                parameters
                    .iter()
                    .map(|p| format!("{p}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        };

        let params = fun.parameters.clone();
        let body = fun.body.clone();
        let fun_return_type = fun.return_type.clone();

        self.push();

        let mut named_args = vec![];

        for (i, p) in params.iter().enumerate() {
            if types_are_compatible(&parameters[i], &p.miku_type) {
                self.add_variable(&AstVariable {
                    name: p.name.clone(),
                    typed: parameters[i].clone(),
                    initializer: None,
                });

                named_args.push((p.name.clone(), parameters[i].clone()));
            } else {
                panic!("lol?");
            }
        }

        if is_method {
            self.add_variable(&AstVariable {
                name: "song".into(),
                typed: Type::Song(title.into()),
                initializer: None,
            });
        }

        let (typed, body) = self.infer_statements(&body);
        let return_type = if let Some(t) = typed {
            t.clone()
        } else {
            Type::Void
        };

        if !types_are_compatible(&return_type, &fun_return_type) {
            panic!(
                "{} {} is supposed to return {} but is returning {}.",
                if is_method { "Melody" } else { "Verse" },
                name,
                fun_return_type,
                return_type
            );
        }

        self.pop();

        self.add_typed_function(AstFunction {
            is_method,
            class: title.to_string(),
            name: name.to_string(),
            parameters: named_args,
            return_type: return_type.clone(),
            body,
        });

        return_type
    }

    fn infer_statement(&mut self, statement: &ast::Statement) -> (Option<Type>, Statement) {
        match &statement.kind {
            ast::StatementKind::VariableDeclaration {
                name,
                miku_type,
                initializer,
            } => {
                let var = self.infer_variable(name, miku_type, initializer);
                self.add_variable(&var);
                (None, Statement::VariableDeclaration(var))
            }
            ast::StatementKind::Assignment { target, value } => {
                let (_, target) = self.infer_expression(target);
                let (_, value) = self.infer_expression(value);
                (None, Statement::Assignment { target, value })
            }
            ast::StatementKind::RepeatLoop { count, body } => {
                let (_, count) = self.infer_expression(count);
                let (tb, body) = self.infer_statements(body);
                (tb, Statement::RepeatLoop { count, body })
            }
            ast::StatementKind::CountdownLoop {
                variable,
                count,
                body,
            } => {
                let (it_type, count) = self.infer_expression(count);

                self.push();
                self.add_variable(&&AstVariable {
                    name: variable.clone(),
                    typed: it_type,
                    initializer: None,
                });

                let (ret, body) = self.infer_statements(body);

                self.pop();
                (
                    ret,
                    Statement::CountdownLoop {
                        variable: variable.clone(),
                        count,
                        body,
                    },
                )
            }
            ast::StatementKind::WhileLoop { condition, body } => {
                let (_, condition) = self.infer_expression(condition);
                let (tb, body) = self.infer_statements(body);
                (tb, Statement::WhileLoop { condition, body })
            }
            ast::StatementKind::ForLoop {
                variable,
                iterable,
                body,
            } => {
                let (it_type, it_value) = self.infer_expression(iterable);

                match it_type {
                    Type::Array(typed) => {
                        let it_type = *typed.clone();

                        self.push();
                        self.add_variable(&&AstVariable {
                            name: variable.clone(),
                            typed: it_type,
                            initializer: None,
                        });

                        let (ret, body) = self.infer_statements(body);

                        self.pop();
                        (
                            ret,
                            Statement::ForLoop {
                                iterator: it_value,
                                variable: variable.clone(),
                                body,
                            },
                        )
                    }
                    Type::Struct(name) => {
                        let Some(struct_data) = self.structs.iter().find(|(_, v)| **v == name)
                        else {
                            todo!();
                        };

                        let mut rets = vec![];

                        let mut bodies = vec![];

                        for (_, ftyped) in struct_data.0.clone() {
                            let mut kv_data = BTreeMap::new();
                            kv_data.insert("key".to_string(), Type::String);
                            kv_data.insert("value".to_string(), ftyped.clone());
                            let kv_struct_name = self.get_struct_name(kv_data);

                            self.push();

                            self.add_variable(&AstVariable {
                                name: variable.clone(),
                                typed: Type::Struct(kv_struct_name),
                                initializer: None,
                            });
                            let (ret, body) = self.infer_statements(body);

                            if let Some(ret) = ret {
                                rets.push(ret);
                            }

                            bodies.push(body);

                            self.pop();
                        }

                        if !rets.iter().all(|r| *r == rets[0]) {
                            panic!("incompatible returned values");
                        }

                        (
                            if rets.len() > 0 {
                                Some(rets[0].clone())
                            } else {
                                None
                            },
                            Statement::ForLoopOnStruct {
                                struct_name: name.clone(),
                                variable: variable.clone(),
                                target: it_value,
                                bodies,
                            },
                        )
                    }
                    _ => panic!("for loop: {it_type:?}"),
                }
            }
            ast::StatementKind::Conditional {
                condition,
                then_branch,
                else_ifs,
                else_branch,
            } => {
                let mut rets = vec![];

                let (_, condition) = self.infer_expression(condition);
                let (r, then_branch) = self.infer_statements(then_branch);
                if r.is_some() {
                    rets.push(r.unwrap());
                }

                let (r, else_ifs) = {
                    let efs = else_ifs
                        .iter()
                        .map(|(c, e)| {
                            let (_, c) = self.infer_expression(c);
                            let (_, e) = self.infer_statements(e);
                            (c, e)
                        })
                        .collect::<Vec<_>>();
                    (None, efs)
                };
                if r.is_some() {
                    rets.push(r.unwrap());
                }

                let (r, else_branch) = if let Some(else_branch_v) = else_branch {
                    let (r, s) = self.infer_statements(else_branch_v);
                    (r, Some(s))
                } else {
                    (None, None)
                };
                if r.is_some() {
                    rets.push(r.unwrap());
                }

                let ret = if rets.len() > 0 {
                    if rets.iter().all(|t| *t == rets[0]) {
                        Some(rets[0].clone())
                    } else {
                        panic!("Multiple returned types");
                    }
                } else {
                    None
                };

                (
                    ret,
                    Statement::Conditional {
                        condition,
                        then_branch,
                        else_ifs,
                        else_branch,
                    },
                )
            }
            ast::StatementKind::SingStatement { expressions } => {
                let exprs = expressions
                    .iter()
                    .map(|expression| self.infer_expression(expression))
                    .collect::<Vec<_>>();

                (None, Statement::Sing(exprs))
            }
            ast::StatementKind::EchoStatement { expressions } => {
                let exprs = expressions
                    .iter()
                    .map(|expression| self.infer_expression(expression))
                    .collect::<Vec<_>>();

                (None, Statement::Echo(exprs))
            }
            ast::StatementKind::TuneStatement {
                variable,
                direction,
                amount,
            } => {
                let (_, variable) = self.infer_expression(variable);
                let direction = match direction {
                    ast::TuneDirection::Up => TuneDirection::Up,
                    ast::TuneDirection::Down => TuneDirection::Down,
                };
                let amount = if let Some(expr) = amount {
                    let (_, expr) = self.infer_expression(expr);
                    Some(expr)
                } else {
                    None
                };
                (
                    None,
                    Statement::TuneStatement {
                        variable,
                        direction,
                        amount,
                    },
                )
            }
            ast::StatementKind::ReturnStatement { value } => {
                if let Some(val) = value {
                    let (t, e) = self.infer_expression(val);
                    (Some(t), Statement::Return(Some(e)))
                } else {
                    (None, Statement::Return(None))
                }
            }
            ast::StatementKind::ExpressionStatement { expression } => {
                let (_, e) = self.infer_expression(expression);
                (None, Statement::Expression(e))
            }
        }
    }

    fn infer_statements(
        &mut self,
        statements: &Vec<ast::Statement>,
    ) -> (Option<Type>, Vec<Statement>) {
        let mut rets = vec![];
        let mut stmts = vec![];

        for stmt in statements {
            let (ret, stmt) = self.infer_statement(stmt);

            if let Some(r) = ret {
                rets.push(r);
            }
            stmts.push(stmt);
        }

        if !rets.iter().all(|r| *r == rets[0]) {
            panic!("incompatible returned values");
        }

        (
            if rets.len() > 0 {
                Some(rets[0].clone())
            } else {
                None
            },
            stmts,
        )
    }

    fn infer_expression(&mut self, expression: &ast::Expression) -> (Type, Expression) {
        match &expression.kind {
            ast::ExpressionKind::Integer(i) => (Type::Int, Expression::Integer(*i)),
            ast::ExpressionKind::Number(n) => (Type::Int, Expression::Number(*n)),
            ast::ExpressionKind::String(s) => (Type::String, Expression::String(s.clone())),
            ast::ExpressionKind::Boolean(b) => (Type::Boolean, Expression::Boolean(*b)),
            ast::ExpressionKind::Identifier(ident) => {
                if self.title == *ident {
                    (
                        Type::Song(ident.to_string()),
                        Expression::Class(ident.to_string()),
                    )
                } else if let Some(var) = self.find_variable(ident) {
                    (var.clone(), Expression::Variable(ident.to_string(), var))
                } else if let Some(_) = self.modules.iter().find(|m| m.title == *ident) {
                    (
                        Type::Song(ident.to_string()),
                        Expression::Class(ident.to_string()),
                    )
                } else {
                    panic!("'{ident}' not found.");
                }
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let (tl, left) = self.infer_expression(left);
                let (tr, right) = self.infer_expression(right);

                if tl != tr {
                    panic!("Can't {operator} {tl} and {tr}.");
                }

                let operator = match operator {
                    ast::BinaryOperator::Add => BinaryOperator::Add,
                    ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
                    ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
                    ast::BinaryOperator::Divide => BinaryOperator::Divide,
                    ast::BinaryOperator::Modulo => BinaryOperator::Modulo,
                    ast::BinaryOperator::Equal => BinaryOperator::Equal,
                    ast::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
                    ast::BinaryOperator::Less => BinaryOperator::Less,
                    ast::BinaryOperator::LessEqual => BinaryOperator::LessEqual,
                    ast::BinaryOperator::Greater => BinaryOperator::Greater,
                    ast::BinaryOperator::GreaterEqual => BinaryOperator::GreaterEqual,
                    ast::BinaryOperator::And => BinaryOperator::And,
                    ast::BinaryOperator::Or => BinaryOperator::Or,
                    ast::BinaryOperator::Harmonic => BinaryOperator::Harmonic,
                    ast::BinaryOperator::Dissonance => BinaryOperator::Dissonance,
                };

                (
                    tl,
                    Expression::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                )
            }
            ast::ExpressionKind::Unary { operator, operand } => {
                let (typed, operand) = self.infer_expression(&operand);
                (
                    typed,
                    Expression::Unary {
                        operator: match operator {
                            ast::UnaryOperator::Not => UnaryOperator::Not,
                            ast::UnaryOperator::Minus => UnaryOperator::Minus,
                            ast::UnaryOperator::Plus => UnaryOperator::Plus,
                        },
                        operand: Box::new(operand),
                    },
                )
            }
            ast::ExpressionKind::Call { callee, arguments } => {
                let (_, c) = self.infer_expression(&callee);

                let mut types = vec![];
                let mut args = vec![];

                for a in arguments {
                    let (typed, arg) = self.infer_expression(a);
                    types.push(typed);
                    args.push(arg);
                }

                if let Expression::Method { target, name } = c {
                    let (classname, is_method) = match &*target {
                        Expression::Class(cn) => (cn.clone(), false),
                        Expression::Variable(vn, typed) => {
                            if let Some(var) = self.find_variable(&vn) {
                                assert_eq!(var, *typed);
                                if let Type::Song(cn) = var {
                                    (cn.clone(), true)
                                } else {
                                    todo!()
                                }
                            } else {
                                todo!()
                            }
                        }
                        Expression::Field {
                            target: _,
                            name: _,
                            typed,
                        } => match typed {
                            Type::Song(song_type) => (song_type.clone(), true),
                            _ => todo!(),
                        },
                        _ => {
                            panic!("target {target:?}");
                        }
                    };

                    let return_type = self.infer_function(&classname, &name, types, is_method);

                    let expr = Expression::Call {
                        target,
                        name,
                        arguments: args,
                        typed: return_type.clone(),
                    };

                    (return_type, expr)
                } else {
                    panic!("{callee:?} is not a method.");
                }
            }
            ast::ExpressionKind::Index {
                object: _,
                index: _,
            } => todo!(),
            ast::ExpressionKind::Member { object, member } => {
                let (t, e) = self.infer_expression(object);

                match t {
                    Type::Song(song_type) => {
                        if song_type == self.title {
                            if let Some(var) = self.find_member_variable(&member) {
                                (
                                    var.clone(),
                                    Expression::Field {
                                        name: member.clone(),
                                        target: Box::new(e),
                                        typed: var,
                                    },
                                )
                            } else if let Some(vars) = self.find_function_in_hierarchy(member) {
                                let mut e = e;
                                for var in vars {
                                    e = Expression::Field {
                                        name: "parent".into(),
                                        target: Box::new(e),
                                        typed: var.clone(),
                                    }
                                }

                                (
                                    Type::Void,
                                    Expression::Method {
                                        name: member.clone(),
                                        target: Box::new(e),
                                    },
                                )
                            } else {
                                println!(
                                    "Song {song_type} doesn't have a variable or function called {member}."
                                );
                                todo!()
                            }
                        } else if let Some(_) =
                            self.modules.iter_mut().find(|m| m.title == song_type)
                        {
                            (
                                Type::Void,
                                Expression::Method {
                                    name: member.clone(),
                                    target: Box::new(e),
                                },
                            )
                        } else {
                            panic!("Unknown Song => {song_type}");
                        }
                    }
                    Type::Struct(name) => {
                        if let Some(struct_data) = self.structs.iter().find(|(_, v)| **v == name) {
                            if let Some(field_type) = struct_data.0.get(member) {
                                (
                                    field_type.clone(),
                                    Expression::Field {
                                        name: member.clone(),
                                        target: Box::new(e),
                                        typed: field_type.clone(),
                                    },
                                )
                            } else {
                                panic!("Harmony doesn't have field {member}.");
                            }
                        } else {
                            println!("{name:?}");
                            println!("{:?}", self.structs);
                            todo!();
                        }
                    }
                    _ => {
                        println!("object: {object:?}");
                        println!("t: {t:?}");
                        println!("e: {e:?}");
                        todo!()
                    }
                }
            }
            ast::ExpressionKind::Play(song_type) => {
                if *song_type == self.title || self.find_module(song_type).is_some() {
                    (
                        Type::Song(song_type.to_string()),
                        Expression::Play(song_type.to_string()),
                    )
                } else {
                    panic!("Can't play {song_type}, it's not a song.");
                }
            }
            ast::ExpressionKind::Array(expressions) => {
                let mut types = vec![];
                let exprs = expressions
                    .iter()
                    .map(|e| {
                        let (t, ee) = self.infer_expression(e);
                        types.push(t);
                        ee
                    })
                    .collect::<Vec<_>>();

                if !types.iter().all(|t| *t == types[0]) {
                    panic!("{types:?}");
                }

                (
                    Type::Array(Box::new(types[0].clone())),
                    Expression::Array(exprs),
                )
            }
            ast::ExpressionKind::Harmony(items) => {
                let mut s_data = BTreeMap::new();

                let struct_type: BTreeMap<String, Type> = items
                    .iter()
                    .map(|(key, value)| {
                        let k = match key {
                            ast::HarmonyKey::Identifier(_) => todo!(),
                            ast::HarmonyKey::String(s) => s.clone(),
                        };

                        let (t, value) = self.infer_expression(value);
                        s_data.insert(k.clone(), value);

                        (k, t)
                    })
                    .collect();

                let struct_name = self.get_struct_name(struct_type);
                (
                    Type::Struct(struct_name.clone()),
                    Expression::Struct(struct_name, s_data),
                )
            }
        }
    }

    fn find_function_in_hierarchy(&mut self, member: &str) -> Option<Vec<Type>> {
        if let Some(vars) = self.find_function_in_hierarchy_impl(member, vec![]) {
            Some(vars[1..].to_vec())
        } else {
            None
        }
    }

    fn find_function_in_hierarchy_impl(
        &mut self,
        member: &str,
        vars: Vec<Type>,
    ) -> Option<Vec<Type>> {
        let mut vars = vars;
        vars.push(Type::Song(self.title.clone()));
        if let Some(_) = self.functions.iter().find(|f| f.name == *member) {
            Some(vars)
        } else if let Some(cover) = self.covers.clone() {
            let Some(module) = self.find_module(&cover) else {
                todo!()
            };

            if let Some(vars) = module.find_function_in_hierarchy_impl(member, vars) {
                Some(vars)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn find_module(&mut self, name: &str) -> Option<&mut Inferrer> {
        self.modules.iter_mut().find(|m| m.title == name)
    }

    fn get_struct_name(&mut self, struct_type: BTreeMap<String, Type>) -> String {
        let mut struct_name = "".into();
        for (k, v) in &self.structs {
            if *k == struct_type {
                struct_name = v.clone();
                break;
            }
        }

        if struct_name.len() == 0 {
            struct_name = format!("CustomType{:02}", self.structs.len());
            self.structs.insert(struct_type, struct_name.clone());
        }

        struct_name
    }

    fn find_variable(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var.clone());
            }
        }

        None
    }

    fn find_member_variable(&self, name: &str) -> Option<Type> {
        for var in &self.typed_variables {
            if var.name == name {
                return Some(var.typed.clone());
            }
        }

        None
    }

    pub fn infer_variable(
        &mut self,
        name: &str,
        miku_type: &Option<MikuType>,
        initializer: &Option<ast::Expression>,
    ) -> AstVariable {
        let (init_type, init) = if let Some(init) = initializer {
            let (t, v) = self.infer_expression(init);
            (t, Some(v))
        } else {
            (Type::Void, None)
        };

        let typed = if let Some(mt) = miku_type {
            if init.is_some() {
                match (mt, &init_type) {
                    (MikuType::Note, Type::Int) => init_type,
                    (MikuType::Pitch, Type::Float) => init_type,
                    (MikuType::Lyric, Type::String) => init_type,
                    (MikuType::Beat, Type::Boolean) => init_type,
                    (MikuType::Track, _) => todo!(),
                    (MikuType::Harmony, other) => {
                        if let Type::Struct(_) = other {
                            other.clone()
                        } else {
                            panic!("harmony: {other:?}");
                        }
                    }
                    (MikuType::Void, _) => todo!(),
                    (_, _) => todo!(),
                }
            } else {
                match mt {
                    MikuType::Note => Type::Int,
                    MikuType::Pitch => Type::Float,
                    MikuType::Lyric => Type::String,
                    MikuType::Beat => Type::Boolean,
                    MikuType::Track => todo!(),
                    MikuType::Harmony => todo!(),
                    MikuType::Void => todo!(),
                }
            }
        } else {
            if init.is_some() {
                init_type.clone()
            } else {
                panic!("Variable is missing a type AND an initializer");
            }
        };

        AstVariable {
            name: name.into(),
            typed,
            initializer: init,
        }
    }

    fn add_variable(&mut self, variable: &AstVariable) {
        let Some(scope) = self.scopes.last_mut() else {
            unreachable!();
        };

        if scope.contains_key(&variable.name) {
            panic!("Duplicate variable {}.", variable.name);
        }

        scope.insert(variable.name.clone(), variable.typed.clone());
    }

    fn add_typed_function(&mut self, function: AstFunction) {
        if !self.typed_functions.contains(&function) {
            self.typed_functions.push(function);
        }
    }
}

fn check_parameters_compatibility(left: &Vec<Type>, right: &Vec<ast::Parameter>) -> bool {
    if left.len() != right.len() {
        return false;
    }

    left.iter()
        .zip(right)
        .all(|(l, r)| types_are_compatible(l, &r.miku_type))
}

fn types_are_compatible(typed: &Type, miku_type: &MikuType) -> bool {
    match (typed, miku_type) {
        (Type::Void, MikuType::Void) => true,
        (Type::Int, MikuType::Note) => true,
        (Type::Float, MikuType::Pitch) => true,
        (Type::String, MikuType::Lyric) => true,
        (Type::Boolean, MikuType::Beat) => true,
        (Type::Array(_), MikuType::Track) => true,
        (Type::Struct(_), MikuType::Harmony) => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub name: String,
    pub miku_type: Option<MikuType>,
    pub initializer: Option<ast::Expression>,
}

#[derive(Debug, PartialEq, Clone)]
struct Function {
    verse: bool,
    class: String,
    name: String,
    parameters: Vec<ast::Parameter>,
    return_type: MikuType,
    body: Vec<ast::Statement>,
}
