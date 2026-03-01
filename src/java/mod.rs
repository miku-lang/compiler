use crate::parser::inferrer::Inferrer;
use crate::parser::inferrer::infer_type;
use crate::parser::typedast::BinaryOperator;
use crate::parser::typedast::Statement;
use crate::parser::typedast::TuneDirection;
use crate::parser::typedast::{Expression, Type};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

pub fn generate(filename: &str, inferrer: Inferrer) -> std::io::Result<()> {
    let mut generator = CodeGenerator::new(inferrer);
    generator.codegen(filename)?;

    Ok(())
}

struct CodeGenerator {
    inferrer: Inferrer,
    depth: usize,
    scopes: Vec<HashMap<String, Type>>,
}

impl CodeGenerator {
    fn new(inferrer: Inferrer) -> Self {
        Self {
            inferrer,
            depth: 0,
            scopes: vec![],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn print_depth(&self, file: &mut File) -> std::io::Result<()> {
        write!(file, "{}", " ".repeat(self.depth * 4))
    }

    fn codegen(&mut self, filename: &str) -> std::io::Result<()> {
        // for module in self.inferrer.modules.clone() {
        //     let title = module.title.clone();
        //     let mut generator = CodeGenerator::new(module);
        //     generator.codegen(&format!("{title}.java"))?;
        // }
        for (name, song_module) in &self.inferrer.current_module {
            match song_module {
                crate::parser::inferrer::ModuleSong::Song(inferrer) => {
                    let title = inferrer.title.clone();
                    let album = if inferrer.album.len() > 0 {
                        format!("{}/", inferrer.album.join("/"))
                    } else {
                        "".to_string()
                    };
                    let mut generator = CodeGenerator::new(inferrer.clone());
                    generator.codegen(&format!("{album}{title}.java"))?;
                }
                crate::parser::inferrer::ModuleSong::Native(native_module) => todo!(),
            }
        }

        let mut file = File::create(filename)?;

        if self.inferrer.album.len() > 0 {
            writeln!(file, "package {};\n", self.inferrer.album.join("."))?;
        }

        write!(file, "class {}", self.inferrer.title)?;
        if let Some(cover) = &self.inferrer.covers {
            write!(file, " extends {cover}")?;
        }
        writeln!(file, " {{")?;
        self.depth += 1;

        for (fields, name) in &self.inferrer.structs {
            self.print_depth(&mut file)?;
            write!(file, "record {name}(")?;
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    write!(file, ", ")?;
                }
                write!(file, "{} {}", self.type_to_java(&field.1), field.0)?;
            }
            writeln!(file, ") {{}}")?;
        }

        writeln!(file)?;

        for var in self.inferrer.typed_variables.clone() {
            self.print_depth(&mut file)?;
            write!(
                file,
                "private {} {}",
                self.type_to_java(&var.typed),
                var.name
            )?;
            if let Some(e) = &var.initializer {
                write!(file, " = ")?;
                self.codegen_expression(&mut file, e)?;
            }
            writeln!(file, ";")?;
        }

        writeln!(file)?;

        let mut has_main = false;
        for fun in self.inferrer.typed_functions.clone() {
            self.print_depth(&mut file)?;
            write!(file, "public ")?;
            if !fun.is_method {
                write!(file, "static ")?;
            }

            write!(file, "{} ", self.type_to_java(&fun.return_type))?;

            if fun.name == "main" {
                write!(file, "miku_main")?;
                has_main = true;
            } else {
                write!(file, "{}", fun.name)?;
            }

            write!(file, "(")?;

            for (i, (name, typed)) in fun.parameters.iter().enumerate() {
                if i > 0 {
                    write!(file, ", ")?;
                }
                write!(file, "{} {}", self.type_to_java(&typed), name)?;
            }

            writeln!(file, ") {{")?;

            self.codegen_statements(&mut file, &fun.body)?;

            self.print_depth(&mut file)?;
            writeln!(file, "}}\n")?;
        }

        if has_main {
            self.print_depth(&mut file)?;
            writeln!(
                file,
                r#"public static void main(String[] args) {{
        {}.miku_main(args);
    }}"#,
                self.inferrer.title
            )?;
        }

        writeln!(file, "}}")?;

        Ok(())
    }

    fn codegen_expression(
        &mut self,
        file: &mut File,
        expression: &Expression,
    ) -> std::io::Result<()> {
        match expression {
            Expression::Integer(i) => write!(file, "{i}")?,
            Expression::Number(_) => todo!(),
            Expression::String(s) => write!(file, "{s}")?,
            Expression::Boolean(b) => write!(file, "{}", if *b { "true" } else { "false" })?,
            Expression::Class(cs) => write!(file, "{}", cs.join("."))?,
            Expression::Variable(name, _) => {
                if name == "song" {
                    write!(file, "this")?;
                } else {
                    write!(file, "{name}")?;
                }
            }
            Expression::Binary {
                operator,
                left,
                right,
            } => {
                self.codegen_expression(file, left)?;
                write!(file, " {} ", self.operator_to_java(&operator))?;
                self.codegen_expression(file, right)?;
            }
            Expression::Array(expressions) => {
                if expressions.len() == 0 {
                    panic!("empty array");
                }

                let t = infer_type(&expressions[0]);
                writeln!(file, "new {}[] {{", self.type_to_java(&t))?;

                self.depth += 1;

                for (i, expr) in expressions.iter().enumerate() {
                    if i > 0 {
                        writeln!(file, ",")?;
                    }

                    self.print_depth(file)?;
                    self.codegen_expression(file, expr)?;
                }

                writeln!(file)?;

                self.depth -= 1;
                self.print_depth(file)?;
                write!(file, "}}")?;
            }
            Expression::Struct(name, fields) => {
                write!(file, "new {name}(")?;
                for (i, (_, expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(file, ", ")?;
                    }
                    self.codegen_expression(file, expr)?;
                }
                write!(file, ")")?;
            }
            Expression::Field {
                target,
                name,
                typed: _,
            } => {
                self.codegen_expression(file, target)?;

                if name != "parent" {
                    write!(file, ".{name}")?;
                }
            }
            Expression::Method { .. } => todo!(),
            Expression::Call {
                target,
                name,
                arguments,
                typed: _,
            } => {
                self.codegen_expression(file, target)?;

                if name == "len" {
                    if let Type::Array(_) = infer_type(target) {
                        write!(file, ".length")?;
                        return Ok(());
                    }
                }
                write!(file, ".{name}(")?;

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(file, ", ")?;
                    }
                    self.codegen_expression(file, arg)?;
                }

                write!(file, ")")?;
            }
            Expression::Play(title) => write!(file, "new {title}()")?,
            Expression::Unary {
                operator: _,
                operand: _,
            } => todo!(),
            Expression::Index { object, index } => {
                self.codegen_expression(file, &object)?;
                write!(file, "[")?;
                self.codegen_expression(file, &index)?;
                write!(file, "]")?;
            }
        };

        Ok(())
    }

    fn codegen_statements(
        &mut self,
        file: &mut File,
        statements: &Vec<Statement>,
    ) -> std::io::Result<()> {
        self.depth += 1;
        self.push();

        for stmt in statements {
            match stmt {
                Statement::VariableDeclaration(variable) => {
                    self.print_depth(file)?;
                    write!(
                        file,
                        "{} {}",
                        self.type_to_java(&variable.typed),
                        variable.name
                    )?;
                    if let Some(expression) = &variable.initializer {
                        write!(file, " = ")?;
                        self.codegen_expression(file, &expression)?;
                    }
                    writeln!(file, ";")?;
                }
                Statement::ForLoop {
                    variable,
                    iterator,
                    body,
                } => {
                    self.push();

                    self.print_depth(file)?;
                    writeln!(file, "{{")?;

                    let it_type = infer_type(iterator);
                    let mut is_tmp_var = false;
                    let typed = match &it_type {
                        Type::Variable(s) => s.clone(),
                        Type::Array(_) => {
                            is_tmp_var = true;
                            Box::new(it_type.clone())
                        }
                        unk => panic!("Unknown type: {unk:?}"),
                    };
                    let inferred_type = match &*typed {
                        Type::Array(internal_type) => internal_type.clone(),
                        Type::Struct(_) => typed.clone(),
                        unk => panic!("{unk:?}"),
                    };

                    self.depth += 1;
                    self.print_depth(file)?;
                    write!(file, "{} k_k_k_k = ", self.type_to_java(&typed))?;

                    if is_tmp_var {
                        let Expression::Array(_) = iterator else {
                            panic!("Can't iterate on a non-track variable");
                        };

                        // write!(file, "new {} {{", self.type_to_java(&inferred_type))?;
                        self.codegen_expression(file, iterator)?;
                        // write!(file, "}}")?;
                    } else {
                        self.codegen_expression(file, iterator)?;
                    }
                    writeln!(file, ";")?;

                    self.print_depth(file)?;
                    writeln!(
                        file,
                        "for (int i_i_i_i = 0; i_i_i_i < k_k_k_k.length; ++i_i_i_i) {{"
                    )?;

                    self.print_depth(file)?;
                    writeln!(
                        file,
                        "    {} {variable} = k_k_k_k[i_i_i_i];",
                        self.type_to_java(&inferred_type)
                    )?;

                    self.push();
                    // self.add_variable(&variable, *inferred_type);

                    self.codegen_statements(file, body)?;
                    self.pop();

                    self.print_depth(file)?;
                    writeln!(file, "}}")?;

                    self.depth -= 1;
                    self.print_depth(file)?;
                    writeln!(file, "}}")?;

                    self.pop();
                }
                Statement::CountdownLoop {
                    variable,
                    count,
                    body,
                } => {
                    self.print_depth(file)?;
                    write!(file, "for (long i_i_i_i = ")?;
                    self.codegen_expression(file, count)?;
                    writeln!(file, "; i_i_i_i > 0; --i_i_i_i) {{")?;
                    self.print_depth(file)?;
                    writeln!(file, "    long {variable} = i_i_i_i;")?;
                    self.codegen_statements(file, body)?;
                    self.print_depth(file)?;
                    writeln!(file, "}}")?;
                }
                Statement::Expression(expression) => {
                    self.print_depth(file)?;
                    self.codegen_expression(file, expression)?;
                    writeln!(file, ";")?;
                }
                Statement::Sing(items) => {
                    for (_, expr) in items {
                        self.print_depth(file)?;
                        write!(file, "System.out.print(")?;
                        self.codegen_expression(file, expr)?;
                        writeln!(file, ");")?;
                    }
                    self.print_depth(file)?;
                    writeln!(file, "System.out.println();")?;
                }
                Statement::Echo(_) => todo!(),
                Statement::Return(expression) => {
                    self.print_depth(file)?;
                    write!(file, "return")?;
                    if let Some(expr) = &expression {
                        write!(file, " ")?;
                        self.codegen_expression(file, expr)?;
                    }
                    writeln!(file, ";")?;
                }
                Statement::WhileLoop { condition, body } => {
                    self.print_depth(file)?;
                    write!(file, "while (")?;
                    self.codegen_expression(file, condition)?;
                    writeln!(file, ") {{")?;
                    self.codegen_statements(file, body)?;
                    self.print_depth(file)?;
                    writeln!(file, "}}")?;
                }
                Statement::ForLoopOnStruct {
                    struct_name,
                    variable,
                    target,
                    bodies,
                } => {
                    let structs = self.inferrer.structs.clone();
                    let Some(struct_data) = structs.iter().find(|s| s.1 == struct_name) else {
                        unreachable!()
                    };
                    let struct_data = struct_data.clone();

                    let fields = struct_data
                        .0
                        .iter()
                        .map(|kv| kv.clone())
                        .collect::<Vec<_>>();
                    let fields = fields.clone();

                    for (i, body) in bodies.iter().enumerate() {
                        let field = fields[i].clone();

                        self.print_depth(file)?;
                        writeln!(file, "{{")?;

                        self.depth += 1;

                        self.push();

                        let mut kv_data = BTreeMap::new();
                        kv_data.insert("key".to_string(), Type::String);
                        kv_data.insert("value".to_string(), field.1.clone());
                        let Some((_, kv_struct)) = self
                            .inferrer
                            .structs
                            .iter()
                            .find(|(data, _)| **data == kv_data)
                        else {
                            unreachable!()
                        };
                        let kv_struct = kv_struct.clone();

                        self.add_variable(&variable, Type::Struct(kv_struct.to_string()));

                        self.print_depth(file)?;
                        write!(
                            file,
                            "{kv_struct} {variable} = new {kv_struct}(\"{}\", ",
                            field.0
                        )?;
                        self.codegen_expression(file, target)?;
                        write!(file, ".{}", field.0)?;
                        writeln!(file, ");")?;

                        self.depth -= 1;
                        self.codegen_statements(file, body)?;
                        self.pop();

                        self.print_depth(file)?;
                        writeln!(file, "}}")?;
                    }
                }
                Statement::Conditional {
                    condition,
                    then_branch,
                    else_ifs,
                    else_branch,
                } => {
                    self.print_depth(file)?;
                    write!(file, "if (")?;
                    self.codegen_expression(file, condition)?;
                    writeln!(file, ") {{")?;
                    self.codegen_statements(file, then_branch)?;
                    self.print_depth(file)?;
                    write!(file, "}}")?;
                    for (condition, statements) in else_ifs {
                        write!(file, " else if (")?;
                        self.codegen_expression(file, condition)?;
                        writeln!(file, ") {{")?;
                        self.codegen_statements(file, statements)?;
                        self.print_depth(file)?;
                        write!(file, "}}")?;
                    }
                    if let Some(statements) = else_branch {
                        writeln!(file, " else {{")?;
                        self.codegen_statements(file, statements)?;
                        self.print_depth(file)?;
                        write!(file, "}}")?;
                    }
                    writeln!(file)?;
                }
                Statement::RepeatLoop { count, body } => {
                    self.print_depth(file)?;
                    write!(file, "for (int i_i_i_i = 0; i_i_i_i < ")?;
                    self.codegen_expression(file, count)?;
                    writeln!(file, "; ++i_i_i_i) {{")?;
                    self.codegen_statements(file, body)?;
                    self.print_depth(file)?;
                    writeln!(file, "}}")?;
                }
                Statement::Assignment { target, value } => {
                    self.print_depth(file)?;
                    self.codegen_expression(file, target)?;
                    write!(file, " = ")?;
                    self.codegen_expression(file, value)?;
                    writeln!(file, ";")?;
                }
                Statement::TuneStatement {
                    variable,
                    direction,
                    amount,
                } => {
                    self.print_depth(file)?;
                    self.codegen_expression(file, variable)?;
                    write!(file, " ")?;
                    match direction {
                        TuneDirection::Up => write!(file, " +")?,
                        TuneDirection::Down => write!(file, " -")?,
                    }
                    write!(file, "= ")?;
                    if let Some(expression) = amount {
                        self.codegen_expression(file, expression)?;
                    } else {
                        write!(file, "1")?;
                    }
                    writeln!(file, ";")?;
                }
            }
        }

        self.pop();
        self.depth -= 1;
        Ok(())
    }

    fn add_variable(&mut self, name: &str, typed: Type) {
        let Some(scope) = self.scopes.last_mut() else {
            unreachable!()
        };

        scope.insert(name.to_string(), typed);
    }

    fn type_to_java(&self, typed: &Type) -> String {
        match typed {
            Type::Void => "void".to_string(),
            Type::Int => "long".to_string(),
            Type::Float => "double".to_string(),
            Type::String => "String".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::Array(t) => format!("{}[]", self.type_to_java(&t)),
            Type::Struct(s) => s.clone(),
            Type::Song(title) => title.clone(),
            Type::Class(_) => todo!(),
            Type::Variable(_) => todo!(),
        }
    }

    fn operator_to_java(&self, operator: &BinaryOperator) -> &'static str {
        match operator {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "*",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::Less => "<",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::Greater => ">",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::Harmonic => todo!(),
            BinaryOperator::Dissonance => todo!(),
        }
    }
}
