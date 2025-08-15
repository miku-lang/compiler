use crate::parser::inferrer::Inferrer;
use crate::parser::native::NativeModule;
use crate::parser::typedast::*;
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Write;

pub fn generate(filename: &str, inferrer: Inferrer) -> std::io::Result<()> {
    let mut generator = CodeGenerator::new(inferrer);
    generator.codegen(filename)?;

    Ok(())
}

fn gen_array(file: &mut File, miku_type: &Type) -> std::io::Result<()> {
    let name = type_to_encoded_name(miku_type);
    let c_type = type_to_c(miku_type);
    writeln!(
        file,
        r#"typedef struct array_{name} {{
    uint32_t length;
    {c_type}* items;
}} array_{name};

int64_t array_{name}_len_array_{name}(array_{name} array) {{
    return array.length;
}}

{c_type}* array_{name}_get(array_{name} array, int64_t index) {{
    if (index >= 0 && index < array.length) {{
        return &array.items[index];
    }} else {{
        static {c_type} tmp;
        memset(&tmp, 0, sizeof({c_type}));
        return &tmp;
    }}
}}

array_{name} array_{name}_create(int64_t count, {c_type}* items) {{
    return (array_{name}){{
        .length = count,
        .items = items,
    }};
}}
"#
    )
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

    fn add_variable(&mut self, name: &str, typed: Type) {
        let Some(scope) = self.scopes.last_mut() else {
            unreachable!()
        };

        scope.insert(name.to_string(), typed);
    }

    fn codegen(&mut self, filename: &str) -> std::io::Result<()> {
        let mut file = File::create(filename)?;

        writeln!(
            file,
            r#"#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
"#
        )?;

        gen_array(&mut file, &Type::Int)?;
        gen_array(&mut file, &Type::Float)?;
        gen_array(&mut file, &Type::String)?;
        gen_array(&mut file, &Type::Boolean)?;

        writeln!(
            file,
            r#"
void print_int(int64_t value) {{
    printf("%lld", value);
}}

void print_float(double value) {{
    printf("%f", value);
}}

void print_string(const char* value) {{
    if (value == nullptr) {{
        printf("null");
    }} else {{
        printf("%s", value);
    }}
}}

void print_bool(bool value) {{
    if (value) {{
        printf("true");
    }} else {{
        printf("false");
    }}
}}
"#
        )?;

        self.codegen_module(&mut file, self.inferrer.clone())?;

        writeln!(
            file,
            r#"int main(int argc, const char** argv) {{
    array_string args;
    args.length = argc;
    args.items = argv;
    return {}_main_array_string(args);
}}"#,
            self.inferrer.title
        )?;

        Ok(())
    }

    fn codegen_native_module(
        &mut self,
        file: &mut File,
        module: NativeModule,
    ) -> std::io::Result<()> {
        writeln!(file, "#include <{}.h>", module.name)?;

        for fun in module.functions {
            write!(
                file,
                "{} {}_{}_{}(",
                type_to_c(&fun.return_type),
                module.name,
                fun.name,
                types_to_string(&fun.parameters)
            )?;
            for (i, p) in fun.parameters.iter().enumerate() {
                if i > 0 {
                    write!(file, ", ")?;
                }
                write!(file, "{} a{i}", type_to_c(&p))?;
            }
            writeln!(file, ") {{")?;
            write!(file, "    ")?;
            if let Type::Void = fun.return_type {
                // do nothing
            } else {
                write!(file, "return ")?;
            }
            write!(file, "{}(", fun.name)?;
            for i in 0..fun.parameters.len() {
                if i > 0 {
                    write!(file, ", ")?;
                }
                write!(file, "a{i}")?;
            }
            writeln!(file, ");")?;
            writeln!(file, "}}\n")?;
        }

        Ok(())
    }

    fn codegen_module(&mut self, file: &mut File, inferrer: Inferrer) -> std::io::Result<()> {
        for module in inferrer.modules.clone() {
            self.codegen_module(file, module)?;
        }

        for module in inferrer.native_modules.clone() {
            self.codegen_native_module(file, module)?;
        }

        for (fields, name) in &inferrer.structs {
            writeln!(file, "typedef struct {name} {{")?;
            for f in fields {
                writeln!(file, "    {} {};", type_to_c(&f.1), f.0)?;
            }
            writeln!(file, "}} {name};\n")?;

            gen_array(file, &Type::Struct(name.to_string()))?;
        }

        let title = inferrer.title.clone();

        writeln!(file, "typedef struct {title} {{")?;
        writeln!(file, "    uint16_t internal_counter;")?;
        if let Some(cover) = &inferrer.covers {
            writeln!(file, "    {cover}* parent;")?;
        }
        for var in &inferrer.typed_variables {
            writeln!(file, "    {} {};", type_to_c(&var.typed), var.name)?;
        }
        writeln!(
            file,
            r#"}} {title};

{title}* {title}_allocate() {{
    {title}* tmp = malloc(sizeof(*tmp));
    memset(tmp, 0, sizeof(*tmp));"#
        )?;

        if let Some(cover) = &inferrer.covers {
            writeln!(file, "    tmp->parent = {cover}_allocate();")?;
            writeln!(file, "    tmp->parent->internal_counter++;")?;
        }

        for var in inferrer.typed_variables.clone() {
            if let Some(init) = &var.initializer {
                write!(file, "    tmp->{} = ", var.name)?;
                self.codegen_expression(file, init)?;
                writeln!(file, ";")?;
            }
        }

        writeln!(
            file,
            r#"    return tmp;
}}

void {title}_deallocate({title}* song) {{
    song->internal_counter--;
    if (song->internal_counter == 0)
    {{"#
        )?;

        // parent's counter shouldn't go over 1
        if let Some(cover) = &inferrer.covers {
            writeln!(file, "        {cover}_deallocate(song->parent);")?;
        }
        writeln!(
            file,
            r#"        free(song);
    }}
}}
"#
        )?;

        self.push();

        for fun in inferrer.typed_functions.clone() {
            assert_eq!(fun.class, *title);

            let mut p_types = fun
                .parameters
                .iter()
                .map(|p| p.1.clone())
                .collect::<Vec<_>>();

            if fun.is_method {
                p_types.insert(0, Type::Song(fun.class.clone()));
            }

            write!(
                file,
                "{} {}_{}_{}(",
                type_to_c(&fun.return_type),
                fun.class,
                fun.name,
                types_to_string(&p_types)
            )?;

            self.push();

            if fun.is_method {
                write!(file, "{}* song", fun.class)?;

                self.add_variable("song", Type::Song(fun.class.clone()));
            }

            for (i, p) in fun.parameters.iter().enumerate() {
                if i > 0 || fun.is_method {
                    write!(file, ", ")?;
                }

                write!(file, "{} {}", type_to_c(&p.1), p.0)?;
                self.add_variable(&p.0, p.1.clone());
            }

            writeln!(file, ") {{")?;

            self.codegen_statements(file, &fun.body)?;
            self.pop();

            writeln!(file, "}}\n")?;
        }

        self.pop();

        Ok(())
    }

    fn codegen_expression(
        &mut self,
        file: &mut File,
        expression: &Expression,
    ) -> std::io::Result<()> {
        match &expression {
            Expression::Integer(i) => write!(file, "{i}"),
            Expression::Number(n) => write!(file, "{n}"),
            Expression::String(s) => write!(file, "{s}"),
            Expression::Variable(name, _) => write!(file, "{name}"),
            Expression::Boolean(b) => write!(file, "{}", if *b { "true" } else { "false" }),
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                self.codegen_expression(file, &*left)?;
                write!(
                    file,
                    " {} ",
                    match operator {
                        BinaryOperator::Add => "+",
                        BinaryOperator::Subtract => "-",
                        BinaryOperator::Multiply => "*",
                        BinaryOperator::Divide => "/",
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
                )?;
                self.codegen_expression(file, &*right)
            }
            Expression::Field {
                target,
                name,
                typed: _,
            } => {
                let target_type = infer_type(target);
                self.codegen_expression(file, target)?;

                let target_type = if let Type::Variable(typed) = target_type {
                    *typed.clone()
                } else {
                    target_type
                };

                match target_type {
                    Type::Song(_) => write!(file, "->{name}"),
                    Type::Struct(_) => write!(file, ".{name}"),
                    unknown => panic!("Unknown variable type {unknown:?}"),
                }
            }
            Expression::Call {
                name,
                target,
                arguments,
                typed: _,
            } => {
                let target_type = infer_type(target);

                let mut types = arguments
                    .iter()
                    .map(|expr| infer_type(expr))
                    .collect::<Vec<_>>();

                let is_method = match &target_type {
                    Type::Song(_) => {
                        types.insert(0, target_type.clone());
                        true
                    }
                    Type::Variable(v) => {
                        if let Type::Song(_) = **v {
                            types.insert(0, *v.clone());
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                match target_type {
                    Type::Array(t) => {
                        let Expression::Array(inner) = &**target else {
                            panic!("Expression is not an harmony");
                        };

                        write!(
                            file,
                            "array_{}_create({}, ",
                            type_to_encoded_name(&*t),
                            inner.len()
                        )?;
                        self.codegen_expression(file, target)?;
                        write!(file, ").length")?;

                        return Ok(());
                    }
                    Type::Class(_) => {
                        // probably a native module, add a check?

                        write!(file, "{name}(")?;
                        for (i, a) in arguments.iter().enumerate() {
                            if i > 0 || is_method {
                                write!(file, ", ")?;
                            }
                            self.codegen_expression(file, a)?;
                        }
                        write!(file, ")")?;
                        return Ok(());
                    }
                    _ => {}
                }

                println!("{target_type:?}: {name:?}");

                write!(
                    file,
                    "{}_{name}_{}(",
                    extract_class_name(&target_type),
                    types_to_string(&types)
                )?;

                if is_method {
                    self.codegen_expression(file, target)?;
                }

                for (i, a) in arguments.iter().enumerate() {
                    if i > 0 || is_method {
                        write!(file, ", ")?;
                    }
                    self.codegen_expression(file, a)?;
                }
                write!(file, ")")
            }
            Expression::Play(song_type) => {
                write!(file, "{song_type}_allocate()")
            }
            Expression::Array(expressions) => {
                if expressions.len() == 0 {
                    panic!("empty array");
                }

                let t = infer_type(&expressions[0]);
                writeln!(file, "({}[]){{", type_to_c(&t))?;

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
                write!(file, "}}")
            }
            Expression::Class(classname) => write!(file, "{classname}"),
            Expression::Struct(name, fields) => {
                write!(file, "({name}){{ ")?;
                for (i, (n, e)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(file, ", ")?;
                    }

                    write!(file, ".{n} = ")?;
                    self.codegen_expression(file, e)?;
                }
                write!(file, " }}")
            }
            Expression::Unary { operator, operand } => {
                write!(
                    file,
                    "{}",
                    match operator {
                        UnaryOperator::Not => "!",
                        UnaryOperator::Minus => "-",
                        UnaryOperator::Plus => "+",
                    }
                )?;
                self.codegen_expression(file, &operand)
            }
            Expression::Index { object, index } => {
                self.codegen_expression(file, &object)?;
                write!(file, "[")?;
                self.codegen_expression(file, &index)?;
                write!(file, "]")
            }
            unknown => panic!("unknown expression: {unknown:?}"),
        }
    }

    fn codegen_statements(
        &mut self,
        file: &mut File,
        statements: &Vec<Statement>,
    ) -> std::io::Result<()> {
        self.depth += 1;
        self.push();

        let mut last_is_return = false;
        for stmt in statements {
            last_is_return = false;
            match &stmt {
                Statement::VariableDeclaration(variable) => {
                    self.print_depth(file)?;
                    write!(file, "{} {}", type_to_c(&variable.typed), variable.name)?;
                    if let Some(expression) = &variable.initializer {
                        write!(file, " = ")?;

                        if let Type::Array(t) = &variable.typed {
                            let Expression::Array(inner) = expression else {
                                panic!("Expression is not an harmony");
                            };

                            write!(
                                file,
                                "array_{}_create({}, ",
                                type_to_encoded_name(&*t),
                                inner.len()
                            )?;
                            self.codegen_expression(file, expression)?;
                            write!(file, ")")?;
                        } else {
                            self.codegen_expression(file, expression)?;
                        }
                    }

                    self.add_variable(&variable.name, variable.typed.clone());

                    writeln!(file, ";")?;

                    if let Type::Song(_) = variable.typed {
                        if variable.initializer.is_some() {
                            self.print_depth(file)?;
                            writeln!(file, "{}->internal_counter++;", variable.name)?;
                        }
                    }
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
                    write!(file, "{} k_k_k_k = ", type_to_encoded_name(&typed))?;

                    if is_tmp_var {
                        let Expression::Array(inner) = iterator else {
                            panic!("Can't iterator a non-track variable");
                        };

                        write!(
                            file,
                            "array_{}_create({}, ",
                            type_to_encoded_name(&inferred_type),
                            inner.len()
                        )?;
                        self.codegen_expression(file, iterator)?;
                        write!(file, ")")?;
                    } else {
                        self.codegen_expression(file, iterator)?;
                    }
                    writeln!(file, ";")?;

                    self.print_depth(file)?;
                    writeln!(
                        file,
                        "for (size_t i_i_i_i = 0; i_i_i_i < k_k_k_k.length; ++i_i_i_i) {{"
                    )?;

                    self.print_depth(file)?;
                    writeln!(
                        file,
                        "    {} {variable} = k_k_k_k.items[i_i_i_i];",
                        type_to_c(&inferred_type)
                    )?;

                    self.push();
                    self.add_variable(&variable, *inferred_type);

                    self.codegen_statements(file, body)?;
                    self.pop();

                    self.print_depth(file)?;
                    writeln!(file, "}}")?;

                    self.depth -= 1;
                    self.print_depth(file)?;
                    writeln!(file, "}}")?;

                    self.pop();
                }
                Statement::Expression(expression) => {
                    self.print_depth(file)?;
                    self.codegen_expression(file, expression)?;
                    writeln!(file, ";")?;
                }
                Statement::Sing(items) => {
                    for i in items {
                        if i.0 == Type::Void {
                            panic!("Can't print a function returning void.");
                        }

                        self.print_depth(file)?;
                        write!(file, "print_{}(", type_to_encoded_name(&i.0))?;
                        self.codegen_expression(file, &i.1)?;
                        writeln!(file, ");")?;
                    }

                    self.print_depth(file)?;
                    writeln!(file, "puts(\"\");")?;
                }
                Statement::Return(expression) => {
                    self.insert_deallocators(file)?;
                    self.print_depth(file)?;
                    write!(file, "return")?;
                    if let Some(expr) = expression {
                        write!(file, " ")?;
                        self.codegen_expression(file, expr)?;
                    }
                    writeln!(file, ";")?;
                    last_is_return = true;
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
                Statement::CountdownLoop {
                    variable,
                    count,
                    body,
                } => {
                    self.print_depth(file)?;
                    write!(file, "for (size_t i_i_i_i = ")?;
                    self.codegen_expression(file, count)?;
                    writeln!(file, "; i_i_i_i > 0; --i_i_i_i) {{")?;
                    self.print_depth(file)?;
                    writeln!(file, "    int64_t {variable} = i_i_i_i;")?;
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
                        writeln!(file, "{kv_struct} {variable} = ({kv_struct}){{")?;

                        self.print_depth(file)?;
                        writeln!(file, "    .key = \"{}\",", field.0)?;

                        self.print_depth(file)?;
                        write!(file, "    .value = ")?;
                        self.codegen_expression(file, target)?;
                        writeln!(file, ".{},", field.0)?;

                        self.print_depth(file)?;
                        writeln!(file, "}};")?;

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
                    write!(file, "for (size_t i_i_i_i = 0; i_i_i_i < ")?;
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
                    match direction {
                        TuneDirection::Up => write!(file, " += ")?,
                        TuneDirection::Down => write!(file, " -= ")?,
                    }
                    if let Some(a) = amount {
                        self.codegen_expression(file, a)?;
                    } else {
                        write!(file, "1")?;
                    }
                    writeln!(file, ";")?;
                }
                Statement::Echo(items) => {
                    for (id, i) in items.iter().enumerate() {
                        if id > 0 {
                            self.print_depth(file)?;
                            writeln!(file, "print_string(\", \");")?;
                        }
                        self.print_depth(file)?;
                        writeln!(file, "print_string(\"{}\");", type_to_debug_name(&i.0))?;
                    }

                    self.print_depth(file)?;
                    writeln!(file, "puts(\"\");")?;
                }
            }
        }

        if !last_is_return {
            self.insert_last_deallocators(file)?;
        }

        self.depth -= 1;
        self.pop();

        Ok(())
    }

    fn print_depth(&self, file: &mut File) -> std::io::Result<()> {
        write!(file, "{}", " ".repeat(self.depth * 4))
    }

    fn insert_last_deallocators(&self, file: &mut File) -> std::io::Result<()> {
        if let Some(scope) = self.scopes.last() {
            for (k, v) in scope {
                if let Type::Song(n) = v {
                    self.print_depth(file)?;
                    writeln!(file, "{n}_deallocate({k});")?;
                }
            }
        }

        Ok(())
    }

    fn insert_deallocators(&self, file: &mut File) -> std::io::Result<()> {
        let mut d = self.depth;
        for scope in self.scopes.iter().rev() {
            for (k, v) in scope {
                if let Type::Song(n) = v {
                    self.print_depth(file)?;
                    writeln!(file, "{n}_deallocate({k});")?;
                }
            }

            d -= 1;
            if d == 0 {
                break;
            }
        }
        Ok(())
    }
}

fn type_to_debug_name(t: &Type) -> String {
    match t {
        Type::Void => "void".into(),
        Type::Int => "note".into(),
        Type::Float => "pitch".into(),
        Type::String => "lyric".into(),
        Type::Boolean => "beat".into(),
        Type::Array(d) => format!("track[{}]", type_to_debug_name(d)),
        Type::Struct(d) => format!("harmony{{{d}}}"),
        Type::Song(d) => format!("{d}"),
        Type::Class(name) => name.clone(),
        Type::Variable(_) => unreachable!(),
    }
}

fn type_to_encoded_name(t: &Type) -> String {
    match t {
        Type::Void => "void".into(),
        Type::Int => "int".into(),
        Type::Float => "float".into(),
        Type::String => "string".into(),
        Type::Boolean => "bool".into(),
        Type::Array(d) => format!("array_{}", type_to_encoded_name(d)),
        Type::Struct(d) => format!("struct_{d}"),
        Type::Song(d) => format!("song_{d}"),
        Type::Class(name) => name.clone(),
        Type::Variable(_) => unreachable!(),
    }
}

fn extract_class_name(t: &Type) -> String {
    match t {
        Type::Song(name) => name.clone(),
        Type::Class(name) => name.clone(),
        Type::Variable(var) => extract_class_name(var),
        unk => panic!("--- {unk:?} ---"),
    }
}

fn type_to_c(t: &Type) -> String {
    match t {
        Type::Void => "void".into(),
        Type::Int => "int64_t".into(),
        Type::Float => "double".into(),
        Type::String => "const char*".into(),
        Type::Boolean => "bool".into(),
        Type::Array(typed) => format!("array_{}", type_to_encoded_name(typed)),
        Type::Struct(name) => name.clone(),
        Type::Song(name) => format!("{name}*"),
        Type::Class(_) => unreachable!(),
        Type::Variable(_) => unreachable!(),
    }
}

fn types_to_string(args: &Vec<Type>) -> String {
    args.iter()
        .map(|e| type_to_encoded_name(e))
        .collect::<Vec<_>>()
        .join("_")
}

pub fn infer_type(expr: &Expression) -> Type {
    match expr {
        Expression::Integer(_) => Type::Int,
        Expression::Number(_) => Type::Float,
        Expression::Variable(_, typed) => Type::Variable(Box::new(typed.clone())),
        Expression::Class(cn) => Type::Class(cn.clone()),
        Expression::Field { typed, .. } => typed.clone(),
        Expression::Struct(typename, _) => Type::Struct(typename.clone()),
        Expression::String(_) => Type::String,
        Expression::Array(items) => {
            assert_ne!(items.len(), 0);
            let inner_type = infer_type(&items[0]);
            Type::Array(Box::new(inner_type))
        }
        Expression::Call {
            target: _,
            name: _,
            arguments: _,
            typed,
        } => typed.clone(),
        unknown => panic!("Unknown expression to infer: {unknown:?}"),
    }
}
