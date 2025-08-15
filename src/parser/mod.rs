pub mod ast;
pub mod inferrer;
pub mod typedast;

use crate::parser::ast::*;
use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "miku.pest"]
pub struct MikuParser;

pub fn parse(filename: &str) -> Program {
    let content = std::fs::read_to_string(filename).expect(&format!("File {filename} not found"));
    let pairs = MikuParser::parse(Rule::program, &content).unwrap();

    assert_eq!(pairs.len(), 1);
    let p = pairs.collect::<Vec<_>>();

    convert_program(p[0].clone())
}

fn convert_program(pair: Pair<Rule>) -> Program {
    assert_eq!(pair.as_rule(), Rule::program);

    let mut program = Program {
        imports: vec![],
        song: SongDeclaration {
            name: "Miku".into(),
            parent: None,
            statements: vec![],
        },
        span: Span::default(),
    };

    let inner = pair.into_inner();

    for pair in inner {
        match pair.as_rule() {
            Rule::import_statement => {
                program.imports.push(convert_import(pair));
            }
            Rule::song_declaration => {
                program.song = convert_song(pair);
            }
            Rule::EOI => {
                break;
            }
            _ => panic!("{pair:?}"),
        }
    }

    program
}

fn convert_import(pair: Pair<Rule>) -> RemixStatement {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let import_type = match inner[0].as_rule() {
        Rule::identifier => {
            let module = inner[0].as_str().to_string();

            let alias = if inner.len() == 2 {
                Some(inner[1].as_str().to_string())
            } else {
                None
            };

            RemixType::Module { module, alias }
        }
        Rule::import_list => {
            let items = inner[0]
                .clone()
                .into_inner()
                .map(|e| {
                    let inner = e.into_inner().collect::<Vec<_>>();
                    let name = inner[0].as_str().to_string();

                    let alias = if inner.len() == 2 {
                        Some(inner[1].as_str().to_string())
                    } else {
                        None
                    };

                    ImportItem {
                        name,
                        alias,
                        span: Span::default(),
                    }
                })
                .collect::<Vec<_>>();
            let module = inner[1].as_str().to_string();
            RemixType::Selective { module, items }
        }
        _ => {
            panic!("{:?}", inner[0]);
        }
    };
    RemixStatement {
        import_type,
        span: Span::default(),
    }
}

fn convert_song(pair: Pair<Rule>) -> SongDeclaration {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let name = inner[0].as_str().to_string();

    let parent = if inner[1].as_rule() == Rule::cover_item {
        let parent_inner = inner[1].clone().into_inner().collect::<Vec<_>>();
        assert_eq!(parent_inner[0].as_rule(), Rule::identifier);
        Some(parent_inner[0].as_str().to_string())
    } else {
        None
    };

    let stmts = inner[if parent.is_some() { 2 } else { 1 }]
        .clone()
        .into_inner()
        .collect::<Vec<_>>();

    let mut statements = vec![];

    for pair in stmts {
        statements.push(match pair.as_rule() {
            Rule::function_declaration => convert_song_function_decl(pair),
            Rule::variable_declaration => convert_song_variable_decl(pair),
            _ => unreachable!(),
        });
    }

    SongDeclaration {
        name,
        parent,
        statements,
    }
}

fn convert_song_function_decl(pair: Pair<Rule>) -> SongStatementKind {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let verse = match inner[0].as_str() {
        "melody" => false,
        "verse" => true,
        _ => unreachable!(),
    };
    let name = inner[1].as_str().to_string();
    let mut parameters = vec![];
    let mut return_type = MikuType::Void;

    let mut index = 2;
    if inner[index].as_rule() == Rule::parameter_list {
        parameters = convert_parameters(inner[index].clone());
        index += 1;
    }

    if inner[index].as_rule() == Rule::mikutype {
        return_type = convert_miku_type(inner[index].as_str());
        index += 1;
    }

    let body = convert_block(inner[index].clone());

    SongStatementKind::MelodyDeclaration {
        verse,
        name,
        parameters,
        return_type,
        body,
    }
}

fn convert_for_loop(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    assert_eq!(inner[0].as_rule(), Rule::identifier);
    let variable = inner[0].as_str().to_string();
    let iterable = convert_expression(inner[1].clone());
    let body = convert_block(inner[2].clone());
    Statement::new(
        StatementKind::ForLoop {
            variable,
            iterable,
            body,
        },
        Span::default(),
    )
}

fn convert_countdown_loop(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    assert_eq!(inner[0].as_rule(), Rule::identifier);
    let variable = inner[0].as_str().to_string();
    let count = convert_expression(inner[1].clone());
    let body = convert_block(inner[2].clone());
    Statement::new(
        StatementKind::CountdownLoop {
            variable,
            count,
            body,
        },
        Span::default(),
    )
}

fn convert_while_loop(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let condition = convert_expression(inner[0].clone());
    let body = convert_block(inner[1].clone());
    Statement::new(
        StatementKind::WhileLoop { condition, body },
        Span::default(),
    )
}

fn convert_repeat_loop(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let count = convert_expression(inner[0].clone());
    let body = convert_block(inner[1].clone());
    Statement::new(StatementKind::RepeatLoop { count, body }, Span::default())
}

fn convert_loop(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let pair = inner[0].clone();
    match pair.as_rule() {
        Rule::repeat_loop => convert_repeat_loop(pair),
        Rule::countdown_loop => convert_countdown_loop(pair),
        Rule::for_loop => convert_for_loop(pair),
        Rule::while_loop => convert_while_loop(pair),
        unknown => panic!("unknown rule: {:?}", unknown),
    }
}

fn convert_assignment(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    assert_eq!(inner[0].as_rule(), Rule::identifier);
    let identifier = inner[0].as_str().to_string();

    let mut expression = Expression::identifier(identifier);

    for id in 1..(inner.len() - 1) {
        let kind = match inner[id].as_rule() {
            Rule::member => {
                let member = inner[id].clone().into_inner().collect::<Vec<_>>();
                let member = member[0].as_str().to_string();
                ExpressionKind::Member {
                    object: Box::new(expression),
                    member,
                }
            }
            Rule::index => {
                let real_name = inner[id].clone().into_inner().collect::<Vec<_>>();
                let index = convert_expression(real_name[0].clone());
                ExpressionKind::Index {
                    object: Box::new(expression),
                    index: Box::new(index),
                }
            }
            _ => todo!(),
        };

        expression = Expression {
            kind,
            span: Span::default(),
        };
    }

    let value = convert_expression(inner[inner.len() - 1].clone());

    Statement::new(
        StatementKind::Assignment {
            target: expression,
            value,
        },
        Span::default(),
    )
}

fn convert_conditional(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let mut if_branch = (Expression::number(0.), vec![]);
    let mut else_ifs = vec![];
    let mut else_branch = None;
    for i in inner {
        match i.as_rule() {
            Rule::if_branch => {
                if_branch = convert_branch(i);
            }
            Rule::else_if_branch => {
                else_ifs.push(convert_branch(i));
            }
            Rule::else_branch => {
                let inner = i.into_inner().collect::<Vec<_>>();
                else_branch = Some(convert_block(inner[0].clone()));
            }
            unknown => panic!("Unknown conditional token {unknown:?}"),
        }
    }
    Statement::new(
        StatementKind::Conditional {
            condition: if_branch.0,
            then_branch: if_branch.1,
            else_ifs,
            else_branch,
        },
        Span::default(),
    )
}

fn convert_branch(pair: Pair<Rule>) -> (Expression, Vec<Statement>) {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let expression = convert_expression(inner[0].clone());
    let block = convert_block(inner[1].clone());

    (expression, block)
}

fn convert_variable_decl(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let (miku_type, initializer) = match inner.len() {
        2 => match inner[1].as_rule() {
            Rule::mikutype => {
                let mtype = convert_miku_type(inner[1].as_str());
                (Some(mtype), None)
            }
            Rule::expression => {
                let initializer = convert_expression(inner[1].clone());
                (None, Some(initializer))
            }
            _ => unreachable!(),
        },
        3 => {
            let mtype = convert_miku_type(inner[1].as_str());
            let initializer = convert_expression(inner[2].clone());
            (Some(mtype), Some(initializer))
        }
        _ => unreachable!(),
    };

    let name = inner[0].as_str().to_string();

    Statement::new(
        StatementKind::VariableDeclaration {
            name,
            miku_type,
            initializer,
        },
        Span::default(),
    )
}

fn convert_song_variable_decl(pair: Pair<Rule>) -> SongStatementKind {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let (miku_type, initializer) = match inner.len() {
        2 => match inner[1].as_rule() {
            Rule::mikutype => {
                let mtype = convert_miku_type(inner[1].as_str());
                (Some(mtype), None)
            }
            Rule::expression => {
                let initializer = convert_expression(inner[1].clone());
                (None, Some(initializer))
            }
            _ => unreachable!(),
        },
        3 => {
            let mtype = convert_miku_type(inner[1].as_str());
            let initializer = convert_expression(inner[2].clone());
            (Some(mtype), Some(initializer))
        }
        _ => unreachable!(),
    };

    let name = inner[0].as_str().to_string();

    SongStatementKind::VariableDeclaration {
        name,
        miku_type,
        initializer,
    }
}

fn convert_miku_type(name: &str) -> MikuType {
    match name {
        "note" => MikuType::Note,
        "pitch" => MikuType::Pitch,
        "lyric" => MikuType::Lyric,
        "beat" => MikuType::Beat,
        "track" => MikuType::Track,
        "harmony" => MikuType::Harmony,
        "void" => MikuType::Void,
        _ => todo!(),
    }
}

fn convert_expression(pair: Pair<Rule>) -> Expression {
    let inner = pair.clone().into_inner().collect::<Vec<_>>();
    match pair.as_rule() {
        Rule::expression => convert_expression(inner[0].clone()),
        Rule::logical_or => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[1].clone());
                Expression::binary(left, BinaryOperator::Or, right)
            }
        }
        Rule::logical_and => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[1].clone());
                Expression::binary(left, BinaryOperator::And, right)
            }
        }
        Rule::equality => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[2].clone());
                Expression::binary(left, BinaryOperator::from_str(inner[1].as_str()), right)
            }
        }
        Rule::comparison => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[2].clone());
                Expression::binary(left, BinaryOperator::from_str(inner[1].as_str()), right)
            }
        }
        Rule::harmonic => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                todo!()
            }
        }
        Rule::additive => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[2].clone());

                Expression::binary(left, BinaryOperator::from_str(inner[1].as_str()), right)
            }
        }
        Rule::multiplicative => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let left = convert_expression(inner[0].clone());
                let right = convert_expression(inner[2].clone());
                Expression::binary(left, BinaryOperator::from_str(inner[1].as_str()), right)
            }
        }
        Rule::unary => {
            if inner.len() == 1 {
                convert_expression(inner[0].clone())
            } else {
                let unary_op = match inner[0].as_str() {
                    "-" => UnaryOperator::Minus,
                    "+" => UnaryOperator::Plus,
                    "!" => UnaryOperator::Not,
                    _ => unreachable!(),
                };

                let expr = convert_expression(inner[1].clone());
                Expression::unary(unary_op, expr)
            }
        }
        Rule::postfix => {
            let mut main = convert_expression(inner[0].clone());
            if inner.len() == 1 {
                main
            } else {
                for i in 1..inner.len() {
                    main = match inner[i].as_rule() {
                        Rule::call => {
                            let args = convert_call(inner[i].clone());
                            Expression::call(main, args)
                        }
                        Rule::member => {
                            let real_name = inner[i].clone().into_inner().collect::<Vec<_>>();
                            let member = real_name[0].as_str().to_string();
                            Expression::member(main, member)
                        }
                        Rule::index => {
                            let real_name = inner[i].clone().into_inner().collect::<Vec<_>>();
                            let index = convert_expression(real_name[0].clone());
                            Expression::index(main, index)
                        }
                        _ => panic!("{:?}", inner[i]),
                    };
                }

                main
            }
        }
        Rule::call => todo!(),
        Rule::index => todo!(),
        Rule::member => todo!(),
        Rule::primary => convert_expression(inner[0].clone()),
        Rule::track_literal => {
            assert_eq!(inner[0].as_rule(), Rule::expression_list);
            let expr_list = inner[0].clone().into_inner().collect::<Vec<_>>();
            let array = expr_list
                .iter()
                .map(|e| convert_expression(e.clone()))
                .collect::<Vec<_>>();

            Expression {
                kind: ExpressionKind::Array(array),
                span: Span::default(),
            }
        }
        Rule::harmony_literal => convert_harmony_literal(inner[0].clone()),
        Rule::harmony_field_list => todo!(),
        Rule::harmony_field => todo!(),
        Rule::boolean_literal => convert_boolean_literal(pair.as_str()),
        Rule::identifier => {
            let ident = pair.as_str().to_string();
            Expression::identifier(ident)
        }
        Rule::number => {
            let string = pair.as_str();
            if string.contains(".") {
                let number = pair.as_str().parse::<f64>().unwrap();
                Expression::number(number)
            } else {
                let number = pair.as_str().parse::<i64>().unwrap();
                Expression::integer(number)
            }
        }
        Rule::string_literal => {
            let string = pair.as_str().to_string();
            Expression::string(string)
        }
        Rule::play_statement => {
            let data = pair.into_inner().collect::<Vec<_>>();
            let string = data[0].as_str().to_string();
            Expression {
                kind: ExpressionKind::Play(string),
                span: Span::default(),
            }
        }
        unknown => {
            panic!("unknown: {:?}", unknown);
        }
    }
}

fn convert_parameters(pair: Pair<Rule>) -> Vec<Parameter> {
    let mut params = vec![];

    for pair in pair.into_inner() {
        let data = pair.into_inner().collect::<Vec<_>>();
        let name = data[0].as_str().to_string();
        let miku_type = convert_miku_type(data[1].as_str());
        params.push(Parameter {
            name,
            miku_type,
            span: Span::default(),
        });
    }

    params
}

fn convert_boolean_literal(value: &str) -> Expression {
    Expression::boolean(match value {
        "true" => true,
        "false" => false,
        _ => panic!("unknown boolean: {value}"),
    })
}

fn convert_harmony_literal(pair: Pair<Rule>) -> Expression {
    let inner = pair.into_inner().collect::<Vec<_>>();
    let mut kv_entry = vec![];

    for i in inner {
        let kv = i.into_inner().collect::<Vec<_>>();
        let k = match kv[0].as_rule() {
            Rule::identifier => HarmonyKey::Identifier(kv[0].as_str().into()),
            Rule::string_literal => HarmonyKey::String(kv[0].as_str().trim_matches('"').into()),
            unknown => panic!("{:?}", unknown),
        };

        let v = convert_expression(kv[1].clone());

        kv_entry.push((k, v));
    }

    Expression {
        kind: ExpressionKind::Harmony(kv_entry),
        span: Span::default(),
    }
}

fn convert_block(pair: Pair<Rule>) -> Vec<Statement> {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let mut stmts = vec![];

    for i in inner {
        stmts.push(convert_statement(i));
    }

    stmts
}

fn convert_statement(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().collect::<Vec<_>>();
    assert_eq!(inner.len(), 1);
    let pair = &inner[0];
    match pair.as_rule() {
        Rule::variable_declaration => convert_variable_decl(pair.clone()),
        Rule::assignment => convert_assignment(pair.clone()),
        Rule::loop_statement => convert_loop(pair.clone()),
        Rule::conditional => convert_conditional(pair.clone()),
        Rule::sing_statement => {
            let expr_list = pair.clone().into_inner().collect::<Vec<_>>();
            let expressions = convert_expression_list(expr_list[0].clone());
            Statement::new(
                StatementKind::SingStatement { expressions },
                Span::default(),
            )
        }
        Rule::echo_statement => {
            let expr_list = pair.clone().into_inner().collect::<Vec<_>>();
            let expressions = convert_expression_list(expr_list[0].clone());
            Statement::new(
                StatementKind::EchoStatement { expressions },
                Span::default(),
            )
        }
        Rule::tune_statement => {
            let expr_list = pair.clone().into_inner().collect::<Vec<_>>();
            let mut list_index = 0;

            assert_eq!(expr_list[list_index].as_rule(), Rule::identifier);
            let identifier = expr_list[list_index].as_str().to_string();

            let mut variable = Expression::identifier(identifier);

            loop {
                list_index += 1;
                if expr_list[list_index].as_rule() != Rule::member {
                    break;
                }

                let member = expr_list[list_index]
                    .clone()
                    .into_inner()
                    .collect::<Vec<_>>();
                let member = member[0].as_str().to_string();

                variable = Expression::member(variable, member);
            }

            let direction = match expr_list[list_index].as_str() {
                "up" => TuneDirection::Up,
                "down" => TuneDirection::Down,
                _ => unreachable!(),
            };
            list_index += 1;
            let amount = if list_index < expr_list.len() {
                Some(convert_expression(expr_list[list_index].clone()))
            } else {
                None
            };
            Statement::new(
                StatementKind::TuneStatement {
                    variable,
                    direction,
                    amount,
                },
                Span::default(),
            )
        }
        Rule::return_statement => {
            let expr = pair.clone().into_inner().collect::<Vec<_>>();
            let value = if expr.len() == 1 {
                Some(convert_expression(expr[0].clone()))
            } else {
                None
            };
            Statement::new(StatementKind::ReturnStatement { value }, Span::default())
        }
        Rule::expression_statement => {
            let expr = pair.clone().into_inner().collect::<Vec<_>>();
            let expression = convert_expression(expr[0].clone());

            Statement::new(
                StatementKind::ExpressionStatement { expression },
                Span::default(),
            )
        }
        unknown => panic!("{unknown:?}: {pair:?}"),
    }
}

fn convert_expression_list(pair: Pair<Rule>) -> Vec<Expression> {
    let inner = pair.into_inner().collect::<Vec<_>>();

    let mut elems = vec![];

    for i in inner {
        elems.push(convert_expression(i));
    }

    elems
}

fn convert_call(pair: Pair<Rule>) -> Vec<Expression> {
    let inner = pair.into_inner().collect::<Vec<_>>();
    if inner.len() == 0 {
        vec![]
    } else {
        convert_expression_list(inner[0].clone())
    }
}
