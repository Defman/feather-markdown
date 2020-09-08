use std::collections::BTreeMap;
use std::borrow::Cow;

mod parser;

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Integer(i64),
    Float(f64),
    Text(Cow<'a, str>),
    Identifier(&'a str),
}

impl Value<'_> {
    fn kind(&self) -> Kind {
        match self {
            Value::Integer(_) => Kind::Number,
            Value::Float(_) => Kind::Float,
            Value::Text(_) => Kind::Text,
            Value::Identifier(_) => Kind::Identifier,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Call(&'a str, Vec<Ast<'a>>, Vec<Ast<'a>>),
    Literal(Value<'a>),
}

fn parse(program: &str) -> Result<Ast, String> {
    parser::parse(program).map_err(|_| "Could not parse".to_owned())
}

type Lambda<'a> = fn(Vec<Value<'a>>, Vec<Value<'a>>) -> Result<Value<'a>, String>;

#[derive(Debug, PartialEq)]
enum Kind {
    Number,
    Float,
    Text,
    Identifier,
}

struct Function<'a> {
    params: Vec<Kind>,
    lambda: Lambda<'a>,
}

fn expected_kind(value: &Value, kind: &Kind) -> Result<(), String> {
    match (value, kind) {
        (Value::Text(_), Kind::Text) => Ok(()),
        (Value::Integer(_), Kind::Number) => Ok(()),
        (Value::Float(_), Kind::Float) => Ok(()),
        _ => Err(format!("expecetd type {:?} got {:?}.", value.kind(), kind)),
    }
}

fn eval<'a>(ast: Ast<'a>, functions: &BTreeMap<String, Function<'a>>) -> Result<Value<'a>, String> {
    match ast {
        Ast::Call(name, arguments, varargs) => match functions.get(name) {
            Some(function) => {
                let arity_check = function.params.len() == arguments.len();
                if !arity_check {
                    return Err("arity check failed".to_owned());
                }

                let arguments = arguments
                    .into_iter()
                    .map(|param_ast| eval(param_ast, functions))
                    .collect::<Result<Vec<Value>, String>>()?;

                arguments
                    .iter()
                    .zip(&function.params)
                    .map(|(value, kind)| expected_kind(value, kind))
                    .collect::<Result<Vec<()>, String>>()?;

                let varargs = varargs
                    .into_iter()
                    .map(|arg| eval(arg, functions))
                    .collect::<Result<Vec<Value>, String>>()?;

                let lambda = function.lambda;
                lambda(arguments, varargs)
            }
            None => Err("No such function".to_owned()),
        },
        Ast::Literal(value) => Ok(value),
    }
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() -> Result<(), String> {
        let mut functions: BTreeMap<String, Function<'_>> = Default::default();
        functions.insert(
            "red".to_owned(),
            Function {
                params: vec![],
                lambda: |_arguments, varargs| {
                    let text = varargs.into_iter().map(|text| match text {
                        Value::Text(text) => text,
                        _ => unreachable!("all varargs are text"),
                    }).collect::<String>();
                    Ok(Value::Text(format!("{{@red/{}}}", text).into()))
                },
            },
        );

        functions.insert("id".to_owned(), 
        Function {
                params: vec![],
                lambda: |_, varargs| {
                    let text = varargs.into_iter().map(|text| match text {
                        Value::Text(text) => text,
                        _ => unreachable!("all varargs are text"),
                    }).collect::<String>();
                    Ok(Value::Text(format!("{{@id/{}}}", text).into()))
                }
            }
        );

        let program = "{@red/some red {@id/bold} text}";
        let ast = parse(program)?;
        let value = eval(ast, &functions)?;
        assert_eq!(value, Value::Text("{@red/some red {@id/bold} text}".into()));
        // assert_eq!(value, Value::Text("some red bold text".into()));
        Ok(())
    }
}
