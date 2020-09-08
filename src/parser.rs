use super::{Ast, Value};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::multispace1,
    combinator::{all_consuming, map},
    multi::many0,
    number::complete::double,
    sequence::{delimited, pair, preceded},
    IResult,
    Err::Error,
};

enum ParseError<I> {
    LiteralOverflow(I),
    Nom(I, nom::error::ErrorKind),
}

impl<I> nom::error::ParseError<I> for ParseError<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        ParseError::Nom(input, kind)
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

fn parse_scope(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    delimited(tag("{"), parse_many, tag("}"))(input)
}

fn parse_identifier(input: &str) -> IResult<&str, &str, ParseError<&str>> {
    take_while1(|c: char| c.is_alphabetic() || c == '_')(input)
}

fn parse_float(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    map(double, |f| Ast::Literal(Value::Float(f)))(input)
}

fn parse_integer(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    let (input, digits) = take_while1(|c: char| c.is_digit(10))(input)?;
    let number =
        i64::from_str_radix(digits, 10).map_err(|_| Error(ParseError::LiteralOverflow(input)))?;
    Ok((input, Ast::Literal(Value::Integer(number))))
}

fn parse_parameters(input: &str) -> IResult<&str, Vec<Ast>, ParseError<&str>> {
    many0(preceded(multispace1, parse_parameter))(input)
}

fn parse_parameter(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    alt((
        map(parse_identifier, |id| Ast::Literal(Value::Identifier(id))),
        parse_integer,
        parse_float,
        parse_scope,
    ))(input)
}

fn parse_call(input: &str) -> IResult<&str, Ast<'_>, ParseError<&str>> {
    let (input, (name, arguments)) =
        delimited(tag("@"), pair(parse_identifier, parse_parameters), tag("/"))(input)?;
    let (input, varargs) = many0(parse_one)(input)?;
    Ok((input, Ast::Call(name, arguments, varargs)))
}

fn parse_text(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    map(
        take_while1(|c: char| c != '@' && c != '{' && c != '}'),
        |text: &str| Ast::Literal(Value::Text(text.into())),
    )(input)
}

fn parse_one(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    alt((parse_call, parse_scope, parse_text))(input)
}

fn parse_many(input: &str) -> IResult<&str, Ast, ParseError<&str>> {
    let (input, mut asts) = many0(parse_one)(input)?;
    if asts.len() == 1 {
        Ok((input, asts.pop().unwrap()))
    } else {
        Ok((input, Ast::Call("id", vec![], asts)))
    }
}

pub fn parse(input: &str) -> Result<Ast, String> {
    let (_, ast) = all_consuming(parse_many)(input).map_err(|_| "could not parse.".to_owned())?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let program = "";
        let result = parse(program);
        assert_eq!(result, Ok(Ast::Call("id", vec![], vec![])));
    }

    #[test]
    fn many() {
        let program = "{one}{two}";
        let result = parse(program);
        assert_eq!(
            result,
            Ok(Ast::Call(
                "id",
                vec![],
                vec![
                    Ast::Literal(Value::Text("one".into())),
                    Ast::Literal(Value::Text("two".into()))
                ]
            ))
        );
    }

    #[test]
    fn simple() {
        let program = "@red/some red text";
        let result = parse(program);

        assert_eq!(
            result,
            Ok(Ast::Call(
                "red",
                vec![],
                vec![Ast::Literal(Value::Text("some red text".into()))]
            ))
        );
    }

    #[test]
    fn complex() {
        let program = "{@color 0 255 0/this is green text @on_hover show_text {This is text @blue/shown on hover}/hover here.}";
        let result = parse(program);
        assert_eq!(
            result,
            Ok(Ast::Call(
                "color",
                vec![
                    Ast::Literal(Value::Integer(0)),
                    Ast::Literal(Value::Integer(255)),
                    Ast::Literal(Value::Integer(0))
                ],
                vec![
                    Ast::Literal(Value::Text("this is green text ".into())),
                    Ast::Call(
                        "on_hover",
                        vec![
                            Ast::Literal(Value::Identifier("show_text")),
                            Ast::Call(
                                "id",
                                vec![],
                                vec![
                                    Ast::Literal(Value::Text("This is text ".into())),
                                    Ast::Call(
                                        "blue",
                                        vec![],
                                        vec![Ast::Literal(Value::Text("shown on hover".into()))]
                                    )
                                ]
                            )
                        ],
                        vec![Ast::Literal(Value::Text("hover here.".into()))]
                    )
                ]
            ))
        )
    }

    #[test]
    fn bad_scopes() {
        let program = "{";
        let result = parse(program);
        assert!(result.is_err());

        let program = "}";
        let result = parse(program);
        assert!(result.is_err());

        let program = "{some text}}";
        let result = parse(program);
        assert!(result.is_err());
    }
}
