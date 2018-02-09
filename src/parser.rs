use lexer::*;
use lexer::Operator::*;

macro_rules! unexpected {
        ($expected:expr, $got:expr) => {Err(format!("expected {}, fount {:?}", $expected, $got))};
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary {
        op: Operator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Call {
        name: String,
        args: Vec<Box<Expr>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    name: String,
    args: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    proto: Box<Prototype>,
    body: Box<Expr>,
}

impl Function {
    pub fn new(proto: Box<Prototype>, body: Box<Expr>) -> Self {
        Function {
            proto: proto,
            body: body,
        }
    }
}

impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Self {
        Prototype {
            name: name,
            args: args,
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn from_source(source: &'a str) -> Parser<'a> {
        Self::from_lexer(Lexer::new(source))
    }

    pub fn from_lexer(mut lexer: Lexer<'a>) -> Parser<'a> {
        let current = lexer.next();
        Parser {
            lexer: lexer,
            current: current,
        }
    }

    pub fn get_next_token(&mut self) {
        self.current = self.lexer.next();
    }

    fn parse_number(&mut self) -> Result<Box<Expr>, String> {
        match self.current {
            Some(Token::Number(n)) => {
                self.get_next_token();
                Ok(Box::new(Expr::Number(n)))
            }
            ref x => unexpected!("number", x),
        }
    }

    fn parse_paren(&mut self) -> Result<Box<Expr>, String> {
        self.get_next_token();
        let v = self.parse_expression()?;
        match self.current {
            Some(Token::ClosingParenthesis) => Ok(v),
            ref x => unexpected!("')'", x),
        }
    }

    /// identifierexpr
    /// ::= identifier
    /// ::= identifier '(' expression* ')'
    fn parse_identifier(&mut self) -> Result<Box<Expr>, String> {
        let id = match self.current {
            Some(Token::Ident(ref s)) => s.clone(),
            _ => return unexpected!("identifier", self.current),
        };
        self.get_next_token();

        if Some(Token::OpeningParenthesis) == self.current {
            let mut args = Vec::new();
            loop {
                let arg = self.parse_expression()?;
                args.push(arg);
                if self.current == Some(Token::ClosingParenthesis) {
                    break;
                }
                if self.current != Some(Token::Comma) {
                    return unexpected!("')' or ',' in argument list", self.current);
                }
            }
            Ok(Box::new(Expr::Call {
                name: id,
                args: args,
            }))
        } else {
            Ok(Box::new(Expr::Variable(id)))
        }
    }

    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    fn parse_primary(&mut self) -> Result<Box<Expr>, String> {
        match self.current {
            Some(Token::Ident(_)) => self.parse_identifier(),
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::OpeningParenthesis) => self.parse_paren(),
            _ => unexpected!("expression", self.current),
        }
    }

    /// binoprhs
    ///   ::= ('+' primary)*
    fn parse_bin_op_rhs(&mut self, prec: u8, mut lhs: Box<Expr>) -> Result<Box<Expr>, String> {
        loop {
            let op = match self.current {
                Some(Token::Operator(op)) => op,
                _ => return Ok(lhs),
            };

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            let tok_prec = match operator_precedence(op) {
                tok_prec if tok_prec < prec => return Ok(lhs),
                tok_prec => tok_prec,
            };

            // eat op
            self.get_next_token();

            let mut rhs = self.parse_primary()?;

            // If BinOp binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            // Add 1 to tok_prec so `a + b * c + d` wille be parsed as:
            // `(a + (b * c)) +d`
            // Not:
            // `a + ((b * c) + d)`
            if let Some(Token::Operator(op)) = self.current {
                match operator_precedence(op) {
                    next_prec if tok_prec < next_prec => {
                        rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?
                    }
                    _ => (),
                }
            }

            lhs = Box::new(Expr::Binary {
                op: op,
                lhs: lhs,
                rhs: rhs,
            });
        }
    }

    /// expression
    ///   ::= primary binoprhs
    ///
    fn parse_expression(&mut self) -> Result<Box<Expr>, String> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_prototype(&mut self) -> Result<Box<Prototype>, String> {
        let fn_name = match self.current {
            Some(Token::Ident(ref id)) => id.clone(),
            _ => return unexpected!("function name in prototype", self.current),
        };
        self.get_next_token();
        if Some(Token::OpeningParenthesis) != self.current {
            return unexpected!("`(` in prototype", self.current);
        }
        let mut arg_names = Vec::new();
        loop {
            self.get_next_token();
            match self.current {
                Some(Token::Ident(ref name)) => {
                    arg_names.push(name.to_string());
                }
                Some(Token::ClosingParenthesis) => {
                    self.get_next_token();
                    return Ok(Box::new(Prototype::new(fn_name, arg_names)));
                }
                _ => {
                    return unexpected!("',' or ')'", self.current);
                }
            }
        }
    }

    /// definition ::= 'def' prototype expression
    pub fn parse_definition(&mut self) -> Result<Box<Function>, String> {
        // eat `def`
        self.get_next_token();

        let proto = self.parse_prototype()?;
        let body = self.parse_expression()?;
        Ok(Box::new(Function::new(proto, body)))
    }

    /// external ::= 'extern' prototype
    pub fn parse_extern(&mut self) -> Result<Box<Prototype>, String> {
        // eat `extern`
        self.get_next_token();
        self.parse_prototype()
    }

    /// toplevelexpr ::= expression
    pub fn parse_top_level(&mut self) -> Result<Box<Function>, String> {
        let proto = Box::new(Prototype::new(String::new(), Vec::new()));
        let body = self.parse_expression()?;
        Ok(Box::new(Function::new(proto, body)))
    }
}

fn operator_precedence(op: Operator) -> u8 {
    match op {
        LessThan => 10,
        Add | Sub => 20,
        Mul => 40,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_parsing() {
        let mut parser = Parser::from_source("1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Box::new(Expr::Number(1.0)));
        let mut parser = Parser::from_source("1234567890");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Box::new(Expr::Number(1234567890.0)));
        let mut parser = Parser::from_source("3.14159");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Box::new(Expr::Number(3.14159)));
        let mut parser = Parser::from_source("1.");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Box::new(Expr::Number(1.0)));
        let mut parser = Parser::from_source(".1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(ast, Box::new(Expr::Number(0.1)));
    }

    #[test]
    fn test_basic_expression_parsing() {
        let mut parser = Parser::from_source("1 + 1");
        let ast = parser.parse_expression().unwrap();
        assert_eq!(
            ast,
            Box::new(Expr::Binary {
                op: Operator::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            })
        )
    }

    #[test]
    fn test_complicated_expression_parsing() {
        let mut parser = Parser::from_source("1 + 2 * 3 - 2");
        let got = parser.parse_expression().unwrap();
        let expected = Box::new(Expr::Binary {
            op: Operator::Sub,
            lhs: Box::new(Expr::Binary {
                op: Operator::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Expr::Number(2.0)),
                    rhs: Box::new(Expr::Number(3.0)),
                }),
            }),
            rhs: Box::new(Expr::Number(2.0)),
        });
        assert_eq!(got, expected)
    }

    #[test]
    fn test_prototype_parsing() {
        let mut parser = Parser::from_source("foo()");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("foo"), vec![]);
        assert_eq!(got, Box::new(expected));
        let mut parser = Parser::from_source("bar(a)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("bar"), vec![String::from("a")]);
        assert_eq!(got, Box::new(expected));
        let mut parser = Parser::from_source("bar(a b c)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(
            String::from("bar"),
            vec![String::from("a"), String::from("b"), String::from("c")],
        );
        assert_eq!(got, Box::new(expected));
    }

    #[test]
    fn test_function_definition_parsing() {
        let mut parser = Parser::from_source("def foo() 1 + 1");
        let got = parser.parse_definition().unwrap();
        let expected = Function::new(
            Box::new(Prototype::new(String::from("foo"), vec![])),
            Box::new(Expr::Binary {
                op: Operator::Add,
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Number(1.0)),
            }),
        );
        assert_eq!(got, Box::new(expected));
    }

    #[test]
    fn test_extern_parsing() {
        let mut parser = Parser::from_source("extern sin(a)");
        let got = parser.parse_extern().unwrap();
        let expected = Prototype::new(String::from("sin"), vec![String::from("a")]);
        assert_eq!(got, Box::new(expected));
    }

}
