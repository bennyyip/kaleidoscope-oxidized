use lexer::*;

use std::collections::HashMap;
use std::sync::Mutex;

macro_rules! unexpected {
        ($expected:expr, $got:expr) => {Err(format!("expected {}, fount {:?}", $expected, $got))};
}

lazy_static! {
    static ref PRECEDENCE_TABLE: Mutex<HashMap<char, u8>> = {
        let mut m = HashMap::new();
        m.insert('=', 2);
        m.insert('<', 10);
        m.insert('+', 20);
        m.insert('-', 20);
        m.insert('*', 40);
        m.insert('/', 40);
        Mutex::new(m)
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(f64),
    Variable(String),

    Unary {
        op: char,
        operand: Box<Expr>,
    },

    Binary {
        op: char,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Call {
        name: String,
        args: Vec<Box<Expr>>,
    },

    If {
        cond: Box<Expr>,
        consequence: Box<Expr>,
        alternative: Box<Expr>,
    },

    // Actually is a `do {} while(cond)` loop
    For {
        var_name: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Box<Expr>,
    },

    Var {
        var_names: Vec<(String, Box<Expr>)>,
        body: Box<Expr>,
    },
}

pub const ANONYMOUS_FUNCTION_NAME: &str = "__MAIN__";

#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub is_operator: bool,
    pub precedence: u8,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub(crate) proto: Box<Prototype>,
    pub(crate) body: Box<Expr>,
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
    pub fn new(name: String, args: Vec<String>, is_operator: bool, precedence: u8) -> Self {
        Prototype {
            name: name,
            args: args,
            is_operator: is_operator,
            precedence: precedence,
        }
    }

    pub fn is_unary_op(&self) -> bool {
        self.is_operator && self.args.len() == 1
    }

    pub fn is_binary_op(&self) -> bool {
        self.is_operator && self.args.len() == 2
    }

    /// caller must ensure `self.is_operator == true`
    pub fn get_op_name(&self) -> &str {
        if self.is_unary_op() {
            &self.name[5..]
        } else if self.is_binary_op() {
            &self.name[6..]
        } else {
            unreachable!()
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
            Some(Token::ClosingParenthesis) => {
                // eat `)`
                self.get_next_token();
                Ok(v)
            }
            ref x => unexpected!("`)`", x),
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
            self.get_next_token();
            loop {
                if self.current == Some(Token::ClosingParenthesis) {
                    // eat `)`
                    self.get_next_token();
                    break;
                }
                if self.current == Some(Token::Comma) {
                    self.get_next_token();
                }
                let arg = self.parse_expression()?;
                args.push(arg);
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
    ///   ::= ifexpr
    ///   ::= forexpr
    ///   ::= varexpr
    fn parse_primary(&mut self) -> Result<Box<Expr>, String> {
        match self.current {
            Some(Token::Ident(_)) => self.parse_identifier(),
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::OpeningParenthesis) => self.parse_paren(),
            Some(Token::If) => self.parse_if(),
            Some(Token::For) => self.parse_for(),
            Some(Token::Var) => self.parse_var(),
            _ => unexpected!("expression", self.current),
        }
    }

    /// unary
    ///   ::= primary
    ///   ::= '!' unary
    fn parse_unary(&mut self) -> Result<Box<Expr>, String> {
        if let Some(Token::Other(op)) = self.current {
            // If this is a unary operator, read it.
            self.get_next_token();
            let operand = self.parse_unary()?;
            Ok(Box::new(Expr::Unary {
                op: op,
                operand: operand,
            }))
        } else {
            // If the current token is not an operator, it must be a primary expr.
            self.parse_primary()
        }
    }

    /// binoprhs
    ///   ::= (op unary)*
    fn parse_bin_op_rhs(&mut self, prec: u8, mut lhs: Box<Expr>) -> Result<Box<Expr>, String> {
        loop {
            let op = match self.current {
                Some(Token::Other(op)) => op,
                _ => return Ok(lhs),
            };

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            let tok_prec = match self.operator_precedence(op) {
                tok_prec if tok_prec < prec => return Ok(lhs),
                tok_prec => tok_prec,
            };

            // eat op
            self.get_next_token();

            let mut rhs = self.parse_unary()?;

            // If BinOp binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            // Add 1 to tok_prec so `a + b * c + d` wille be parsed as:
            // `(a + (b * c)) +d`
            // Not:
            // `a + ((b * c) + d)`
            if let Some(Token::Other(op)) = self.current {
                match self.operator_precedence(op) {
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
        let lhs = self.parse_unary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    /// prototype
    ///   ::= id '(' id* ')'
    ///   ::= binary LETTER number? (id, id)
    ///   ::= unary LETTER (id)
    fn parse_prototype(&mut self) -> Result<Box<Prototype>, String> {
        // kind:
        // 0 = identifier, 1 = unary, 2 = binary.
        let (fn_name, kind, binary_precedence) = match self.current {
            Some(Token::Ident(ref id)) => {
                let fn_name = id.clone();
                self.get_next_token();
                (fn_name, 0, 30)
            }

            Some(Token::Binary) => {
                self.get_next_token();

                let op = match self.current {
                    Some(Token::Other(op)) => op,
                    _ => return unexpected!("operator name in prototype", self.current),
                };
                let fn_name = format!("binary{}", op);
                self.get_next_token();

                let binary_precedence = match self.current {
                    Some(Token::Number(n)) => {
                        let n = n as u8;
                        if n < 1 || n > 100 {
                            return unexpected!(
                                "precedence between 1..100 in operator prototype",
                                self.current
                            );
                        } else {
                            self.get_next_token();
                            n
                        }
                    }
                    _ => 30,
                };

                PRECEDENCE_TABLE
                    .lock()
                    .unwrap()
                    .insert(op, binary_precedence);

                (fn_name, 2, binary_precedence)
            }

            Some(Token::Unary) => {
                self.get_next_token();
                let fn_name = match self.current {
                    Some(Token::Other(op)) => format!("unary{}", op),
                    _ => return unexpected!("operator name in prototype", self.current),
                };
                self.get_next_token();
                (fn_name, 1, 30)
            }

            _ => return unexpected!("function name in prototype", self.current),
        };

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

                    if kind > 0 && arg_names.len() != kind {
                        return Err(format!(
                            "expected {} operands, got {}",
                            kind,
                            arg_names.len()
                        ));
                    } else {
                        return Ok(Box::new(Prototype::new(
                            fn_name,
                            arg_names,
                            kind > 0,
                            binary_precedence,
                        )));
                    }
                }
                _ => {
                    return unexpected!("expression or `)`", self.current);
                }
            }
        }
    }

    /// ifexpr ::= 'if' expression 'then' expression 'else' expression
    pub fn parse_if(&mut self) -> Result<Box<Expr>, String> {
        // eat `if`
        self.get_next_token();
        let cond = self.parse_expression()?;
        match self.current {
            Some(Token::Then) => {
                // eat `then`
                self.get_next_token();
                let consequence = self.parse_expression()?;
                match self.current {
                    Some(Token::Else) => {
                        self.get_next_token();
                        let alternative = self.parse_expression()?;
                        Ok(Box::new(Expr::If {
                            cond: cond,
                            consequence: consequence,
                            alternative: alternative,
                        }))
                    }
                    _ => unexpected!("`else`", self.current),
                }
            }
            _ => unexpected!("`then`", self.current),
        }
    }

    /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
    pub fn parse_for(&mut self) -> Result<Box<Expr>, String> {
        // eat `for`
        self.get_next_token();

        let id_name = match self.current {
            Some(Token::Ident(ref ident)) => ident.to_string(),
            _ => return unexpected!("identifier after for", self.current),
        };
        // eat `ident`
        self.get_next_token();

        match self.current {
            Some(Token::Other('=')) => (),
            _ => return unexpected!("`=` after for", self.current),
        }
        // eat `=`
        self.get_next_token();

        let start = self.parse_expression()?;

        match self.current {
            Some(Token::Comma) => (),
            _ => return unexpected!("`,` after for", self.current),
        }
        // eat `,`
        self.get_next_token();

        let end = self.parse_expression()?;

        let step = match self.current {
            Some(Token::Comma) => {
                // eat `,`
                self.get_next_token();
                Some(self.parse_expression()?)
            }
            _ => None,
        };

        match self.current {
            Some(Token::In) => (),
            _ => return unexpected!("`in` after for", self.current),
        }
        // eat `in`
        self.get_next_token();

        let body = self.parse_expression()?;

        Ok(Box::new(Expr::For {
            var_name: id_name,
            start: start,
            end: end,
            step: step,
            body: body,
        }))
    }

    /// varexpr ::= 'var' identifier ('=' expression)?
    ///                    (',' identifier ('=' expression)?)* 'in' expression
    pub fn parse_var(&mut self) -> Result<Box<Expr>, String> {
        // eat `var`
        self.get_next_token();

        let mut var_names = vec![];

        loop {
            let name = match self.current {
                Some(Token::Ident(ref name)) => name.clone(),
                _ => return unexpected!("identifier in `var` expression", self.current),
            };
            self.get_next_token(); // eat ident

            let val = match self.current {
                Some(Token::Other('=')) => {
                    self.get_next_token();
                    self.parse_expression()?
                }
                _ => Box::new(Expr::Number(0.0)),
            };

            var_names.push((name, val));

            match self.current {
                Some(Token::Comma) => self.get_next_token(),
                _ => break,
            }
        }

        match self.current {
            Some(Token::In) => self.get_next_token(),
            _ => return unexpected!("`in` keyword after `var`, got {:?}", self.current),
        }

        let body = self.parse_expression()?;

        Ok(Box::new(Expr::Var {
            var_names: var_names,
            body: body,
        }))
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
        let proto = Box::new(Prototype::new(
            ANONYMOUS_FUNCTION_NAME.to_string(),
            Vec::new(),
            false,
            0,
        ));
        let body = self.parse_expression()?;
        Ok(Box::new(Function::new(proto, body)))
    }

    pub fn current(&self) -> Option<Token> {
        self.current.clone()
    }

    fn operator_precedence(&self, op: char) -> u8 {
        *PRECEDENCE_TABLE
            .lock()
            .unwrap()
            .get(&op)
            .expect("cannot find operator precedence")
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
                op: '+',
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
            op: '-',
            lhs: Box::new(Expr::Binary {
                op: '+',
                lhs: Box::new(Expr::Number(1.0)),
                rhs: Box::new(Expr::Binary {
                    op: '*',
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
        let expected = Prototype::new(String::from("foo"), vec![], false, 0);
        assert_eq!(got, Box::new(expected));
        let mut parser = Parser::from_source("bar(a)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(String::from("bar"), vec![String::from("a")], false, 0);
        assert_eq!(got, Box::new(expected));
        let mut parser = Parser::from_source("bar(a b c)");
        let got = parser.parse_prototype().unwrap();
        let expected = Prototype::new(
            String::from("bar"),
            vec![String::from("a"), String::from("b"), String::from("c")],
            false,
            0,
        );
        assert_eq!(got, Box::new(expected));
    }

    #[test]
    fn test_function_definition_parsing() {
        let mut parser = Parser::from_source("def foo() 1 + 1");
        let got = parser.parse_definition().unwrap();
        let expected = Function::new(
            Box::new(Prototype::new(String::from("foo"), vec![]), false, 0),
            Box::new(
                Expr::Binary {
                    op: '+',
                    lhs: Box::new(Expr::Number(1.0)),
                    rhs: Box::new(Expr::Number(1.0)),
                },
                false,
            ),
        );
        assert_eq!(got, Box::new(expected));
    }

    #[test]
    fn test_extern_parsing() {
        let mut parser = Parser::from_source("extern sin(a)");
        let got = parser.parse_extern().unwrap();
        let expected = Prototype::new(String::from("sin"), vec![String::from("a")], false, 0);
        assert_eq!(got, Box::new(expected));
    }

}
