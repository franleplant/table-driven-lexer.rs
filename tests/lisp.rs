extern crate table_driven_lexer;

use std::default::Default;
use table_driven_lexer::{InitialState, EndState, ErrorState, Token, Lexer, Delta, Action};

#[derive(Debug, PartialEq, Clone)]
enum TokenCategory {
    //Specials
    Default,
    Error,
    // Posta
    ParOpen,
    ParClose,
    OpMat,
    OpRel,
    Str,
    Id,
    Number,
    Define,
    If,
    And,
    Not,
    Bool,
}

impl Default for TokenCategory {
    fn default() -> TokenCategory {
        TokenCategory::Default
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum State {
    Initial,
    End,
    Error,
    Id,
    TrailingWS,
    OpRelComp,
    Str,
    StrEnd,
    Number,
}

impl InitialState for State {
    fn initial_state() -> State {
        State::Initial
    }
}

impl EndState for State {
    fn end_state() -> State {
        State::End
    }
}

impl ErrorState for State {
    fn error_state() -> State {
        State::Error
    }
}


fn get_delta() -> Delta<TokenCategory, State> {
    use TokenCategory as Cat;
    use State::*;

    vec![
        (
            Initial,
            Box::new(|c| c.is_whitespace()),
            Initial,
            Box::new(action_null)
        ),
        (
            Initial,
            Box::new(|c| c == '('),
            End,
            build_action(Cat::ParOpen, true)
        ),
        (
            Initial,
            Box::new(|c| c == ')'),
            End,
            build_action(Cat::ParClose, true)
        ),
        (
            Initial,
            Box::new(|c| c == '+' || c == '-' || c == '*'),
            TrailingWS,
            build_action(Cat::OpMat, true)
        ),
        (
            Initial,
            Box::new(|c| c == '='),
            TrailingWS,
            build_action(Cat::OpRel, true)
        ),
        (
            Initial,
            Box::new(|c| c == '>' || c == '<'),
            OpRelComp,
            build_action(Cat::OpRel, true)
        ),
        (
            Initial,
            Box::new(|c| c == '"'),
            Str,
            build_action(Cat::Str, true)
        ),
        (
            Initial,
            Box::new(|c| c.is_alphabetic()),
            Id,
            build_action(Cat::Id, true)
        ),
        (
            Initial,
            Box::new(|c| c.is_numeric()),
            Number,
            build_action(Cat::Number, true)
        ),
        (
            Initial,
            Box::new(|_| true),
            Error,
            build_error_action("BAD INIT TOKEN")
        ),

        (
            TrailingWS,
            Box::new(|c| c.is_whitespace()),
            End,
            Box::new(action_null)
        ),
        (
            TrailingWS,
            Box::new(|_| true),
            Error,
            build_error_action("WHITESPACE EXPECTED")
        ),

        (
            OpRelComp,
            Box::new(|c| c == '='),
            TrailingWS,
            build_action(Cat::OpRel, false)
        ),
        (
            OpRelComp,
            Box::new(|c| c.is_whitespace()),
            TrailingWS,
            Box::new(action_lambda)
        ),
        (
            OpRelComp,
            Box::new(|_| true),
            Error,
            build_error_action("WHITESPACE OR = EXPECTED")
        ),

        (Id, Box::new(|c| c.is_alphabetic()), Id, Box::new(action_id)),
        (
            Id,
            Box::new(|c| c.is_whitespace()),
            TrailingWS,
            Box::new(action_id_try_reserved)
        ),
        (
            Id,
            Box::new(|c| c == ')'),
            End,
            Box::new(action_id_try_reserved)
        ),
        (Id, Box::new(|_| true), Error, build_error_action("BAD Id")),

        (
            Number,
            Box::new(|c| c.is_numeric()),
            Number,
            build_action(Cat::Number, false)
        ),
        (
            Number,
            Box::new(|c| c.is_whitespace()),
            TrailingWS,
            Box::new(action_lambda)
        ),
        (Number, Box::new(|c| c == ')'), End, Box::new(action_lambda)),
        (
            Number,
            Box::new(|_| true),
            Error,
            build_error_action("BAD Number")
        ),

        (
            Str,
            Box::new(|c| c == '"'),
            StrEnd,
            build_action(Cat::Str, false)
        ),
        (Str, Box::new(|_| true), Str, build_action(Cat::Str, false)),

        (
            StrEnd,
            Box::new(|c| c.is_whitespace()),
            TrailingWS,
            Box::new(action_lambda)
        ),
        (StrEnd, Box::new(|c| c == ')'), End, Box::new(action_lambda)),
        (
            StrEnd,
            Box::new(|_| true),
            Error,
            build_error_action("BAD Str")
        ),
    ]
}

fn get_category_for_id(s: &String) -> TokenCategory {
    match s.as_str() {
        "define" => TokenCategory::Define,
        "if" => TokenCategory::If,
        "and" => TokenCategory::And,
        "not" => TokenCategory::Not,
        "true" => TokenCategory::Bool,
        "false" => TokenCategory::Bool,
        _ => TokenCategory::Id,
    }
}

fn action_null(
    _: char,
    _: &mut usize,
    _: &mut usize,
    _: &mut String,
    _: &mut Token<TokenCategory>,
) {
}

fn action_lambda(
    _: char,
    index: &mut usize,
    _: &mut usize,
    _: &mut String,
    _: &mut Token<TokenCategory>,
) {
    *index -= 1;
}

fn build_action(category: TokenCategory, save_col: bool) -> Box<Action<TokenCategory>> {
    Box::new(move |c, index, line_offset, _, token| {
        token.lexeme.push(c);
        token.category = category.clone();
        if save_col {
            token.col_number = *index - *line_offset;
        }
    })
}

fn build_error_action(some_error: &'static str) -> Box<Action<TokenCategory>> {
    Box::new(move |c, _, _, error, token| {
        token.lexeme.push(c);
        token.category = TokenCategory::Error;
        *error = format!("ERROR: {}", some_error);
    })
}


fn action_id_try_reserved(
    _: char,
    index: &mut usize,
    _: &mut usize,
    _: &mut String,
    token: &mut Token<TokenCategory>,
) {
    token.category = get_category_for_id(&token.lexeme);
    *index -= 1;
}

fn action_id(
    c: char,
    _: &mut usize,
    _: &mut usize,
    _: &mut String,
    token: &mut Token<TokenCategory>,
) {
    token.lexeme.push(c);
    token.category = get_category_for_id(&token.lexeme);
}

#[test]
fn lex_ok() {
    use TokenCategory::*;

    let cases = vec![
        ("> 123", vec![(OpRel, ">", 1, 1), (Number, "123", 1, 3)]),
        (
            "(define (myfn x y)\n(+ 123\nx\ny))",
            vec![
                (ParOpen, "(", 1, 1),
                (Define, "define", 1, 2),
                (ParOpen, "(", 1, 9),
                (Id, "myfn", 1, 10),
                (Id, "x", 1, 15),
                (Id, "y", 1, 17),
                (ParClose, ")", 1, 18),
                (ParOpen, "(", 2, 1),
                (OpMat, "+", 2, 2),
                (Number, "123", 2, 4),
                (Id, "x", 3, 1),
                (Id, "y", 4, 1),
                (ParClose, ")", 4, 2),
                (ParClose, ")", 4, 3),
            ]
        ),
    ];

    for (src, expected) in cases {
        let tokens = Lexer::new(src.to_string(), get_delta()).lex();
        let case_msg = format!("\ntokens {:?},\nexpected {:?}", tokens, expected);
        assert_eq!(tokens.len(), expected.len(), "{:?}", case_msg);

        println!("++++++++++++++");
        println!("TEST CASE");
        println!("{}", src);
        println!("++++++++++++++");

        for i in 0..tokens.len() {
            let ref t = tokens[i];
            let (ref category, ref lexeme, line, col) = expected[i];
            let category = category;
            let lexeme = lexeme.to_string();

            println!(
                "{:<10} {:<10} {:<4} {:<4}",
                format!("{:?}", t.category),
                t.lexeme,
                t.line_number,
                t.col_number
            );

            let case_msg = format!("{:?}, expected {:?}", t, expected[i]);

            assert_eq!(t.category, *category, "category {:?}", case_msg);
            assert_eq!(t.lexeme, lexeme, "lexeme {:?}", case_msg);
            assert_eq!(t.line_number, line, "line_number {:?}", case_msg);
            assert_eq!(t.col_number, col, "col_number {:?}", case_msg);
        }
    }


}

#[test]
#[should_panic]
fn lex_fails() {

    let cases = vec!["abc123"];

    for src in cases {
        let _ = Lexer::new(src.to_string(), get_delta()).lex();
    }
}

#[test]
fn get_next_token() {
    use TokenCategory::*;

    let cases = vec![
        (Id, "hello"),
        (Number, "1234"),
        (OpRel, ">="),
        (Str, "\"hello 123\""),
    ];

    for (category, lexeme) in cases {
        let lexeme = lexeme.to_string();

        let case_msg = format!("category {:?} lexeme {:?}", category, lexeme);


        let maybe_token = Lexer::new(lexeme.clone(), get_delta()).get_next_token();
        let token = maybe_token.unwrap().unwrap();
        assert_eq!(token.category, category, "{:?}, {}", case_msg, 1);
        assert_eq!(token.lexeme, lexeme, "{:?}, {}", case_msg, 2);

        let maybe_token = Lexer::new(lexeme.clone() + " ", get_delta()).get_next_token();
        let token = maybe_token.unwrap().unwrap();
        assert_eq!(token.category, category, "{:?}, {}", case_msg, 3);
        assert_eq!(token.lexeme, lexeme, "{:?}, {}", case_msg, 4);
    }

}
