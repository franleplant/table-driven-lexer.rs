use std::fmt::Debug;
use std::default::Default;
use std::collections::HashSet;
//TODO
//- Move the whole lisp lexer spec into /test directory

#[derive(Debug, PartialEq, Clone)]
pub enum TokenCategory {
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
    INITIAL,
    END,
    ERROR,
    ID,
    TRAILING_WHITESPACE,
 OPREL_COMPOSITE,
 STRING,
 STRING_END,
 NUMBER,
}

impl InitialState for State {
    fn initial_state() -> State {
        State::INITIAL
    }
}

impl EndState for State {
    fn end_state() -> State {
        State::END
    }
}

impl ErrorState for State {
    fn error_state() -> State {
        State::ERROR
    }
}


pub trait InitialState {
    fn initial_state() -> Self;
}

pub trait EndState {
    fn end_state() -> Self;
}

pub trait ErrorState {
    fn error_state() -> Self;
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Token<C: Debug + PartialEq + Clone + Default> {
    category: C,
    lexeme: String,
    line_number: usize,
    col_number: usize,
}


type Action<C> = Fn(char, &mut usize, &mut usize, &mut String, &mut Token<C>);
type Match = Fn(char) -> bool;
type Delta<C, S> = Vec<(S, Box<Match>, S, Box<Action<C>>)>;

pub struct Lexer<C: Debug + PartialEq + Clone + Default, S: Debug + PartialEq + Clone + InitialState + EndState + ErrorState > {
    delta: Delta<C, S>,
    start_index: usize,
    chars: Vec<char>,
    line_number: usize,
    newline_indices: HashSet<usize>,
    line_offset: usize,
}

impl<C: Debug + PartialEq + Clone + Default, S: Debug + PartialEq + Clone + InitialState + EndState + ErrorState> Lexer<C, S> {
    pub fn new(src: String, delta: Delta<C, S>) -> Lexer<C, S> {
        let src = src + " ";

        Lexer {
            delta: delta,
            start_index: 0,
            chars: src.chars().collect(),
            line_number: 1,
            newline_indices: HashSet::new(),
            line_offset: 0,
        }
    }

    // Lex until completion
    pub fn lex(&mut self) -> Vec<Token<C>> {
        let mut tokens: Vec<Token<C>> = vec![];

        while let Some(token_result) = self.get_next_token() {
            match token_result {
                Ok(token) => tokens.push(token),
                Err(error) => panic!("ERROR {:?} in\n{:?}", error, tokens),
            }
        }

        tokens
    }


    pub fn get_next_token(&mut self) -> Option<Result<Token<C>, String>> {
        let mut index = self.start_index;
        let mut state: S = InitialState::initial_state();
        let mut error = false;
        let mut error_string = String::new();

        let mut token = Token {
            line_number: self.line_number,
            ..Default::default()
        };

        loop {
            if state == EndState::end_state() || state == ErrorState::error_state() {
                break;
            }

            if index >= self.chars.len() {
                break;
            }

            let c = self.chars[index];
            let mut found = false;
            //println!("c {:?} ", c);


            if c == '\n' && !self.newline_indices.contains(&index) {
                self.newline_indices.insert(index);
                self.line_number += 1;
                self.line_offset = index + 1;

                if state == InitialState::initial_state() {
                    token.line_number = self.line_number;
                }
            }

            for &(ref from_state, ref is_match, ref to_state, ref action) in &self.delta {
                if state != *from_state {
                    continue;
                }

                if !is_match(c) {
                    continue;
                }

                found = true;
                index += 1;
                action(c,
                       &mut index,
                       &mut self.line_offset,
                       &mut error_string,
                       &mut token);
                state = to_state.clone();
                break;
            }

            if !found {
                return Some(Err(format!("Error: TOKEN NOT FOUND ({:?}) in {:?}, {}",
                                        error_string,
                                        index,
                                        token.lexeme)));
            }
        }

        self.start_index = index;

        if error_string.len() != 0 {
            error = true;
        }

        if error {
            return Some(Err(format!("Error in {:?}, {}", index, token.lexeme)));
        }

        //println!("END");
        return if token.category == Default::default() {
                   None
               } else {
                   Some(Ok(token))
               };
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex() {
        use TokenCategory::*;

        let cases = vec![("> 123", vec![(OpRel, ">", 1, 1), (Number, "123", 1, 3)]),
                         ("(define (myfn x y)\n(+ 123\nx\ny))",
                          vec![(ParOpen, "(", 1, 1),
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
                               (ParClose, ")", 4, 3)])];

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

                println!("{:<10} {:<10} {:<4} {:<4}",
                         format!("{:?}", t.category),
                         t.lexeme,
                         t.line_number,
                         t.col_number);

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

        let cases = vec![(Id, "hello"),
                         (Number, "1234"),
                         (OpRel, ">="),
                         (Str, "\"hello 123\"")];

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

    fn action_null(_: char,
                   _: &mut usize,
                   _: &mut usize,
                   _: &mut String,
                   _: &mut Token<TokenCategory>) {
    }

    fn action_lambda(_: char,
                     index: &mut usize,
                     _: &mut usize,
                     _: &mut String,
                     _: &mut Token<TokenCategory>) {
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


    fn action_id_try_reserved(_: char,
                              index: &mut usize,
                              _: &mut usize,
                              _: &mut String,
                              token: &mut Token<TokenCategory>) {
        token.category = get_category_for_id(&token.lexeme);
        *index -= 1;
    }

    fn action_id(c: char,
                 _: &mut usize,
                 _: &mut usize,
                 _: &mut String,
                 token: &mut Token<TokenCategory>) {
        token.lexeme.push(c);
        token.category = get_category_for_id(&token.lexeme);
    }


    fn get_delta() -> Delta<TokenCategory, State> {
        use TokenCategory::*;
        use State::*;

        vec![(INITIAL, Box::new(|c| c.is_whitespace()), INITIAL, Box::new(action_null)),
             (INITIAL, Box::new(|c| c == '('), END, build_action(ParOpen, true)),
             (INITIAL, Box::new(|c| c == ')'), END, build_action(ParClose, true)),
             (INITIAL, Box::new(|c| c == '+' || c == '-' || c == '*'), TRAILING_WHITESPACE, build_action(OpMat, true)),
             (INITIAL, Box::new(|c| c == '='), TRAILING_WHITESPACE, build_action(OpRel, true)),
             (INITIAL, Box::new(|c| c == '>' || c == '<'), OPREL_COMPOSITE, build_action(OpRel, true)),
             (INITIAL, Box::new(|c| c == '"'), STRING, build_action(Str, true)),
             (INITIAL, Box::new(|c| c.is_alphabetic()), ID, build_action(Id, true)),
             (INITIAL, Box::new(|c| c.is_numeric()), NUMBER, build_action(Number, true)),
             (INITIAL, Box::new(|_| true), ERROR, build_error_action("BAD INIT TOKEN")),

             (TRAILING_WHITESPACE, Box::new(|c| c.is_whitespace()), END, Box::new(action_null)),
             (TRAILING_WHITESPACE, Box::new(|_| true), END, build_error_action("WHITESPACE EXPECTED")),

             (OPREL_COMPOSITE, Box::new(|c| c == '='), TRAILING_WHITESPACE, build_action(OpRel, false)),
             (OPREL_COMPOSITE, Box::new(|c| c.is_whitespace()), TRAILING_WHITESPACE, Box::new(action_lambda)),
             (OPREL_COMPOSITE, Box::new(|_| true), ERROR, build_error_action("WHITESPACE OR = EXPECTED")),

             (ID, Box::new(|c| c.is_alphabetic()), ID, Box::new(action_id)),
             (ID, Box::new(|c| c.is_whitespace()), TRAILING_WHITESPACE, Box::new(action_id_try_reserved)),
             (ID, Box::new(|c| c == ')'), END, Box::new(action_id_try_reserved)),
             (ID, Box::new(|_| true), ERROR, build_error_action("BAD ID")),

             (NUMBER, Box::new(|c| c.is_numeric()), NUMBER, build_action(Number, false)),
             (NUMBER, Box::new(|c| c.is_whitespace()), TRAILING_WHITESPACE, Box::new(action_lambda)),
             (NUMBER, Box::new(|c| c == ')'), END, Box::new(action_lambda)),
             (NUMBER, Box::new(|_| true), ERROR, build_error_action("BAD NUMBER")),

             (STRING, Box::new(|c| c == '"'), STRING_END, build_action(Str, false)),
             (STRING, Box::new(|_| true), STRING, build_action(Str, false)),

             (STRING_END, Box::new(|c| c.is_whitespace()), TRAILING_WHITESPACE, Box::new(action_lambda)),
             (STRING_END, Box::new(|c| c == ')'), END, Box::new(action_lambda)),
             (STRING_END, Box::new(|_| true), ERROR, build_error_action("BAD STRING"))]
    }
}
