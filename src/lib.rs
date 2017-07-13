use std::fmt::Debug;
use std::default::Default;
//TODO
//- Token has line and col numbers
//- Internal State is generic and can be an Enum

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
pub struct Token<C: Debug + PartialEq + Clone + Default> {
    category: C,
    lexeme: String,
}


type Action<C> = Fn(char, &mut usize, &mut String, &mut Token<C>);
type Match = Fn(char) -> bool;
type Delta<C> = Vec<(&'static str, Box<Match>, &'static str, Box<Action<C>>)>;

pub struct Lexer<C: Debug + PartialEq + Clone + Default> {
    delta: Delta<C>,
    start_index: usize,
    chars: Vec<char>,
}

impl<C: Debug + PartialEq + Clone + Default> Lexer<C> {
    pub fn new(src: String, delta: Delta<C>) -> Lexer<C> {
        let src = src + " ";

        Lexer {
            delta: delta,
            start_index: 0,
            chars: src.chars().collect(),
        }
    }

    // Lex until completion
    pub fn lex(&mut self) -> Vec<Token<C>>{
        let mut tokens: Vec<Token<C>> = vec![];

        loop {
            if self.start_index >= self.chars.len() {
                break
            }

            let (error, maybe_token, last_index) = self.get_next_token();
            if error {
                panic!("ERROR: is error {:?}, token {:?}, last index {:?} in\n{:?}", error, maybe_token, last_index, tokens);
            }

            if let Some(token) = maybe_token {
                tokens.push(token);
                //# print (token["category"], token["lexeme"]);
            }
        }

        tokens
    }


    pub fn get_next_token(&mut self) -> (bool, Option<Token<C>>, usize) {
        let mut index = self.start_index;
        let mut state = "INITIAL";
        let mut error = false;
        let mut error_string = String::new();

        let mut token = Token {
            category: Default::default(),
            lexeme: String::new(),
        };

        loop {
            if state == "END" || state == "ERROR" {
                break;
            }

            if index >= self.chars.len() {
                break;
            }

            let c = self.chars[index];
            let mut found = false;
            //println!("c {:?} ", c);

            for &(ref from_state, ref is_match, ref to_state, ref action) in &self.delta {
                if state != *from_state {
                    continue;
                }

                if !is_match(c) {
                    continue;
                }

                found = true;
                index += 1;
                action(c, &mut index, &mut error_string, &mut token);
                state = to_state;
                break;
            }

            if !found {
                //TODO proper error reporting
                println!("ERROR");
                error = true;
                break;
            }
        }

        if error_string.len() != 0 {
            error = true;
        }

        let maybe_token = if token.category == Default::default() {
            None
        } else {
            Some(token)
        };

        //println!("error {:?} category {:?} lexeme {:?}", error, category, lexeme);

        self.start_index = index;

        return (error, maybe_token, index);
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex() {
        use TokenCategory::*;

        let cases = vec![
            ("> 123", vec![
                (OpRel, ">"),
                (Number, "123"),
            ]),
            ("(define (myfn x y)\n  (+ 123 x y))", vec![
                (ParOpen, "("),
                (Define, "define"),
                (ParOpen, "("),
                (Id, "myfn"),
                (Id, "x"),
                (Id, "y"),
                (ParClose, ")"),
                (ParOpen, "("),
                (OpMat, "+"),
                (Number, "123"),
                (Id, "x"),
                (Id, "y"),
                (ParClose, ")"),
                (ParClose, ")"),
            ]),
        ];

        for (src, expected) in cases {
            let tokens = Lexer::new(src.to_string(), get_delta()).lex();
            let case_msg = format!("tokens {:?}, expected {:?}", tokens, expected);
            assert_eq!(tokens.len(), expected.len(), "{:?}", case_msg);

            for i in 0..tokens.len() {
                let ref t = tokens[i];
                let (ref category, ref lexeme) = expected[i];
                let category = category;
                let lexeme = lexeme.to_string();


                assert_eq!(t.category, *category, "{:?}, {:?}", 1, case_msg);
                assert_eq!(t.lexeme, lexeme, "{:?}, {:?}", 2, case_msg);
            }
        }


    }

    #[test]
    #[should_panic]
    fn lex_fails() {

        let cases = vec![
            "abc123",
        ];

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
            (Str, "\"hello 123\"")
        ];

        for (category, lexeme) in cases {
            let lexeme = lexeme.to_string();

            let case_msg = format!("category {:?} lexeme {:?}", category, lexeme);


            let (_, token, _) = Lexer::new(lexeme.clone(), get_delta()).get_next_token();
            let token = token.unwrap();
            assert_eq!(token.category, category, "{:?}, {}", case_msg, 1);
            assert_eq!(token.lexeme, lexeme, "{:?}, {}", case_msg, 2);

            let (_, token, _) = Lexer::new(lexeme.clone() + " ", get_delta()).get_next_token();
            let token = token.unwrap();
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

    fn action_null(_: char, _: &mut usize, _: &mut String, _: &mut Token<TokenCategory>) {}

    fn action_lambda(_: char, index: &mut usize, _: &mut String, _: &mut Token<TokenCategory>) {
        *index -= 1
    }

    fn build_action(category: TokenCategory) -> Box<Action<TokenCategory>> {
        Box::new(move |c, _, _, token| {
                     token.lexeme.push(c);
                     token.category = category.clone();
                 })
    }

    fn build_error_action(some_error: &'static str) -> Box<Action<TokenCategory>> {
        Box::new(move |c, _, error, token| {
                     token.lexeme.push(c);
                     token.category = TokenCategory::Error;
                     *error = format!("ERROR: {}", some_error);
                 })
    }


    fn action_id_try_reserved(_: char,
                              index: &mut usize,
                              _: &mut String,
                              token: &mut Token<TokenCategory>) {
        token.category = get_category_for_id(&token.lexeme);
        *index -= 1;
    }

    fn action_id(c: char, _: &mut usize, _: &mut String, token: &mut Token<TokenCategory>) {
        token.lexeme.push(c);
        token.category = get_category_for_id(&token.lexeme);
    }


    fn get_delta() -> Delta<TokenCategory> {
        use TokenCategory::*;

        vec![("INITIAL", Box::new(|c| c.is_whitespace()), "INITIAL", Box::new(action_null)),
             ("INITIAL", Box::new(|c| c == '('), "END", build_action(ParOpen)),
             ("INITIAL", Box::new(|c| c == ')'), "END", build_action(ParClose)),
             ("INITIAL",
              Box::new(|c| c == '+' || c == '-' || c == '*'),
              "TRAILING_WHITESPACE",
              build_action(OpMat)),
             ("INITIAL", Box::new(|c| c == '='), "TRAILING_WHITESPACE", build_action(OpRel)),
             ("INITIAL",
              Box::new(|c| c == '>' || c == '<'),
              "OPREL_COMPOSITE",
              build_action(OpRel)),
             ("INITIAL", Box::new(|c| c == '"'), "STRING", build_action(Str)),
             ("INITIAL", Box::new(|c| c.is_alphabetic()), "ID", build_action(Id)),
             ("INITIAL", Box::new(|c| c.is_numeric()), "NUMBER", build_action(Number)),
             ("INITIAL", Box::new(|_| true), "ERROR", build_error_action("BAD INIT TOKEN")),

             ("TRAILING_WHITESPACE", Box::new(|c| c.is_whitespace()), "END", Box::new(action_null)),
             ("TRAILING_WHITESPACE",
              Box::new(|_| true),
              "END",
              build_error_action("WHITESPACE EXPECTED")),

             ("OPREL_COMPOSITE",
              Box::new(|c| c == '='),
              "TRAILING_WHITESPACE",
              build_action(OpRel)),
             ("OPREL_COMPOSITE",
              Box::new(|c| c.is_whitespace()),
              "TRAILING_WHITESPACE",
              Box::new(action_lambda)),
             ("OPREL_COMPOSITE",
              Box::new(|_| true),
              "ERROR",
              build_error_action("WHITESPACE OR = EXPECTED")),

             ("ID", Box::new(|c| c.is_alphabetic()), "ID", Box::new(action_id)),
             ("ID",
              Box::new(|c| c.is_whitespace()),
              "TRAILING_WHITESPACE",
              Box::new(action_id_try_reserved)),
             ("ID", Box::new(|c| c == ')'), "END", Box::new(action_id_try_reserved)),
             ("ID", Box::new(|_| true), "ERROR", build_error_action("BAD ID")),

             ("NUMBER", Box::new(|c| c.is_numeric()), "NUMBER", build_action(Number)),
             ("NUMBER",
              Box::new(|c| c.is_whitespace()),
              "TRAILING_WHITESPACE",
              Box::new(action_lambda)),
             ("NUMBER", Box::new(|c| c == ')'), "END", Box::new(action_lambda)),
             ("NUMBER", Box::new(|_| true), "ERROR", build_error_action("BAD NUMBER")),

             ("STRING", Box::new(|c| c == '"'), "STRING_END", build_action(Str)),
             ("STRING", Box::new(|_| true), "STRING", build_action(Str)),

             ("STRING_END",
              Box::new(|c| c.is_whitespace()),
              "TRAILING_WHITESPACE",
              Box::new(action_lambda)),
             ("STRING_END", Box::new(|c| c == ')'), "END", Box::new(action_lambda)),
             ("STRING_END", Box::new(|_| true), "ERROR", build_error_action("BAD STRING"))]
    }



}
