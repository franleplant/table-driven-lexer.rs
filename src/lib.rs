use std::fmt::Debug;
use std::default::Default;
//TODO
//- Category is typed by an enum
//- Category is generic
//- Token has line and col numbers

#[derive(Debug, PartialEq, Clone)]
enum TokenCategory {
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

#[derive(Debug, PartialEq, Clone)]
struct Token<C: Debug + PartialEq + Clone + Default> {
    category: C,
    lexeme: String,
}


type Action<C> = Fn(char, &mut usize, &mut String, &mut Token<C>);
type Match = Fn(char) -> bool;
type Delta<C> = Vec<(&'static str, Box<Match>, &'static str, Box<Action<C>>)>;

struct Lexer<C: Debug + PartialEq + Clone + Default> {
    delta: Delta<C>,
}

impl<C: Debug + PartialEq + Clone + Default> Lexer<C> {
    fn get_next_token(&self, src: String, start_index: usize) -> (bool, Option<Token<C>>, usize) {
        let mut index = start_index;
        let mut state = "INITIAL";
        let mut error = false;
        let mut error_string = String::new();

        let mut token = Token { category: Default::default(), lexeme: String::new()};

        loop {
            if state == "END" || state == "ERROR" {
                break;
            }

            if index >= src.len() {
                break;
            }

            let chars: Vec<char> = src.chars().collect();
            let c = chars[index];
            let mut found = false;
            //println!("c {:?} ", c);

            //TODO naming sucks
            for &(ref d_state, ref is_match, ref next_state, ref action) in &self.delta {
                if state != *d_state {
                    continue
                }

                if !is_match(c) {
                    continue
                }

                found = true;
                index += 1;
                action(c, &mut index, &mut error_string, &mut token);
                state = next_state;
                break;
            }

            if !found {
                //TODO proper error reporting
                println!("ERROR");
                break;
            }
        }

        if error_string.len() != 0 {
            error = true;
        }

        //println!("error {:?} category {:?} lexeme {:?}", error, category, lexeme);
        //let mut maybe_token = None;
        //if token.category != "" {
            //maybe_token = Some(token);
        //}

        return (error, Some(token), index)
    }
}

fn get_category_for_id(s: String) -> &'static str {
    match s.as_str() {
        "define" => "DEFINE",
        "if" => "IF",
        "and" => "AND",
        _ => "ID"
    }
}

fn action_null(_: char, _: &mut usize, _: &mut String, _: &mut Token<String>) {
}

fn action_lambda(_: char, index: &mut usize, _: &mut String, _: &mut Token<String>) {
    *index -= 1
}

fn build_action(category: &'static str) -> Box<Action<String>> {
    Box::new(move |c: char, _: &mut usize, _: &mut String, token: &mut Token<String> | {
        token.lexeme.push(c);
        token.category = category.to_string();
    })
}

// TODO can I omit closure argument types?
fn build_error_action(some_error: &'static str) -> Box<Action<String>> {
    Box::new(move |c: char, _: &mut usize, error: &mut String, token: &mut Token<String> | {
        token.lexeme.push(c);
        token.category = "ERROR".to_string();
        *error = format!("ERROR: {}", some_error);
    })
}


fn action_id_try_reserved(_: char, index: &mut usize, _: &mut String, token: &mut Token<String>) {
    //TODO get_category_for_id should accept a reference to a String
    token.category = get_category_for_id(token.lexeme.clone()).to_string();
    *index -= 1;
}

fn action_id(c: char, _: &mut usize, _: &mut String, token: &mut Token<String>) {
    token.lexeme.push(c);
    token.category = get_category_for_id(token.lexeme.clone()).to_string();
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_next_token() {

        let delta: Delta<String> = vec![
    ("INITIAL"            , Box::new(|c| c.is_whitespace())                     , "INITIAL"            , Box::new(action_null)           ),
    ("INITIAL"            , Box::new(|c| c == '(')                        , "END"                , build_action("PAROPEN")         ),
    ("INITIAL"            , Box::new(|c| c == ')')                        , "END"                , build_action("PARCLOSE")        ),
    ("INITIAL"            , Box::new(|c| c == '+' || c == '-' || c == '*'), "TRAILING_WHITESPACE", build_action("OPMAT")                 ),
    ("INITIAL"            , Box::new(|c| c == '=')                        , "TRAILING_WHITESPACE", build_action("OPREL")                 ),
    ("INITIAL"            , Box::new(|c| c == '>' || c == '<')            , "OPREL_COMPOSITE"    , build_action("OPREL")                 ),
    ("INITIAL"            , Box::new(|c| c == '"')                        , "STRING"             , build_action("STRING")                ),
    ("INITIAL"            , Box::new(|c| c.is_alphabetic())                     , "ID"                 , build_action("ID")                    ),
    ("INITIAL"            , Box::new(|c| c.is_numeric())                     , "NUMBER"             , build_action("NUMBER")                ),
    ("INITIAL"            , Box::new(|_| true)                            , "ERROR"              , build_error_action("BAD INIT TOKEN")   ),

    ("TRAILING_WHITESPACE", Box::new(|c| c.is_whitespace())                     , "END"                , Box::new(action_null)),
    ("TRAILING_WHITESPACE", Box::new(|_| true)                            , "END"                , build_error_action("WHITESPACE EXPECTED")),

    ("OPREL_COMPOSITE"    , Box::new(|c| c == '=')                       , "TRAILING_WHITESPACE", build_action("OPREL")),
    ("OPREL_COMPOSITE"    , Box::new(|c| c.is_whitespace())              , "TRAILING_WHITESPACE", Box::new(action_lambda)),
    ("OPREL_COMPOSITE"    , Box::new(|_| true)                           , "ERROR"           , build_error_action("WHITESPACE OR = EXPECTED")),

    ("ID"                 , Box::new(|c| c.is_alphabetic())                     , "ID"                 , Box::new(action_id)),
    ("ID"                 , Box::new(|c| c.is_whitespace())                     , "TRAILING_WHITESPACE", Box::new(action_id_try_reserved)),
    ("ID"                 , Box::new(|c| c == ')')                        , "END"                , Box::new(action_id_try_reserved)           ),
    ("ID"                 , Box::new(|_| true)                            , "ERROR"              , build_error_action("BAD ID")    ),

    ("NUMBER"             , Box::new(|c| c.is_numeric())                     , "NUMBER"             , build_action("NUMBER")         ),
    ("NUMBER"             , Box::new(|c| c.is_whitespace())                     , "TRAILING_WHITESPACE", Box::new(action_lambda)),
    ("NUMBER"             , Box::new(|c| c == ')')                        , "END"                , Box::new(action_lambda)         ),
    ("NUMBER"             , Box::new(|_| true)                            , "ERROR"              , build_error_action("BAD NUMBER")),

    ("STRING"             , Box::new(|c| c == '"')                        , "STRING_END"         , build_action("STRING")         ),
    ("STRING"             , Box::new(|_| true)                            , "STRING"             , build_action("STRING")         ),

    ("STRING_END"         , Box::new(|c| c.is_whitespace())               , "TRAILING_WHITESPACE", Box::new(action_lambda)),
    ("STRING_END"         , Box::new(|c| c == ')')                        , "END"                , Box::new(action_lambda)        ),
    ("STRING_END"         , Box::new(|_| true)                            , "ERROR"              , build_error_action("BAD STRING")),
        ];



        let l = Lexer {
            delta: delta,
        };



        let (_, token, _) = l.get_next_token("hello".to_string(), 0);
        let token = token.unwrap();
        assert_eq!(token.category, "ID".to_string());
        assert_eq!(token.lexeme, "hello".to_string());

        let (_, token, _) = l.get_next_token("12345".to_string(), 0);
        let token = token.unwrap();
        assert_eq!(token.category, "NUMBER".to_string());
        assert_eq!(token.lexeme, "12345".to_string());

        //println!("test");
        let (_, token, _) = l.get_next_token(">=".to_string(), 0);
        let token = token.unwrap();
        assert_eq!(token.category, "OPREL".to_string());
        assert_eq!(token.lexeme, ">=".to_string());

        let (_, token, _) = l.get_next_token(">= ".to_string(), 0);
        let token = token.unwrap();
        assert_eq!(token.category, "OPREL".to_string());
        assert_eq!(token.lexeme, ">=".to_string());

        let (_, token, _) = l.get_next_token("\"hello 123\"".to_string(), 0);
        let token = token.unwrap();
        assert_eq!(token.category, "STRING".to_string());
        assert_eq!(token.lexeme, "\"hello 123\"".to_string());
    }


}
