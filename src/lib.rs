
//TODO
//- Category is typed by an enum
//- Category is generic
//- Token has line and col numbers

type Token = (String, String);
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


type Action = Fn(char, &mut usize, &mut String, &mut String, &mut String);
type Match = Fn(char) -> bool;
type Delta = Vec<(&'static str, Box<Match>, &'static str, Box<Action>)>;

struct Lexer {
    delta: Delta,
}

impl Lexer {
    fn get_next_token(&self, src: String, start_index: usize) -> (bool, Option<Token>, usize) {
        let mut index = start_index;
        let mut state = "INITIAL";
        let mut error = false;
        let mut error_string = String::new();
        let mut lexeme = String::new();
        let mut category = String::new();

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
                action(c, &mut index, &mut error_string, &mut category, &mut lexeme);
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
        let mut token = None;
        if category != "" {
            token = Some((category, lexeme));
        }

        return (error, token, index)
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

fn actionNull(c: char, index: &mut usize, error: &mut String, category: &mut String, lexeme: &mut String) {
}

fn actionLambda(c: char, index: &mut usize, error: &mut String, category: &mut String, lexeme: &mut String) {
    *index -= 1
}

fn buildAction(a_category: &'static str) -> Box<Action> {
    Box::new(move |c: char, index: &mut usize, error: &mut String, category: &mut String, lexeme: &mut String | {
        lexeme.push(c);
        *category = a_category.to_string();
    })
}

fn buildErrorAction(error: &'static str) -> Box<Action> {
    Box::new(move |c: char, index: &mut usize, error: &mut String, category: &mut String, lexeme: &mut String | {
        lexeme.push(c);
        *category = "ERROR".to_string();
        *error = format!("ERROR: {}", error);
    })
}


fn actionIdTryReserved(c: char, index: &mut usize, error: &mut String, mut category: &mut String, lexeme: &mut String) {
    *category = get_category_for_id(lexeme.clone()).to_string();
    *index -= 1;
}

fn actionId(c: char, index: &mut usize, error: &mut String, mut category: &mut String, lexeme: &mut String) {
    lexeme.push(c);
    *category = get_category_for_id(lexeme.clone()).to_string();
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_next_token() {

        let delta: Delta = vec![
    ("INITIAL"            , Box::new(|c| c.is_whitespace())                     , "INITIAL"            , Box::new(actionNull)           ),
    ("INITIAL"            , Box::new(|c| c == '(')                        , "END"                , buildAction("PAROPEN")         ),
    ("INITIAL"            , Box::new(|c| c == ')')                        , "END"                , buildAction("PARCLOSE")        ),
    ("INITIAL"            , Box::new(|c| c == '+' || c == '-' || c == '*'), "TRAILING_WHITESPACE", buildAction("OPMAT")                 ),
    ("INITIAL"            , Box::new(|c| c == '=')                        , "TRAILING_WHITESPACE", buildAction("OPREL")                 ),
    ("INITIAL"            , Box::new(|c| c == '>' || c == '<')            , "OPREL_COMPOSITE"    , buildAction("OPREL")                 ),
    ("INITIAL"            , Box::new(|c| c == '"')                        , "STRING"             , buildAction("STRING")                ),
    ("INITIAL"            , Box::new(|c| c.is_alphabetic())                     , "ID"                 , buildAction("ID")                    ),
    ("INITIAL"            , Box::new(|c| c.is_numeric())                     , "NUMBER"             , buildAction("NUMBER")                ),
    ("INITIAL"            , Box::new(|c| true)                            , "ERROR"              , buildErrorAction("BAD INIT TOKEN")   ),

    ("TRAILING_WHITESPACE", Box::new(|c| c.is_whitespace())                     , "END"                , Box::new(actionNull)),
    ("TRAILING_WHITESPACE", Box::new(|c| true)                            , "END"                , buildErrorAction("WHITESPACE EXPECTED")),

    ("OPREL_COMPOSITE"    , Box::new(|c| c == '=')                       , "TRAILING_WHITESPACE", buildAction("OPREL")),
    ("OPREL_COMPOSITE"    , Box::new(|c| c.is_whitespace())              , "TRAILING_WHITESPACE", Box::new(actionLambda)),
    ("OPREL_COMPOSITE"    , Box::new(|c| true)                           , "ERROR"           , buildErrorAction("WHITESPACE OR = EXPECTED")),

    ("ID"                 , Box::new(|c| c.is_alphabetic())                     , "ID"                 , Box::new(actionId)                      ),
    ("ID"                 , Box::new(|c| c.is_whitespace())                     , "TRAILING_WHITESPACE", Box::new(actionIdTryReserved)           ),
    ("ID"                 , Box::new(|c| c == ')')                        , "END"                , Box::new(actionIdTryReserved)           ),
    ("ID"                 , Box::new(|c| true)                            , "ERROR"              , buildErrorAction("BAD ID")    ),

    ("NUMBER"             , Box::new(|c| c.is_numeric())                     , "NUMBER"             , buildAction("NUMBER")         ),
    ("NUMBER"             , Box::new(|c| c.is_whitespace())                     , "TRAILING_WHITESPACE", Box::new(actionLambda)                    ),
    ("NUMBER"             , Box::new(|c| c == ')')                        , "END"                , Box::new(actionLambda)                    ),
    ("NUMBER"             , Box::new(|c| true)                            , "ERROR"              , buildErrorAction("BAD NUMBER")),

    ("STRING"             , Box::new(|c| c == '"')                        , "STRING_END"         , buildAction("STRING")         ),
    ("STRING"             , Box::new(|c| true)                            , "STRING"             , buildAction("STRING")         ),

    ("STRING_END"         , Box::new(|c| c.is_whitespace())                     , "TRAILING_WHITESPACE", Box::new(actionLambda)                    ),
    ("STRING_END"         , Box::new(|c| c == ')')                        , "END"                , Box::new(actionLambda)                    ),
    ("STRING_END"         , Box::new(|c| true)                            , "ERROR"              , buildErrorAction("BAD STRING")),
        ];



        let l = Lexer {
            delta: delta,
        };



        let (_, token, _) = l.get_next_token("hello".to_string(), 0);
        assert_eq!(token.unwrap(), ("ID".to_string(), "hello".to_string()));

        let (_, token, _) = l.get_next_token("12345".to_string(), 0);
        assert_eq!(token.unwrap(), ("NUMBER".to_string(), "12345".to_string()));

        let (_, token, _) = l.get_next_token(">=".to_string(), 0);
        //println!("test");
        assert_eq!(token.unwrap(), ("OPREL".to_string(), ">=".to_string()));

        let (_, token, _) = l.get_next_token(">= ".to_string(), 0);
        assert_eq!(token.unwrap(), ("OPREL".to_string(), ">=".to_string()));

        let (_, token, _) = l.get_next_token("\"hello 123\"".to_string(), 0);
        assert_eq!(token.unwrap(), ("STRING".to_string(), "\"hello 123\"".to_string()));
    }


}
