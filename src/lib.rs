use std::fmt::Debug;
use std::default::Default;
use std::collections::HashSet;


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
    pub category: C,
    pub lexeme: String,
    pub line_number: usize,
    pub col_number: usize,
}


pub type Action<C> = Fn(char, &mut usize, &mut usize, &mut String, &mut Token<C>);
pub type Match = Fn(char) -> bool;
pub type Delta<C, S> = Vec<(S, Box<Match>, S, Box<Action<C>>)>;

pub struct Lexer<
    C: Debug + PartialEq + Clone + Default,
    S: Debug + PartialEq + Clone + InitialState + EndState + ErrorState,
> {
    delta: Delta<C, S>,
    start_index: usize,
    chars: Vec<char>,
    line_number: usize,
    newline_indices: HashSet<usize>,
    line_offset: usize,
}

impl<
    C: Debug + PartialEq + Clone + Default,
    S: Debug + PartialEq + Clone + InitialState + EndState + ErrorState,
> Lexer<C, S> {
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
                action(
                    c,
                    &mut index,
                    &mut self.line_offset,
                    &mut error_string,
                    &mut token,
                );
                state = to_state.clone();
                break;
            }

            if !found {
                return Some(Err(format!(
                    "Error: TOKEN NOT FOUND ({:?}) in {:?}, {}",
                    error_string,
                    index,
                    token.lexeme
                )));
            }
        }

        self.start_index = index;

        if state == ErrorState::error_state() {
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
mod tests {}
