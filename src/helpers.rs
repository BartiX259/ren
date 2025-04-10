use std::iter::Peekable;
use std::vec::IntoIter;
use std::fmt;

/// A peekable iterator over I that stores its current index
pub struct VecIter<I> {
    iter: Peekable<IntoIter<I>>,
    index: usize,
}

impl<I> VecIter<I>
where
    I: Clone,
{
    /// Create a new VecIter from a Vec<I>
    pub fn new(vec: Vec<I>) -> Self {
        Self { iter: vec.into_iter().peekable(), index: 0 }
    }

    /// Peek at the next item without advancing the iterator
    pub fn peek(&mut self) -> Option<&I> {
        self.iter.peek()
    }

    /// Get the current index
    pub fn current_index(&self) -> usize {
        self.index
    }

    /// Get the current index - 1
    pub fn prev_index(&self) -> usize {
        if self.index <= 0 {
            return 0;
        } else {
            return self.index - 1;
        }
    }

    /// Advance the iterator and return the next item
    pub fn next(&mut self) -> Option<I> {
        let item = self.iter.next();
        if item.is_some() {
            self.index += 1;
        }
        item
    }
}

/// A buffer that automatically indents itself
pub struct IndentedBuf {
    buf: String,
    indent: usize,
    indent_width: usize,
    is_new_line: bool,
    comment_pos: usize,
    pub last_line: String
}

impl IndentedBuf {
    pub fn new(indent_width: usize, comment_pos: usize) -> Self {
        Self {
            buf: String::new(),
            indent: 0,
            indent_width,
            comment_pos,
            is_new_line: true,
            last_line: String::new()
        }
    }

    pub fn push_line<T: AsRef<str>>(&mut self, line: T) {
        self.push(&line);
        self.buf.push('\n');
        self.is_new_line = true;
    }

    pub fn push<T: AsRef<str>>(&mut self, str: T) {
        let line = str.as_ref(); // Convert to &str
        if self.is_new_line {
            self.last_line.clear();
            self.buf.push_str(&" ".repeat(self.indent * self.indent_width));
        }
        self.buf.push_str(line);
        self.is_new_line = false;
        self.last_line.push_str(line);
    }

    pub fn comment<T: AsRef<str>>(&mut self, str: T) {
        self.buf.pop();
        let len = self.last_line.len();
        if len < self.comment_pos {
            self.buf.push_str(&" ".repeat(self.comment_pos - len));
        }
        self.buf.push_str(format!("; {}\n", str.as_ref()).as_str());
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    pub fn get_output(self) -> String {
        self.buf
    }
}

/// String literal

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringFragment {
    String(String),
    Char(u32)
}

// impl StringFragment {
//     pub fn char_to_string(i: u32) -> String {
//         match i {
//             10 => "\\n",
//             _ => panic!("Unknown character")
//         }.to_string()
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLit {
    pub frags: Vec<StringFragment>
}

impl fmt::Display for StringLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut start = true;
        for frag in self.frags.iter() {
            if !start {
                write!(f, ", ")?;
            } else {
                start = false;
            }
            match frag {
                StringFragment::String(s) => write!(f, "\"{}\"", s),
                StringFragment::Char(i) => write!(f, "{}", i),
            }?
        }
        Ok(())
    }
}

impl StringLit {
    pub fn len(&self) -> usize {
        let mut res = 0;
        for frag in self.frags.iter() {
            match frag {
                StringFragment::String(s) => res += s.chars().count(),
                StringFragment::Char(_) => res += 1,
            }
        }
        res
    }
    // pub fn to_string(self) -> String {
    //     let mut res = String::new();
    //     for frag in self.frags.iter() {
    //         match frag {
    //             StringFragment::String(s) => res += s,
    //             StringFragment::Char(i) => res += StringFragment::char_to_string(*i).as_str(),
    //         }
    //     }
    //     res
    // }
}
