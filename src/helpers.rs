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

    /// Peek at the item two steps ahead, without advancing the iterator
    pub fn peek2(&mut self) -> Option<I> {
        let mut clone = self.iter.clone();
        clone.next();
        clone.peek().cloned()
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
    Byte(u8),
    Unicode(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLit {
    pub frags: Vec<StringFragment>
}

impl fmt::Display for StringLit {
    // CORRECTED a typo here: fmt.Formatter -> fmt::Formatter
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        let mut write_comma = |f: &mut fmt::Formatter<'_>| -> fmt::Result {
            if first {
                first = false;
                Ok(())
            } else {
                write!(f, ", ")
            }
        };

        for frag in &self.frags {
            match frag {
                StringFragment::String(s) if !s.is_empty() => {
                    write_comma(f)?;
                    // Use backticks for NASM strings, escaping any literal backticks.
                    let escaped_s = s.replace('`', "\\`");
                    write!(f, "`{}`", escaped_s)?;
                }
                StringFragment::Byte(b) => {
                    write_comma(f)?;
                    write!(f, "0x{:02X}", b)?;
                }
                StringFragment::Unicode(c) => {
                    if let Some(ch) = std::char::from_u32(*c) {
                        let mut buf = [0; 4];
                        for &byte in ch.encode_utf8(&mut buf).as_bytes() {
                            write_comma(f)?;
                            write!(f, "0x{:02X}", byte)?;
                        }
                    }
                }
                _ => {} // Ignore empty String fragments.
            }
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
                StringFragment::Byte(b) => if *b != 0 { res += 1 },
                StringFragment::Unicode(c) => if *c != 0 { res += std::char::from_u32(*c).map_or(0, |c| c.len_utf8()) },
            }
        }
        res
    }
}
