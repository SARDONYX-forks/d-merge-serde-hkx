//! Compare two `Debug` representations of Havok classes token by token.
//!
//! # Why `Debug`?
//! All generated classes implement `Debug`, and it prints every field(including the fields that are not written to XML),
//! with floats in the shortest round-trip representation. So it can be used as a generic structural comparison.
use std::collections::HashMap;

/// Comparison options.
#[derive(Debug, Clone, Copy, Default)]
pub struct CmpOptions {
    /// Allow the error of XML float format(`{:.06}`).
    pub float_tolerance: bool,
    /// Ignore `w` of `Vector4` in `Matrix3`, `Rotation`, `QsTransform` and `Transform`.
    ///
    /// These are not written to XML by the specification of Havok XML. (e.g. `Matrix3` = `(x y z)(x y z)(x y z)`)
    pub ignore_xml_lost_w: bool,
}

#[derive(Debug, Clone, PartialEq)]
enum Token<'a> {
    Ident(&'a str),
    Str(&'a str),
    Num(&'a str),
    Punct(char),
}

fn tokenize(s: &str) -> Vec<Token<'_>> {
    let bytes = s.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        let next_is_num = bytes
            .get(i + 1)
            .is_some_and(|n| n.is_ascii_digit() || matches!(n, b'i' | b'N'));
        match c {
            b' ' | b'\n' | b'\r' | b'\t' => i += 1,
            b'"' => {
                let start = i + 1;
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                tokens.push(Token::Str(&s[start..i.min(bytes.len())]));
                i += 1;
            }
            b'0'..=b'9' | b'-' | b'+' if c.is_ascii_digit() || next_is_num => {
                let start = i;
                i += 1;
                while i < bytes.len() {
                    let n = bytes[i];
                    let is_exp_sign =
                        matches!(n, b'-' | b'+') && matches!(bytes[i - 1], b'e' | b'E');
                    if n.is_ascii_alphanumeric() || n == b'.' || n == b'_' || is_exp_sign {
                        i += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Num(&s[start..i]));
            }
            c if c.is_ascii_alphabetic() || c == b'_' => {
                let start = i;
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                    i += 1;
                }
                let ident = &s[start..i];
                if matches!(ident, "NaN" | "inf") {
                    tokens.push(Token::Num(ident));
                } else {
                    tokens.push(Token::Ident(ident));
                }
            }
            _ => {
                // NOTE: multi bytes chars are only in strings.
                let ch = s[i..].chars().next().unwrap_or('?');
                tokens.push(Token::Punct(ch));
                i += ch.len_utf8();
            }
        }
    }
    tokens
}

fn parse_float(s: &str) -> Option<f64> {
    match s {
        "NaN" | "-NaN" => Some(f64::NAN),
        "inf" | "+inf" => Some(f64::INFINITY),
        "-inf" => Some(f64::NEG_INFINITY),
        _ => s.parse().ok(),
    }
}

/// Is the difference within the XML float error(`{:.06}` -> parse as f32)?
fn float_eq(a: &str, b: &str) -> bool {
    let is_float = |s: &str| s.contains(['.', 'e', 'E', 'N', 'i']);
    if !is_float(a) || !is_float(b) {
        return false; // integer must be exact.
    }
    let (Some(a), Some(b)) = (parse_float(a), parse_float(b)) else {
        return false;
    };
    if a.is_nan() && b.is_nan() {
        return true;
    }
    #[allow(clippy::float_cmp)]
    if a == b {
        return true;
    }
    // `{:.06}`: abs error <= 0.5e-6, then f32 rounding: rel error <= 2^-24
    let tol = 2.0_f64.mul_add(a.abs().max(b.abs()) * f64::from(f32::EPSILON), 5.1e-7);
    (a - b).abs() <= tol
}

/// Mismatch info.
#[derive(Debug, Clone)]
pub struct Mismatch {
    /// e.g. `hkaSkeleton.m_referencePose[3].transition.x`
    pub path: String,
    pub left: String,
    pub right: String,
}

impl core::fmt::Display for Mismatch {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}:\n  - {}\n  + {}", self.path, self.left, self.right)
    }
}

type Frame = (String, String, Option<usize>); // (struct name, field name, seq index)

fn path_string(stack: &[Frame]) -> String {
    let mut p = String::new();
    for (name, field, index) in stack {
        if !name.is_empty() && p.is_empty() {
            p.push_str(name);
        }
        if let Some(index) = index {
            p.push_str(&format!("[{index}]"));
        }
        if !field.is_empty() {
            p.push('.');
            p.push_str(field);
        }
    }
    p
}

/// `w` of `Vector4` in `Matrix3`/`Rotation`/`QsTransform`/`Transform`
fn is_xml_lost_w(stack: &[Frame]) -> bool {
    let n = stack.len();
    if n < 2 {
        return false;
    }
    let (vec_name, field, _) = &stack[n - 1];
    let (parent, _, _) = &stack[n - 2];
    vec_name == "Vector4"
        && field == "w"
        && matches!(
            parent.as_str(),
            "Matrix3" | "Rotation" | "QsTransform" | "Transform"
        )
}

fn show(tokens: &[Token], i: usize) -> String {
    tokens[i..tokens.len().min(i + 6)]
        .iter()
        .map(|t| match t {
            Token::Ident(s) | Token::Num(s) => (*s).to_string(),
            Token::Str(s) => format!("{s:?}"),
            Token::Punct(c) => c.to_string(),
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Compare two `Debug` strings.
///
/// - `rename`: Pointer names(strings) of `left` are renamed before comparison.
///   (Pointer names are renumbered in binary deserialization.)
///
/// # Errors
/// Returns the first mismatch.
pub fn compare(
    left: &str,
    right: &str,
    opt: CmpOptions,
    rename: Option<&HashMap<String, String>>,
) -> Result<(), Mismatch> {
    let l = tokenize(left);
    let r = tokenize(right);

    let mut stack: Vec<Frame> = vec![Frame::default()];
    for (li, (lt, rt)) in l.iter().zip(&r).enumerate() {
        // update path
        match lt {
            Token::Ident(name) if l.get(li + 1) == Some(&Token::Punct('{')) => {
                stack.push(((*name).to_string(), String::new(), None));
            }
            Token::Ident(field) if l.get(li + 1) == Some(&Token::Punct(':')) => {
                if let Some(top) = stack.last_mut() {
                    top.1 = (*field).to_string();
                }
            }
            Token::Punct('[') => stack.push((String::new(), String::new(), Some(0))),
            Token::Punct(',') => {
                if let Some((_, _, Some(index))) = stack.last_mut() {
                    *index += 1;
                }
            }
            Token::Punct('}' | ']') => {
                stack.pop();
                if stack.is_empty() {
                    stack.push(Frame::default());
                }
            }
            _ => {}
        }

        let same = match (lt, rt) {
            (Token::Str(a), Token::Str(b)) => {
                let a = rename.and_then(|m| m.get(*a)).map_or(*a, String::as_str);
                a == *b
            }
            (Token::Num(a), Token::Num(b)) => {
                a == b
                    || (opt.float_tolerance && float_eq(a, b))
                    || (opt.ignore_xml_lost_w && is_xml_lost_w(&stack))
            }
            _ => lt == rt,
        };
        if !same {
            return Err(Mismatch {
                path: path_string(&stack),
                left: show(&l, li),
                right: show(&r, li),
            });
        }
    }

    if l.len() != r.len() {
        return Err(Mismatch {
            path: path_string(&stack),
            left: format!("token len {}", l.len()),
            right: format!("token len {}", r.len()),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_tolerance() {
        let opt = CmpOptions {
            float_tolerance: true,
            ..Default::default()
        };
        assert!(compare("A { x: 0.12345679 }", "A { x: 0.123457 }", opt, None).is_ok());
        assert!(compare("A { x: 0.1234 }", "A { x: 0.1235 }", opt, None).is_err());
        assert!(compare("A { x: NaN }", "A { x: NaN }", opt, None).is_ok());
        assert!(compare("A { x: -inf }", "A { x: -inf }", opt, None).is_ok());
        assert!(compare("A { x: 1e-45 }", "A { x: 0.0 }", opt, None).is_ok());
        assert!(compare("A { x: 3.4028235e38 }", "A { x: 3.4028235e38 }", opt, None).is_ok());
        // integer must be exact
        assert!(compare("A { x: Number(1) }", "A { x: Number(2) }", opt, None).is_err());
    }

    #[test]
    fn lost_w() {
        let opt = CmpOptions {
            ignore_xml_lost_w: true,
            ..Default::default()
        };
        let a = "QsTransform { transition: Vector4 { x: 1.0, y: 2.0, z: 3.0, w: 5.0 }, quaternion: Quaternion { x: 0.0, y: 0.0, z: 0.0, scaler: 1.0 } }";
        let b = "QsTransform { transition: Vector4 { x: 1.0, y: 2.0, z: 3.0, w: 0.0 }, quaternion: Quaternion { x: 0.0, y: 0.0, z: 0.0, scaler: 1.0 } }";
        assert!(compare(a, b, opt, None).is_ok());

        // `Vector4` alone must keep `w`.
        let err = compare(
            "Vector4 { x: 1.0, w: 5.0 }",
            "Vector4 { x: 1.0, w: 0.0 }",
            opt,
            None,
        )
        .unwrap_err();
        assert_eq!(err.path, "Vector4.w");
    }

    #[test]
    fn path_and_rename() {
        let a = r##"hkbFoo { m_bar: [Pointer("#0001"), Pointer("#0002")] }"##;
        let b = r##"hkbFoo { m_bar: [Pointer("#0010"), Pointer("#0099")] }"##;
        let rename = HashMap::from([
            ("#0001".to_string(), "#0010".to_string()),
            ("#0002".to_string(), "#0020".to_string()),
        ]);
        let err = compare(a, b, CmpOptions::default(), Some(&rename)).unwrap_err();
        assert_eq!(err.path, "hkbFoo.m_bar[1]");
    }
}
