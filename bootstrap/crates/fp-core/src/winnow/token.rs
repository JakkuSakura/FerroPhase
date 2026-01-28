use crate::winnow::error::{ContextError, ErrMode};
use crate::winnow::ModalResult;
use std::ops::RangeBounds;

pub fn literal<'a>(lit: &'static str) -> impl Fn(&mut &'a str) -> ModalResult<&'a str> {
    move |input: &mut &'a str| {
        if input.starts_with(lit) {
            let (head, tail) = input.split_at(lit.len());
            *input = tail;
            Ok(head)
        } else {
            Err(ErrMode::Backtrack(ContextError::new()))
        }
    }
}

pub fn take_while<'a, F, R>(
    range: R,
    mut pred: F,
) -> impl FnMut(&mut &'a str) -> ModalResult<&'a str>
where
    F: FnMut(char) -> bool,
    R: RangeBounds<usize> + Clone,
{
    move |input: &mut &'a str| {
        let (min, max) = bounds(range.clone());
        let mut count = 0usize;
        let mut idx = 0usize;
        for (offset, ch) in input.char_indices() {
            if !pred(ch) {
                break;
            }
            count += 1;
            idx = offset + ch.len_utf8();
            if let Some(max) = max {
                if count >= max {
                    break;
                }
            }
        }
        if count < min {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        let (head, tail) = input.split_at(idx);
        *input = tail;
        Ok(head)
    }
}

pub fn take_till<'a, F, R>(
    range: R,
    mut pred: F,
) -> impl FnMut(&mut &'a str) -> ModalResult<&'a str>
where
    F: FnMut(char) -> bool,
    R: RangeBounds<usize> + Clone,
{
    move |input: &mut &'a str| {
        let (min, max) = bounds(range.clone());
        let mut count = 0usize;
        let mut idx = 0usize;
        for (offset, ch) in input.char_indices() {
            if pred(ch) {
                break;
            }
            count += 1;
            idx = offset + ch.len_utf8();
            if let Some(max) = max {
                if count >= max {
                    break;
                }
            }
        }
        if count < min {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        let (head, tail) = input.split_at(idx);
        *input = tail;
        Ok(head)
    }
}

pub fn take_until<'a, R>(range: R, needle: &'static str) -> impl Fn(&mut &'a str) -> ModalResult<&'a str>
where
    R: RangeBounds<usize> + Clone,
{
    move |input: &mut &'a str| {
        let (min, max) = bounds(range.clone());
        if let Some(pos) = input.find(needle) {
            let slice = &input[..pos];
            let count = slice.chars().count();
            if count < min {
                return Err(ErrMode::Backtrack(ContextError::new()));
            }
            if let Some(max) = max {
                if count > max {
                    return Err(ErrMode::Backtrack(ContextError::new()));
                }
            }
            *input = &input[pos..];
            Ok(slice)
        } else {
            Err(ErrMode::Backtrack(ContextError::new()))
        }
    }
}

fn bounds(range: impl RangeBounds<usize>) -> (usize, Option<usize>) {
    let min = match range.start_bound() {
        std::ops::Bound::Included(v) => *v,
        std::ops::Bound::Excluded(v) => v.saturating_add(1),
        std::ops::Bound::Unbounded => 0,
    };
    let max = match range.end_bound() {
        std::ops::Bound::Included(v) => Some(*v),
        std::ops::Bound::Excluded(v) => Some(v.saturating_sub(1)),
        std::ops::Bound::Unbounded => None,
    };
    (min, max)
}
