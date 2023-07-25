use std::fmt;

pub fn display_fn(f: impl Fn(&mut fmt::Formatter<'_>) -> fmt::Result) -> impl fmt::Display {
    DisplayFn(f)
}

struct DisplayFn<F>(F);
impl<F> fmt::Display for DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(formatter)
    }
}
