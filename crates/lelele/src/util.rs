use std::fmt;

pub fn display_fn<F>(f: F) -> impl fmt::Display
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    struct DisplayFn<F> {
        f: F,
    }
    impl<F> fmt::Display for DisplayFn<F>
    where
        F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
    {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            (self.f)(formatter)
        }
    }
    DisplayFn { f }
}
