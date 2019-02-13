pub struct Debug<T>(pub T);

impl<T> std::fmt::Debug for Debug<T> {
    default fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "unknown value of type {}",
                std::intrinsics::type_name::<T>()
            )
        }
    }
}

impl<T> std::fmt::Debug for Debug<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
