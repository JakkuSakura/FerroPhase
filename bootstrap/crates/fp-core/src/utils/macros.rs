/// A macro to generate a common set of derives for a struct.
/// especially Clone, Debug, PartialEq, Eq, Hash
#[macro_export]
macro_rules! common_struct {
    (
        no_debug
        $(#[$attr:meta])*
        pub struct $name:ident { $($t:tt)* }
    ) => {
        #[derive(Clone, PartialEq, Hash)]
        $(#[$attr])*
        pub struct $name {
            $($t)*
        }
    };
    (

        $(#[$attr:meta])*
        pub struct $name:ident { $($t:tt)* }
    ) => {
        crate::common_struct!(
            no_debug
            $(#[$attr])*
            #[derive(Debug)]
            pub struct $name { $($t)* }
        );
    };

    (
        $(#[$attr:meta])*
        pub struct $name:ident;
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name;
    };
}
