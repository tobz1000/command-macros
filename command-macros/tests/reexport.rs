#![cfg_attr(feature = "nightly", feature(proc_macro_hygiene))]

#[cfg(feature = "nightly")] extern crate command_macros;

#[cfg(feature = "nightly")]
mod tests {
    #[test]
    fn reexport_via_use() {
        use command_macros::{command, command_args};
        let _ = command!(foo --bar);
        let _ = command_args!(foo --bar);
    }
}
