# TODO

- documentation
- errors in `command_args!([x])` highlight the whole macro rather than `[x]`
  - error bubbles up to procedural macro source code?

e.g. - for `command!(foo [0..1])` (correct spanning):

```
error[E0277]: the trait bound `{integer}: std::convert::AsRef<std::ffi::OsStr>` is not satisfied
 --> src/main.rs:8:19
  |
8 |     command!(foo [0..1]);
  |                   ^^^^ the trait `std::convert::AsRef<std::ffi::OsStr>` is not implemented for `{integer}`
  |
```

for `command_args!(foo [0..1])` (incorrect spanning):

```
error[E0277]: the trait bound `std::ffi::OsString: std::convert::From<{integer}>` is not satisfied
 --> src/main.rs:8:23
  |
8 |     command_args!(foo [0..1]);
  |                       ^^^^^^ the trait `std::convert::From<{integer}>` is not implemented for `std::ffi::OsString`
  |
  = help: the following implementations were found:
            <std::ffi::OsString as std::convert::From<&T>>
            <std::ffi::OsString as std::convert::From<std::borrow::Cow<'a, std::ffi::OsStr>>>
            <std::ffi::OsString as std::convert::From<std::boxed::Box<std::ffi::OsStr>>>
            <std::ffi::OsString as std::convert::From<std::path::PathBuf>>
            <std::ffi::OsString as std::convert::From<std::string::String>>
  = note: required because of the requirements on the impl of `std::convert::Into<std::ffi::OsString>` for `{integer}`
  = note: required by `std::convert::Into::into`

error[E0277]: the trait bound `std::ffi::OsString: std::convert::From<{integer}>` is not satisfied
  --> /home/toby/sources/command-macros/command-macros-plugin/src/lib.rs:79:1
   |
79 | / pub fn command_args(input: TokenStream) -> TokenStream {
80 | |     try_generate(input, CommandArgsGenerator::new())
81 | | }
   | |_^ the trait `std::convert::From<{integer}>` is not implemented for `std::ffi::OsString`
   | 
  ::: src/main.rs:8:5
   |
8  |       command_args!(foo [0..1]);
   |       -------------------------- in this macro invocation
   |
   = help: the following implementations were found:
             <std::ffi::OsString as std::convert::From<&T>>
             <std::ffi::OsString as std::convert::From<std::borrow::Cow<'a, std::ffi::OsStr>>>
             <std::ffi::OsString as std::convert::From<std::boxed::Box<std::ffi::OsStr>>>
             <std::ffi::OsString as std::convert::From<std::path::PathBuf>>
             <std::ffi::OsString as std::convert::From<std::string::String>>
   = note: required because of the requirements on the impl of `std::convert::Into<std::ffi::OsString>` for `{integer}`
   = note: required by `std::convert::Into::into`

error: aborting due to 2 previous errors
```

## command vs command_args differences
- `(expr)` must be `Into<OsString>` rather than `AsRef<OsString>`
    - `expr` is moved instead of referenced
- `[expr]` must be `impl IntoIterator<Item=impl Info<OsStr>>` rather than `impl IntoIterator<Item=impl AsRef<OsStr>>,`
