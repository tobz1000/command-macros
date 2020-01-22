# TODO

- documentation

## command vs command_args differences

- `(expr)` must be `Into<OsString>` rather than `AsRef<OsString>`
  - `expr` is moved instead of referenced
- `[expr]` must be `impl IntoIterator<Item=impl Info<OsStr>>` rather than `impl IntoIterator<Item=impl AsRef<OsStr>>,`
