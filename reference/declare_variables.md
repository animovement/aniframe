# Declare one variable role and restructure the frame to match

The shared kernel behind the `set_` / `add_` / `remove_` functions.
Reads the other two roles from the metadata so the frame is always
restructured against a complete, consistent declaration.

## Usage

``` r
declare_variables(data, role, variables, strict = TRUE)
```

## Arguments

- data:

  An aniframe or anievent object.

- role:

  One of `"what"`, `"when"`, `"where"`.

- variables:

  Character vector of column names to declare.

## Value

`data`, restructured and re-declared.
