# A role the data already declares, when its columns are still there

Casting an object that is already an aniframe should not re-derive what
it has been told. It does fall back to detection when the declared
columns are gone, so a cast still repairs a frame whose metadata has
drifted rather than erroring on it.

## Usage

``` r
get_declared_if_present(data, field)
```

## Arguments

- data:

  Data frame, possibly carrying metadata.

- field:

  One of the `variables_*` metadata fields.

## Value

The declared column names, or `NULL` to detect instead.
