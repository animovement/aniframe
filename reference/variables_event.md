# Declare which columns carry per-frame event labels

`variables_event` names the `aniframe` columns holding per-frame
categorical event labels, split into two kinds:

- **state** columns are interval-valued — a run of identical values is
  one durative bout. List them coarse to fine when they nest.

- **point** columns are instantaneous — every non-`NA` frame is its own
  zero-length event.

[`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md)
reads the declaration to know what to encode, and the print header
surfaces it as "State event variables" / "Point event variables".

These functions declare the columns and check they exist, so the
metadata cannot promise a column the frame doesn't have.
[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
refuses the field for that reason.

- `set_variables_event()` replaces the side(s) you name and leaves the
  other alone, so `set_variables_event(data, state = "x")` swaps the
  state declaration without touching any point columns. Clear a side by
  naming it explicitly:
  `set_variables_event(data, point = character())`.

- `add_variables_event()` appends to the side(s) you name, leaving the
  other untouched.

- `remove_variables_event()` drops the named columns from whichever side
  they are on — a column can only be one kind, so it needs no `state` /
  `point` argument.

- `get_variables_event()` reads the declaration back as a named list.

Only an `aniframe` can carry this declaration: an `anievent` is already
the encoded form, with its events in `channel` and `label`.

## Usage

``` r
get_variables_event(data)

set_variables_event(data, state = NULL, point = NULL)

add_variables_event(data, state = NULL, point = NULL)

remove_variables_event(data, variables)
```

## Arguments

- data:

  An aniframe object.

- state, point:

  Character vectors of column names. `NULL` (the default) leaves that
  side of the declaration as it was.

- variables:

  Character vector of column names to undeclare.

## Value

For the setters, `data` with the declaration recorded. For
`get_variables_event()`, a named list with `state` and `point` entries.

## See also

[`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md),
which consumes the declaration;
[`set_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
and friends for the other variable roles.

## Examples

``` r
af <- aniframe(
  time = 1:5,
  x = 1:5,
  y = 1:5,
  behaviour = factor(c("REM", "REM", "wake", "wake", "wake")),
  call = factor(c(NA, "alarm", NA, NA, NA), levels = "alarm")
)

af <- set_variables_event(af, state = "behaviour", point = "call")
get_variables_event(af)
#> $state
#> [1] "behaviour"
#> 
#> $point
#> [1] "call"
#> 

# Naming one side leaves the other alone
get_variables_event(set_variables_event(af, state = "behaviour"))
#> $state
#> [1] "behaviour"
#> 
#> $point
#> [1] "call"
#> 

# Clearing a side is explicit
get_variables_event(set_variables_event(af, point = character()))
#> $state
#> [1] "behaviour"
#> 
#> $point
#> character(0)
#> 

# Declaring a column that isn't there is caught
try(add_variables_event(af, state = "grooming"))
#> Error in ensure_declared_cols_exist(data, c(declared$state, declared$point),  : 
#>   Event variable not found in data: "grooming".
#> ℹ Create the column first, then declare it.
```
