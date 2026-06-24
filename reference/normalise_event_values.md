# Normalise an event-column vector to character labels

Logical → column name on TRUE, `NA` on FALSE. Factor / character →
character. Lets a single kernel handle both binary (logical) and
multi-level (factor / character) inputs.

## Usage

``` r
normalise_event_values(x, col_name)
```
