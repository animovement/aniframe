# The metadata fields that declare which columns carry which role

Writing any of these has consequences beyond the metadata list — at the
least the named columns must exist, and for the three structural roles
the frame is retyped, reordered and regrouped to match — so they are
reachable only through their own setters.

## Usage

``` r
declaration_metadata_fields()
```

## Value

Character vector of metadata field names.
