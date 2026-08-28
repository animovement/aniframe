# Ensure the object can carry an event declaration

`variables_event` names per-frame columns, which only an aniframe has.
An anievent already *is* the encoded form — its events live in `channel`
and `label` — so
[`to_anievent()`](https://animovement.dev/anicore/reference/to_anievent.md)
drops the field rather than inheriting it.

## Usage

``` r
ensure_can_declare_events(data)
```

## Arguments

- data:

  Object to test.

## Value

`TRUE`, invisibly.
