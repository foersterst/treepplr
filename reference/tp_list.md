# Collect TreePPL options in a list

`tp_list()` collects options given either as separate `name = value`
arguments or as a single list, and returns them as one named list. It is
how
[`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md)
and [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) read
their `...`, so both functions accept either form. Use it directly to
build a set of options once and reuse it.

## Usage

``` r
tp_list(...)
```

## Arguments

- ...:

  Either separate arguments (`name = value`), or a single list of such
  arguments.

## Value

A named list of options.

## Details

Option names are the `tpplc` flags without the leading `--`, as listed
by
[`tp_treeppl_options()`](http://treeppl.org/treepplr/reference/tp_treeppl_options.md).
Hyphens may be written as underscores (`subsample_size` for
`--subsample-size`). The value sets how the flag is passed to `tpplc`:

- `name = value` (a number or a string) gives `--name value`;

- `name = TRUE` gives `--name`, for flags without a value;

- `name = FALSE` or `name = NULL` leaves the flag out.

If a single list is supplied, it is returned as is. A list can't be
combined with other arguments: use `c(opts, seed = 42)` instead of
`opts, seed = 42`.

## Examples

``` r
# separate arguments
tp_list(particles = 1000, subsample = TRUE, subsample_size = 10)
#> $particles
#> [1] 1000
#> 
#> $subsample
#> [1] TRUE
#> 
#> $subsample_size
#> [1] 10
#> 

# a single list gives the same result
opts <- list(particles = 1000, subsample = TRUE, subsample_size = 10)
identical(tp_list(opts), tp_list(particles = 1000, subsample = TRUE, subsample_size = 10))
#> [1] TRUE

if (FALSE) { # \dontrun{
# reuse the same runtime options for several runs
run_opts <- tp_list(particles = 1000, sweeps = 2)
sampler <- tp_sampler(model = "crbd", method = "smc-bpf")
data_path <- tp_data(data_input = "crbd")
result_1 <- tp_run(sampler, data_path, run_opts)

# to add options, extend the list (a list can't be mixed with other arguments)
result_2 <- tp_run(sampler, data_path, c(run_opts, seed = 42))
} # }
```
