# Parse the output of a TreePPL MCMC run

`tp_parse_mcmc()` reads the JSON output of a TreePPL run with a Markov
chain Monte Carlo method (e.g. `"mcmc"`, `"mcmc-naive"`) and returns the
samples as a tidy data frame (tibble), with one row per iteration.

## Usage

``` r
tp_parse_mcmc(json_path, wide = TRUE)
```

## Arguments

- json_path:

  Path(s) to one or more JSON output files, as returned by
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md). Each
  file is one run.

- wide:

  Logical. If `TRUE` (default), one row per iteration and one column per
  parameter. If `FALSE`, one row per iteration and parameter, with
  columns `parameter` and `sample`.

## Value

A tibble (tidy data frame) with columns:

- `run`: run number, in the order of `json_path`.

- `iteration`: iteration number within its run.

- one column per parameter (`wide = TRUE`), or `parameter` and `sample`
  (`wide = FALSE`).

## Details

Both TreePPL output formats are read, and the format is detected
automatically:

- by default, the file holds one line with all samples
  (`{"samples": [...], "weights": [...], "normConst": ...}`);

- with incremental printing (`incremental_printing = TRUE` in
  [`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md)),
  each sample is on its own line, followed by an empty summary line,
  which is ignored.

TreePPL writes NaN and infinite values as `{"__float__": "nan"}`; these
are read as R's `NaN`, `Inf` and `-Inf`. MCMC weights are all equal, so
they are not returned.

Parameters are named after the fields of the model's return type when
the output has them (e.g. `lambda`, `mu`), and `x1`, `x2`, ...
otherwise. Non-scalar values (vectors, trees) are kept as list-columns.

Files without samples are skipped with a warning. If no file has
samples, the function stops with an error.

## See also

[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md), which
calls this function for MCMC runs, and
[`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
for SMC output.

## Examples

``` r
if (FALSE) { # \dontrun{
sampler <- tp_sampler(model = "crbd", method = "mcmc")
run <- tp_run(sampler, data, iterations = 1000, n_runs = 2)

# Parse output files directly
samples <- tp_parse_mcmc(c("out1.json", "out2.json"))
samples_long <- tp_parse_mcmc("out1.json", wide = FALSE)

# Incremental printing gives the same result
sampler <- tp_sampler(model = "crbd", method = "mcmc", incremental_printing = TRUE)
run <- tp_run(sampler, data, iterations = 1000)
} # }
```
