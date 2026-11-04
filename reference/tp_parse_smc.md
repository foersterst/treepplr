# Parse the output of a TreePPL SMC run

`tp_parse_smc()` reads the JSON output of a TreePPL run with a
sequential Monte Carlo method (e.g. `"smc-bpf"`, `"smc-apf"`) and
returns the samples, their weights and the normalising constant of each
sweep as a tibble.

## Usage

``` r
tp_parse_smc(json_path, wide = TRUE)
```

## Arguments

- json_path:

  Path(s) to one or more JSON output files, as returned by
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md). Sweeps
  are numbered consecutively across files.

- wide:

  Logical. If `TRUE` (default), one row per particle and one column per
  parameter. If `FALSE`, one row per particle and parameter, with
  columns `parameter` and `sample`.

## Value

A tibble with columns:

- `sweep`: sweep number.

- `particle`: particle number within its sweep.

- `log_weight`: the particle's log weight.

- `norm_constant`: the sweep's log normalising constant.

- `norm_weight`: the normalised weight (see Details).

- one column per parameter (`wide = TRUE`), or `parameter` and `sample`
  (`wide = FALSE`).

## Details

Each line of an SMC output file is one sweep, with the particles'
`samples`, their log `weights` and the sweep's log normalising constant
(`normConst`). TreePPL writes NaN and infinite values as
`{"__float__": "-inf"}`; these are read as R's `NaN`, `Inf` and `-Inf`.

Sweeps are skipped when they have no samples (every particle had zero
weight) or a normalising constant that is NaN or `-Inf`; a message
reports how many. Particles with a non-finite log weight are dropped. If
no sweep has usable samples, the function stops with an error: try more
particles, or check the input data and priors.

The normalised weight of a particle is
`exp(log_weight + norm_constant - max(log_weight + norm_constant))`,
taken over all particles of all sweeps, so the heaviest particle gets
weight 1.

Parameters are named after the fields of the model's return type when
the output has them (e.g. `lambda`, `mu`), and `x1`, `x2`, ...
otherwise. Non-scalar values (vectors, trees) are kept as list-columns.

## See also

[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md), which
calls this function for SMC runs, and
[`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md)
for MCMC output.

## Examples

``` r
if (FALSE) { # \dontrun{
sampler <- tp_sampler(model = "crbd", method = "smc-bpf")
data <- tp_data(model = "crbd", dataList = list(tree = tree, rho = 0.5))
run <- tp_run(sampler, data, particles = 1000, sweeps = 10)

# Parse an output file directly
samples <- tp_parse_smc("out.json")
samples_long <- tp_parse_smc("out.json", wide = FALSE)
} # }
```
