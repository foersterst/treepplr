# Assess MCMC convergence for a TreePPL analysis

Computes per-parameter effective sample size (ESS) and, when multiple
runs are available, the upper limit of the Gelman-Rubin potential scale
reduction factor (R-hat), using the coda package.

## Usage

``` r
tp_mcmc_convergence(treeppl_out)
```

## Arguments

- treeppl_out:

  A tibble produced by
  [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md),
  in either long or wide format.

## Value

A tibble with one row per parameter, containing:

- parameter:

  Parameter name.

- ess:

  Effective sample size, pooled across all runs.

- rhat_upper:

  Upper limit of the Gelman-Rubin R-hat statistic. Only computed when
  `treeppl_out` contains more than one run; otherwise `NA`, with a
  message explaining why.

## Details

Output produced from an unnamed return type in TreePPL (`.tppl` file) is
not supported and will raise an error, since there is no reliable way to
distinguish multiple parameters.

## Examples

``` r
if (FALSE) { # \dontrun{

# CRBD model using MCMC
run_mcmc <- tp_run(
sampler = tp_compile(model = "crbd", method = "mcmc"),
data = tp_data(data_input = "crbd"),
iterations = 10,
n_runs = 2,
n_processes = 2
)

# tp_run() already returns the output of tp_parse_mcmc(), so we can call
# tp_mcmc_convergence() directly:
tp_mcmc_convergence(run_mcmc)
} # }
```
