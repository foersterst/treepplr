# Run a TreePPL sampler

`tp_run()` executes a compiled TreePPL sampler on the given data, saves
the raw JSON output to disk, prints a run summary and, when a parser is
available for the model/method combination, imports the output to the
current R session.

## Usage

``` r
tp_run(
  sampler,
  data,
  dir = NULL,
  out_file_name = "out",
  n_runs = 1,
  n_processes = 3,
  verbose = TRUE,
  ...
)
```

## Arguments

- sampler:

  A TreePPL sampler (class `tppls`), produced by
  [`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md).

- data:

  Path to the input data, produced by
  [`tp_data()`](http://treeppl.org/treepplr/reference/tp_data.md).

- dir:

  Path to the directory where the output is saved. If `NULL`, uses
  [`tp_tempdir()`](http://treeppl.org/treepplr/reference/tp_tempdir.md).

- out_file_name:

  Name of the output file, without extension. With more than one run,
  the run number is appended (e.g., `out1.json`, `out2.json`).

- n_runs:

  Number of independent runs of the sampler.

- n_processes:

  Number of parallel processes used when `n_runs > 1`. It is capped at
  `n_runs`.

- verbose:

  Logical. Print a run summary on completion?

- ...:

  Runtime options, as `name = value` pairs or a single list (see
  [`tp_list()`](http://treeppl.org/treepplr/reference/tp_list.md)). See
  `tp_treeppl_options("runtime")` for all supported arguments. Names are
  written without `--`, and underscores are converted to hyphens (e.g.,
  `subsample_size = 10` becomes `--subsample-size 10`). Compile options
  are not allowed here; pass them to
  [`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md)
  instead.

## Value

If a parser is available, a workable R object (usually a tidy data
frame, a list, or an object of class `phylo`, depending on the model).

## Details

Previous output files with the same `out_file_name` in `dir` are deleted
before running.

If the model belongs to a category without an available parser (e.g.
`"host-repertoire-evolution"`, `"tree-inference"`), or if the inference
method is neither SMC nor MCMC, no parsing is attempted: a message
points to the output directory and the paths of the raw output files are
returned.

## Examples

``` r
if (FALSE) { # \dontrun{
# SMC
sampler <- tp_sampler(model = "coin", method = "smc-bpf")
data_path <- tp_data(data_input = "coin")
result <- tp_run(sampler, data_path, particles = 2000, n_runs = 2)

# MCMC, with runtime options given as a list
sampler <- tp_sampler(model = "coin", method = "mcmc-naive")
result <- tp_run(sampler, data_path, tp_list(iterations = 2000))
} # }
```
