# Package index

## Installation

- [`tp_installing_treeppl()`](http://treeppl.org/treepplr/reference/tp_installing_treeppl.md)
  : Platform-dependent TreePPL self-contained installation
- [`tp_treeppl_options()`](http://treeppl.org/treepplr/reference/tp_treeppl_options.md)
  : TreePPL compiler options

## Models

- [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)
  : Models implemented in treepplr
- [`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md)
  : List the input arguments a TreePPL model expects
- [`tp_write_model()`](http://treeppl.org/treepplr/reference/tp_write_model.md)
  : Write a custom model to tp_tempdir()

## Preparing input

- [`tp_phylo_to_tpPhylo()`](http://treeppl.org/treepplr/reference/tp_phylo_to_tpPhylo.md)
  :

  Convert an [`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html)
  tree into a TreePPL tree

- [`tp_dna_to_int()`](http://treeppl.org/treepplr/reference/tp_dna_to_int.md)
  : Convert a DNA alignment into a TreePPL integer matrix

- [`tp_matrix_to_tppl()`](http://treeppl.org/treepplr/reference/tp_matrix_to_tppl.md)
  :

  Convert an R matrix into a TreePPL `Matrix[Real]`

- [`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md)
  : Write an R list to a TreePPL JSON input file

- [`tp_data()`](http://treeppl.org/treepplr/reference/tp_data.md) :
  Write the JSON input file for a TreePPL model

- [`tp_input_check()`](http://treeppl.org/treepplr/reference/tp_input_check.md)
  : Check a JSON input file against a TreePPL model

## Running TreePPL

- [`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md)
  : Build a TreePPL sampler for a given model and inference method
- [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) : Run a
  TreePPL sampler
- [`tp_list()`](http://treeppl.org/treepplr/reference/tp_list.md) :
  Collect TreePPL options in a list

## Reading output

- [`tp_parse_smc()`](http://treeppl.org/treepplr/reference/tp_parse_smc.md)
  : Parse the output of a TreePPL SMC run
- [`tp_parse_mcmc()`](http://treeppl.org/treepplr/reference/tp_parse_mcmc.md)
  : Parse the output of a TreePPL MCMC run
- [`tp_parse_host_rep()`](http://treeppl.org/treepplr/reference/tp_parse_host_rep.md)
  : Parse TreePPL json output for host repertoire model
- [`tp_map_tree()`](http://treeppl.org/treepplr/reference/tp_map_tree.md)
  : Find the Maximum A Posteriori (MAP) Tree from weighted samples

## Diagnostics

- [`tp_smc_convergence()`](http://treeppl.org/treepplr/reference/tp_smc_convergence.md)
  : Check for convergence across multiple SMC sweeps.
- [`tp_mcmc_convergence()`](http://treeppl.org/treepplr/reference/tp_mcmc_convergence.md)
  : Assess MCMC convergence for a TreePPL analysis

## Utilities

- [`tp_tempdir()`](http://treeppl.org/treepplr/reference/tp_tempdir.md)
  : Temporary directory for running TreePPL
- [`tp_open_dir()`](http://treeppl.org/treepplr/reference/tp_open_dir.md)
  : Open a directory in the system's file browser
- [`tp_open_tppl()`](http://treeppl.org/treepplr/reference/tp_open_tppl.md)
  : Open a file in the editor
