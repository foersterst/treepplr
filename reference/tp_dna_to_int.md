# Convert a DNA alignment into a TreePPL integer matrix

Recodes an alignment as integers, taxa in rows and sites in columns:
`A = 0`, `C = 1`, `G = 2`, `T = 3`. Every other symbol (gaps `-`, `?`,
`N` and IUPAC ambiguity codes) becomes `missing`. Pass the result to
[`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md)
after [`unname()`](https://rdrr.io/r/base/unname.html) to get an
`Int[][]`.

## Usage

``` r
tp_dna_to_int(aln, missing = 4L)
```

## Arguments

- aln:

  A DNA alignment of class `DNAbin`, as returned by
  [`ape::read.FASTA()`](https://rdrr.io/pkg/ape/man/read.dna.html) or
  [`ape::read.dna()`](https://rdrr.io/pkg/ape/man/read.dna.html). All
  sequences must have the same length.

- missing:

  Integer code used for any symbol other than A, C, G or T.

## Value

An integer matrix with taxon names as row names.

## Details

The missing-data code depends on the model: the tree-inference models
use `4L` (the default) and the phenotype-molecular model (`qt.tppl`)
uses `-1L`.

## Examples

``` r
if (FALSE) { # \dontrun{
aln <- ape::read.FASTA("alignment.fasta")
tp_dna_to_int(aln) # tree-inference models
tp_dna_to_int(aln, missing = -1L) # qt model
} # }
```
