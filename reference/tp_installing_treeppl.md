# Platform-dependent TreePPL self-contained installation

`tp_installing_treeppl()` looks for the `tpplc` compiler associated with
the package and, if it is not found, downloads and unpacks it.

## Usage

``` r
tp_installing_treeppl(download = TRUE, keep_previous = FALSE)
```

## Arguments

- download:

  Logical. Download the associated `tpplc` version if it is not present
  locally.

- keep_previous:

  Logical. Keep previously downloaded `tpplc` versions instead of
  deleting them before the new download.

## Value

The path to the TreePPL compiler.

## Details

The search order is:

1.  the `TPPLC` environment variable;

2.  on Windows, a manual installation of `tpplc` (no self-contained
    build is available);

3.  an already unpacked copy in `/tmp`;

4.  a downloaded tarball in the `treeppl` directory next to the local
    treepplr installation (downloaded first if `download = TRUE`).
