# treepplr

## Overview

treepplr is the R Interface to [TreePPL](https://treeppl.org), a
probabilistic programming language for phylogenetics.

treepplr converts data to a format readable by TreePPL, reads the
TreePPL output, and connects to downstream analyses in model-specific
packages, such as the [evolnets](https://github.com/maribraga/evolnets)
package for the host repertoire evolution model.

See the package’s documentation [here](https://treeppl.org/treepplr).
There you will find all package functions under
[Reference](https://treeppl.org/treepplr/reference/index.html), and
basic examples of running various models under
[Articles](https://treeppl.org/treepplr/articles/index.html).

## Installation

You can install treepplr like so:

``` r

if(!require("pak", quietly = TRUE)) {
  install.packages("pak")
  library(pak)
} else {
 library(pak)
}

pak::pak("treeppl/treepplr")
```

This will only install the R package. The TreePPL compiler will not be
downloaded and installed until you run your first analysis, and the
TreePPL compiler is called. During the download, you will see a message
like this

    [xx%] Downloaded xxxxxx bytes...
    TreePPL initialisation ...please wait...
    TreePPL initialisation : Done

But you can force this download and installation

    treepplr::tp_installing_treeppl()

In subsequent analyses, the TreePPL compiler will be called directly,
skipping this step.
