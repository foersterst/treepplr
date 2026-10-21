#The goal of this file is to performing task at the loading of the package

######Version Change ##########
####Use to pull the tag of the last version of TreePPL release on the following function
#repo_info <- gh::gh("GET /repos/treeppl/treeppl/releases")
#version <- repo_info[[1]]$tag_name
##################

#' TreePPL version
#'
#' @description
#' TreePPL program version in use for this package version
#'@export
TPPLC_VERSION <- "0.4"

.onLoad <- function(libname, pkgname){
  tp_installing_treeppl(download =  FALSE)
}

# Cool loading message: works with both load_all() and library()
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    cli::format_inline(
      "\U0001F333 {.pkg treepplr} {utils::packageVersion(pkgname)} \U0001F333"
    )
  )
}
