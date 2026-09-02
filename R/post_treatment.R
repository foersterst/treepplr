#' Parse TreePPL SMC output into a tidy data frame
#'
#' Converts a JSON file from an SMC analysis produced by TreePPL into a
#' single tidy tibble of particles, their samples, and normalized weights.
#' The function internally removes sweeps with an undefined normalizing constant.
#'
#' @param json_path The full path to the (SMC) JSON file produced by TreePPL.
#' @param wide Logical. If \code{TRUE} (default), return the data frame in wide format,
#' with one column per parameter. If \code{FALSE}, return the data frame in long format,
#' with parameter names and values stored in \code{parameter}
#' and \code{sample} columns.
#'
#' @return A tibble with one row per particle, containing:
#'   \describe{
#'     \item{sweep}{Sweep index.}
#'     \item{parameter}{Parameter name, if present in the input JSON.}
#'     \item{sample}{Sampled value.}
#'     \item{log_weight}{Log weight of the particle.}
#'     \item{norm_constant}{Log normalizing constant for the sweep.}
#'     \item{norm_weight}{Normalized weight, rescaled so the maximum
#'       total log weight across all particles is 1.}
#'   }
#'
#' @examples
#' \dontrun{
#' # fit a CRBD model
#' run_smc <- tp_run(
#'   data = tp_data(data_input = "crbd"),
#'   sampler = tp_compile(
#'     model = "crbd",
#'     method = "smc-apf",
#'     sweeps = 2,
#'     particles = 10
#'   )
#' )
#'
#' # get the path to the output JSON file:
#' out_file <- list.files(
#'   path = tp_tempdir(),
#'   pattern = "out",
#'   full.names = TRUE
#' )
#'
#' # parse JSON to a tidy data frame
#' tp_parse_smc(json_path = out_file)
#' }
#'
#' @export
tp_parse_smc <- function(json_path, wide = TRUE) {
  # read in the JSON file(s)
  treeppl_out <- readr::read_lines(json_path)
  treeppl_out <- lapply(treeppl_out, jsonlite::fromJSON, simplifyVector = FALSE)

  # parse sweeps
  parse_sweep <- function(sweep, sweep_id) {
    # remove sweeps with nan norm const
    if (identical(sweep$normConst, "nan")) {
      message("Removing sweep without normalizing constant (sweep ", sweep_id, ")")
      return(NULL)
    }

    # sanity check to detect mismatches in the number of samples & weights
    if (length(sweep$samples) != length(sweep$weights)) {
      stop(
        "Sweep ", sweep_id, ": samples (n=", length(sweep$samples),
        ") and weights (n=", length(sweep$weights), ") have different lengths."
      )
    }

    norm_const <- sweep$normConst

    # convert weight lists to numeric vectors, considering that some weights in the list
    # may not be numeric, e.g., {"__float__": "-inf"}
    log_weights <- purrr::map_dbl(sweep$weights, function(w) {
      if (is.list(w) && !is.null(w[["__float__"]])) {
        # use R convention for "nan" and "inf" (i.e., NaN, Inf, -Inf)
        val <- tolower(as.character(w[["__float__"]]))
        dplyr::case_when(
          val == "-inf" ~ -Inf,
          val == "inf" ~ Inf,
          val == "nan" ~ NaN,
          TRUE ~ as.numeric(val)
        )
      } else {
        as.numeric(w)
      }
    })

    samples <- sweep$samples

    has_parameter_names <- is.list(samples[[1]]) && !is.null(samples[[1]][["__data__"]])

    # if the json has parameter names
    if (has_parameter_names) {
      samples_df <- purrr::imap_dfr(samples, function(s, particle_id) {
        param_values <- s[["__data__"]]
        tibble::tibble(
          particle = particle_id,
          parameter = names(param_values),
          sample = as.numeric(unlist(param_values))
        )
      })
    } else {
      samples_df <- purrr::imap_dfr(samples, function(s, particle_id) {
        tibble::tibble(
          particle = particle_id,
          sample = as.numeric(unlist(s))
        )
      })
    }

    # here we create indexes for the particles so that we can use them to
    # match weights with samples using left_join()
    weights_df <- tibble::tibble(
      particle = seq_along(log_weights),
      log_weight = log_weights
    )

    # left_join() ensures the correct alignment of samples with their weights
    samples_df |>
      dplyr::left_join(weights_df, by = "particle") |>
      dplyr::mutate(
        sweep = sweep_id,
        norm_constant = norm_const
      ) |>
      dplyr::select(-"particle")
  }

  # parse sweeps one at time and rbind the results
  result_df <- purrr::imap_dfr(treeppl_out, parse_sweep)

  if (nrow(result_df) == 0) {
    stop("All sweeps failed")
  }

  # calculate the normalized weight and return
  result_df <- result_df |>
    dplyr::filter(!is.infinite(.data$log_weight)) |>
    dplyr::mutate(
      total_lweight = .data$log_weight + .data$norm_constant,
      norm_weight = exp(.data$total_lweight - max(.data$total_lweight))
    ) |>
    dplyr::select(-"total_lweight") |>
    dplyr::relocate("sweep")


  if (!wide) {
  return(result_df)
  } else if (wide && !"parameter" %in% colnames(result_df)) {
    message("Parameter names could not be found; returning data frame in long format.")
    return(result_df)
  } else if ("parameter" %in% colnames(result_df) && wide) {
    result_df <- result_df |>
      dplyr::group_by(sweep, parameter) |>
      dplyr::mutate(particle = dplyr::row_number()) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        id_cols = c(sweep, particle, log_weight, norm_constant, norm_weight),
        names_from = parameter,
        values_from = sample
      )
    return(result_df)
  }
}


#' Parse TreePPL MCMC output into a tidy data frame
#'
#' Converts JSON file(s) produced by an MCMC analysis in TreePPL into a
#' single tidy tibble of samples, one row per iteration.
#'
#' @param json_path The full path to the (MCMC) JSON file(s) produced by TreePPL.
#' @param wide Logical. If \code{TRUE} (default), return the data frame in wide format,
#' with one column per parameter. If \code{FALSE}, return the data frame in long format,
#' with parameter names and values stored in \code{parameter}
#' and \code{sample} columns.
#'
#' @return A tibble with one row per iteration, containing:
#'   \describe{
#'     \item{run}{Run index.}
#'     \item{parameter}{Parameter name, if present in the input JSON.}
#'     \item{sample}{Sampled value.}
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # Let's use a quick CRBD model as example
#' run_mcmc <- tp_run(
#' sampler = tp_compile(model = "crbd", method = "mcmc", iterations = 10),
#' data = tp_data(data_input = "crbd"),
#' n_runs = 2, # this will produce two JSON files as output
#' n_processes = 2
#' )
#'
#' # get the path to the output JSON file; note that the number of JSON
#' # files produced is equal to n_runs specified above
#' out_file <- list.files(
#'   path = tp_tempdir(),
#'   pattern = "out",
#'   full.names = TRUE
#' )
#'
#' # parse JSON to a tidy data frame
#' tp_parse_mcmc(json_path = out_file)
#' }
#'
#' @export
tp_parse_mcmc <- function(json_path, wide = TRUE) {
  # read in the JSON file(s)
  treeppl_out <- readr::read_lines(json_path)
  treeppl_out <- lapply(treeppl_out, jsonlite::fromJSON, simplifyVector = FALSE)

  # parse mcmc runs
  parse_run <- function(run, run_id) {
    samples <- run$samples
    has_parameter_names <- is.list(samples[[1]]) && !is.null(samples[[1]][["__data__"]])

    if (has_parameter_names) {
      purrr::imap_dfr(samples, function(s, iteration_id) {
        param_values <- s[["__data__"]]
        tibble::tibble(
          run = run_id,
          iteration = iteration_id,
          parameter = names(param_values),
          sample = as.numeric(unlist(param_values))
        )
      })
    } else {
      purrr::imap_dfr(samples, function(s, iteration_id) {
        tibble::tibble(
          run = run_id,
          iteration = iteration_id,
          sample = as.numeric(unlist(s))
        )
      })
    }
  }

  result_df <- purrr::imap_dfr(treeppl_out, parse_run)

  if (nrow(result_df) == 0) {
    stop("All runs failed")
  }

  # long or wide
  if (!wide) {
    return(result_df)
  } else if (wide && !"parameter" %in% colnames(result_df)) {
    message("Parameter names could not be found; returning data frame in long format.")
    return(result_df)
  } else if ("parameter" %in% colnames(result_df) && wide) {
    result_df <- result_df |>
      tidyr::pivot_wider(
        id_cols = c(run, iteration),
        names_from = parameter,
        values_from = sample
      )
    return(result_df)
  }
}


#' Parse TreePPL json output for host repertoire model
#'
#' @description
#' `tp_parse_host_rep` takes TreePPL json output from inference with the
#' model of host repertoire evolution and returns a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run].
#'
#' @return A list (n = sweeps) of data frames with the output from inference
#' in TreePPL under the host repertoire evolution model.
#' @export

tp_parse_host_rep <- function(treeppl_out) {
  result_list <- list()

  for (index in seq_along(treeppl_out)) {
    output_trppl <- treeppl_out[[index]]

    nbr_lam <- length(output_trppl[1][[1]][[1]][[1]]$lambda)
    nbr_col <- 14 + nbr_lam
    name_lam <- c()

    for (i in 1:nbr_lam) {
      name_lam <- c(name_lam, paste0("lambda", i))
    }

    result <- data.frame(matrix(ncol = nbr_col, nrow = 0))

    colnames(result) <- c(
      "iteration",
      "log_weight",
      "log_norm_const",
      "mu",
      "beta",
      name_lam,
      "node_index",
      "branch_start_time",
      "branch_end_time",
      "start_state",
      "end_state",
      "transition_time",
      "parent_index",
      "child1_index",
      "child2_index"
    )

    # for (i in seq_along(output_trppl[1][[1]])) {
    for (i in seq_along(output_trppl$samples)) {
      res <- data.frame(matrix(ncol = nbr_col, nrow = 0))
      colnames(res) <- c(
        "iteration",
        "log_weight",
        "log_norm_const",
        "mu",
        "beta",
        name_lam,
        "node_index",
        "branch_start_time",
        "branch_end_time",
        "start_state",
        "end_state",
        "transition_time",
        "parent_index",
        "child1_index",
        "child2_index"
      )

      tree <- output_trppl[1][[1]][[i]][[1]]$tree$`__data__`

      state <- paste(tree$repertoire, collapse = "")

      lambda <- output_trppl[1][[1]][[i]][[1]]$lambda

      names(lambda) <- name_lam

      res <- peel_tree(
        tree,
        i,
        pindex = NA,
        output_trppl$weights[i],
        output_trppl$normConst,
        output_trppl[1][[1]][[i]][[1]]$mu,
        output_trppl[1][[1]][[i]][[1]]$beta,
        lambda,
        prev_age = NA,
        state,
        res
      )
      result <- rbind(result, res)
    }
    result_list[[index]] <- result
  }
  return(result_list)
}

# Recursive function to go deep in the tree
peel_tree <- function(subtree,
                      index,
                      pindex,
                      lweight,
                      lnorm_const,
                      mu,
                      beta,
                      lambda,
                      prev_age,
                      start_state,
                      result) {
  base <- c(
    iteration = as.numeric(index - 1),
    log_weight = as.numeric(lweight),
    log_norm_const = as.numeric(lnorm_const),
    mu = as.numeric(mu),
    beta = as.numeric(beta),
    lambda,
    node_index = as.numeric(subtree$label - 1),
    branch_start_time = as.numeric(prev_age),
    branch_end_time = as.numeric(subtree$age),
    start_state = as.numeric(start_state),
    end_state = NA,
    transition_time = NA,
    parent_index = as.numeric(pindex),
    child1_index = NA,
    child2_index = NA
  )

  if (!is.null(subtree$left)) {
    base[["child1_index"]] <-
      as.numeric(subtree$left$`__data__`$label - 1)
    base[["child2_index"]] <-
      as.numeric(subtree$right$`__data__`$label - 1)
  }

  base[["end_state"]] <- base[["start_state"]]

  chang_nbr <- length(subtree$history)
  if (chang_nbr != 0) {
    df <- data.frame(matrix(ncol = 2, nrow = chang_nbr))
    for (i in 1:chang_nbr) {
      # "end_state"
      df[i, 1] <-
        as.numeric(paste(subtree$history[[i]]$`__data__`$repertoire,
          collapse = ""
        ))
      # "transition_time"
      df[i, 2] <- as.numeric(subtree$history[[i]]$`__data__`$age)
    }
    df <- df[order(-df$X2), ]
    for (j in 1:chang_nbr) {
      base[["start_state"]] <- base[["end_state"]]
      base[["end_state"]] <- df[j, 1]
      base[["transition_time"]] <- df[j, 2]
      result[nrow(result) + 1, ] <- base
    }
  } else {
    result[nrow(result) + 1, ] <- base
  }

  if (!is.null(subtree$left)) {
    result <- peel_tree(
      subtree$left$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
    result <- peel_tree(
      subtree$right$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
  }
  result
}


#' Check for convergence across multiple SMC sweeps.
#'
#' @param treeppl_out a data frame outputted by [tp_parse_smc()].
#'
#' @returns Variance in the normalizing constants across SMC sweeps.
#' @export
#'
tp_smc_convergence <- function(treeppl_out) {
  zs <- treeppl_out |>
    dplyr::slice_head(n = 1, by = .data$sweep) |>
    dplyr::pull(.data$norm_constant)

  return(stats::var(zs))
}


#' Assess MCMC convergence for a TreePPL analysis
#'
#' Computes per-parameter effective sample size (ESS) and, when multiple
#' runs are available, the upper limit of the Gelman-Rubin potential scale
#' reduction factor (R-hat), using the \pkg{coda} package.
#'
#' @param treeppl_out A tibble produced by \code{tp_parse_mcmc()}, in either long
#' or wide format.
#'
#' @details
#' Output produced from an unnamed return type in TreePPL (`.tppl` file) is not
#' supported and will raise an error, since there is no reliable way to
#' distinguish multiple parameters.
#'
#' @return A tibble with one row per parameter, containing:
#'   \describe{
#'     \item{parameter}{Parameter name.}
#'     \item{ess}{Effective sample size, pooled across all runs.}
#'     \item{rhat_upper}{Upper limit of the Gelman-Rubin R-hat statistic.
#'       Only computed when `treeppl_out` contains more than one run;
#'       otherwise `NA`, with a message explaining why.}
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # CRBD model using MCMC
#' run_mcmc <- tp_run(
#' sampler = tp_compile(model = "crbd", method = "mcmc", iterations = 10),
#' data = tp_data(data_input = "crbd"),
#' n_runs = 2,
#' n_processes = 2
#' )
#'
#' # tp_run() already returns the output of tp_parse_mcmc(), so we can call
#' # tp_mcmc_convergence() directly:
#' tp_mcmc_convergence(run_mcmc)
#' }
#'
#' @export
tp_mcmc_convergence <- function(treeppl_out) {
  # if the input is in wide format
  if (!"sample" %in% colnames(treeppl_out)) {
    treeppl_out <- treeppl_out |>
      tidyr::pivot_longer(
        cols = -c(run, iteration),
        names_to = "parameter",
        values_to = "sample"
      )
  }

  # sanity check
  has_parameter <- "parameter" %in% names(treeppl_out)
  if (!has_parameter) {
    stop(
      "Output JSON has no parameter names.\n",
      "Re-run the TreePPL model with a named return type (.tppl file) ",
      "so that parameters can be identified."
    )
  }

  # coda::mcmc objects
  runs <- sort(unique(treeppl_out$run))
  parameters <- unique(treeppl_out$parameter)

  chains_by_parameter <- purrr::map(parameters, function(p) {
    chains <- purrr::map(runs, function(r) {
      vals <- treeppl_out |>
        dplyr::filter(.data$run == r, .data$parameter == p) |>
        dplyr::arrange(.data$iteration) |>
        dplyr::pull(.data$sample)
      coda::mcmc(vals)
    })
    coda::mcmc.list(chains)
  })
  names(chains_by_parameter) <- parameters
  # ESS
  ess <- purrr::map_dbl(chains_by_parameter, coda::effectiveSize)
  result <- tibble::tibble(parameter = names(ess), ess = ess)

  # Gelman and Rubin's R-hat
  if (length(runs) > 1) {
    rhat_upper <- purrr::map_dbl(chains_by_parameter, function(chain_list) {
      coda::gelman.diag(chain_list)$psrf[, "Upper C.I."]
    })
    result$rhat_upper <- rhat_upper[result$parameter]
  } else {
    result$rhat_upper <- NA
    message(
      "Only one run detected; Gelman-Rubin R-hat requires >= 2 runs and was not computed."
    )
  }

  if (!has_parameter) {
    result <- dplyr::select(result, -"parameter")
  }
  return(result)
}


#' Find the Maximum A Posteriori (MAP) Tree from weighted samples
#'
#' @param trees_out The list returned by [treepplr::tp_json_to_phylo]
#' (containing $trees and $weights)
#'
#' @returns The MAP tree as a phylo object
#' @export
#'
tp_map_tree <- function(trees_out) {
  trees <- trees_out$trees
  weights <- trees_out$weights

  # Handle Log-Weights (Optional Check)
  # If weights are negative (log-scale), convert them to probabilities first
  if (any(weights < 0)) {
    message("Log-weights detected. converting to relative probabilities...")
    # Subtract max to avoid underflow/overflow issues
    weights <- exp(weights - max(weights))
  }

  # Identify unique topologies
  trees_ready <- lapply(trees, function(tree) {
    # normalize edge lengths for the tip reordering
    tree$edge.length <- tree$edge.length / max(tree$edge.length)
    # Ladderize to fix edge indices
    tree_lad <- ladderize_tree(tree)
    # Order tip labels as similarly as possible
    tree_ord <- bnpsd::tree_reorder(tree_lad, sort(tree_lad$tip.label))
    # Remove edge lengths to only focus on topology
    tree_ord$edge.length <- NULL
    return(tree_ord)
  })

  # This compresses the list into unique tree topologies
  unique_topologies <- ape::unique.multiPhylo(trees_ready, use.edge.length = FALSE)

  # Map every original tree to a unique topology index
  # match_indices <- match(trees_ready, unique_topologies)
  match_indices <- attr(unique_topologies, "old.index")

  # Sum weights for each unique topology
  # tapply splits the weights by the index and sums them
  topology_probs <- tapply(weights, match_indices, sum)

  # Identify the Best Topology
  best_index <- as.numeric(names(which.max(topology_probs)))

  # Calculate posterior probability of this MAP topology
  map_prob <- max(topology_probs) / sum(topology_probs)

  # Compute Mean Branch Lengths for the MAP Topology
  # We take all samples that matched the MAP topology...
  matching_indices <- which(match_indices == best_index)
  matching_trees <- trees[matching_indices]
  matching_weights <- weights[matching_indices]

  # ...and compute a consensus to average their branch lengths.
  # Ideally, we should do a weighted average of the lengths,
  # but ape::consensus uses simple mean. For most purposes, this is sufficient.
  final_map <- map <- phangorn::allCompat(matching_trees, rooted = TRUE) |>
    phangorn::add_edge_length(matching_trees,
      fun = function(x) stats::weighted.mean(x, matching_weights)
    )

  print(paste("MAP Topology found"))
  print(paste("Posterior Probability:", round(map_prob, 4)))
  print(paste(
    "Based on the topology of", length(matching_indices),
    "samples out of", length(trees)
  ))

  return(final_map)
}
