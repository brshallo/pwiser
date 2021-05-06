# import more intelligently, see: here:
# https://github.com/ropensci-org/pkgreviewr/issues/3
# https://stackoverflow.com/questions/32535773/using-un-exported-function-from-another-r-package
key_deparse <- dplyr:::key_deparse
peek_mask <- dplyr:::peek_mask
context_peek_bare <- dplyr:::context_peek_bare
context_poke <- dplyr:::context_poke
data_mask_top <- dplyr:::data_mask_top

#### These functions are all essentially modified copies of dplyr::across() set-up

###
pairwise_setup_impl <- function(cols, fns, names, .caller_env, is_commutative, mask = peek_mask("across()"), .top_level = FALSE) {
  cols <- enquo(cols)

  if (.top_level) {
    # FIXME: this is a little bit hacky to make top_across()
    #        work, otherwise mask$across_cols() fails when calling
    #        self$current_cols(across_vars_used)
    #        it should not affect anything because it is expected that
    #        across_setup() is only ever called on the first group anyway
    #        but perhaps it is time to review how across_cols() work
    mask$set_current_group(1L)
  }
  # `across()` is evaluated in a data mask so we need to remove the
  # mask layer from the quosure environment (#5460)
  cols <- quo_set_env(cols, data_mask_top(quo_get_env(cols), recursive = FALSE, inherit = TRUE))

  vars <- tidyselect::eval_select(cols, data = mask$across_cols())
  vars <- names(vars)

  # build .col_x, .col_y depending on commutative or not
  length_vars <- length(vars)
  if (is_commutative) {
    k <- 1L
    .col_x <- vector("integer", choose(length_vars, 2))
    .col_y <- vector("integer", choose(length_vars, 2))
    for (v in seq_along(vars)) {
      if (v == length(vars)) break
      for (u in ((v + 1):length_vars)) {
        .col_x[k] <- vars[v]
        .col_y[k] <- vars[u]
        k <- k + 1L
      }
    }
  } else {
    .col_x_list <- list()
    .col_y_list <- list()
    for (v in seq_along(vars)) {
      .col_x_list[[v]] <- rep(vars[v], length_vars - 1)
      .col_y_list[[v]] <- vars[((1:length_vars) != v)]
    }
    .col_x <- unlist(.col_x_list)
    .col_y <- unlist(.col_y_list)
  }

  if (is.null(fns)) {
    if (!is.null(names)) {
      glue_mask <- pairwise_glue_mask(.caller_env, .col_x = .col_x, .col_y = .col_y, .fn = "1")
      names <- vec_as_names(glue(names, .envir = glue_mask), repair = "check_unique")
    }

    value <- list(vars = vars, fns = fns, names = names)
    return(value)
  }

  # apply `.names` smart default
  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{.col_x}_{.col_y}"
    fns <- list("1" = fns)
  } else {
    names <- names %||% "{.col_x}_{.col_y}_{.fn}"
  }

  if (!is.list(fns)) {
    abort(c("Problem with `across()` input `.fns`.",
            i = "Input `.fns` must be NULL, a function, a formula, or a list of functions/formulas."
    ))
  }

  fns <- purrr::map(fns, as_function)

  # make sure fns has names, use number to replace unnamed
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  glue_mask <- glue_mask <- pairwise_glue_mask(.caller_env,
                                               .col_x = rep(.col_x, each = length(fns)),
                                               .col_y = rep(.col_y, each = length(fns)),
                                               .fn = rep(names_fns, length(.col_y))
  )

  names <- vec_as_names(glue(names, .envir = glue_mask), repair = "check_unique")

  list(vars = vars, fns = fns, names = names)
}

###
pairwise_glue_mask <- function(.col_x, .col_y, .fn, .caller_env) {
  glue_mask <- env(.caller_env, .col_x = .col_x, .col_y = .col_y, .fn = .fn)
  # TODO: we can make these bindings louder later
  env_bind_active(
    glue_mask,
    col_x = function() glue_mask$.col_x, col_y = function() glue_mask$.col_y,
    fn = function() glue_mask$.fn
  )
  glue_mask
}

###
pairwise_setup <- function(cols, fns, names, key, .caller_env, is_commutative) {
  mask <- peek_mask("across()")
  value <- mask$across_cache_get(key)
  if (is.null(value)) {
    value <- pairwise_setup_impl({{ cols }},
                                 fns = fns, names = names, .caller_env = .caller_env, mask = mask,
                                 .top_level = FALSE,
                                 is_commutative = is_commutative
    )
    mask$across_cache_add(key, value)
  }
  value
}


#' Apply a function (or functions) across all combinations of pairs of selected columns
#'
#' @description
#' `pairwise()` creates all combinations of columns and then applies function(s)
#' to these.
#'
#' `pairwise()` largely mirror `dplyr::across()` in style (and is meant to be
#' used primarily within `dplyr::mutate()` and `dplyr::summarise()`).
#'
#' @param .cols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#' @param .fns Functions to apply to each pair of the selected columns.
#'   Possible values are:
#'
#'   - A function
#'   - A purrr-style lambda, e.g. `~ stats::cor.test(.x, .y)$p.value`
#'   - A list of functions / lambdas, e.g.
#'   ````
#'     list(difference = `-`, ratio = ~ .x / .y)
#'   ````
#'
#'   The output length of a function should in most cases be 1 (in the
#'   summarisng case) or the length of an individual input (in the mutating
#'   case), similar to what is expected by `summarise()` and `mutate()`
#'   respectively.
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#' @param .names A glue specification that describes how to name the output For
#'   pairwise, it can use `{.col_x}` and `{.col_y}` for the columns that gets
#'   passed and uses the following format with multiple `.fns`:
#'   `"{.col_x}_{.col_y}_{.fn}"`
#'
#' @param .is_commutative If `TRUE`, only create new column for
#'   `{.col_x}_{.col_y}` (not `{.col_y}_{.col_x}`). Use to save computation time
#'   when applying commutative functions (e.g. pearson's correlation).
#'
#' @return `pairwise()` returns a tibble with one column for each possible pairwise combination in `.cols`.
#' @seealso `dplyr::across()`, `corrr::colpair_map()`, `{widyr}`, `recipes::step_interact()`, `recipes::step_ratio`
#' @export
#'
#' @examples
#' library(dplyr)
#' library(pwiser)
#' library(palmerpenguins)
#'
#' penguins <- na.omit(penguins)
#'
#' # Grouped summary of correlations
#' penguins %>%
#'   group_by(species) %>%
#'   summarise(pairwise(contains("_mm"), ~stats::cor.test(.x, .y)$p.value, .is_commutative = TRUE),
#'             n = n())
#'
#' # Building new columns
#' penguins %>%
#'   mutate(pairwise(contains("_mm"),
#'                   list(ratio = `/`, difference = `-`),
#'                   .names = "features_{.fn}_{.col_x}_{.col_y}"),
#'          n = n()) %>%
#'   glimpse()
pairwise <- function(.cols = everything(), .fns = NULL, ..., .names = NULL, .is_commutative = FALSE) {
  key <- key_deparse(sys.call())
  setup <- pairwise_setup({{ .cols }}, fns = .fns, names = .names, key = key, .caller_env = caller_env(), is_commutative = .is_commutative)

  vars <- setup$vars
  if (length(vars) == 0L) {
    return(new_tibble(list(), nrow = 1L))
  }
  fns <- setup$fns
  names <- setup$names

  mask <- peek_mask()
  data <- mask$current_cols(vars)

  if (is.null(fns)) {
    nrow <- length(mask$current_rows())
    data <- new_data_frame(data, n = nrow, class = c("tbl_df", "tbl"))

    if (is.null(names)) {
      return(data)
    } else {
      return(set_names(data, names))
    }
  }


  n_cols <- length(data)
  n_fns <- length(fns)

  if (.is_commutative) {
    total_pairwise_cols <- choose(length(data), 2)
  } else {
    total_pairwise_cols <- length(data) * (length(data) - 1)
  }

  seq_n_cols <- seq_len(n_cols)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", total_pairwise_cols * n_fns)

  # Reset `cur_column()` info on exit
  old_var <- context_peek_bare("column")
  on.exit(context_poke("column", old_var), add = TRUE)

  # Loop in such an order that all functions are applied
  # to a single column before moving on to the next column
  for (i in seq_n_cols) {
    for (h in seq_n_cols) {
      if (.is_commutative & h <= i) next
      if (i == h) next
      var_x <- vars[[i]]
      var_y <- vars[[h]]

      col_x <- data[[i]]
      col_y <- data[[h]]

      context_poke("column", var_x)

      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(col_x, col_y, ...)
        k <- k + 1L
      }
    }
  }

  size <- vec_size_common(!!!out)
  out <- vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  new_data_frame(out, n = size, class = c("tbl_df", "tbl"))
}

