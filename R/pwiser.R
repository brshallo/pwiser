#' @description To learn more about pwiser see https://github.com/brshallo/pwiser
#' @keywords internal
#' @import rlang dplyr
#' @rawNamespace import(vctrs, except = data_frame)
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom stats setNames update
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom lifecycle deprecated
"_PACKAGE"

# We're importing vctrs without `data_frame()` because we currently
# reexport the deprecated `tibble::data_frame()` function
