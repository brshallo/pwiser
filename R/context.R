
# context accessors -------------------------------------------------------

context_env <- dplyr:::context_env
# context_env <- new_environment()
context_poke <- function(name, value) {
  old <- context_env[[name]]
  context_env[[name]] <- value
  old
}
context_peek_bare <- function(name) {
  ### THIS IS A TOTAL HACK
  ### also removes any safeguards around where is used
  # context_env[[name]]
  dplyr:::context_env[[name]]
}

context_peek <- function(name, fun, location = "dplyr verbs") {
  context_peek_bare(name) %||%
    abort(glue("`{fun}` must only be used inside {location}."))
}
context_local <- function(name, value, frame = caller_env()) {
  old <- context_poke(name, value)
  expr <- expr(on.exit(context_poke(!!name, !!old), add = TRUE))
  eval_bare(expr, frame)
}

peek_column <- function() {
  context_peek("column", "cur_column()", "`across()`")
}
local_column <- function(x, frame = caller_env()) {
  context_local("column", x, frame = frame)
}

peek_mask <- function(fun = "peek_mask()") {
  context_peek("mask", fun)
}
local_mask <- function(x, frame = caller_env()) {
  context_local("mask", x, frame = frame)
}
