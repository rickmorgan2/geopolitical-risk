# Load an R/ pipeline script's definitions into a fresh environment without
# running its top-level run calls (those download data or need a database).
load_script <- function(file, skip_calls) {
  env   <- new.env(parent = globalenv())
  exprs <- parse(repo_file("R", file), keep.source = FALSE)
  is_skipped <- function(e) is.call(e) && as.character(e[[1]])[1] %in% skip_calls
  quietly(for (e in exprs) if (!is_skipped(e)) eval(e, envir = env))
  env
}
