#' @title Dynamic branching over output or input files.
#' @rdname tar_files
#' @description
#' A clone of `tarchetypes::tar_files` except that `targets::tar_cue(mode = "always")` 
#' is not hardcoded for argument `cue` of the upstream target.
#' @inherit tarchetypes::tar_files
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Do not use temp files in real projects
#'   # or else your targets will always rerun.
#'   paths <- unlist(replicate(2, tempfile()))
#'   file.create(paths)
#'   list(
#'     jftargets::tar_files(x, paths)
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
#' @export

tar_files_raw <- function(
    name,
    command,
    tidy_eval = targets::tar_option_get("tidy_eval"),
    packages = targets::tar_option_get("packages"),
    library = targets::tar_option_get("library"),
    format = c("file", "url", "aws_file"),
    repository = targets::tar_option_get("repository"),
    iteration = targets::tar_option_get("iteration"),
    error = targets::tar_option_get("error"),
    memory = targets::tar_option_get("memory"),
    garbage_collection = targets::tar_option_get("garbage_collection"),
    deployment = targets::tar_option_get("deployment"),
    priority = targets::tar_option_get("priority"),
    resources = targets::tar_option_get("resources"),
    storage = targets::tar_option_get("storage"),
    retrieval = targets::tar_option_get("retrieval"),
    cue = targets::tar_option_get("cue")
){
  targets::tar_assert_chr(name, "name must be a character.")
  targets::tar_assert_scalar(name, "name must have length 1.")
  
  targets::tar_assert_nonmissing(
    command,
    paste("target", name, "has no command.")
  )
  targets::tar_assert_scalar(
    as.expression(command),
    paste("the command of target", name, "must have length 1.")
  )
  
  command <- as.expression(command)[[1]]
  name_files <- paste0(name, "_files")
  format <- match.arg(format)
  upstream <- targets::tar_target_raw(
    name = name_files,
    command = command,
    pattern = NULL,
    packages = packages,
    library = library,
    format = "rds",
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
  name_files_sym <- as.symbol(name_files)
  downstream <- targets::tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(as.call(c(as.symbol("map"), list(name_files_sym)))),
    packages = character(0),
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    resources = resources,
    storage = "main",
    retrieval = "main",
    cue = cue
  )
  out <- list(upstream, downstream)
  names(out) <- c(name_files, name)
  out
}

#' @rdname tar_files
#' @export

tar_files <- function(
    name,
    command,
    tidy_eval = targets::tar_option_get("tidy_eval"),
    packages = targets::tar_option_get("packages"),
    library = targets::tar_option_get("library"),
    format = c("file", "url", "aws_file"),
    repository = targets::tar_option_get("repository"),
    iteration = targets::tar_option_get("iteration"),
    error = targets::tar_option_get("error"),
    memory = targets::tar_option_get("memory"),
    garbage_collection = targets::tar_option_get("garbage_collection"),
    deployment = targets::tar_option_get("deployment"),
    priority = targets::tar_option_get("priority"),
    resources = targets::tar_option_get("resources"),
    storage = targets::tar_option_get("storage"),
    retrieval = targets::tar_option_get("retrieval"),
    cue = targets::tar_option_get("cue")
) {
  name <- targets::tar_deparse_language(substitute(name))
  envir <- targets::tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  format <- match.arg(format)
  tar_files_raw(
    name = name,
    command = command,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}