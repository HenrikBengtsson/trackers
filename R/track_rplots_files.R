#' Alert about newly produced Rplots*.pdf files
#'
#' Look for newly produced `Rplots*.pdf` files, which may be produced
#' when running in batch mode or when screen devices are not available.
#' See `?options` and option `'device'`.
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @examples
#' \dontrun{
#' track_rplots_files()
#' }
#'
#' @importFrom utils file_test
#' @export
track_rplots_files <- make_task_callback(name = "Rplots file tracker", local({
  prev_files <- NULL

  message <- function(msg, ...) {
    msg <- paste(cli_prefix(), msg, sep = "")
    msg <- cli_blurred(msg)
    base::message(msg, ...)
  }

  function(expr, value, ok, visible) {
    files <- dir(pattern = "Rplots[0-9]*.pdf$")
    if (length(files) == 0) return(TRUE)
    files <- files[file_test("-f", files)]
    if (length(files) == 0) return(TRUE)

    ## Setup?
    if (is.null(prev_files)) {
      info <- lapply(files, FUN = file.info)
      names(info) <- files
      prev_files <<- info
      return(TRUE)
    }
    
    ## Any files dropped?
    if (length(prev_files) > 0) {
      dropped <- setdiff(names(prev_files), files)
      if (length(dropped) > 0) {
              message("Graphics files removed: ",
	        paste(sQuote(dropped), collapse = ", "))
        prev_files <<- prev_files[files]
      }
    }
    
    for (kk in seq_along(files)) {
      file <- files[kk]
      info <- file.info(file)
      if (file %in% names(prev_files)) {
        prev_file <- prev_files[[file]]
	if (!identical(info$size, prev_file$size)) {
	  why <- sprintf(" (file size %d -> %d bytes)",
	                 prev_file$size, info$size)
          message(sprintf("Graphics file modified%s: %s", why, sQuote(file)))
	} else if (!identical(info$mtime, prev_file$mtime)) {
	  why <- sprintf(" (mtime %s -> %s)", prev_file$mtime, info$mtime)
          message(sprintf("Graphics file modified%s: %s", why, sQuote(file)))
	}
      } else {
        message(sprintf("Graphics file added: %s", sQuote(file)))
        prev_files[file] <- info
      }
    }

    TRUE
  }
}))
