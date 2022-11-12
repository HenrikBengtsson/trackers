#' Warn when files are added or removed
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @details
#' Set R option \option{tracker.files} to `FALSE` to disable.
#'
#' @examples
#' \dontrun{
#' track_files()
#' }
#'
#' @export
track_files <- make_task_callback(name = "Files tracker", local({
  cache <- list()
  cache[[getwd()]] <- dir(all.files = TRUE)
  
  function(expr, value, ok, visible) {
    if (!isTRUE(getOption("tracker.files", TRUE))) return(TRUE)
    pwd <- getwd()
    last <- cache[[pwd]]
    curr <- dir(all.files = TRUE)

    ## Initialize?
    if (is.null(expr)) {
      cache[[pwd]] <<- curr
      return()
    }
    
    if (!identical(curr, last)) {
      diff <- list(
        added   = setdiff(curr, last),
        removed = setdiff(last, curr)
      )
      diff <- vapply(names(diff), FUN = function(name) {
        vars <- diff[[name]]
        nvars <- length(vars)
        if (nvars == 0L) return(NA_character_)
        sprintf("%d file%s %s (%s)",
                nvars, if (nvars == 1) "" else "s", name,
                paste(sQuote(vars), collapse = ", "))
      }, FUN.VALUE = NA_character_)
      diff <- diff[!is.na(diff)]
      msg <- paste(cli_prefix(), diff, sep = "")
      msg <- cli_blurred(msg)
      lapply(msg, FUN = message)
      cache[[pwd]] <<- curr
    }
    TRUE
  }
}))
