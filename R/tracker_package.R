#' Warn when packages are loaded or unloaded
#'
#' @inheritParams tracker_envvars
#'
#' @inherit tracker_envvars return
#'
#' @details:
#' Set R option \option{tracker.packages} to `FALSE` to disable.
#'
#' @examples
#' \dontrun{
#' addTaskCallback(tracker_package, name = "Package tracker")
#' }
#'
#' @export
tracker_package <- local({
  startup <- TRUE
  last <- loadedNamespaces()
  
  function(expr, value, ok, visible) {
    if (!isTRUE(getOption("tracker.packages", TRUE))) return(TRUE)
    curr <- loadedNamespaces()

    ## Avoid reporting on changes occuring during startup
    if (startup) {
      startup <<- FALSE
      last <<- curr
    }

    if (!identical(curr, last)) {
      diff <- list(
        loaded   = sort(setdiff(curr, last)),
        unloaded = sort(setdiff(last, curr))
      )
      diff <- vapply(names(diff), FUN = function(name) {
        vars <- diff[[name]]
        nvars <- length(vars)
        if (nvars == 0L) return(NA_character_)
        sprintf("%d package%s %s (%s)",
                nvars, if (nvars == 1) "" else "s", name,
                paste(sQuote(vars), collapse = ", "))
      }, FUN.VALUE = NA_character_)
      diff <- diff[!is.na(diff)]
      msg <- paste(cli_prefix(), "loadedNamespaces() changed: ", diff, sep = " ")
      msg <- cli_blurred(msg)
      lapply(msg, FUN = message)
      last <<- curr
    }
    TRUE
  }
})
