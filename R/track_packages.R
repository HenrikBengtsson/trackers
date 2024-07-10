#' Warn when packages are loaded/unloaded or attached/detached
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @details:
#' Set R option \option{tracker.packages} to `FALSE` to disable.
#'
#' @examples
#' \dontrun{
#' track_packages()
#' }
#'
#' @export
track_packages <- make_task_callback(name = "Packages tracker", local({
  attachedPackages <- function() {
    pkgs <- grep("^package:", search(), value = TRUE)
    sub("^package:", "", pkgs)
  }
  
  startup <- TRUE
  last <- list(loaded = loadedNamespaces(), attached = attachedPackages())
  
  function(expr, value, ok, visible) {
    if (!isTRUE(getOption("tracker.packages", TRUE))) return(TRUE)
    curr <- list(loaded = loadedNamespaces(), attached = attachedPackages())

    ## Avoid reporting on changes occuring during startup
    if (startup) {
      startup <<- FALSE
      last <<- curr
    }

    if (!identical(curr, last)) {
      diff <- list(
        attached = sort(setdiff(curr$attached, last$attached)),
        detached = sort(setdiff(last$attached, curr$attached)),
        loaded   = sort(setdiff(curr$loaded, last$loaded)),
        unloaded = sort(setdiff(last$loaded, curr$loaded))
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
      msg <- paste(cli_prefix(), diff, sep = "")
      msg <- cli_blurred(msg)
      lapply(msg, FUN = message)
      last <<- curr
    }
    TRUE
  }
}))
