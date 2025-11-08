#' Trace all calls to parallel::detectCores()
#'
#' @param action (character) What action to take when called.
#'
#' @param allow (list) List of functions that are allowed to call
#' `parallel::detectCores()` and that this tracer will ignore.
#'
#' @param enable (logical) Enable or disable tracing.
#'
#' @param calls (logical) If TRUE, the call stack is part of the
#' condition message, otherwise not.
#'
#' @return Nothing.
#'
#' This tracer detects whenever `parallel::detectCores()` is called.
#' It can then produce an informative warning or and error.  If an error,
#' then no connections will be closed.
#'
#' @export
trace_detectCores <- function(action = c("warning", "error"), allow = list(base::sys.save.image), enable = TRUE, calls = TRUE) {
  action <- match.arg(action)
  stopifnot(is.logical(enable), length(enable) == 1L, !is.na(enable))
  stopifnot(is.logical(calls), length(calls) == 1L, !is.na(calls))

  stopifnot(
    is.list(allow),
    all(vapply(allow, FUN.VALUE = NA, FUN = function(x) {
      is.function(x) || is.language(x)
    }))
  )

  ## Always disable
  suppressMessages({
    untrace(parallel::detectCores, where = baseenv())
  })

  if (enable) {
    expr_action <- if (action == "error") {
      quote({
        msg <- sprintf("%s. However, it was prevented from taking place", msg)
        stop(msg, call. = TRUE)
      })
    } else {
      quote({
        msg <- sprintf("%s. Please use parallelly::availableCores() instead", msg)
        warning(msg, call. = TRUE, immediate. = TRUE)
      })
    }

    tracer <- bquote({
      calls <- sys.calls()
      calls <- calls[seq_len(length(calls) - 5L)]
      
      ## Allow some calls?
      allows <- .(allow)
      for (allow in allows) {
        for (call in calls) {
          if (identical(call[[1]], allow)) return()
        }
      }

      msg <- "[BAD PRACTICE] Detected a call to parallel::detectCores()"
      if (.(calls)) {
        if (length(calls) > 0) {
          calls <- lapply(calls, FUN = function(call) {
            call <- call[1]
            if (typeof(call[[1]]) == "closure") call <- "<closure>"
            deparse(call, width.cutoff = Inf)
          })
          calls <- unlist(calls, use.names = FALSE)
          calls <- paste(calls, collapse = " -> ")
          calls <- gsub("[[:space:]]+", " ", calls)
       } else {
          calls <- "a direct call"
        }
        msg <- sprintf("%s via %s", msg, calls)
      }
      .(expr_action)
    })
    
    suppressMessages({
      trace(parallel::detectCores, where = baseenv(), print = FALSE,
            at = 1L, tracer = tracer)
    })
  }

  invisible()
}
