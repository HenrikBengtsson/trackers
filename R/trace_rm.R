#' Trace all rm(list = ls()) calls
#'
#' @param action (character) What action to take when called.
#'
#' @param enable (logical) Enable or disable tracing.
#'
#' @return Nothing.
#'
#' @details
#' Calling `rm(list = ls())` is considered bad manners.
#'
#' This tracer detects whenever `rm(list = ls())` is called.
#' It can then produce an informative warning or and error.  If an error,
#' then the call is not completed.
#'
#' @export
trace_rm <- function(action = c("error", "warning"), enable = TRUE) {
  action <- match.arg(action)
  stopifnot(is.logical(enable), length(enable) == 1L, !is.na(enable))

  ## Always disable
  suppressMessages({
    untrace(base::rm, where = baseenv())
  })

  if (enable) {
    expr_action <- if (action == "error") {
      quote({
        msg <- sprintf("%s; it was prevented from taking place.", msg)
        stop(msg, call. = FALSE)
      })
    } else {
      quote({
        msg <- sprintf("%s; it is never a good idea to call rm() this way.", msg)
        warning(msg, call. = FALSE, immediate. = TRUE)
      })
    }

    tracer <- bquote({
      call <- deparse(sys.calls()[[1]])
      if (call == "rm(list = ls())") {
        msg <- sprintf("[BAD HABIT] Detected an %s call", call)
        .(expr_action)
      }
    })
    
    suppressMessages({
      trace(base::rm, where = baseenv(), print = FALSE,
            at = 1L, tracer = tracer)
    })
  }

  invisible()
}
