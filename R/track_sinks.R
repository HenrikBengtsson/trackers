#' Warn when the output or message stream is sinked
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @details
#' The behavior of this tracker can be configured via R option
#' \option{tracker.sink.delta};
#'
#' * `TRUE`: Report only when there is a change in the sinks
#' * `FALSE`: (default) Report whenever there is an active sink
#'
#' @examples
#' \dontrun{
#' track_sinks()
#' }
#'
#' @export
track_sinks <- make_task_callback(name = "Sinks tracker", local({
  last <- list(n_out = NA_integer_, n_msg = NA_integer_)

  function(expr, value, ok, visible) {
    n_out <- sink.number(type = "output")
    n_msg <- sink.number(type = "message") - 2L

    if (isTRUE(getOption("tracker.sink.delta", FALSE)) && !is.na(last$n_out)) {
      d_out <- n_out - last$n_out
      d_msg <- n_msg - last$n_msg
      if (d_out == 0 && d_msg == 0) {
        msg <- NULL
      } else if (d_out > 0 && d_msg == 0) {
        msg <- sprintf("%d (%+d) output sink", n_out, d_out)
      } else if (d_out == 0 && d_msg > 0) {
        msg <- sprintf("%d (%+d) message sink", n_msg, d_msg)
      } else {
        msg <- sprintf("%d (%+d) output & %d (%+d) message sink",
                       d_out, n_msg, d_out, n_msg)
      }
    } else {
      if (n_out == 0 && n_msg == 0) {
        msg <- NULL
      } else if (n_out > 0 && n_msg == 0) {
        msg <- sprintf("%d output sink", n_out)
      } else if (n_out == 0 && n_msg > 0) {
        msg <- sprintf("%d message sink", n_msg)
      } else {
        msg <- sprintf("%d output & %d message sink", n_out, n_msg)
      }
    }

    if (!is.null(msg)) {
      msg <- sprintf("%s%s\n", cli_prefix(), msg)
      msg <- cli_blurred(msg)
      if (n_msg == 0) {
        ## (a) Output to 'message' stream, unless that is sink:ed
        message(msg)
      } else if (n_out == 0) {
        ## (b) Output to 'output' stream, unless that is also sink:ed
        cat(msg, "\n", sep = "")
      } else {
        ## (c) Output to 'console' stream, as a last resort
        console_output(msg)
      }
    }
    
    last$n_out <<- n_out
    last$n_msg <<- n_msg

    TRUE
  }
}))
