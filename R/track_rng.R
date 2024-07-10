#' Warn when the .Random.seed has changed
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @details
#'
#' Global variables that are monitored:
#' * `.Random.seed`
#'
#' @examples
#' \dontrun{
#' tracker_rng()
#' }
#'
#' @export
track_rng <- make_task_callback(name = "RNG tracker", local({
  last <- .GlobalEnv$.Random.seed
  
  function(expr, value, ok, visible) {
    curr <- .GlobalEnv$.Random.seed
    if (!identical(curr, last)) {
      msg <- paste(cli_prefix(), ".Random.seed changed", sep="")
      msg <- cli_blurred(msg)
      message(msg)
      last <<- curr
    }
    TRUE
  }
}))
