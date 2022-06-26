#' Warn when the .Random.seed has changed
#'
#' @inheritParams tracker_envvars
#'
#' @inherit tracker_envvars return
#'
#' @details
#'
#' Global variables that are monitored:
#' * `.Random.seed`
#'
#' @examples
#' \dontrun{
#' addTaskCallback(tracker_rng, name = "RNG tracker")
#' }
#'
#' @export
tracker_rng <- local({
  last <- .GlobalEnv$.Random.seed
  
  function(expr, value, ok, visible) {
    curr <- .GlobalEnv$.Random.seed
    if (!identical(curr, last)) {
      msg <- paste(cli_prefix(), ".Random.seed changed")
      msg <- cli_blurred(msg)
      message(msg)
      last <<- curr
    }
    TRUE
  }
})
