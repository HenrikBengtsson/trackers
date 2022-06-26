#' Warn when the .Random.seed has changed
#'
#' @inheritParams tracker_envvars
#'
#' @return Always TRUE
#'
#' @details
#'
#' Global variables that are monitored:
#' * `.Random.seed`
#'
#' @export
tracker_rng <- local({
  last <- .GlobalEnv$.Random.seed
  
  function(expr, value, ok, visible) {
    curr <- .GlobalEnv$.Random.seed
    if (!identical(curr, last)) {
      msg <- paste(prefix(), ".Random.seed changed")
      msg <- cli_blurred(msg)
      message(msg)
      last <<- curr
    }
    TRUE
  }
})
