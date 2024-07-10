#' Warn when processing time ellapsed
#'
#' @param action (character) Enable or disable tracking.
#'
#' @return
#' Nothing.
#'
#' @seealso
#' [base::addTaskCallback()]
#'
#' @examples
#' \dontrun{
#' track_time()
#' }
#'
#' @export
track_time <- make_task_callback(name = "Processing time tracker", local({
  last <- NULL

  function(expr, value, ok, visible) {
    current <- proc.time()
    
    if (!is.null(last) && !is.null(expr)) {
      dt <- current - last
      if (any(dt > 0)) {
        names(dt) <- c("user", "system", "elapsed", "user_children", "system_children")
        dt <- dt[-3]
        dt <- dt[dt > 0]
        info <- paste(sprintf("%s=%gs", names(dt), dt), collapse = ", ")
        msg <- sprintf("Processing time: %s", info)
        note(msg)
      }
      
      current <- proc.time()
    }
    
    last <<- current
    
    TRUE
  }
}))
