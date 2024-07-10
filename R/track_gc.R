#' Warn when garbage collector has been run
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
#' track_gc()
#' }
#'
#' @export
track_gc <- make_task_callback(name = "Garbage collector tracker", local({
  last <- NULL

  function(expr, value, ok, visible) {
    current <- gc.time()
    
    if (!is.null(last) && !is.null(expr)) {
      dt <- current - last
      if (any(dt > 0)) {
        names(dt) <- c("user", "system", "elapsed", "user_children", "system_children")
        dt <- dt[dt > 0]
        info <- paste(sprintf("%s=%gs", names(dt), dt), collapse = ", ")
        msg <- sprintf("Garbage collector: %s", info)
        note(msg)
      }
      
      current <- gc.time()
    }
    
    last <<- current
    
    TRUE
  }
}))
