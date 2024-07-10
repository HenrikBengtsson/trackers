#' Report on garbage collection
#'
#' @param threshold (numeric) Minimum garbage-collection time (in seconds) to
#' report on.
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
track_gc <- make_task_callback(name = "Garbage collector tracker", args = list(threshold = 0.0), local({
  last <- NULL

  function(expr, value, ok, visible, data) {
    current <- gc.time()
    
    if (!is.null(last) && !is.null(expr)) {
      dt <- current - last
      if (any(dt > 0)) {
        names(dt) <- c("user", "system", "elapsed", "user_children", "system_children")
        dt <- dt[dt > 0]
        threshold <- data$threshold
        if (any(dt > threshold)) {
          info <- paste(sprintf("%s=%gs", names(dt), dt), collapse = ", ")
          msg <- sprintf("Garbage collector: %s", info)
        }
      }
      
      current <- gc.time()
    }
    
    last <<- current
    
    TRUE
  }
}))
