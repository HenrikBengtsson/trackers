#' Report on processing time
#'
#' @param threshold (numeric) Minimum processing time (in seconds) to
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
#' track_time(threshold = 0.2) # track anything greater than 0.2s
#' }
#'
#' @export
track_time <- make_task_callback(name = "Processing time tracker", args = list(threshold = 0.0), local({
  last <- NULL

  function(expr, value, ok, visible, data) {
    current <- proc.time()
    
    if (!is.null(last) && !is.null(expr)) {
      dt <- current - last
      if (any(dt > 0)) {
        names(dt) <- c("user", "system", "elapsed", "user_children", "system_children")
        dt <- dt[-3]
        dt <- dt[dt > 0]
        threshold <- data$threshold
        if (any(dt > threshold)) {
          info <- paste(sprintf("%s=%gs", names(dt), dt), collapse = ", ")
          msg <- sprintf("Processing time: %s", info)
          note(msg)
        }
      }
      
      current <- proc.time()
    }
    
    last <<- current
    
    TRUE
  }
}))
