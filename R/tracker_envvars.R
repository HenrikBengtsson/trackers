#' Warn when environment variables have been changed
#'
#' @param expr The \R \link[base:expression]{expression} called.
#'
#' @param value The result of the evaluated expression.
#'
#' @param ok A logical indicating whether it was successfully completed
#' or not. (As of R 4.2.1, this is
#' \link[base:addTaskCallback]{always TRUE at present}.)
#'
#' @param visible A logical indicating whether the result was printed or not.
#'
#' @return
#' Always TRUE, which is used to tell [base::addTaskCallback()] to keep
#' this callback tracker function after it's called.
#'
#' @seealso
#' [base::addTaskCallback()]
#'
#' @export
tracker_envvars <- local({
  last <- NULL
  
  function(expr, value, ok, visible) {
    current <- as.list(Sys.getenv())
    if (!is.null(last)) {
      msg <- NULL
      names <- names(current)
      names_last <- names(last)

      ## Envvars added?
      changed <- setdiff(names, names_last)
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Environment variables added: [n=%d] %s.", length(changed), paste(sQuote(changed), collapse = ", ")))
      }
      
      ## Environment variables removed?
      changed <- setdiff(names_last, names)
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Environment variables removed: [n=%d] %s.", length(changed), paste(sQuote(changed), collapse = ", ")))
      }
      
      ## Environment variables changed?
      changed <- intersect(names, names_last)
      changed <- setdiff(changed, "prompt")
      
      for (kk in seq_along(changed)) {
        name <- changed[kk]
        if (identical(current[[name]], last[[name]])) changed[kk] <- ""
      }
      changed <- changed[nzchar(changed)]
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Environment variables changed: [n=%d] %s.", length(changed), paste(sQuote(changed), collapse = ", ")))
      }
      if (length(msg) > 0L) {
        note(paste(msg, collapse = " "))
      }
    }
    last <<- current
    
    TRUE
  }
})
