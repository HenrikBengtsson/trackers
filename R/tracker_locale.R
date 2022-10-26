#' Warn when locale have been changed
#'
#' @param expr The \R \link[base:expression]{expression} called.
#'
#' @param value The result of the evaluated expression.
#'
#' @param ok A logical indicating whether it was successfully completed
#' or not. (This is \link[base:addTaskCallback]{always TRUE at present}.)
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
#' @examples
#' \dontrun{
#' addTaskCallback(tracker_locale, name = "Locale tracker")
#' }
#'
#' @export
tracker_locale <- local({
  parse_locale <- function(x) {
    x <- unlist(strsplit(x, split = ";", fixed = TRUE))
    pattern <- "^([^=]+)=(.*)$"
    names <- gsub(pattern, "\\1", x)
    values <- gsub(pattern, "\\2", x)
    structure(values, names = names)
  }
  
  last <- NULL
  
  function(expr, value, ok, visible) {
    current <- parse_locale(Sys.getlocale())

    if (!is.null(last) && !is.null(expr)) {
      msg <- NULL
      names <- names(current)
      names_last <- names(last)

      ## Locale components added?
      changed <- setdiff(names, names_last)
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Locale components added: [n=%d] %s.", length(changed), paste(sprintf("%s=%s", changed, sQuote(current[changed])), collapse = ", ")))
      }
      
      ## Locale components removed?
      changed <- setdiff(names_last, names)
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Locale components removed: [n=%d] %s.", length(changed), paste(sprintf("%s=%s", changed, sQuote(last[changed])), collapse = ", ")))
      }
      
      ## Locale components changed?
      changed <- intersect(names, names_last)
      changed <- setdiff(changed, "prompt")
      
      for (kk in seq_along(changed)) {
        name <- changed[kk]
        if (identical(current[[name]], last[[name]])) changed[kk] <- ""
      }
      changed <- changed[nzchar(changed)]
      if (length(changed) > 0L) {
        msg <- c(msg, sprintf("Locale components changed: [n=%d] %s.", length(changed), paste(sprintf("%s=%s (was %s)", changed, sQuote(current[changed]), sQuote(last[changed])), collapse = ", ")))
      }
      
      if (length(msg) > 0L) {
        note(paste(msg, collapse = " "))
      }
    }
    
    last <<- current
    
    TRUE
  }
})
