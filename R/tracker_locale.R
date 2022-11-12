#' Warn when locale have been changed
#'
#' @inherit tracker_envvars return
#'
#' @seealso
#' [base::addTaskCallback()]
#'
#' @examples
#' \dontrun{
#' tracker_locale()
#' }
#'
#' @export
track_locale <- function() {
  tracker_locale(NULL)  ## initiate locale tracker
  addTaskCallback(tracker_locale, name = "Locale tracker")
}

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
