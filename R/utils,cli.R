prefix <- function() {
  getOption("trackers.prefix", "TRACKER: ")
}

note <- function(..., prefix = prefix(), dangerous = FALSE) {
  msg <- sprintf(...)
  if (dangerous) msg <- paste("[DANGEROUS] ", msg, sep = "")
  msg <- paste(prefix, msg, sep = "")
  msg <- cli_blurred(msg)
  message(msg)
}

has_crayon <- local({
  result <- NA
  function() {
    if (is.na(result)) result <<- requireNamespace("crayon", quietly=TRUE)
    result
  }
})

cli_blurred <- function(msg) {
  if (has_crayon()) {
    msg <- crayon::blurred(msg)
  }
  msg
}

cli_bold <- function(msg) {
  if (has_crayon()) {
    msg <- crayon::bold(msg)
  }
  msg
}

cli_yellow <- function(msg) {
  if (has_crayon()) {
    msg <- crayon::yellow(msg)
  }
  msg
}
