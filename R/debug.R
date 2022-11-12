commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)

trim <- function(s) sub("[\t\n\f\r ]+$", "", sub("^[\t\n\f\r ]+", "", s))

## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep="", collapse=", ", last_collapse=NULL,
                   max_head=if (missing(last_collapse)) 3 else Inf,
                   max_tail=if (is.finite(max_head)) 1 else Inf,
                   abbreviate="...") {
  max_head <- as.double(max_head)
  max_tail <- as.double(max_tail)
  if (is.null(last_collapse)) last_collapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep = sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > max_head + max_tail + 1) {
    head <- x[seq_len(max_head)]
    tail <- rev(rev(x)[seq_len(max_tail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (last_collapse == collapse) {
      x <- paste(x, collapse = collapse)
    } else {
      x_head <- paste(x[1:(n - 1)], collapse = collapse)
      x <- paste(x_head, x[n], sep = last_collapse)
    }
  }

  x
}

now <- function(x = Sys.time(), format = "[%H:%M:%OS3] ") {
  ## format(x, format = format) ## slower
  format(as.POSIXlt(x, tz = ""), format = format)
}

mdebug <- function(..., debug = getOption("trackers.debug", FALSE)) {
  if (!debug) return()
  message(sprintf(...))
  invisible(TRUE)
} ## mdebug()

mdebugf <- function(..., appendLF = TRUE, prefix = now(), debug = getOption("trackers.debug", FALSE)) {
  if (!debug) return()
  message(prefix, sprintf(...), appendLF = appendLF)
}

#' @importFrom utils capture.output str
mstr <- function(..., debug = getOption("trackers.debug", FALSE)) {
  if (!debug) return()
  bfr <- capture.output(str(...))
  bfr <- paste(bfr, collapse = "\n")
  message(bfr, appendLF = TRUE)
}
