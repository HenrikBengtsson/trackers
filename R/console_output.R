console_output <- function(...) {
  fh <- tempfile()
  on.exit(file.remove(fh))
  cat(..., file = fh)
  if (.Platform$OS.type == "windows") {
    file.show(fh, pager = "console", header = "", title = "",
              delete.file = FALSE)
  } else {
    system(sprintf("cat %s", fh))
  }
  invisible()
}

