.onLoad <- function(libname, pkgname) {
  debug <- getOption("trackers.debug", NULL)
  if (is.null(debug)) {
    debug <- Sys.getenv("R_TRACKERS_DEBUG", NA_character_)
    if (!is.na(debug)) {
      options(trackers.debug = isTRUE(as.logical(debug)))
    }
  }
    
  track_files(action = "init")
  track_locale(action = "init")
}
