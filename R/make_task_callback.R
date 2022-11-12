make_task_callback <- function(task_callback, name) {
  id <- NULL

  stopifnot(is.function(task_callback))
  stopifnot(is.character(name), length(name) == 1L)
  
  task_callback_handler <- function(action = c("on", "off", "init")) {
    action <- match.arg(action)
    if (action == "on") {
      if (is.null(id)) {
        id <<- addTaskCallback(task_callback, name = name)
      }
    } else if (action == "off") {
      if (!is.null(id)) {
        removeTaskCallback(id)
        id <<- NULL
      }
    } else if (action == "init") {
      do.call(task_callback, args = list(expr = NULL))
    }
  }

  task_callback_handler
}
