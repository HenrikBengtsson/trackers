make_task_callback <- function(task_callback, name, init = NULL) {
  id <- NULL

  stopifnot(is.function(task_callback))
  stopifnot(is.character(name), length(name) == 1L)
  
  task_callback_handler <- function(action = c("on", "off")) {
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
    }
  }

  ## Initiate
  if (!is.null(init)) {
    do.call(task_callback, args = init)
  }

  task_callback_handler
}
