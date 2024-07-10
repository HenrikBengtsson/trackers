make_task_callback <- function(task_callback, args = list(), name) {
  id <- NULL

  stopifnot(is.function(task_callback))
  stopifnot(is.list(args), length(args) == 0 || !is.null(names(args)))
  stopifnot(is.character(name), length(name) == 1L)
  
  task_callback_handler <- function(action = c("on", "off", "init")) {
    action <- match.arg(action)
    if (action == "on") {
      if (!is.null(id)) {
        removeTaskCallback(id)
        id <<- NULL
      }
      id <<- if (length(args) == 0) {
        addTaskCallback(task_callback, name = name)
      } else {
        data <- mget(names(args))
        addTaskCallback(task_callback, data = data, name = name)
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
  
  ## Add custom arguments
  if (length(args) > 0) {
    formals(task_callback_handler) <- c(args, formals(task_callback_handler))
  }

  task_callback_handler
}
