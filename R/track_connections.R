#' Warn when the connections are opened or closed
#'
#' @inheritParams track_envvars
#'
#' @inherit track_envvars return
#'
#' @examples
#' \dontrun{
#' track_connections()
#' }
#'
#' @export
track_connections <- make_task_callback(name = "Connections tracker", local({
  last <- NULL

  get_connections <- function() {
    idxs <- getAllConnections()
    con <- lapply(idxs, FUN = function(idx) {
      info <- summary.connection(idx)
      info <- c(list(index = idx, info))
      as.data.frame(info)      
    })
    do.call(rbind, con)
  }

  diff_connections <- function(a, b) {
    a_keys <- unname(apply(a, MARGIN = 1L, FUN = function(con) paste(as.character(con), collapse = ",")))
    b_keys <- unname(apply(b, MARGIN = 1L, FUN = function(con) paste(as.character(con), collapse = ",")))

    opened <- b[integer(0L), ]
    diff <- setdiff(b_keys, a_keys)
    if (length(diff) > 0L) opened <- b[match(diff, table = b_keys), ]

    closed <- b[integer(0L), ]
    diff <- setdiff(a_keys, b_keys)
    if (length(diff) > 0L) closed <- a[match(diff, table = a_keys), ]

    list(opened = opened, closed = closed)
  }

  function(expr, value, ok, visible) {
    curr <- get_connections()
    if (is.null(last)) last <<- curr

    diff <- diff_connections(last, curr)
    msg <- NULL
    for (name in names(diff)) {
       delta <- diff[[name]]
       n <- nrow(delta)
       if (n > 0L) {
         what <- if (n == 1L) "connection" else "connections"
         msg <- c(msg, sprintf("%d %s %s", n, what, name))
       }
    }
    
    if (!is.null(msg)) {
      msg <- paste(msg, collapse = " & ");
      n <- nrow(curr) - 3L
      what <- if (n == 1L) "connection" else "connections"
      msg <- sprintf("%s => Now %d opened %s", msg, n, what)
      msg <- sprintf("%s%s", cli_prefix(), msg)
      msg <- cli_blurred(msg)
      message(msg)
    }
    
    last <<- curr

    TRUE
  }
}))
