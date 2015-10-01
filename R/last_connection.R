#' The last database connection initiated by this package
#'
#' @return the latest database connection initiated by this package.
#'   If none exists, the option \code{berdie.database.yml} will be used to
#'   attempt to load one from a database.yml file. If this option or file
#'   does not exist, and a Syberia project is currently loaded, it will
#'   attempt to use the current \code{syberiaStructure::syberia_root()} to find the relative
#'   \code{config/database.yml} file. Otherwise, it will
#'   return \code{NULL}.
#' @seealso \code{\link{postgresql_connection}}
#' @export
last_connection <- function() {
  conn <- get_cache('last_connection')
  if (is(conn, "error")) {
    conn <- NULL
  }
  if (is(conn, 'PqConnection')) {
    isValidConnection <- tryCatch(dbIsValid(dbSendQuery(conn, 'select 1')), error = function(e) FALSE)
    if (!isValidConnection)
      conn <- NULL
  }

  conn %||%
  postgresql_connection(getOption('berdie.database.yml'), strict = FALSE) %||%
  (if ('syberia' %in% .packages())
     postgresql_connection(strict = FALSE,
       file.path(syberiaStructure::syberia_root(), 'config', 'database.yml'))
   else NULL)
}
