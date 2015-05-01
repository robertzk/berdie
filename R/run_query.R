#' Run an SQL query and return the results as a data.frame
#'
#' @param query character. The SQL query to run. This can also be left
#'   blank to obtain the latest database connection.
#' @param conn JDBCConnection. The database connection to use. By default
#'   this is the last connection opened with this package, or if non-existent,
#'   a new connection using \code{getOption('database.yml')}.
#' @seealso \code{\link{postgresql_connection}}
#' @export
#' @return a data.frame with the results of the query. If the \code{query}
#'   parameter was blank, the last database connection will be returned. 
#'   (If no database connection was used, note this may spin up a new one
#'   dependending on your configuration. See \code{\link{last_connection}}.)
run_query <- function(query, conn = last_connection()) {
  if (missing(query)) return(last_connection())
  stopifnot(is(conn, 'JDBCConnection') || is(conn, 'DBIConnection')) 
  stopifnot(is.character(query))
  if (is(conn, 'JDBCConnection')) {
    res <- RJDBC::dbSendQuery(conn, query)
    RJDBC::fetch(res, n = -1)
  } else {
    res <- DBI::dbSendQuery(conn, query)
    DBI::fetch(res, n = -1)
  }
}

