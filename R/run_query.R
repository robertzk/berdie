#' Run an SQL query and return the results as a data.frame
#'
#' @param query character. The SQL query to run.
#' @param conn JDBCConnection. The database connection to use. By default
#'   this is the last connection opened with this package, or if non-existent,
#'   a new connection using \code{getOption('database.yml')}.
#' @seealso \code{\link{postgresql_connection}}
#' @export
#' @return a data.frame with the results of the query
run_query <- function(query, conn = last_connection()) { 
  stopifnot(is(conn, 'JDBCConnection')) 
  stopifnot(is.character(query))
  res <- dbSendQuery(conn, query)
  fetch(res, n = -1)
}

