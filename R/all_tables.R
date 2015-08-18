#' Lists all tables within DB 
#'
#' @param conn PqConnection. The database connection to use. By default
#'   this is the last connection opened with this package, or if non-existent,
#'   a new connection using \code{getOption('database.yml')}.
#' @seealso \code{\link{postgresql_connection}}
#' @export
all_tables <- function(conn = last_connection()) {
  stopifnot(is(conn, 'PqConnection'))
  DBI::dbListTables(conn)
}

