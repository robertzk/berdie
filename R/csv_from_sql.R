#' Creates a .csv file from a text SQL query or .SQL file
#' @name csv_from_sql
#' @param sql character.SQL filepath or SQL Query.
#' @param output_csv character. The filepath of the query's results.  (Include .csv for best results)
#' @param use.pp logical. Utilizes the pp() function to run R code contained within hastag brackets #{R_Code}
#'
#' @examples
#' \dontrun{
#' csv_from_sql("/path/to/query.SQL", "/path/to/file.csv")
#' }
#' @author Mike Bruno
#' @export
csv_from_sql <- function(sql, output_csv, use.pp = TRUE, ...) {
  query <- prep_sql(sql)
  if (use.pp)  query <- eval(substitute(pp(query)), env = parent.frame())
  result <- run_query(query)
  write.csv(result, file = output_csv, row.names = FALSE, na = "")
}