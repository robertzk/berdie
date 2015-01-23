#' Converts .SQL files (or other readable files) into a format readable by the run_query() function.
#'
#' @param path_to_query character.  The filepath of the SQL file to be formatted.  This should contain the SQL code, can be .SQL or other readable type.
#' @return string of condensed SQL query with '--' comments removed and spacing reduced 
#' @examples 
#' \dontrun{
#' prep_sql("/path/to/query.SQL")
#' }
#' @author Mike Bruno
#' @export
prep_sql <- function(path_to_query) {
  # Check to see if the Path is actually a query.  If not, parse the file.
  if (identical(is.sql(path_to_query), FALSE)) {
   # Read The SQL File
   lines <- readLines(path_to_query)
   # Remove Comments
   vec <- vapply(lines, gsub, character(1), pattern = "--.*$", replacement = "")
   # Collapse into 1 line
   sql <- paste(vec, collapse=" ")
 } else {
   sql <- path_to_query
 }
  # Remove Tabs and Spaces - easier to read
  sql <- gsub("\t", "", sql)
  sql <- gsub("[[:space:]]+", " ", sql)
  
  sql
}

is.sql <- function(test_string) {
  # Tests for the words "SELECT" and "FROM"
  result <- grepl("SELECT", test_string, ignore.case = TRUE)
  result <- result & grepl("FROM", test_string, ignore.case = TRUE)
  
  result 
}
