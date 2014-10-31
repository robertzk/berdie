#' Converts .SQL files (or other readable files) into a format readable by the run_query() function.
#' @param path_to_query character.  The filepath of the SQL file to be formatted.  This should contain the SQL code, can be .SQL or other readable type.
#' @return string of condensed SQL query with '--' comments removed and spacing reduced 
#' @examples 
#' \dontrun{
#' prep_sql("/path/to/query.SQL")
#' }
#' @author Mike Bruno
#' @export
prep_sql <- function(path_to_query) {
  
  # Read The SQL File
  lines <- readLines(path_to_query)
  # Remove Comments
  vec <- vapply(lines, gsub, character(1), pattern = "--.*$", replacement = "")
  # Collapse into 1 line
  sql <- paste(vec, collapse=" ")
  # Remove Tabs and Spaces - easier to read
  sql <- gsub("\t", "", sql)
  sql <- gsub("[[:space:]]+", " ", sql)
  
  sql
}
