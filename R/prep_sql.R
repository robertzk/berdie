#' Converts SQL files into a format readable by the run_query() function.
#' @param path character.  The filepath of the SQL file to be formatted
#' @return string of SQL query with '--' comments removed and spacing reduced 
#'  

prep_sql <- function(path) {
  # Read The SQL File
  lines <- readLines(path)
  # Remove Comments
  vec <- vapply(lines, gsub, character(1), pattern = "--.*$", replacement = "")
  # Collapse into 1 line
  sql <- paste(vec, collapse=" ")
  # Remove Tabs and Spaces - easier to read
  sql <- gsub("\t","",sql)
  sql <- gsub("[[:space:]]+", " ", sql)
  
  sql
}
