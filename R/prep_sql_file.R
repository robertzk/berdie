#' Converts SQL files into a format readable by the run_query() function.
#' @param path character.  The filepath of the SQL file to be formatted
#' @return string of SQL query with '--' comments removed and spacing reduced 
#'  

prep_sql_file <- function(path) {
  # Read The SQL File
  lines <- readLines(path)
  # Remove Comments
  vec = vector(length = length(lines))
  vec = sapply(lines, function(x) gsub("--.*$","",x), USE.NAMES = F)
  # Collapse into 1 line
  sql <- paste(vec, collapse=" ")
  # Remove Tabs - easier to read
  sql <- gsub("\t","",sql)
  sql <- gsub("  "," ",sql)
  sql <- gsub("  "," ",sql)
  
  return(sql)
}