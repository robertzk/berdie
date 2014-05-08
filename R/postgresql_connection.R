#' Obtain a connection to a PostGreSQL database.
#'
#' By current default, the database configuration will always be read
#' from the development environment.
#'
#' Your database.yml should look like:
#'
#' development:
#'   adapter: postgresql
#'   database: <name>
#'   host: <domain>
#'   port: <port #>
#'   username: <username>
#'   password: <password>
#'
#' @param database.yml character. The location of the database.yml file
#'   to use. This could, for example, be directory from a Rails project.
#' @param verbose logical. Whether or not to print messages indicating
#'   loading is in progress.
#' @return JDBCConnection representing the activated PostGreSQL connection.
#' @export
postgresql_connection <- function(database.yml, verbose = TRUE, strict = TRUE) {
  if (is.null(database.yml)) { if (strict) stop('database.yml is NULL') else return(NULL) }
  if (!file.exists(database.yml)) {
    if (strict) stop("Provided database.yml file does not exist: ", database.yml)
    else return(NULL)
  }

  if (verbose) message("* Loading postgresql connection...\n")
  database.yml <- paste(readLines(database.yml), collapse = "\n")
  config.database <- yaml.load(database.yml)
  if (!'development' %in% names(config.database))
    stop(pp("Unable to load database settings from config/database.yml ",
            "for environment '#{config.environment}'"))
  config.database <- config.database[['development']]
  
  jdbc.jar <- file.path(find.package('berdie'), 'vendor', 'jars', 
                        'postgresql-9.2-1003.jdbc4.jar')
  pgsql <- JDBC("org.postgresql.Driver", jdbc.jar, "`")
  conn.url <- c('jdbc:postgresql://', config.database$host, ':',
                config.database$port, '/', config.database$database, '?user=',
                config.database$username, '&password=', config.database$password,
                '&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory')
  conn.url <- paste(conn.url, collapse = '')
  database.connection <- tryCatch(dbConnect(pgsql, conn.url),
                                  error = function(e) e)
  if (inherits(database.connection, 'try-error'))  {
    if (strict) stop(database.connection)
    else return(NULL)
  }
  if (verbose) message("* Postgresql connection loaded...\n")
  set_cache(database.connection, 'last_connection')
  database.connection
}


