#' Obtain a connection to a PostGreSQL database.
#'
#' By default, this function will read from the `development` environment.
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
#' @param env character. What environment to use. The default is
#'   `"development"`.
#' @param verbose logical. Whether or not to print messages indicating
#'   loading is in progress.
#' @return JDBCConnection representing the activated PostGreSQL connection.
#' @export
postgresql_connection <- function(database.yml, env = 'development',
                                  verbose = TRUE, strict = TRUE) {
  stopifnot(is.character(env))
  if (is.null(database.yml)) { if (strict) stop('database.yml is NULL') else return(NULL) }
  if (!file.exists(database.yml)) {
    if (strict) stop("Provided database.yml file does not exist: ", database.yml)
    else return(NULL)
  }

  if (verbose) message("* Loading postgresql connection...\n")
  database.yml <- paste(readLines(database.yml), collapse = "\n")
  config.database <- yaml.load(database.yml)
  if (!env[1] %in% names(config.database))
    stop(pp("Unable to load database settings from config/database.yml ",
            "for environment '#{paste(env, collapse = '/')}'"))
  config.database <- config.database[[env]]
  
  jdbc.jar <- file.path(find.package('berdie'), 'vendor', 'jars', 
                        'postgresql-9.2-1003.jdbc4.jar')
  pgsql <- RJDBC::JDBC("org.postgresql.Driver", jdbc.jar, "`")
  stopifnot(all(c('database', 'username') %in% names(config.database)))
  conn.url <- c('jdbc:postgresql://', config.database$host %||% '127.0.0.1', ':',
                config.database$port %||% '5432', '/', config.database$database, '?user=',
                config.database$username, '&password=', config.database$password %||% '')
  if ('ssl' %in% names(config.database) &&
      !identical(tolower(config.database$ssl), 'false'))
    conn.url <- c(conn.url, '&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory')
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


