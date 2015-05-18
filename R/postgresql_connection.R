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
#' @return PqConnection representing the activated PostGreSQL connection.
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

  database.connection <-
    tryCatch(dbConnect(RPostgres::Postgres(), dbname = config.database$database,
                                              host = config.database$host,
                                              port = config.database$port %||% 5432,
                                              user = config.database$username,
                                              password = config.database$password),
             error = function(e) e)

  if (inherits(database.connection, 'try-error'))  {
    if (strict) stop(database.connection)
    else return(NULL)
  }
  if (verbose) message("* Postgresql connection loaded...\n")
  set_cache(database.connection, 'last_connection')
  database.connection
}
