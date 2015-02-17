#' Obtain a connection to a database.
#'
#' By default, this function will read from the `cache` environment.
#'
#' Your database.yml should look like:
#'
#' development:
#'   adapter: PostgreSQL
#'   username: <username>
#'   password: <password>
#'   database: <name>
#'   host: <domain>
#'   port: <port #>
#'
#' @param database.yml character. The location of the database.yml file
#'   to use. This could, for example, some file in the config directory.
#' @param env character. What environment to use. The default is
#'   `"cache"`.
#' @param verbose logical. Whether or not to print messages indicating
#'   loading is in progress.
#' @return the database connection.
db_connection <- function(database.yml, env = "cache",
                          verbose = TRUE, strict = TRUE) {
  if (is.null(database.yml)) { if (strict) stop('database.yml is NULL') else return(NULL) }
  if (!file.exists(database.yml)) {
    if (strict) stop("Provided database.yml file does not exist: ", database.yml)
    else return(NULL)
  }

  if (verbose) message("* Loading database connection...\n")
  database.yml <- paste(readLines(database.yml), collapse = "\n")
  config.database <- yaml::yaml.load(database.yml)
  if (!missing(env) && !is.null(env)) {
    if (!env %in% names(config.database))
      stop(paste0("Unable to load database settings from database.yml ",
              "for environment '", env, "'"))
    config.database <- config.database[[env]]
  } else if (missing(env) && length(config.database) == 1) {
    config.database <- config.database[[1]]
  }
  # Authorization arguments needed by the DBMS instance
  # TODO: (RK) Inform user if they forgot database.yml entries.
  do.call(DBI::dbConnect, append(list(drv = DBI::dbDriver(config.database$adapter)), 
    config.database[!names(config.database) %in% "adapter"]))
}

