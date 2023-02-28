# Developed from IRkernel logging system
# https://github.com/IRkernel/IRkernel/blob/a862774dc0758b23ef4e911a3c0b1d9e550652bc/R/logging.r

# basic smdocker logging system
# Error messages only shown
# Levels: 4 = DEBUG, 3 = INFO/MSG, 2 = WARNING, 1 = ERROR

#' @importFrom utils modifyList flush.console
#' @importFrom jsonlite toJSON

#' @title smdocker logging system
#' @description Ability to configure smdocker logging system, through the use of smdocker
#' helper function \code{smdocker_log} or `R:base` \code{options} function. `options` configurable
#' parameters:
#' \itemize{
#'    \item{`smdocker.log_level`} {(integer): The minimum log level that should be tracked}
#'    \item{`smdocker.log_file`} {
#'        (character): path for logs to populate, default output logs to console.
#'    }
#'    \item{`smdocker.log_timestamp_fmt`} {(character): see [format.POSIXct()]}
#' }
#' @param level (integer): the level logging threshold.
#' \itemize{
#'     \item{4L :} {DEBUG}
#'     \item{3L :} {INFO}
#'     \item{2L :} {WARNING}
#'     \item{1L :} {ERROR}
#' }
#' @param file (character): path for logs to populate, default output logs to console.
#' @param timestamp_fmt (character): timestamp format, see [format.POSIXct()].
#' @examples
#' \dontrun{
#' # log to a file
#' temp_file <- tempfile()
#' smdocker_log(file = temp_file)
#'
#' # change log threshold to INFO
#' smdocker_log(level = 3L)
#'
#' # reset to default config
#' smdocker_log()
#'
#' # options() equivalents:
#'
#' # log to a file
#' temp_file <- tempfile()
#' options(smdocker.log_file = temp_file)
#'
#' # change log threshold to INFO
#' options(smdocker.log_level = 3L)
#' }
#' @return \code{NULL} invisible
#' @export
smdocker_log <- function(level = 3L,
                         file = "",
                         timestamp_fmt = "%Y-%m-%d %H:%M:%OS3") {
  stopifnot(
    "`level` must be integer" = is.integer(level),
    "`file` must be character" = is.character(file),
    "`timestamp_fmt` must be character" = is.character(timestamp_fmt)
  )
  # create directory if doesn't exist
  if (grepl("/", file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  }

  log_config <- list(
    smdocker.log_level = level,
    smdocker.log_file = file,
    smdocker.log_timestamp_fmt = timestamp_fmt
  )
  do.call(options, log_config)
  return(invisible())
}

# default log settings
smdocker_logging_opt <- list(
  smdocker.log_level = 3L,
  smdocker.log_file = "",
  smdocker.log_timestamp_fmt = "%Y-%m-%d %H:%M:%OS3"
)

# Help parse log messages that contain % i.e. encoded urls
parse_msg <- function(...) {
  if (length(list(...)) == 1) paste(...) else sprintf(...)
}

log_debug <- function(...) {
  if (isTRUE(getOption("smdocker.log_level") >= 4L)) {
    log_msg("DEBUG", parse_msg(...))
  }
}

log_info <- function(...) {
  if (isTRUE(getOption("smdocker.log_level") >= 3L)) {
    log_msg("INFO", parse_msg(...))
  }
}

log_warn <- function(...) {
  if (isTRUE(getOption("smdocker.log_level") >= 2L)) {
    log_msg("WARN", parse_msg(...))
  }
}

log_error <- function(...) {
  if (isTRUE(getOption("smdocker.log_level") >= 1L)) {
    log_msg("ERROR", parse_msg(...))
  }
}

log_msg <- function(lvl, msg) {
  log_file <- getOption("smdocker.log_file")
  now <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  cat(
    sprintf("%s [%s]: %s\n", log_color(lvl), now, msg),
    file = log_file,
    append = TRUE
  )
  on.exit(flush.console())
}

log_params <- function(msg, params) {
  log_debug("%s:\n%s", msg, toJSON(params, pretty = T, auto_unbox = T))
}

log_color <- function(lvl) {
  color <- switch(lvl,
    DEBUG = style_debug,
    INFO = style_info,
    WARN = style_warn,
    ERROR = style_error,
    stop("unknown level: ", lvl)
  )
  color(lvl)
}

init_log_config <- function() {
  #---- set up logging ----
  log_opt_name <- names(smdocker_logging_opt)

  # check R options for log settings
  r_options <- lapply(log_opt_name, getOption)
  names(r_options) <- log_opt_name
  smdocker_logging_opt <- modifyList(
    smdocker_logging_opt, Filter(Negate(is.null), r_options)
  )

  # check environment variables for log settings
  env_options <- lapply(
    gsub(".", "_", toupper(log_opt_name), fixed = TRUE),
    Sys.getenv,
    unset = NA
  )
  names(env_options) <- c(log_opt_name)
  smdocker_logging_opt <- modifyList(
    smdocker_logging_opt, Filter(Negate(is.na), env_options)
  )
  # ensure log level is an integer
  smdocker_logging_opt$smdocker.log_level <- as.integer(smdocker_logging_opt$smdocker.log_level)

  do.call(options, smdocker_logging_opt)
}

init_log_styles <- function() {
  # set up log colors
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()) {
    style_error <- crayon::make_style("#BB3333", colors = 256)
    style_warn <- crayon::make_style("#EEBB50", colors = 256)
    style_info <- function(...) paste(...)
    style_debug <- crayon::make_style("#808080", grey = TRUE)
  } else {
    style_error <- function(...) paste(...)
    style_warn <- style_error
    style_info <- style_error
    style_debug <- style_error
  }

  # make color functions available inside the package
  assign("style_error", style_error, envir = parent.env(environment()))
  assign("style_warn", style_warn, envir = parent.env(environment()))
  assign("style_info", style_info, envir = parent.env(environment()))
  assign("style_debug", style_debug, envir = parent.env(environment()))
}
