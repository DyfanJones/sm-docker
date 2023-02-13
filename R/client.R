sm_docker_cache <- new.env(parent = emptyenv())

#' @include utils.R

paws_session <- function(aws_access_key_id = NULL,
                         aws_secret_access_key = NULL,
                         aws_session_token = NULL,
                         region_name = NULL,
                         profile_name = NULL,
                         endpoint = NULL,
                         disable_ssl = FALSE,
                         anonymous = FALSE,
                         refresh = FALSE,
                         ...) {
  self <- NULL
  if (!refresh) {
    self <- sm_docker_cache
  }
  if (length(self) == 0) {
    stopifnot(
      "`aws_access_key_id` is required to be a character vector" = (
        is.character(aws_access_key_id) || is.null(aws_access_key_id)
      ),
      "`aws_secret_access_key` is required to be a character vector" = (
        is.character(aws_secret_access_key) || is.null(aws_secret_access_key)
      ),
      "`aws_session_token` is required to be a character vector" = (
        is.character(aws_session_token) || is.null(aws_session_token)
      ),
      "`region_name` is required to be a character vector" = (
        is.character(region_name) || is.null(region_name)
      ),
      "`profile_name` is required to be a character vector" = (
        is.character(profile_name) || is.null(profile_name)
      ),
      "`endpoint` is required to be a character vector" = (
        is.character(endpoint) || is.null(endpoint)
      ),
      "`disable_ssl` is required to be a character vector" = (
        is.logical(disable_ssl)
      ),
      "`anonymous` is required to be a logical vector" = (
        is.logical(anonymous)
      )
    )
    region_name <- region_name %||% get_region(profile_name)
    self[["config"]] <- .cred_set(
      aws_access_key_id,
      aws_secret_access_key,
      aws_session_token,
      profile_name,
      region_name,
      endpoint,
      disable_ssl,
      anonymous,
      ...
    )
  }
  return(invisible(self))
}

.cred_set <- function(aws_access_key_id,
                      aws_secret_access_key,
                      aws_session_token,
                      profile_name,
                      region_name,
                      endpoint,
                      disable_ssl,
                      anonymous,
                      ...) {
  add_list <- function(x) if (length(x) == 0) NULL else x
  config <- list()
  credentials <- list()
  cred <- list()

  cred$access_key_id <- aws_access_key_id
  cred$secret_access_key <- aws_secret_access_key
  cred$session_token <- aws_session_token

  credentials$creds <- add_list(cred)
  credentials$profile <- profile_name
  credentials$anonymous <- anonymous
  config$credentials <- add_list(credentials)
  config$region <- region_name
  config$endpoint <- endpoint
  config$disable_ssl <- disable_ssl

  return(modifyList(config, list(...)))
}
