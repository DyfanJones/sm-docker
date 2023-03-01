self <- new.env(parent = emptyenv())

#' @include utils.R

#' @title Set `paws` `config` across `smdocker` package
#' @description This function sets up `paws` client config list for all `AWS` calls.
#' This function only needs to be used when changing default settings when
#' connecting to `AWS`.
#' @param aws_access_key_id (character): AWS access key ID
#' @param aws_secret_access_key (character): AWS secret access key
#' @param aws_session_token (character): AWS temporary session token
#' @param region_name (character): Default region when creating new connections
#' @param profile_name (character): The name of a profile to use. If not given,
#'              then the default profile is used.
#' @param disable_ssl (logical): Whether or not to use `SSL`. By default, `SSL` is used.
#' @param anonymous (logical): Set up anonymous credentials when connecting to `AWS`.
#' @param refresh (logical): Refresh cached smdocker `config`
#' @param ... Other parameters within \code{paws} client.
#' @examples
#' # Set up connection using profile
#' smdocker_config(profile_name = "smdocker_example")
#'
#' # Reset connection to connect to a different region
#' smdocker_config(
#'   profile_name = "smdocker_example",
#'   region_name = "us-east-1",
#'   refresh = TRUE
#' )
#' @return Invisible list, containing credentials for `paws` clients.
#' @seealso \link[paws.compute]{ecr} \link[paws.developer.tools]{codebuild}
#' \link[paws.machine.learning]{sagemaker} \link[paws.management]{cloudwatchlogs}
#' \link[paws.security.identity]{iam} \link[paws.security.identity]{sts}
#' \link[paws.storage]{s3}
#' @export
smdocker_config <- function(aws_access_key_id = NULL,
                            aws_secret_access_key = NULL,
                            aws_session_token = NULL,
                            region_name = NULL,
                            profile_name = NULL,
                            disable_ssl = FALSE,
                            anonymous = FALSE,
                            refresh = FALSE,
                            ...) {
  config <- NULL
  if (!refresh) {
    config <- self$config
  }
  if (is.null(config)) {
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
      "`disable_ssl` is required to be a character vector" = (
        is.logical(disable_ssl)
      ),
      "`anonymous` is required to be a logical vector" = (
        is.logical(anonymous)
      )
    )
    region_name <- region_name %||% get_region(profile_name)
    config <- .cred_set(
      aws_access_key_id,
      aws_secret_access_key,
      aws_session_token,
      profile_name,
      region_name,
      disable_ssl,
      anonymous,
      ...
    )
    assign("config", config, envir = self)
  }
  return(invisible(config))
}

.cred_set <- function(aws_access_key_id,
                      aws_secret_access_key,
                      aws_session_token,
                      profile_name,
                      region_name,
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
  config$disable_ssl <- disable_ssl

  return(modifyList(config, list(...)))
}
