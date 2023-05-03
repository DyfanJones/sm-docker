#' @include utils.R
#' @include logs.R

#' @importFrom jsonlite read_json
#' @importFrom paws.developer.tools codebuild
#' @importFrom paws.security.identity sts
#' @importFrom paws.compute ecr

codebuild_project_init <- function(s3_location,
                                   role,
                                   repository = NULL,
                                   compute_type = NULL,
                                   vpc_config = NULL) {
  metadata <- .get_studio_metadata()
  metadata$s3_location <- s3_location
  metadata$role <- role
  metadata$repo_name <- NULL
  metadata$compute_type <- compute_type %||% "BUILD_GENERAL1_SMALL"
  metadata$vpc_config <- vpc_config

  if (!is.null(repository)) {
    repo_split <- str_split(repository, ":", 2)[[1]]
    metadata$repo_name <- repo_split[1]
    metadata$tag <- repo_split[2]
  }

  if (!is.null(metadata$domain_id) & !is.null(metadata$user_profile_name)) {
    project_name_prefix <- sprintf(
      "sagemaker-studio-%s-%s-",
      metadata$domain_id,
      metadata$user_profile_name
    )
    project_name_prefix <- (
      if (nchar(project_name_prefix) > 239) {
        substr(project_name_prefix, 1, 238)
      } else {
        project_name_prefix
      })
    metadata$project_name <- paste0(project_name_prefix, uuid::UUIDgenerate())
    if (is.null(metadata$repo_name)) {
      metadata$repo_name <- sprintf(
        "sagemaker-studio-%s",
        metadata$domain_id
      )
      metadata$tag <- metadata$user_profile_name
    }
  } else {
    metadata$project_name <- paste0(
      "sagemaker-studio-image-build-",
      uuid::UUIDgenerate()
    )
    if (is.null(metadata$repo_name)) {
      metadata$repo_name <- "sagemaker-studio"
      metadata$tag <- "latest"
    }
  }

  return(metadata)
}

.get_studio_metadata <- function() {
  metadata_file_path <- "/opt/ml/metadata/resource-metadata.json"
  if (!file.exists(metadata_file_path)) {
    return(list(NULL, NULL))
  }

  metadata <- read_json(metadata_file_path)
  return(list(
    domain_id = metadata$DomainId,
    user_profile_name = metadata$UserProfileName
  ))
}

codebuild_create_project <- function(metadata) {
  config <- smdocker_config()
  client <- codebuild(config)
  region <- config$region

  caller_identity <- sts(config)$get_caller_identity()
  account <- caller_identity[["Account"]]
  partition <- str_split(caller_identity[["Arn"]], ":")[[1]][[2]]

  args <- list(
    "name" = metadata$project_name,
    "description" = sprintf(
      "Build the image for %s in SageMaker Studio",
      metadata$repo_name
    ),
    "source" = list("type" = "S3", "location" = metadata$s3_location),
    "artifacts" = list("type" = "NO_ARTIFACTS"),
    "environment" = list(
      "type" = "LINUX_CONTAINER",
      "image" = "aws/codebuild/standard:4.0",
      "computeType" = metadata$compute_type,
      "environmentVariables" = list(
        list("name" = "AWS_DEFAULT_REGION", "value" = region),
        list("name" = "AWS_ACCOUNT_ID", "value" = account),
        list("name" = "IMAGE_REPO_NAME", "value" = metadata$repo_name),
        list("name" = "IMAGE_TAG", "value" = metadata$tag)
      ),
      "privilegedMode" = TRUE
    ),
    "serviceRole" = sprintf(
      "arn:%s:iam::%s:role/%s", partition, account, metadata$role
    )
  )

  if (!is.null(metadata$vpc_config)) {
    args[["vpcConfig"]] <- metadata$vpc_config
  }

  log_params("Create Codebuild project", args)
  do.call(client$create_project, args)
  return(invisible(NULL))
}

.create_repo_if_required <- function(metadata) {
  config <- smdocker_config()
  client <- ecr(config)
  tryCatch(
    {
      client$create_repository(repositoryName = metadata$repo_name)
      log_info("Created ECR Repository %s", metadata$repo_name)
    },
    error = function(err) {
      code <- paws_error_code(err)
      if (code == "RepositoryAlreadyExistsException") {
        return(NULL)
      }
      stop(err)
    }
  )
}

codebuild_start_build <- function(metadata) {
  config <- smdocker_config()
  client <- codebuild(config)

  response <- client$start_build(projectName = metadata$project_name)
  return(response[["build"]][["id"]])
}

.wait_for_build <- function(build_id, poll_seconds = 10) {
  config <- smdocker_config()
  client <- codebuild(config)
  status <- client$batch_get_builds(ids = list(build_id))
  while (status$builds[[1]]$buildStatus == "IN_PROGRESS") {
    writeLines(".", sep = "")
    flush(stdout())
    Sys.sleep(poll_seconds)
    status <- client$batch_get_builds(ids = list(build_id))
  }
  writeLines("")
  log_info("Build complete, status = %s", status[["builds"]][[1]][["buildStatus"]])
  log_info("Logs at %s", status[["builds"]][[1]][["logs"]][["deepLink"]])
}

.get_image_uri <- function(metadata) {
  config <- smdocker_config()
  client <- ecr(config)
  tryCatch(
    {
      repository_uri <- client$describe_repositories(
        repositoryNames = list(metadata$repo_name)
      )[["repositories"]][[1]][["repositoryUri"]]
      return(sprintf("%s:%s", repository_uri, metadata$tag))
    },
    error = function(err) {
      log_info("Unable to get Image URI. Error: %s", err$message)
    }
  )
}


codebuild_build <- function(metadata, log = TRUE) {
  .create_repo_if_required(metadata)
  id <- codebuild_start_build(metadata)
  if (log) {
    logs_for_build(id, wait = TRUE)
  } else {
    .wait_for_build(id)
  }
  image_uri <- .get_image_uri(metadata)
  if (!is.null(image_uri)) {
    log_info("Image URI: %s", image_uri)
  }
  return(image_uri)
}
