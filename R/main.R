#' @include builder.R
#' @include sm_role.R

#' @title Use `AWS CodeBuild` to build docker images and push them to Amazon `ECR`
#' @description This function takes a directory containing a
#' [dockerfile](https://docs.docker.com/engine/reference/builder/), and builds it on
#' [`AWS CodeBuild`](https://aws.amazon.com/codebuild/). The resulting image is
#' then stored in [`AWS ECR`](https://aws.amazon.com/ecr/) for later use.
#' @param repository (character): The `ECR` repository:tag for the image
#' (default: `sagemaker-studio-${domain_id}:latest`)
#' @param compute_type (character): The [`CodeBuild`](https://aws.amazon.com/codebuild/) compute type (default: `BUILD_GENERAL1_SMALL`)
#' @param role (character): The `IAM` role name for `CodeBuild` to use (default: the Studio execution role).
#' @param dir (character): Directory to build
#' @param bucket (character): The S3 bucket to use for sending data to `CodeBuild` (if None,
#' use the `SageMaker SDK` default bucket).
#' @param vpc_id (character): The Id of the `VPC` that will host the `CodeBuild` Project
#' (such as `vpc-05c09f91d48831c8c`).
#' @param subnet_ids (list): List of `subnet` ids for the `CodeBuild` Project
#' (such as `subnet-0b31f1863e9d31a67`)
#' @param security_group_ids (list): List of security group ids for
#' the `CodeBuild` Project (such as `sg-0ce4ec0d0414d2ddc`).
#' @param log (logical): Show the logs of the running `CodeBuild` build
#' @param ... docker build parameters
#' <https://docs.docker.com/engine/reference/commandline/build/#options>
#' (NOTE: use "_" instead of "-" for example: docker optional parameter
#' \code{build-arg} becomes \code{build_arg})
#' @examples
#' \dontrun{
#' # Execute on current directory.
#' sm_build()
#'
#' # Execute on different directory.
#' sm_build(dir = "my-project")
#'
#' # Add extra docker arguments
#' sm_build(
#'   file = "/path/to/Dockerfile",
#'   build_arg = "foo=bar"
#' )
#' }
#' @return invisible character vector of `AWS ECR` image `uri`.
#' @export
sm_build <- function(repository = NULL,
                     compute_type = c(
                       "BUILD_GENERAL1_SMALL", "BUILD_GENERAL1_MEDIUM",
                       "BUILD_GENERAL1_LARGE", "BUILD_GENERAL1_2XLARGE"
                     ),
                     role = NULL,
                     dir = ".",
                     bucket = NULL,
                     vpc_id = NULL,
                     subnet_ids = list(),
                     security_group_ids = list(),
                     log = TRUE,
                     ...) {
  stopifnot(
    "`repository` is required to be a character vector" = (
      is.character(repository) || is.null(repository)
    ),
    "`role` is required to be a character vector" = (
      is.character(role) || is.null(role)
    ),
    "`dir` is required to be a character vector" = (
      is.character(dir)
    ),
    "`bucket` is required to be a character vector" = (
      is.character(bucket) || is.null(bucket)
    ),
    "`vpc_id` is required to be a character vector" = (
      is.character(vpc_id) || is.null(vpc_id)
    ),
    "`subnet_ids` is required to be a list" = (
      is.list(subnet_ids)
    ),
    "`security_group_ids` is required to be a list" = (
      is.list(security_group_ids)
    ),
    "`log` is required to be a logical" = (
      is.logical(log)
    )
  )

  extra_args <- list(...)

  compute_type <- match.arg(compute_type)

  # Validate that the path to the Dockerfile is within the PWD.
  if (any(names(extra_args) %in% c("f", "file"))) {
    file_path <- extra_args[["file"]] %||% extra_args[["f"]]
    if (!file.exists(file.path(dir, file_path))) {
      stop(sprintf(
        "The value of the -f/file argument [%s] is outside the working directory [%s]",
        file_path, dir
      ))
    }
  }

  if (!is.null(repository)) {
    repo_len <- lengths(regmatches(repository, gregexpr(":", repository)))
    if (repo_len != 1) {
      stop(sprintf(
        "%s is not a valid repository:tag", repository
      ), call. = F)
    }
  }

  image_uri <- build_image(
    repository, get_role(role), dir, bucket, compute_type,
    construct_vpc_config(vpc_id, subnet_ids, security_group_ids), extra_args,
    log = log
  )
  return(invisible(image_uri))
}

construct_vpc_config <- function(vpc_id = NULL,
                                 subnet_ids = list(),
                                 security_group_ids = list()) {
  if (is.null(vpc_id)) {
    return(NULL)
  } else {
    if (length(subnet_ids) == 0 | length(security_group_ids) == 0) {
      stop(
        "Invalid input of the VPC configuration. Please either provide all of the ",
        "VPC arguments or none of them, ",
        "in which case the CodeBuild Project, by default, will not run within a VPC.",
        call. = F
      )
    }
    vpc_config <- list(
      "vpcId" = vpc_id,
      "subnets" = subnet_ids,
      "securityGroupIds" = security_group_ids
    )
  }
  return(vpc_config)
}
