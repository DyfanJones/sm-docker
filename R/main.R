#' @include builder.R
#' @include sm_role.R

#' @title AWS CodeBuild and pushing to Amazon ECR
#' @param repository The ECR repository:tag for the image
#' (default: sagemaker-studio-${domain_id}:latest)
#' @param compute_type The CodeBuild compute type (default: BUILD_GENERAL1_SMALL)
#' @param role The IAM role name for CodeBuild to use (default: the Studio execution role).
#' @param dir Directory to build
#' @param bucket The S3 bucket to use for sending data to CodeBuild (if None,
#' use the SageMaker SDK default bucket).
#' @param vpc_id The Id of the VPC that will host the CodeBuild Project
#' (such as vpc-05c09f91d48831c8c).
#' @param subnet_ids The comma-separated list of subnet ids for the CodeBuild Project
#' (such as subnet-0b31f1863e9d31a67)
#' @param security_group_ids The comma-separated list of security group ids for
#' the CodeBuild Project (such as sg-0ce4ec0d0414d2ddc).
#' @param log Show the logs of the running CodeBuild build
#' @param ... docker build parameters
#' <https://docs.docker.com/engine/reference/commandline/build/#options>
#' (NOTE: use "_" instead of "-" for example: docker optional parameter
#' \code{build-arg} becomes \code{build_arg})
#' @export
sm_build <- function(repository,
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

  # format docker parameters
  names(extra_args) <- ifelse(
    nchar(extra_args) == 1, paste0("-", names(extra_args)),
    paste0("--", names(extra_args))
  )
  names(extra_args) <- gsub("_", "-", names(extra_args))
  extra_args <- lapply(
    extra_args,
    function(arg) if (is.logical(arg)) tolower(arg) else arg
  )

  if (lengths(regmatches(repository, gregexpr(":", repository))) > 1) {
    stop(sprintf(
      "Error parsing reference: %s is not a valid repository/tag", repository
    ), call. = F)
  }

  build_image(
    repository, get_role(role), dir, bucket, compute_type,
    construct_vpc_config(vpc_id, subnet_ids, security_group_ids), extra_args,
    log = log
  )
}

construct_vpc_config <- function(vpc_id = NULL,
                                 subnet_ids = list(),
                                 security_group_ids = list()) {
  if (is.null(vpc_id)) {
    return(NULL)
  } else {
    if (length(subnet_ids) == 0 | length(security_group_ids)) {
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
