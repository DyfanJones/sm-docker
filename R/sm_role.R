#' @importFrom jsonlite read_json
#' @importFrom paws.machine.learning sagemaker
#' @importFrom paws.security.identity sts iam

NOTEBOOK_METADATA_FILE <- "/opt/ml/metadata/resource-metadata.json"

get_role <- function(role = NULL) {
  if (is.null(role)) {
    role <- sagemaker_get_execution_role()
  }
  return(gsub("arn:aws:iam::[0-9]+:role/", "", role, perl = T))
}

################################################################################
# Developed from:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/session.py#L5396-L5417
# See the NOTICE file at the top of this package for attribution.
################################################################################

#' @title Return the `AWS ARN` execution role from `AWS SageMaker`
#' @description Return the [`AWS ARN` execution role](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html)
#' from `AWS SageMaker`
#' @return Character containing the `AWS ARN` role retrieved from `AWS SageMaker`
#' @export
sagemaker_get_execution_role <- function() {
  arn <- sagemaker_get_caller_identity_arn()
  if (grepl(":role/", arn)) {
    return(arn)
  }
  message <- sprintf(
    paste(
      "The current AWS identity is not a role: %s, therefore it cannot",
      "be used as a SageMaker execution role"
    ),
    arn
  )
  stop(message, call. = F)
}

################################################################################
# Developed from:
# https://github.com/aws/sagemaker-python-sdk/blob/master/src/sagemaker/session.py#L3977-L4056
# See the NOTICE file at the top of this package for attribution.
################################################################################

sagemaker_get_caller_identity_arn <- function() {
  config <- smdocker_config()
  client <- sagemaker(config)

  if (file.exists(NOTEBOOK_METADATA_FILE)) {
    metadata <- read_json(NOTEBOOK_METADATA_FILE)
    instance_name <- metadata[["ResourceName"]]
    domain_id <- metadata[["DomainId"]]
    user_profile_name <- metadata[["UserProfileName"]]
    space_name <- metadata[["SpaceName"]]

    tryCatch(
      {
        if (is.null(domain_id)) {
          instance_desc <- client$describe_notebook_instance(NotebookInstanceName = instance_name)
          return(instance_desc$RoleArn)
        }

        # In Space app, find execution role from DefaultSpaceSettings on domain level
        if (!is.null(space_name)) {
          domain_desc <- client$describe_domain(DomainId=domain_id)
          return(domain_desc[["DefaultSpaceSettings"]][["ExecutionRole"]])
        }

        user_profile_desc <- client$describe_user_profile(
          DomainId = domain_id, UserProfileName = user_profile_name
        )

        # First, try to find role in userSettings
        if (!islistempty(user_profile_desc[["UserSettings"]][["ExecutionRole"]])) {
          return(user_profile_desc[["UserSettings"]][["ExecutionRole"]])
        }

        # If not found, fallback to the domain
        domain_desc <- client$describe_domain(DomainId = domain_id)
        return(domain_desc[["DefaultUserSettings"]][["ExecutionRole"]])
      },
      error = function(e) {
        log_debug(
          "Couldn't call 'describe_notebook_instance' to get the Role \nARN of the instance %s.",
          instance_name
        )
      }
    )
  }

  assumed_role <- sts(
    modifyList(config, list(
      region = config$region,
      endpoint = sts_regional_endpoint(config$region)
    ))
  )$get_caller_identity()[["Arn"]]

  role <- gsub("^(.+)sts::(\\d+):assumed-role/(.+?)/.*$", "\\1iam::\\2:role/\\3", assumed_role)

  # Call IAM to get the role's path
  role_name <- substr(role, gregexpr("/", role)[[1]][1] + 1, nchar(role))
  tryCatch(
    {
      role <- iam(config)$get_role(RoleName = role_name)[["Role"]][["Arn"]]
    },
    error = function(e) {
      log_warn(
        "Couldn't call 'get_role' to get Role ARN from role name %s to get Role path.",
        role_name
      )
      # This conditional has been present since the inception of SageMaker
      # Guessing this conditional's purpose was to handle lack of IAM permissions
      # https://github.com/aws/sagemaker-python-sdk/issues/2089#issuecomment-791802713
      if (grepl("AmazonSageMaker-ExecutionRole", assumed_role)) {
        log_warn(paste(
          "Assuming role was created in SageMaker AWS console,",
          "as the name contains `AmazonSageMaker-ExecutionRole`.",
          "Defaulting to Role ARN with service-role in path.",
          "If this Role ARN is incorrect, please add",
          "IAM read permissions to your role or supply the",
          "Role Arn directly."
        ))
        role <<- gsub(
          "^(.+)sts::(\\d+):assumed-role/(.+?)/.*$",
          "\\1iam::\\2:role/service-role/\\3",
          assumed_role
        )
        return(role)
      }
    }
  )
  return(role)
}
