#' @include code_build.R
#' @include logs.R
#' @include utils.R

#' @importFrom paws.security.identity sts
#' @importFrom paws.storage s3
#' @importFrom zip zip

upload_zip_file <- function(repo_name,
                            bucket = NULL,
                            extra_args = list(),
                            dir = ".") {
  config <- smdocker_config()
  if (is.null(bucket)) {
    bucket <- sagemaker_default_bucket()
  }

  random_suffix <- paste(
    sample(c(letters, LETTERS), size = 16, replace = T),
    collapse = ""
  )
  key <- sprintf("codebuild-sagemaker-container-%s.zip", random_suffix)

  tmp_dir <- tempfile()
  tmp <- tempfile(fileext = ".zip")

  origdir <- getwd()
  on.exit(setwd(origdir), add = TRUE)
  on.exit(unlink(c(tmp, tmp_dir), recursive = TRUE, force = TRUE), add = TRUE)

  dir <- normalizePath(dir)

  file_ls_src <- list.files(dir, recursive = T, all.files = T, full.names = T)
  file_ls_dest <- gsub(dir, tmp_dir, file_ls_src, fixed = T)
  dir_ls <- list.dirs(dir, recursive = T, full.names = F)

  # copy directory to temporary location
  lapply(file.path(tmp_dir, dir_ls), dir.create, recursive = TRUE)
  lapply(seq_along(file_ls_src), function(i) {
    file.copy(
      file_ls_src[i], file_ls_dest[i]
    )
  })

  setwd(tmp_dir)
  buildspec_replaced <- readLines(
    system.file("buildspec.template.yml", package = "smdocker")
  )
  buildspec_replaced <- gsub("REPLACE_ME_BUILD_ARGS", extra_args, buildspec_replaced)
  writeLines(buildspec_replaced, file.path(tmp_dir, "buildspec.yml"))

  zip::zip(
    zipfile = tmp,
    files = list.files(recursive = T)
  )

  client <- paws.storage::s3(config)
  client$put_object(
    Bucket = bucket,
    Key = key,
    Body = tmp
  )
  log_debug("Uploaded Codebuild project zip file to: s3://%s/%s", bucket, key)
  return(list(Bucket = bucket, Key = key))
}

sagemaker_default_bucket <- function() {
  config <- smdocker_config()
  region <- config$region
  account <- sts(config)$get_caller_identity()[["Account"]]
  default_bucket <- sprintf("sagemaker-%s-%s", region, account)
  .create_s3_bucket_if_it_does_not_exist(
    bucket_name = default_bucket, region = region
  )
  return(default_bucket)
}

.create_s3_bucket_if_it_does_not_exist <- function(bucket_name, region) {
  config <- smdocker_config()
  client <- s3(config)
  resp <- tryCatch(
    {
      client$head_bucket(Bucket = bucket_name)
    },
    error = function(error) {
      error
    }
  )

  # check if bucket exists: HTTP 404 bucket not found
  if (inherits(resp, "http_404")) {
    tryCatch(
      {
        client$create_bucket(
          Bucket = bucket_name,
          CreateBucketConfiguration = list(LocationConstraint = region)
        )
        log_info("Created S3 bucket: %s", bucket_name)
      },
      error = function(err) {
        error_code <- paws_error_code(err)
        message <- err$error_response$Message
        if (identical(error_code, "BucketAlreadyOwnedByYou")) {
          invisible(NULL)
        } else if (identical(error_code, "OperationAborted") &&
          grepl("conflicting conditional operation", message)) {
          invisible(NULL)
        } else {
          stop(err)
        }
      }
    )
  }
}

delete_zip_file <- function(bucket, key) {
  config <- smdocker_config()
  client <- s3(config)
  tryCatch(
    {
      client$delete_object(Bucket = bucket, Key = key)
    },
    http_403 = function(err) {
      log_error(err$message)
      log_error("Failed to delete: s3://%s/%s", bucket, key)
    }
  )
}

build_image <- function(repository,
                        role,
                        dir,
                        bucket,
                        compute_type,
                        vpc_config,
                        extra_args,
                        log = TRUE) {
  s3 <- upload_zip_file(
    repository, bucket,
    extra_docker_args(extra_args),
    dir
  )

  on.exit(delete_zip_file(s3$Bucket, s3$Key))

  metadata <- codebuild_project_init(
    s3_location = sprintf("%s/%s", s3$Bucket, s3$Key),
    role = role,
    repository = repository,
    compute_type = compute_type,
    vpc_config = vpc_config
  )
  codebuild_create_project(metadata)
  codebuild_build(metadata, log)
  return(invisible())
}
