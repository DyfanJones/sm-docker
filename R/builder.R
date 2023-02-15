#' @importFrom zip zip

upload_zip_file <- function(repo_name,
                            bucket = NULL,
                            extra_args = list(),
                            dir = ".") {
  self <- paws_session()
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

  on.exit({
    fs::file_delete(tmp)
    fs::dir_delete(tmp_dir)
    setwd(origdir)
  })

  dir <- normalizePath(dir)

  fs::dir_create(tmp_dir)
  fs::dir_copy(
    dir,
    tmp_dir
  )

  setwd(file.path(tmp_dir, basename(dir)))
  buildspec_replaced <- readLines(
    system.file("buildspec.template.yml", package = "smdocker")
  )
  buildspec_replaced <- gsub("REPLACE_ME_BUILD_ARGS", extra_args, buildspec_replaced)
  writeLines(buildspec_replaced, file.path(tmp_dir, basename(dir), "buildspec.yml"))

  zip::zip(
    zipfile = tmp,
    files = list.files(recursive = T)
  )

  client <- paws::s3(self$config)
  client$put_object(
    Bucket = bucket,
    Key = key,
    Body = tmp
  )
  return(list(Bucket = bucket, Key = key))
}


sagemaker_default_bucket <- function() {
  self <- paws_session()
  region <- self$config$region
  account <- paws::sts(self$config)$get_caller_identity()[["Account"]]
  default_bucket <- sprintf("sagemaker-%s-%s", region, account)
  .create_s3_bucket_if_it_does_not_exist(
    bucket_name = default_bucket, region = region
  )
  return(defaukt_bucket)
}

.create_s3_bucket_if_it_does_not_exist <- function(bucket_name, region) {
  self <- paws_session()
  client <- paws::s3(self$config)
  resp <- tryCatch(
    {
      client$head_bucket(Bucket = bucket_name)
    },
    error = function(e) {
      e
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
      error = function(e) {
        error_code <- paws_error_code(e)
        message <- e$error_response$Message
        if (identical(error_code, "BucketAlreadyOwnedByYou")) {
          invisible(NULL)
        } else if (identical(error_code, "OperationAborted") &&
          grepl("conflicting conditional operation", message)) {
          invisible(NULL)
        } else {
          stop(e)
        }
      }
    )
  }
}

delete_zip_file <- function(bucket, key) {
  self <- paws_session()
  client <- paws::s3(self$config)
  client$delete_object(Bucket = bucket, Key = key)
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
    paste(
      names(extra_args),
      lapply(extra_args, paste, collapse = " "),
      collapse = " "
    ),
    dir
  )

  on.exit(delete_zip_file(s3$Bucket, s3$Key))

  metadata <- code_build_project_init(
    s3_location = sprintf("%s/%s", s3$Bucket, s3$Key),
    role = role,
    repository = repository,
    compute_type = compute_type,
    vpc_config = vpc_config
  )
  log_params("create_project", metadata)
  .create_project(metadata)
  build(metadata, log)
  return(invisible())
}
