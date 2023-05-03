test_that("check codebuild_project_init default", {
  mock_get_studio_metadata <- mock2(list(domain_id = "foo", user_profile_name = "bar"))
  mockery::stub(codebuild_project_init, ".get_studio_metadata", mock_get_studio_metadata)
  mockery::stub(codebuild_project_init, "uuid::UUIDgenerate", "aaaa-bbbb-cccc")
  actual <- codebuild_project_init("mybucket/location", "dummy")
  expect_equal(
    actual, list(
      domain_id = "foo",
      user_profile_name = "bar",
      s3_location = "mybucket/location",
      role = "dummy",
      compute_type = "BUILD_GENERAL1_SMALL",
      project_name = "sagemaker-studio-foo-bar-aaaa-bbbb-cccc",
      repo_name = "sagemaker-studio-foo",
      tag = "bar"
    )
  )
})

test_that("check codebuild_project_init default no studio metadata", {
  mock_get_studio_metadata <- mock2(list())
  mockery::stub(codebuild_project_init, ".get_studio_metadata", mock_get_studio_metadata)
  mockery::stub(codebuild_project_init, "uuid::UUIDgenerate", "aaaa-bbbb-cccc")
  actual <- codebuild_project_init("mybucket/location", "dummy")
  expect_equal(
    actual, list(
      s3_location = "mybucket/location",
      role = "dummy",
      compute_type = "BUILD_GENERAL1_SMALL",
      project_name = "sagemaker-studio-image-build-aaaa-bbbb-cccc",
      repo_name = "sagemaker-studio",
      tag = "latest"
    )
  )
})

test_that("check codebuild_project_init with repo", {
  mock_get_studio_metadata <- mock2(list())
  mockery::stub(codebuild_project_init, ".get_studio_metadata", mock_get_studio_metadata)
  mockery::stub(codebuild_project_init, "uuid::UUIDgenerate", "aaaa-bbbb-cccc")
  actual <- codebuild_project_init("mybucket/location", "dummy", "my-repo:latest")
  expect_equal(
    actual, list(
      s3_location = "mybucket/location",
      role = "dummy",
      compute_type = "BUILD_GENERAL1_SMALL",
      repo_name = "my-repo",
      tag = "latest",
      project_name = "sagemaker-studio-image-build-aaaa-bbbb-cccc"
    )
  )
})

test_that("check codebuild_project_init long project_name_prefix", {
  set.seed(123)
  mock_get_studio_metadata <- mock2(
    list(
      domain_id = paste(sample(letters, 200, replace = T), collapse = ""),
      user_profile_name = paste(sample(letters, 200, replace = T), collapse = "")
    )
  )
  mockery::stub(codebuild_project_init, ".get_studio_metadata", mock_get_studio_metadata)
  mockery::stub(codebuild_project_init, "uuid::UUIDgenerate", "aaaa-bbbb-cccc")
  actual <- codebuild_project_init("mybucket/location", "dummy", "my-repo:latest")
  expect_equal(
    actual, list(
      domain_id = "osncjrvketnvyzesyyichzgjisdnqkgulojmgiijwugufybehlmrayyfuoiozptfkhvvgpqvrqbdmevsytvynywchplyncngcwvzouehsjrjlbjvltnqnvchnsoxqwkgowzfngjefpxuwkdlnsygvzigbpmsxtogdahtypxvkpthcdtlvqjtkywhnumbkmnfyhlzdmnu",
      user_profile_name = "pwahhjyhruiggzjxvwzkaysjuvmkkytzgywztxyiiewnnfazjqzqzuguzitfrqeytcynabdjaeuyhwyumrjfgipqumuhugtrqabqbmxqcrbelymjfjjfppcdcwcpbowcigidbvzlikgmekaslbuqlcyrovcsyrqfjhjzmtfttiphgjqrhsjwkqlbsmmsvjpedrapqlub",
      s3_location = "mybucket/location",
      role = "dummy",
      compute_type = "BUILD_GENERAL1_SMALL",
      repo_name = "my-repo",
      tag = "latest",
      project_name = "sagemaker-studio-osncjrvketnvyzesyyichzgjisdnqkgulojmgiijwugufybehlmrayyfuoiozptfkhvvgpqvrqbdmevsytvynywchplyncngcwvzouehsjrjlbjvltnqnvchnsoxqwkgowzfngjefpxuwkdlnsygvzigbpmsxtogdahtypxvkpthcdtlvqjtkywhnumbkmnfyhlzdmnu-pwahhjyhruiggzjxvwzkaaaa-bbbb-cccc"
    )
  )
})

test_that("check .get_studio_metadata file exists", {
  mock_file_exists <- mock2(TRUE)
  mock_read_json <- mock2(list(DomainId = "foo", UserProfileName = "bar"))
  mockery::stub(.get_studio_metadata, "file.exists", mock_file_exists)
  mockery::stub(.get_studio_metadata, "read_json", mock_read_json)
  actual <- .get_studio_metadata()
  expect_equal(actual, list(domain_id = "foo", user_profile_name = "bar"))
})

test_that("check .get_studio_metadata file doesn't exist", {
  mock_file_exists <- mock2(FALSE)
  mockery::stub(.get_studio_metadata, "file.exists", mock_file_exists)
  actual <- .get_studio_metadata()
  expect_equal(actual, list(NULL, NULL))
})

test_that("check codebuild_create_project", {
  metadata <- list(
    domain_id = "qux",
    user_profile_name = "cho",
    s3_location = "mybucket/location",
    role = "dummy",
    compute_type = "BUILD_GENERAL1_SMALL",
    project_name = "sagemaker-studio-qux-cho-EoYncPXQKZnyzAeY",
    repo_name = "sagemaker-studio-qux",
    tag = "latest"
  )

  mock_smdocker_config <- mock2(list(region = "us-east-1"))

  # mock codebuild client
  mock_create_project <- mock2(list())
  mock_codebuild <- mock2(list(create_project = mock_create_project))

  # mock sts client
  mock_get_caller_identity <- mock2(list(Account = 123456789, Arn = "foo:bar"))
  mock_sts <- mock2(list(get_caller_identity = mock_get_caller_identity))

  mockery::stub(codebuild_create_project, "smdocker_config", mock_smdocker_config)
  mockery::stub(codebuild_create_project, "codebuild", mock_codebuild)
  mockery::stub(codebuild_create_project, "sts", mock_sts)

  actual <- codebuild_create_project(metadata)
  expect_equal(
    mock_arg(mock_create_project),
    list(
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
          list("name" = "AWS_DEFAULT_REGION", "value" = "us-east-1"),
          list("name" = "AWS_ACCOUNT_ID", "value" = 123456789),
          list("name" = "IMAGE_REPO_NAME", "value" = metadata$repo_name),
          list("name" = "IMAGE_TAG", "value" = metadata$tag)
        ),
        "privilegedMode" = TRUE
      ),
      "serviceRole" = "arn:bar:iam::123456789:role/dummy"
    )
  )
})

test_that("check codebuild_create_project with vpc", {
  metadata <- list(
    domain_id = "qux",
    user_profile_name = "cho",
    s3_location = "mybucket/location",
    role = "dummy",
    compute_type = "BUILD_GENERAL1_SMALL",
    project_name = "sagemaker-studio-qux-cho-EoYncPXQKZnyzAeY",
    repo_name = "sagemaker-studio-qux",
    tag = "latest",
    vpc_config = "vpc_dummy"
  )

  mock_smdocker_config <- mock2(list(region = "us-east-1"))

  # mock codebuild client
  mock_create_project <- mock2(list())
  mock_codebuild <- mock2(list(create_project = mock_create_project))

  # mock sts client
  mock_get_caller_identity <- mock2(list(Account = 123456789, Arn = "foo:bar"))
  mock_sts <- mock2(list(get_caller_identity = mock_get_caller_identity))

  mockery::stub(codebuild_create_project, "smdocker_config", mock_smdocker_config)
  mockery::stub(codebuild_create_project, "codebuild", mock_codebuild)
  mockery::stub(codebuild_create_project, "sts", mock_sts)

  actual <- codebuild_create_project(metadata)
  expect_equal(
    mock_arg(mock_create_project),
    list(
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
          list("name" = "AWS_DEFAULT_REGION", "value" = "us-east-1"),
          list("name" = "AWS_ACCOUNT_ID", "value" = 123456789),
          list("name" = "IMAGE_REPO_NAME", "value" = metadata$repo_name),
          list("name" = "IMAGE_TAG", "value" = metadata$tag)
        ),
        "privilegedMode" = TRUE
      ),
      "serviceRole" = "arn:bar:iam::123456789:role/dummy",
      "vpcConfig" = "vpc_dummy"
    )
  )
})

test_that("check create_repo_if_required doesn't exist", {
  mock_smdocker_config <- mock2(list())

  # mock ecr client
  mock_create_repository <- mock2(list())
  mock_ecr <- mock2(list(create_repository = mock_create_repository))

  mockery::stub(.create_repo_if_required, "ecr", mock_ecr)

  .create_repo_if_required(list(repo_name = "dummy"))

  expect_equal(mock_arg(mock_create_repository), list(repositoryName = "dummy"))
})

test_that("check create_repo_if_required exist", {
  mock_error <- structure(
    list(
      message = "error",
      error_response = list("Code" = "RepositoryAlreadyExistsException")
    ),
    class = c("error", "condition")
  )
  mock_smdocker_config <- mock2(list())
  # mock ecr client
  mock_create_repository <- mock2(stop(mock_error))
  mock_ecr <- mock2(list(create_repository = mock_create_repository))
  mock_paws_error_code <- mock2(side_effect = paws_error_code)

  mockery::stub(.create_repo_if_required, "ecr", mock_ecr)
  mockery::stub(.create_repo_if_required, "paws_error_code", mock_paws_error_code)

  .create_repo_if_required(list(repo_name = "dummy"))

  expect_equal(mock_arg(mock_paws_error_code), list(mock_error))
})

test_that("check create_repo_if_required error", {
  mock_error <- structure(
    list(
      message = "mock error",
      error_response = list("Code" = "RandomAWSError")
    ),
    class = c("error", "condition")
  )

  mock_smdocker_config <- mock2(list())
  # mock ecr client
  mock_create_repository <- mock2(stop(mock_error))
  mock_ecr <- mock2(list(create_repository = mock_create_repository))

  mockery::stub(.create_repo_if_required, "ecr", mock_ecr)

  expect_error(
    .create_repo_if_required(list(repo_name = "dummy")),
    "mock error"
  )
})


test_that("check codebuild_start_build", {
  mock_smdocker_config <- mock2(list())
  # mock codebuild client
  mock_start_build <- mock2(list(build = list(id = 1)))
  mock_codebuild <- mock2(list(start_build = mock_start_build))

  mockery::stub(codebuild_start_build, "codebuild", mock_codebuild)

  expect_equal(codebuild_start_build(list(project_name = "foo")), 1)
  expect_equal(
    mock_arg(mock_start_build), list(
      projectName = "foo"
    )
  )
})

test_that("check wait_for_build", {
  mock_smdocker_config <- mock2(list())
  # mock codebuild client
  mock_batch_get_builds <- mock2(
    list(
      builds = list(list(buildStatus = "IN_PROGRESS"))
    ),
    list(
      builds = list(list(buildStatus = "COMPLETE", logs = list(deepLink = 1)))
    )
  )
  mock_codebuild <- mock2(list(batch_get_builds = mock_batch_get_builds))
  mockery::stub(.wait_for_build, "codebuild", mock_codebuild)

  .wait_for_build("foo", 1)

  expect_equal(mock_call_no(mock_batch_get_builds), 2)
})

test_that("check get_image_uri", {
  metadata <- list(
    repo_name = "foo",
    tag = "bar"
  )
  mock_smdocker_config <- mock2(list())
  # mock ecr client
  mock_describe_repositories <- mock2(
    list(
      repositories = list(list(repositoryUri = "12345"))
    )
  )
  mock_ecr <- mock2(list(describe_repositories = mock_describe_repositories))
  mockery::stub(.get_image_uri, "ecr", mock_ecr)

  repo_id <- .get_image_uri(metadata)

  expect_equal(repo_id, "12345:bar")
})

test_that("check get_image_uri error", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  metadata <- list(
    repo_name = "foo",
    tag = "bar"
  )
  mock_smdocker_config <- mock2(list())
  # mock ecr client
  mock_describe_repositories <- mock2(
    stop("cho")
  )
  mock_ecr <- mock2(list(describe_repositories = mock_describe_repositories))
  mockery::stub(.get_image_uri, "ecr", mock_ecr)

  repo_id <- .get_image_uri(metadata)
  result <- readLines(temp_file)

  expect_true(
    grepl("^INFO.*: Unable to get Image URI. Error: cho", result[[1]])
  )
})

test_that("check codebuild_build log = TRUE", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  mock_create_repo_if_required <- mock2()
  mock_codebuild_start_build <- mock2(12)
  mock_logs_for_build <- mock2()
  mock_wait_for_build <- mock2()
  mock_get_image_uri <- mock2("foo:bar")

  mockery::stub(
    codebuild_build, ".create_repo_if_required", mock_create_repo_if_required
  )
  mockery::stub(
    codebuild_build, "codebuild_start_build", mock_codebuild_start_build
  )
  mockery::stub(
    codebuild_build, "logs_for_build", mock_logs_for_build
  )
  mockery::stub(
    codebuild_build, ".wait_for_build", mock_wait_for_build
  )
  mockery::stub(
    codebuild_build, ".get_image_uri", mock_get_image_uri
  )


  expect_no_error(codebuild_build(list(), log = TRUE))

  expect_true(
    grepl("^INFO.*: Image URI: foo:bar", readLines(temp_file)[[1]])
  )
  unlink(temp_file)
})

test_that("check codebuild_build log = FALSE", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  mock_create_repo_if_required <- mock2()
  mock_codebuild_start_build <- mock2(12)
  mock_logs_for_build <- mock2()
  mock_wait_for_build <- mock2()
  mock_get_image_uri <- mock2("foo:bar")

  mockery::stub(
    codebuild_build, ".create_repo_if_required", mock_create_repo_if_required
  )
  mockery::stub(
    codebuild_build, "codebuild_start_build", mock_codebuild_start_build
  )
  mockery::stub(
    codebuild_build, "logs_for_build", mock_logs_for_build
  )
  mockery::stub(
    codebuild_build, ".wait_for_build", mock_wait_for_build
  )
  mockery::stub(
    codebuild_build, ".get_image_uri", mock_get_image_uri
  )


  expect_no_error(codebuild_build(list(), log = FALSE))

  expect_true(
    grepl("^INFO.*: Image URI: foo:bar", readLines(temp_file)[[1]])
  )
  unlink(temp_file)
})
