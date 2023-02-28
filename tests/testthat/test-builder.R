test_that("check upload_zip_file project files", {
  mock_smdocker_config <- mock2()
  mock_put_object <- mock2()
  mock_paws_storage_s3 <- mock2(
    list(put_object = mock_put_object)
  )
  mock_sagemaker_default_bucket <- mock2("default")
  mock_zip <- mock2(side_effect = zip::zip)

  mockery::stub(upload_zip_file, "smdocker_config", mock_smdocker_config)
  mockery::stub(upload_zip_file, "paws.storage::s3", mock_paws_storage_s3)
  mockery::stub(upload_zip_file, "zip::zip", mock_zip)

  orig_path <- getwd()

  upload_zip_file(
    repo_name = "dummy",
    bucket = "foo",
    extra_args = ".",
    dir = "project"
  )

  zip_actual <- mock_arg(mock_zip)
  s3_actual <- mock_arg(mock_put_object)
  expect_true(grepl("file.*zip", zip_actual$zipfile))
  expect_equal(
    zip_actual$files, c("buildspec.yml", "dockerfile", "helloworld.R")
  )
  expect_true(
    grepl("codebuild-sagemaker-container-[A-Za-z]+\\.zip", s3_actual$Key)
  )
  expect_true(grepl("file.*zip", s3_actual$Body))
  expect_equal(orig_path, getwd())
})

test_that("check upload_zip_file check buildspec", {
  mock_smdocker_config <- mock2()
  mock_put_object <- mock2()
  mock_paws_storage_s3 <- mock2(
    list(put_object = mock_put_object)
  )
  mock_sagemaker_default_bucket <- mock2("default")
  mock_zip <- mock2(side_effect = zip::zip)
  mock_unlink <- mock2()

  mockery::stub(upload_zip_file, "smdocker_config", mock_smdocker_config)
  mockery::stub(upload_zip_file, "paws.storage::s3", mock_paws_storage_s3)
  mockery::stub(upload_zip_file, "zip::zip", mock_zip)
  mockery::stub(upload_zip_file, "unlink", mock_unlink)

  orig_path <- getwd()

  upload_zip_file(
    repo_name = "dummy",
    bucket = "foo",
    extra_args = ". -f bar --build-args cho=qux",
    dir = "project"
  )

  location <- sapply(
    mockery::mock_args(mock_unlink),
    function(x) x[[1]]
  )
  buildspec <- readLines(file.path(location[[2]], "buildspec.yml"))
  expect_true(grepl("\\. -f bar --build-args cho=qux", buildspec[[19]]))
  expect_equal(orig_path, getwd())
  unlink(location, recursive = TRUE, force = TRUE)
})

test_that("check delete_zip_file", {
  mock_smdocker_config <- mock2()

  # mock s3 client
  mock_delete_object <- mock2()
  mock_s3 <- mock2(
    list(
      delete_object = mock_delete_object
    )
  )

  mockery::stub(delete_zip_file, "smdocker_config", mock_smdocker_config)
  mockery::stub(delete_zip_file, "s3", mock_s3)

  expect_no_error(delete_zip_file("foo", "bar"))
  expect_equal(
    mock_arg(mock_delete_object), list(
      Bucket = "foo", Key = "bar"
    )
  )
})

test_that("check delete_zip_file failure", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  mock_smdocker_config <- mock2()

  # mock s3 client
  mock_delete_object <- mock2(
    stop(
      structure(list(message = "dummy error"), class = c("http_403", "error", "condition"))
    )
  )
  mock_s3 <- mock2(
    list(delete_object = mock_delete_object)
  )

  mockery::stub(delete_zip_file, "smdocker_config", mock_smdocker_config)
  mockery::stub(delete_zip_file, "s3", mock_s3)

  expect_no_error(delete_zip_file("foo", "bar"))
  result <- readLines(temp_file)
  expect_true(grepl("^.*ERROR.*: dummy error", result[[1]]))
  expect_true(grepl("^.*ERROR.*: Failed to delete: s3://foo/bar", result[[2]]))
  unlink(temp_file)
})


test_that("check build_image", {
  mock_upload_zip_file <- mock2(list(
    Bucket = "dummy", Key = "path/to/file"
  ))
  mock_delete_zip_file <- mock2()
  mock_codebuild_project_init <- mock2()
  mock_codebuild_create_project <- mock2()
  mock_codebuild_build <- mock2()

  mockery::stub(build_image, "upload_zip_file", mock_upload_zip_file)
  mockery::stub(build_image, "delete_zip_file", mock_delete_zip_file)
  mockery::stub(build_image, "codebuild_project_init", mock_codebuild_project_init)
  mockery::stub(build_image, "codebuild_create_project", mock_codebuild_create_project)
  mockery::stub(build_image, "codebuild_build", mock_codebuild_build)


  build_image(
    repository = "repo:latest",
    role = "foo.bar",
    dir = ".",
    bucket = "dummy",
    compute_type = "compute1",
    vpc_config = "vpc_1",
    extra_args = list(file = "dockerfile")
  )

  expect_equal(mock_arg(mock_upload_zip_file), list(
    "repo:latest", "dummy", ". --file dockerfile", "."
  ))
  expect_equal(mock_arg(mock_codebuild_project_init), list(
    s3_location = "dummy/path/to/file",
    role = "foo.bar",
    repository = "repo:latest",
    compute_type = "compute1",
    vpc_config = "vpc_1"
  ))
})
