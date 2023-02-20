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

  upload_zip_file(
    repo_name = "dummy",
    bucket = "foo",
    extra_args = ".",
    dir = "project"
  )

  zip_actual <- mock_arg(mock_zip)
  s3_actual <- mock_arg(mock_put_object)
  expect_true(grepl(sprintf("%s/file.*zip", tempdir()), zip_actual$zipfile))
  expect_equal(
    zip_actual$files, c("buildspec.yml", "dockerfile", "helloworld.R")
  )
  expect_true(
    grepl("codebuild-sagemaker-container-[A-Za-z]+\\.zip", s3_actual$Key)
  )
  expect_true(grepl(sprintf("%s/file.*zip", tempdir()), s3_actual$Body))
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
  unlink(location, recursive = TRUE, force = TRUE)
})
