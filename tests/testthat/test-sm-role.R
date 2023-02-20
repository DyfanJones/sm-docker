test_that("check get_role default", {
  mock_sagemaker_get_execution_role <- mock2(
    "arn:aws:iam::012345678910:role/made.up"
  )
  mockery::stub(
    get_role, "sagemaker_get_execution_role", mock_sagemaker_get_execution_role
  )
  actual <- get_role()
  expect_equal(
    actual, "made.up"
  )
})

test_that("check get_role with role", {
  actual <- get_role("dummy")
  expect_equal(actual, "dummy")
})

test_that("check get_role default", {
  mock_sagemaker_get_caller_identity_arn <- mock2(
    "arn:aws:iam::012345678910:role/made.up"
  )
  mockery::stub(
    sagemaker_get_execution_role,
    "sagemaker_get_caller_identity_arn",
    mock_sagemaker_get_caller_identity_arn
  )
  actual <- sagemaker_get_execution_role()
  expect_equal(
    actual, "arn:aws:iam::012345678910:role/made.up"
  )
})

test_that("check get_role bad role", {
  mock_sagemaker_get_caller_identity_arn <- mock2(
    "arn:aws:iam::012345678910:iam/made.up"
  )
  mockery::stub(
    sagemaker_get_execution_role,
    "sagemaker_get_caller_identity_arn",
    mock_sagemaker_get_caller_identity_arn
  )
  expect_error(
    sagemaker_get_execution_role(),
    "The current AWS identity is not a role:"
  )
})
