test_that("check smdocker_config default", {
  mock_get_region <- mock2("foo")
  mockery::stub(smdocker_config, "get_region", mock_get_region)
  actual <- smdocker_config(refresh = TRUE)

  expect_equal(
    actual, list(
      credentials = list(
        anonymous = FALSE
      ),
      region = "foo",
      disable_ssl = FALSE
    )
  )
})

test_that("check smdocker_config full parameters", {
  actual <- smdocker_config(
    aws_access_key_id = "foo",
    aws_secret_access_key = "bar",
    aws_session_token = "cho",
    region_name = "qux",
    refresh = TRUE
  )

  expect_equal(
    actual, list(
      credentials = list(
        creds = list(
          access_key_id = "foo",
          secret_access_key = "bar",
          session_token = "cho"
        ),
        anonymous = FALSE
      ),
      region = "qux",
      disable_ssl = FALSE
    )
  )
})

test_that("check smdocker_config wrong parameters", {
  kwargs <- list(
    list(
      aws_access_key_id = 1,
      refresh = TRUE
    ),
    list(
      aws_secret_access_key = 1,
      refresh = TRUE
    ),
    list(
      aws_session_token = 1,
      refresh = TRUE
    ),
    list(
      region_name = 1,
      refresh = TRUE
    ),
    list(
      aws_access_key_id = 1,
      refresh = TRUE
    )
  )
  for (arg in kwargs) {
    expect_error(
      do.call(smdocker_config, arg),
      regexp = names(args)[[1]]
    )
  }
})

test_that("check smdocker_config cache", {
  mock_get_region <- mock2("foo", cycle = T)
  mockery::stub(smdocker_config, "get_region", mock_get_region)
  actual1 <- smdocker_config(refresh = TRUE)
  actual2 <- smdocker_config()
  expect_equal(actual1, actual2)
})
