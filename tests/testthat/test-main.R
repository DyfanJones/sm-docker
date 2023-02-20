test_that("check sm_build default", {
  mock_build_image <- mock2()
  mock_get_role <- mock2("sagemaker_role")
  mockery::stub(sm_build, "build_image", mock_build_image)
  mockery::stub(sm_build, "get_role", mock_get_role)
  sm_build()
  expect_equal(
    mock_arg(mock_build_image), list(
      NULL, "sagemaker_role", ".",
      NULL, "BUILD_GENERAL1_SMALL",
      NULL, list(),
      log = TRUE
    )
  )
})

test_that("check sm_build extra docker args", {
  mock_build_image <- mock2()
  mock_get_role <- mock2("sagemaker_role")
  mockery::stub(sm_build, "build_image", mock_build_image)
  mockery::stub(sm_build, "get_role", mock_get_role)
  sm_build(dir = "project", f = "dockerfile", build_args = "foo=bar")
  expect_equal(
    mock_arg(mock_build_image), list(
      NULL, "sagemaker_role", "project",
      NULL, "BUILD_GENERAL1_SMALL",
      NULL, list(f = "dockerfile", build_args = "foo=bar"),
      log = TRUE
    )
  )
})

test_that("check sm_build invalid file", {
  expect_error(
    sm_build(dir = "project", file = "made-up"),
    regexp = "The value of the -f/file argument"
  )
})

test_that("check sm_build invalid repository", {
  expect_error(
    sm_build(repository = "made-up:foo:bar"),
    regexp = "is not a valid repository:tag"
  )
})

test_that("check construct_vpc_config", {
  actual <- construct_vpc_config(
    vpc_id = "foo",
    subnet_ids = list("subnet-0d984f080338960bb", "subnet-0ac3e96808c8092f2"),
    security_group_ids = list("sg-0d31b4042f2902cd0")
  )
  expect_equal(
    actual, list(
      vpcId = "foo",
      subnets = list("subnet-0d984f080338960bb", "subnet-0ac3e96808c8092f2"),
      securityGroupIds = list("sg-0d31b4042f2902cd0")
    )
  )
})

test_that("check construct_vpc_config", {
  expect_error(
    smdocker:::construct_vpc_config(
      vpc_id = "foo",
      subnet_ids = list("subnet-0d984f080338960bb", "subnet-0ac3e96808c8092f2")
    ),
    regexp = "Invalid input of the VPC configuration."
  )
})

test_that("check sm_build wrong parameters", {
  kwargs <- list(
    list(
      repository = 1
    ),
    list(
      role = 1
    ),
    list(
      dir = 1
    ),
    list(
      vpc_id = 1
    ),
    list(
      subnet_ids = 1
    ),
    list(
      security_group_ids = 1
    ),
    list(
      log = 1
    )
  )
  for (args in kwargs) {
    expect_error(
      do.call(sm_build, args),
      regexp = names(args)[[1]]
    )
  }
  expect_error(
    sm_build(compute_type = "made-up")
  )
})
