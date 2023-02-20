test_that("check extra_docker_args", {
  extra_args <- list(
    file = "hi",
    add_host = "host:ip"
  )

  expect_equal(
    extra_docker_args(extra_args),
    ". --file hi --add-host host:ip"
  )
})

test_that("check extra_docker_args no args", {
  extra_args <- list()

  expect_equal(
    extra_docker_args(extra_args),
    "."
  )
})

test_that("check extra_docker_args single argument", {
  extra_args <- list(
    f = "hi",
    add_host = "host:ip"
  )

  expect_equal(
    extra_docker_args(extra_args),
    ". -f hi --add-host host:ip"
  )
})

test_that("check extra_docker_args multiple build-args", {
  extra_args <- list(
    f = "hi",
    build_args = c("foo=bar", "baz=qux")
  )

  expect_equal(
    extra_docker_args(extra_args),
    ". -f hi --build-args foo=bar --build-args baz=qux"
  )
})

test_that("check regional_hostname", {
  region <- c(
    "cn-bar",
    "us-gov-bar",
    "us-iso-bar",
    "us-isob-bar",
    "bar"
  )

  actual <- sapply(
    region,
    regional_hostname,
    service_name = "foo",
    USE.NAMES = F
  )
  expect_equal(
    actual,
    c(
      "foo.cn-bar.amazonaws.com.cn",
      "foo.us-gov-bar.amazonaws.com",
      "foo.us-iso-bar.c2s.ic.gov",
      "foo.us-isob-bar.sc2s.sgov.gov",
      "foo.bar.amazonaws.com"
    )
  )
})

test_that("check sts_regional_endpoint", {
  actual <- sts_regional_endpoint("dummy")
  expect_equal(
    actual, "https://sts.dummy.amazonaws.com"
  )
})

test_that("check get_region default", {
  mock_pkg_method <- mock2(function(...) {
    "foo"
  })
  mockery::stub(get_region, "pkg_method", mock_pkg_method)
  actual <- get_region()
  expect_equal(actual, "foo")
})

test_that("check get_region no region found", {
  mock_pkg_method <- mock2(function(...) stop(""))
  mockery::stub(get_region, "pkg_method", mock_pkg_method)
  actual <- get_region()
  expect_equal(actual, "us-east-1")
})

test_that("check pkg_method method found", {
  expect_no_error(
    pkg_method("get_region", "paws.common")
  )
})

test_that("check pkg_method not found", {
  expect_error(
    pkg_method("made-up", "foobar"),
    "made-up requires the foobar package, please install it first and try again"
  )
})

test_that("check if retry is working correctly", {
  temp_file <- tempfile()
  options("smdocker.log_file" = temp_file)

  err_fun <- function() {
    stop(
      structure(list(message = "dummy error"), class = c("http_500", "error", "condition"))
    )
  }

  expect_error(
    retry_api_call(
      err_fun(),
      2
    ),
    "dummy error"
  )

  result <- readLines(temp_file)
  expect_true(
    grepl("^.*ERROR.*: Request failed. Retrying in 0.2 seconds...", result[[1]])
  )
  expect_true(
    grepl("^.*ERROR.*: Request failed. Retrying in 0.4 seconds...", result[[2]])
  )
  unlink(temp_file)
})

test_that("check miscellaneous functions", {
  expect_equal(
    str_split("foo:bar:cho", ":"),
    list(c("foo", "bar", "cho"))
  )
  expect_equal(
    str_split("foo:bar:cho", ":", 1),
    list(c("foo:bar:cho"))
  )
  expect_equal(
    str_split("foo:bar:cho", ":", 2),
    list(c("foo", "bar:cho"))
  )
  expect_true(
    islistempty(list())
  )
  expect_false(
    islistempty(list("foo"))
  )
})
