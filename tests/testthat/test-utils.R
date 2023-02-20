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
