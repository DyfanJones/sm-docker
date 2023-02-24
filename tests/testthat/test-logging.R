# ensure all log configs are set to defaults
smdocker_log()

test_that("check default logging settings", {
  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 3L)
  expect_equal(log_file, "")
  expect_equal(log_timestamp_fmt, "%Y-%m-%d %H:%M:%OS3")
})

test_that("check updating smdocker log config", {
  temp_file <- tempfile()
  smdocker_log(level = 3L, file = temp_file, timestamp_fmt = "%Y-%m-%d %H:%M")

  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 3L)
  expect_equal(log_file, temp_file)
  expect_equal(log_timestamp_fmt, "%Y-%m-%d %H:%M")
  unlink(temp_file)
})

test_that("check updating smdocker log config with wrong parameter types", {
  expect_error(
    smdocker_log(level = "3"),
    ".*level.*"
  )
  expect_error(
    smdocker_log(file = 1),
    ".*file.*"
  )
  expect_error(
    smdocker_log(timestamp_fmt = 1),
    ".*timestamp_fmt.*"
  )
})

test_that("check if file created in none existing directory", {
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "demo.log")
  smdocker_log(file = temp_file)
  expect_true(dir.exists(temp_dir))
  unlink(temp_file)
})

test_that("check reset log config", {
  smdocker_log(
    level = 4L,
    file = "made-up",
    timestamp_fmt = "%Y-%m-%d %H:%M"
  )
  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 4L)
  expect_equal(log_file, "made-up")
  expect_equal(log_timestamp_fmt, "%Y-%m-%d %H:%M")

  smdocker_log()
  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 3L)
  expect_equal(log_file, "")
  expect_equal(log_timestamp_fmt, "%Y-%m-%d %H:%M:%OS3")
})

test_that("ensure init_log_config doesn't modified already set log config", {
  smdocker_log(
    level = 3L,
    file = "made-up",
    timestamp_fmt = "%Y-%m-%d %H:%M"
  )

  init_log_config()

  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 3L)
  expect_equal(log_file, "made-up")
  expect_equal(log_timestamp_fmt, "%Y-%m-%d %H:%M")
})

test_that("update log config from environmental variables", {
  Sys.setenv("SMDOCKER_LOG_LEVEL" = 4L)
  Sys.setenv("SMDOCKER_LOG_TIMESTAMP_FMT" = "made-up")
  Sys.setenv("SMDOCKER_LOG_FILE" = "dummy-file")

  init_log_config()

  log_level <- getOption("smdocker.log_level")
  log_file <- getOption("smdocker.log_file")
  log_timestamp_fmt <- getOption("smdocker.log_timestamp_fmt")

  expect_equal(log_level, 4L)
  expect_equal(log_file, "dummy-file")
  expect_equal(log_timestamp_fmt, "made-up")
  lapply(
    c("smdocker_LOG_LEVEL", "smdocker_LOG_TIMESTAMP_FMT", "smdocker_LOG_FILE"),
    Sys.unsetenv
  )
})

test_that("check log messages", {
  temp_file <- tempfile()
  # ERROR Logging level
  options("smdocker.log_level" = 1L)
  options("smdocker.log_file" = temp_file)
  log_debug("foo")
  log_info("foo")
  log_warn("foo")
  log_error("foo")

  result <- readLines(temp_file)
  expect_true(grepl("^.*ERROR.*: foo", result))
  unlink(temp_file)

  # WARN Logging level
  options("smdocker.log_level" = 2L)
  options("smdocker.log_file" = temp_file)
  log_debug("foo")
  log_info("foo")
  log_warn("foo")
  log_error("foo")

  result <- readLines(temp_file)
  expect_true(grepl("^.*WARN.*: foo", result[[1]]))
  expect_true(grepl("^.*ERROR.*: foo", result[[2]]))
  unlink(temp_file)

  # INFO Logging level
  options("smdocker.log_level" = 3L)
  options("smdocker.log_file" = temp_file)
  log_debug("foo")
  log_info("foo")
  log_warn("foo")
  log_error("foo")

  result <- readLines(temp_file)
  expect_true(grepl("^.*INFO.*: foo", result[[1]]))
  expect_true(grepl("^.*WARN.*: foo", result[[2]]))
  expect_true(grepl("^.*ERROR.*: foo", result[[3]]))
  unlink(temp_file)

  # DEBUG Logging level
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)
  log_debug("foo")
  log_info("foo")
  log_warn("foo")
  log_error("foo")

  result <- readLines(temp_file)
  expect_true(grepl("^.*DEBUG.*: foo", result[[1]]))
  expect_true(grepl("^.*INFO.*: foo", result[[2]]))
  expect_true(grepl("^.*WARN.*: foo", result[[3]]))
  expect_true(grepl("^.*ERROR.*: foo", result[[4]]))
  unlink(temp_file)
})

test_that("check log encoded message", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  # log encoded url message
  log_debug("foo%20bar")
  log_info("foo%20bar")
  log_warn("foo%20bar")
  log_error("foo%20bar")

  result <- readLines(temp_file)
  expect_true(grepl("^.*DEBUG.*: foo%20bar", result[[1]]))
  expect_true(grepl("^.*INFO.*: foo%20bar", result[[2]]))
  expect_true(grepl("^.*WARN.*: foo%20bar", result[[3]]))
  expect_true(grepl("^.*ERROR.*: foo%20bar", result[[4]]))
  unlink(temp_file)
})

test_that("check log message additional arguments", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  # log with arguments
  log_debug("foo %s", "bar")
  log_info("foo %s", "bar")
  log_warn("foo %s", "bar")
  log_error("foo %s", "bar")

  result <- readLines(temp_file)
  expect_true(grepl("^.*DEBUG.*: foo bar", result[[1]]))
  expect_true(grepl("^.*INFO.*: foo bar", result[[2]]))
  expect_true(grepl("^.*WARN.*: foo bar", result[[3]]))
  expect_true(grepl("^.*ERROR.*: foo bar", result[[4]]))
  unlink(temp_file)
})

test_that("check log params", {
  temp_file <- tempfile()
  options("smdocker.log_level" = 4L)
  options("smdocker.log_file" = temp_file)

  # log with arguments
  log_params("dummy message", list(foo = "bar"))

  result <- readLines(temp_file)

  expect_true(grepl("^.*DEBUG.*: dummy message:", result[[1]]))
  expect_equal(
    jsonlite::fromJSON(paste0(result[2:4], collapse = "")), list(
      foo = "bar"
    )
  )
  unlink(temp_file)
})
