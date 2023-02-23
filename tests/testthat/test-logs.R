test_that("check log_stream ", {
  mock_get_log_events <- mock2(
    list(
      events = list(
        list(
          "timestamp" = 123,
          "message" = "string1",
          "ingestionTime" = 456
        )
      ),
      "nextForwardToken" = "string2",
      "nextBackwardToken" = "string3"
    ),
    list(
      events = list()
    )
  )
  mock_cloudwatch <- mock2(list(get_log_events = mock_get_log_events))
  mock_client <- mock_cloudwatch()

  now <- Sys.time()

  logs <- log_stream(
    mock_client,
    log_group = "foo",
    stream_name = "bar",
    start_time = now,
    skip = 0
  )
  args <- mockery::mock_args(mock_get_log_events)
  expect_equal(logs, list(list(
    "timestamp" = 123,
    "message" = "string1",
    "ingestionTime" = 456
  )))
  expect_equal(args, list(
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = NULL
    ),
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = "string2"
    )
  ))
})

test_that("check log_stream multiple timestamp", {
  mock_get_log_events <- mock2(
    list(
      events = list(
        list(
          "timestamp" = 101,
          "message" = "string1",
          "ingestionTime" = 104
        ),
        list(
          "timestamp" = 101,
          "message" = "string2",
          "ingestionTime" = 104
        )
      ),
      "nextForwardToken" = "string2",
      "nextBackwardToken" = "string3"
    ),
    list(
      events = list(
        list(
          "timestamp" = 102,
          "message" = "string4",
          "ingestionTime" = 102
        )
      ),
      "nextForwardToken" = "string5",
      "nextBackwardToken" = "string6"
    ),
    list(
      events = list()
    )
  )
  mock_cloudwatch <- mock2(list(get_log_events = mock_get_log_events))
  mock_client <- mock_cloudwatch()

  now <- Sys.time()

  logs <- log_stream(
    mock_client,
    log_group = "foo",
    stream_name = "bar",
    start_time = now,
    skip = 2
  )

  args <- mockery::mock_args(mock_get_log_events)
  expect_equal(logs, list(
    list(
      "timestamp" = 101,
      "message" = "string2",
      "ingestionTime" = 104
    ),
    list(
      "timestamp" = 102,
      "message" = "string4",
      "ingestionTime" = 102
    )
  ))
  expect_equal(args, list(
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = NULL
    ),
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = "string2"
    ),
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = "string5"
    )
  ))
})

test_that("check log_stream multiple timestamp", {
  mock_get_log_events <- mock2(
    list(
      events = list(
        list(
          "timestamp" = 102,
          "message" = "string4",
          "ingestionTime" = 102
        )
      ),
      "nextForwardToken" = "string5",
      "nextBackwardToken" = "string6"
    ),
    list(
      events = list()
    )
  )
  mock_cloudwatch <- mock2(list(get_log_events = mock_get_log_events))
  mock_client <- mock_cloudwatch()

  now <- Sys.time()

  logs <- log_stream(
    mock_client,
    log_group = "foo",
    stream_name = "bar",
    start_time = now,
    skip = 3
  )

  args <- mockery::mock_args(mock_get_log_events)
  expect_equal(logs, list())
  expect_equal(args, list(
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = NULL
    ),
    list(
      logGroupName = "foo",
      logStreamName = "bar",
      startTime = now,
      startFromHead = TRUE,
      nextToken = "string5"
    )
  ))
})


test_that("check logs_for_build", {
  now <- Sys.time()
  mock_smdocker_config <- mock2()

  # mock codebuild client
  mock_batch_get_builds <- mock2(
    list(
      builds = list(list(
        buildStatus = "IN_PROGRESS",
        logs = list(
          groupName = character(0),
          streamName = character(0)
        )
      ))
    ),
    list(
      builds = list(list(
        buildStatus = "COMPLETE",
        logs = list(
          groupName = "foo",
          streamName = "bar"
        )
      ))
    ),
    list(
      builds = list(list(
        buildStatus = "COMPLETE",
        logs = list(
          groupName = "foo",
          streamName = "bar"
        )
      ))
    )
  )
  mock_codebuild <- mock2(list(batch_get_builds = mock_batch_get_builds))

  # mock codebuild client
  mock_cloudwatch <- mock2()

  mock_log_stream <- mock2(
    list(
      list(
        "timestamp" = 101,
        "message" = "string1",
        "ingestionTime" = 101
      ),
      list(
        "timestamp" = 102,
        "message" = "string2",
        "ingestionTime" = 102
      )
    ),
    list(
      list(
        "timestamp" = 103,
        "message" = "string3",
        "ingestionTime" = 103
      )
    ),
    list(
      list(
        "timestamp" = 104,
        "message" = "string4",
        "ingestionTime" = 104
      )
    ),
    list(
      list(
        "timestamp" = 105,
        "message" = "string5",
        "ingestionTime" = 105
      )
    )
  )

  mock_check_job_call <- mock2(FALSE, TRUE)
  mockery::stub(logs_for_build, "smdocker_config", mock_smdocker_config)
  mockery::stub(logs_for_build, "codebuild", mock_codebuild)
  mockery::stub(logs_for_build, "cloudwatchlogs", mock_cloudwatch)
  mockery::stub(logs_for_build, "log_stream", mock_log_stream)
  mockery::stub(logs_for_build, "check_job_call", mock_check_job_call)

  expect_no_error(logs_for_build(
    build_id = "build_id",
    wait = T,
    poll = .5
  ))
})

test_that("check check_job_call", {
  expect_false(check_job_call(Sys.time() + 1))
  expect_true(check_job_call(Sys.time() - 31))
})
