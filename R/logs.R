log_env <- new.env(parent = emptyenv())

LogState <- list(
  STARTING = 1,
  WAIT_IN_PROGRESS = 2,
  TAILING = 3,
  JOB_COMPLETE = 4,
  COMPLETE = 5
)

log_stream <- function(client,
                       log_group,
                       stream_name,
                       start_time,
                       skip) {
  next_token <- NULL
  event_count <- 1
  events <- list()
  while (event_count > 0) {
    response <- retry_api_call(
      client$get_log_events(
        logGroupName = log_group,
        logStreamName = stream_name,
        startTime = start_time,
        startFromHead = TRUE,
        nextToken = next_token
      ),
      retries = 15
    )

    event_count <- length(response$events)
    if (event_count) {
      next_token <- response$nextForwardToken
      events <- c(events, response$events)
    }

    if (event_count == 0) break

    if (event_count > skip) {
      events <- events[skip:event_count]
      skip <- 1
    } else {
      skip <- skip - event_count
      events <- list()
    }
  }
  return(events)
}

logs_for_build <- function(build_id, wait = FALSE, poll = 10) {
  self <- paws_session()
  codebuild <- paws::codebuild(self$config)
  log_params("batch_get_builds", list(ids = list(build_id)))
  description <- codebuild$batch_get_builds(
    ids = list(build_id)
  )[["builds"]][[1]]
  status <- description[["buildStatus"]]

  DESCRIPTION <<- description
  log_group <- description[["logs"]]$groupName
  stream_name <- description[["logs"]]$streamName

  print(log_group)
  print(stream_name)

  positions <- rep(list(list(timestamp = 0, skip = 1)), length(stream_name))
  names(positions) <- stream_name
  log_env$positions <- positions

  client <- paws::cloudwatchlogs(self$config)

  job_already_completed <- if (status == "IN_PROGRESS") FALSE else TRUE

  state <- (
    if (wait & !job_already_completed) LogState$STARTING else LogState$COMPLETE
  )
  dot <- TRUE

  while (state == LogState$STARTING & is.null(log_group)) {
    Sys.sleep(poll)
    description <- codebuild$batch_get_builds(
      ids = list(build_id)
    )$builds[[1]]
    log_group <- description$logs$groupName
    stream_name <- description$logs$streamName
  }

  if (state == LogState$STARTING) {
    state <- LogState$TAILING
  }

  last_describe_job_call <- Sys.time()
  dot_printed <- FALSE

  while (TRUE) {
    events <- lapply(stream_name, function(s) {
      log_stream(
        client,
        log_group,
        s,
        log_env$positions[[s]]$timestamp,
        log_env$positions[[s]]$skip
      )
    })

    for (e in seq_along(events)) {
      msg <- vapply(
        events[[e]], function(l) trimws(l$message, which = "right"),
        FUN.VALUE = character(1)
      )
      # break if nothing exists in list
      if (islistempty(msg)) break
      writeLines(msg)

      count <- length(events[[e]])
      if (events[[e]][[count]]$timestamp == log_env$positions[[e]]$timestamp) {
        log_env$positions[[e]]$timestamp <- events[[e]][[count]]$timestamp
        log_env$positions[[e]]$skip <- count + 1
      } else {
        log_env$positions[[e]]$timestamp <- events[[e]][[count]]$timestamp
        log_env$positions[[e]]$skip <- 1
      }
    }

    if (state == LogState$COMPLETE) {
      break
    }
    Sys.sleep(poll)
    if (dot) {
      writeLines(".", sep = "")
      flush(stdout())
      dot_printed <- TRUE
    }
    if (state == LogState$JOB_COMPLETE) {
      state <- LogState$COMPLETE
    } else if ((Sys.time() - last_describe_job_call) >= 30) {
      description <- codebuild$batch_get_builds(
        ids = list(build_id)
      )$builds[[1]]
      status <- description$buildStatus

      last_describe_job_call <- Sys.time()

      status <- description$buildStatus

      if (status != "IN_PROGRESS") {
        writeLines("")
        state <- LogState$JOB_COMPLETE
      }
    }
  }
  if (wait) {
    if (dot) {
      writeLines("")
    }
  }
}
