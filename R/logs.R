#' @importFrom paws.developer.tools codebuild
#' @importFrom paws.management cloudwatchlogs
#' @importFrom utils flush.console

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
    position <- length(events) + 1
    if (event_count) {
      next_token <- response$nextForwardToken
      events[[position]] <- response$events
    }

    if (event_count == 0) break
    if ((event_count + 1) > skip) {
      events[[position]] <- events[[position]][skip:event_count]
      skip <- 1
    } else {
      skip <- skip - event_count
      events[[position]] <- list()
    }
  }
  return(unlist(events, recursive = FALSE))
}

logs_for_build <- function(build_id, wait = FALSE, poll = 10) {
  config <- smdocker_config()
  codebuild_client <- codebuild(config)
  description <- codebuild_client$batch_get_builds(
    ids = list(build_id)
  )[["builds"]][[1]]
  status <- description[["buildStatus"]]

  log_group <- description[["logs"]]$groupName
  stream_name <- description[["logs"]]$streamName

  client <- cloudwatchlogs(config)
  job_already_completed <- if (status == "IN_PROGRESS") FALSE else TRUE

  state <- (
    if (wait & !job_already_completed) LogState$STARTING else LogState$COMPLETE
  )
  dot <- TRUE

  while (state == LogState$STARTING & identical(log_group, character(0))) {
    Sys.sleep(poll)
    description <- codebuild_client$batch_get_builds(
      ids = list(build_id)
    )[["builds"]][[1]]
    log_group <- description$logs$groupName
    stream_name <- description$logs$streamName
  }

  positions <- rep(list(list(timestamp = 0, skip = 1)), length(stream_name))
  names(positions) <- stream_name

  if (state == LogState$STARTING) {
    state <- LogState$TAILING
  }

  last_describe_job_call <- Sys.time()
  dot_printed <- FALSE

  while (TRUE) {
    events <- lapply(stream_name, function(s) {
      log_stream(
        client = client,
        log_group = log_group,
        stream_name = s,
        start_time = positions[[s]]$timestamp,
        skip = positions[[s]]$skip
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
      flush.console()

      if (dot) {
        dot <- FALSE
        if (dot_printed) {
          writeLines("")
          flush.console()
        }
      }
      count <- length(events[[e]])
      if (events[[e]][[count]]$timestamp == positions[[e]]$timestamp) {
        positions[[e]]$timestamp <- events[[e]][[count]]$timestamp
        positions[[e]]$skip <- count + 1
      } else {
        positions[[e]]$timestamp <- events[[e]][[count]]$timestamp
        positions[[e]]$skip <- 1
      }
    }
    if (state == LogState$COMPLETE) {
      break
    }
    Sys.sleep(poll)
    if (dot) {
      writeLines(".", sep = "")
      flush.console()
      dot_printed <- TRUE
    }
    if (state == LogState$JOB_COMPLETE) {
      state <- LogState$COMPLETE
    } else if (check_job_call(last_describe_job_call)) {
      description <- codebuild_client$batch_get_builds(
        ids = list(build_id)
      )$builds[[1]]
      status <- description$buildStatus

      last_describe_job_call <- Sys.time()

      status <- description$buildStatus
      if (status != "IN_PROGRESS") {
        writeLines("")
        flush.console()
        state <- LogState$JOB_COMPLETE
      }
    }
  }
  if (wait) {
    if (dot) {
      writeLines("")
      flush.console()
    }
  }
}

check_job_call <- function(last_describe_job_call) {
  (Sys.time() - last_describe_job_call) >= 30
}
