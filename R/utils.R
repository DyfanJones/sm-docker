#' @importFrom utils getFromNamespace
#' @importFrom stats runif

`%||%` <- function(x, y) if (is.null(x)) y else x

# get parent pkg function and method
pkg_method <- function(fun, pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(fun, " requires the ", pkg, " package, please install it first and try again",
      call. = F
    )
  }
  fun_name <- getFromNamespace(fun, pkg)
  return(fun_name)
}

get_region <- function(...) {
  fun <- pkg_method("get_region", "paws.common")
  tryCatch(
    {
      fun(...)
    },
    error = function(e) {
      "us-east-1"
    }
  )
}

regional_hostname <- function(service_name, region) {
  hostname <- list(
    "%s.%s.amazonaws.com.cn" = "cn-*",
    "%s.%s.amazonaws.com" = "us-gov-*",
    "%s.%s.c2s.ic.gov" = "us-iso-*",
    "%s.%s.sc2s.sgov.gov" = "us-isob-*",
    "%s.%s.amazonaws.com" = "*"
  )
  matches <- hostname[sapply(hostname, function(x) grepl(x, region))]
  matches <- matches[order(nchar(matches), decreasing = TRUE)][1]
  return(sprintf(names(matches), service_name, region))
}

sts_regional_endpoint <- function(region) {
  endpoint_data <- regional_hostname("sts", region)
  return(sprintf("https://%s", endpoint_data))
}

islistempty <- function(obj) {
  (is.null(obj) || length(obj) == 0)
}

str_split <- function(path, pattern, n = -1L) {
  out <- strsplit(path, pattern)
  lapply(out, function(x) {
    if (n == -1L) {
      return(x)
    } else {
      str_n <- paste(x[n:length(x)], collapse = pattern)
      if (n == 1) {
        return(str_n)
      }
      return(c(x[1:(n - 1)], str_n))
    }
  })
}

paws_error_code <- function(error) {
  return(error[["error_response"]][["__type"]] %||% error[["error_response"]][["Code"]])
}

retry_api_call <- function(expr, retries = 5) {
  # if number of retries is equal to 0 then retry is skipped
  if (retries == 0) {
    resp <- tryCatch(eval.parent(substitute(expr)),
      error = function(e) e
    )
  }

  for (i in seq_len(retries)) {
    resp <- tryCatch(eval.parent(substitute(expr)),
      error = function(e) e
    )

    if (inherits(resp, "http_500")) {
      # stop retry if statement is an invalid request
      if (grepl("InvalidRequestException", resp)) {
        stop(resp)
      }

      backoff_len <- runif(n = 1, min = 0, max = (2^i - 1))

      message(resp, "Request failed. Retrying in ", round(backoff_len, 1), " seconds...")

      Sys.sleep(backoff_len)
    } else {
      break
    }
  }

  if (inherits(resp, "error")) stop(resp)

  resp
}

extra_docker_args <- function(extra_args) {
  # format docker parameters
  names(extra_args) <- ifelse(
    nchar(names(extra_args)) == 1, paste0("-", names(extra_args)),
    paste0("--", names(extra_args))
  )
  names(extra_args) <- gsub("_", "-", names(extra_args))
  extra_args <- lapply(
    extra_args,
    function(arg) if (is.logical(arg)) tolower(arg) else arg
  )

  paste(".",
    paste(
      names(extra_args),
      lapply(
        names(extra_args),
        function(n) paste(extra_args[[n]], collapse = sprintf(" %s ", n))
      ),
      collapse = " "
    )
  )
}

