#' Get number of jobs to run in parallel.
#'
#' Determines the number of jobs which are going to run in parallel.
#' This function is inspired by the python function in joblib.
get_n_jobs <- function(n_jobs) {
  if (n_jobs == 0) {
    stop('n_jobs == 0 in parallel has no meaning.')
  }
  else if (is.null(n_jobs)) {
    n_jobs <- 1
  }
  else if (n_jobs < 0) {
    n_jobs <- max(parallel::detectCores() + 1 + n_jobs, 1)
  }

  n_jobs
}
