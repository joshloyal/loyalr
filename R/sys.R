#' Check if R is running on a mac.
#'
#' @export
is_mac <- function() {
  Sys.info()["sysname"] == 'Darwin'
}
