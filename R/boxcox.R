#' Extract the 1 - \alpha confidence interval from a box-cox transformation.
#'
#' @param boxcox_fit The result of calling MASS::boxcox.
#' @param alpha The desired significance level.
#' @return A list containing the maximum likelihood estimator of lambda
#'   (lambda_hat) and the (1 - alpha) CI for lambda.
#'
#' @export
boxcox_interval <- function(boxcox_fit, alpha = 0.05) {
  lambdas <- boxcox_fit$x
  likelihood <- boxcox_fit$y

  # maximum likelihood estimate
  lambda_hat <- lambdas[which.max(likelihood)]

  # note that the box-cox CI can be constructed as follows:
  # - { lambda : L(lambda) > L(lambda_hat) - 0.5 * qchisq(1 - alpha, df = 1) }
  #   i.e. 2 * (L(lambda_hat) - L(lambda)) ~ chisq
  ci_lambdas <- lambdas[
    likelihood > (max(likelihood) - 0.5 * qchisq(1 - alpha, df = 1))]
  interval <- range(ci_lambdas)

  list(lambda_hat=lambda_hat, interval=interval)
}
