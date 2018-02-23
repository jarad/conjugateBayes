#' Inverse gamma distribution
#'
#' Probability density function, cumulative distribution function, quantile
#' function, and random variable generator for the inverse gamma distribution
#' with parameters shape and scale.
#'
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'   the number required.
#' @param shape,scale shape and scale parameters. Must be positive.
#' @param log,logp logical; if TRUE, probabilities/densities p are returned as log(p)
#' @param lower.tail logical; if TRUE (default), probabilities are p[X\le X],
#'   otherwise, P[X>x]
#'
#' @details
#'   The inverse gamma distribution with parameters shape = a and scale = b has density
#'
#'        f(x) = b^a / Gamma(a) x^{-a-1} exp(-b/x)
#'
#'   for x\ge 0, a>0, b>0. (Gamma(a) is the function implemented by R's gamma()
#'   and defined in its help.)
#'
#'   The mean is \frac{b}{a-1} (if a>1) and
#'   the variance is \frac{b^2}{(a-1)^2(a-2)} (if a>2).
#'
#'   These functions are all based on the gamma distribution.
#'   Please see the documentation for issues using the gamma distribution,
#'   e.g. issues when the shape parameter is small.
#'
#' @return
#'   dgamma gives the density, pgamma gives the distribution function,
#'   qgamma gives the quantile function, and rgamma generates random deviates.
#'
#' @name dinvgamma
#' @export
#' @seealso
#'   \code{\link{dgamma}}
#' @examples
#' dinvgamma(2,1) == dgamma(1/2,1)/2^2
#'
#'
dinvgamma <- function(x, shape, scale = 1, log) {
  dgamma(1/x, shape = shape, rate = scale, log = log)/x^2
}

#' @rdname dinvgamma
#' @export
qinvgamma <- function(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  1/qgamma(1-p, shape = shape, rate = scale, lower.tail = lower.tail, log.p = log.p)
}

#' @rdname dinvgamma
#' @export
pinvgamma <- function(q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  pgamma(q, shape = shape, rate = scale, lower.tail = !lower.tail, log.p = log.p)
}

#' @rdname dinvgamma
#' @export
rinvgamma <- function(n, shape, scale = 1) {
  1/rgamma(n, shape = shape, rate = scale)

}
