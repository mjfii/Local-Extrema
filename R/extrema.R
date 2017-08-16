#' Create Local Extrema Object
#'
#' @description This function returns an S3 class, 'local.extrema', which contains local maxima and minima, the
#' interval used to derive those extrema, and a data frame with xy coordinates that may be used for plotting.
#' @param x A numeric vector or integer vector for use in determining local extrema.
#' @param interval An integer value to specify the degree to which changes in local extrema should be
#' calculated and identified.
#' @param min_per_change A numeric value denoting the minimum change to recognize a reversal. Default value is
#' NA.  NA treated as there is no minimum for reversal.
#' @keywords extrema maxima minima
#' @export
#' @examples
#' rv <- extrema(slv, 3)
#' rv$interval
#' rv$maxima
#' rv$minima
#' @author mjfii
#' @references base
extrema <- function(x, interval, min_per_change = NA) {

   stopifnot(validate_x(x) == TRUE)

   rv <- list()
   rv$interval <- validate_interval(interval)
   rv$maxima <- maxima(x, rv$interval)
   rv$minima <- minima(x, rv$interval)

   rv$results <- results(x, rv$maxima, rv$minima)

   class(rv) <- 'local.extrema'

   return(rv)
}
