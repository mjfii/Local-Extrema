#' Plot Local Extrema Object
#'
#' @description This function returns a \code{\link[ggplot2]{ggplot}} visualization based on the results of the \code{\link{extrema}} function.
#' @param x The only parameter is a `local.extrema` S3 class object.
#' @keywords extrema maxima minima
#' @export
#' @note The function validates that the input is the appropriate class; if it is not, the function will return NULL.
#' @examples
#' rv <- extrema(slv, 5)
#' plot_extrema(rv)
#' @author mjfii
#' @references base, ggplot2
plot_extrema <- function(x) {

   if (class(x)[1] != 'local.extrema') {
      stop('This plotting functionality requires the object class of \'local.extrema\'.  Exiting function...' , call. = FALSE)
      return(NULL)
   }

   plot <- ggplot2::ggplot()

   if (length(x$minima) != 0 & length(x$maxima) != 0) {

      reversals <- reversals(x$results)
      rz <- reversal_zones(reversals)

      if (nrow(rz[rz$yvar < 0, ]) > 0) {
         plot <- plot + ggplot2::geom_rect(data = rz[rz$yvar < 0, ], ggplot2::aes_(xmin=~xstart, xmax=~xend, ymin = -Inf, ymax = Inf), alpha = 0.25 , fill = '#FF1919')
      }

      if (nrow(rz[rz$yvar >= 0, ]) > 0) {
         plot <- plot + ggplot2::geom_rect(data = rz[rz$yvar >= 0, ], ggplot2::aes_(xmin=~xstart, xmax=~xend, ymin = -Inf, ymax = Inf), alpha = 0.25 , fill = '#00FF48')
      }

   }

   plot <- plot + ggplot2::geom_line(ggplot2::aes(x = x$results$x, y = x$results$y), stat='identity') +
                  ggplot2::geom_point(mapping = ggplot2::aes(x = x$results$x, y = x$results$y), color = 'black', size = 1)

   if (length(x$maxima) != 0) {
      plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes(x = x$maxima, y = x$results$y[x$maxima]), color = '#B20000', size = 2)
   }

   if (length(x$minima) != 0) {
      plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes(x = x$minima, y = x$results$y[x$minima]), color = '#00B233', size = 2)
   }

   plot <- plot + ggplot2::xlab('x = element ordinal') +
                  ggplot2::ylab('y = element value') +
                  ggplot2::guides(fill=FALSE)

   return(plot)
}
