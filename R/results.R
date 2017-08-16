results <- function(x, mx, mn) {

   rv <- data.frame(x = seq_along(x), y = x)
   rv$is_maxima <- rv$x %in% mx
   rv$is_minima <- rv$x %in% mn

   return(rv)

}
