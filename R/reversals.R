reversals <- function(x) {

   rv <- x[x$is_maxima | x$is_minima, ]

   rv$prior_is_maxima <- c(NA,rv$is_maxima[1:nrow(rv)-1])

   rv$prior_is_minima <- c(NA,rv$is_minima[2:nrow(rv)-1])

   rv <- rv[(rv$is_maxima != rv$prior_is_maxima & rv$is_minima != rv$prior_is_minima) | is.na(rv$prior_is_maxima), ]

   rv <- rv[ , c(1:4)]

   row.names(rv) <- c(1:nrow(rv))

   return(rv)
}
