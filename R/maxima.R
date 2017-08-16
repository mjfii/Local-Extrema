maxima <- function (x, interval) {

   rv <- integer()

   var <- which(diff(sign(diff(x))) < 0)

   for (i in var) {

      lb <- lower_bound(i, interval)
      ub <- upper_bound(i, interval, length(x))
      range <- c(lb:i, (i + 2):ub)

      if(all(x[range] <= x[i + 1])) {
         rv <- c(rv, i + 1)
      }
   }

   return(as.integer(rv))
}

lower_bound <- function(x, interval) {
   rv <- x - abs(interval) + 1
   rv <- ifelse(rv > 0, rv, 1)
   return(rv)
}

upper_bound <- function(x, interval, c) {
   rv <- x + abs(interval) + 1
   rv <- ifelse(rv <= c, rv, c)
   return(rv)
}
