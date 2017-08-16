reversal_zones <- function(x) {

   reversal_zones <- data.frame(xstart = x[, c(1)],
                                ystart = x[, c(2)],
                                xend = c(x[2:nrow(x), c(1)], NA),
                                yend = c(x[2:nrow(x), c(2)], NA),
                                type = x[, c(3)])

   reversal_zones <- reversal_zones[c(1:nrow(reversal_zones)-1), -5]
   reversal_zones$yvar <- (reversal_zones$yend - reversal_zones$ystart)
   reversal_zones$ypvar <- (reversal_zones$yvar - reversal_zones$ystart)

   return(reversal_zones)
}
