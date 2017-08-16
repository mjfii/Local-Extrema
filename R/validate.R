validate_interval <- function(interval) {

   dv <- 5L

   # ensure length of 1
   if(length(interval) > 1) {
      warning('The \'interval\' arguement is a vector with multiple elements.  Using the first element.', call. = FALSE)
      interval <- interval[1]
   }

   # ensure no zero length
   if(length(interval) == 0) {
      warning('The \'interval\' arguement is a vector without any elements.  Using the default value.', call. = FALSE)
      return(dv)
   }

   # test for numeric arguments
   if (class(interval) != 'integer' & class(interval) != 'numeric') {
      warning('The \'interval\' arguement must be numeric.  Using the default value.', call. = FALSE)
      return(dv)
   }

   # test for a negative interval
   if (interval < 0) {
      warning('The \'interval\' arguement was negative.  Using the absolute value.', call. = FALSE)
      return(abs(interval))
   }

   # test for zero interval
   if (interval == 0) {
      warning('The \'interval\' arguement was zero.  Using the default value.', call. = FALSE)
      return(dv)
   }

   return(as.integer(interval))

}

validate_x <- function(x) {

   # test for numeric arguments
   if (class(x) != 'integer' & class(x) != 'numeric') {
      warning('The \'x\' arguement vector class must be \'integer\' or \'numeric\'.  Exiting function...', call. = FALSE)
      return(FALSE)
   }

   return(TRUE)
}


# get plot length
# if (plot_build_length > length(x)) {
#    plot_build_length <- length(x)
# }
#
# rv$plot_build_length <- as.integer(plot_build_length)
