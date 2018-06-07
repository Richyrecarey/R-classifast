#' Estimates errores given a function to use
#'
#' @param method The function to
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom nnet multinom



error_estimation <- function(method, param = NULL, train, kfold, cv.iter){
  # For a given method, function like "multinomial()" for example
  # and a set of parameters, we calculate the kfold.error for those
  # parameters

  # This function will be called for several sets of parameters
  # CV.iter will be one for now

  n <- nrow(train)
  p <- ncol(train)
  error.kfold <- numeric(kfold * cv.iter)
  # Length of each set.fold
  length.set <- floor(n/kfold) + n - kfold*floor(n/kfold)



  # Cross validation
  for (it in 1:cv.iter){
    # For each iteration:

    # Split the data, one full sample for iteration
    index <- sample(n, n, replace = FALSE)

    # A train - test for each iteration:
    # Select the index of the current data.set
    for(i in 1:kfold){
      ind <-
    }





  }




}

