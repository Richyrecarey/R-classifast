#' Apply logistic function to the data
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom nnet multinom

logistic <- function(train, test, formula){
  # We will be given 2 data.frames of train and test data
  # Both with p+1 variables V1,...Vp,y

  # Variables that may be needed
  p <- ncol(train) - 1

  # We fit the model on the train data
  # We use the formula = "formula", for which
  # the object "train" is designed
  model <- multinom(formula, train, trace = FALSE)

  # Calculate error on train set
  pred.train <- predict(model)
  # The labels are in the (p+1)-th column
  error.train <- mean(pred.train == train[[p+1]])*100

  # Predictions and error on the test set
  # We have to put as "newdata", a dataframe with
  # the same names as the original "x" (V1,...,Vp)
  pred.test <- predict(model, test[-(p+1)])
  error.test <- mean(pred.test == test[[p+1]])

  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(model = model,
              error.train = error.train,
              error.test = error.test,
              coefs = coefficients(model),
              AIC = model$AIC))
}
