#' Apply logistic function to the data
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom nnet multinom

logistic <- function(train, test, kfold, cv.iter, formula){
  # We will be given 2 data.frames of train and test data
  # Both with p variables V1,...Vp,y

  # Variables that may be needed
  p <- ncol(train)

  # We fit the model on the train data
  # We use the formula = "formula", for which
  # the object "train" is designed
  model <- multinom(formula, train, trace = FALSE)

  # Calculate error on train set
  pred.train <- predict(model)
  # The labels are in the (p)-th column (last one)
  error.train <- mean(pred.train == train[[p]])*100

  # Predictions and error on the test set
  # We have to put as "newdata", a dataframe with
  # the same names as the original "x" (V1,...,Vp)
  pred.test <- predict(model, test[-(p)])
  error.test <- mean(pred.test == test[[p]])

  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(model = model,
              error.test = error.test,
              error.train = error.train,
              coefs = coefficients(model),
              AIC = model$AIC))
}

############################################################


#' Apply logistic function to the data
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom nnet multinom

logistic <- function(train, test, kfold, cv.iter, formula){
  # We will be given 2 data.frames of train and test data
  # Both with p variables V1,...Vp,y

  # Variables that may be needed
  p <- ncol(train)

  # Check if this is a binary problem
  if(length(unique(train[, p])) == 2){
    # Add logistic regression
    model <- 2
  } else {
    #Add multinomial
    model  <- 3
  }





  ##################### ERROR CALCULATION ###############


  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(model = model,
              error.test = error.test,
              error.train = error.train)
}

