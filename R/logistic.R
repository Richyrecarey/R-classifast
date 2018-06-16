#' Apply logistic function to the data
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom nnet multinom

logistic <- function(train, test, kfold, split, cv.iter, formula){
  # We will be given 2 data.frames of train and test data
  # Both with p variables V1,...Vp,y

  # Now p has got one more value, y
  p <- ncol(train)

  # Check if this is a binary problem
  if(length(unique(train[, p])) == 2){
    # Add logistic regression
    print("Binary Logistic not yet developed")



  } else {
    #Add multinomial

    # First the full model
    # formula = y ~ V1 + ...
    model  <- nnet::multinom(formula, train, trace = FALSE)

    # Test and train accuracy
    pred.test <- predict(model, test[-(p)])
    accuracy.test <- mean(pred.test == test[[p]])*100
    accuracy.train <- mean(predict(model) == train[[p]])*100

    # K-fold accuracy utilizando "split"
    accuracy.kfold <- numeric(length = kfold)
    for (i in 1:kfold){
      # Adjust the model on the data with index that are not split[[i]]
      model.kf <- nnet::multinom(formula, train[-split[[i]], ], trace = FALSE)
      pred.kf  <- predict(model.kf, train[split[[i]], -(p)])
      accuracy.kfold[i] <- mean(pred.kf == train[split[[i]], p])*100
    }
    accuracy.kfold <- mean(accuracy.kfold)
  }


  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(model = model,
              accuracy.kfold = accuracy.kfold,
              accuracy.test = accuracy.test,
              accuracy.train = accuracy.train))
}

