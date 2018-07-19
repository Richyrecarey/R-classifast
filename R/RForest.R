#' Apply Random Forest algorithm
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return Random Forest model for \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom randomForest randomForest

RForest <- function (train, test, kfold, split){
  p <- ncol(train)
  n <- nrow(train)

  # Wraper to use with lapply and vectorize the kfold
  wrapper <- function(index, data = train) {
    # Training of the model
    model <- randomForest::randomForest(train[-index, -(p)], train[-index, p],
                                        xtest = train[index, -(p)],
                                        ytest = train[index, p])
    nfail <- (model$test$confusion[1,2] + model$test$confusion[2,1])
  }



  #kfold accuracy computation
  rffold <- lapply(split, wrapper, data = train)
  accuracy.kfold <- 100*(1 - sum(unlist(rffold))/n)


  #Computation of the test accuracy and predictions
  model <- randomForest::randomForest(train[, -(p)], train[, p],
                                      xtest = test[, -(p)],
                                      ytest = test[, p],
                                      keep.forest = TRUE)
  accuracy.test <- 100*(1 - (model$test$confusion[1,2] + model$test$confusion[2,1]) / nrow(test))
  pred.test <- model$test$predicted

  #Train accuracy
  pred.train <- predict(model, train[, -(p)])
  accuracy.train <- mean((model$response) == train[[p]]) * 100

  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(accuracy.kfold = accuracy.kfold,
              accuracy.test = accuracy.test,
              accuracy.train = accuracy.train,
              labels.test = pred.test))
}

