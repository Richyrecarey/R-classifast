#' Apply knn algorithm
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom e1071 tune

SVM <- function(train, test, kfold){

  p <- ncol(train)
  n <- nrow(train)

  # Calculous
  model <- e1071::tune(svm,
                train.x = train[, -p],
                train.y = train[, p],
                ranges = list(gamma = 2^(-2:2), cost = 2^(1:4)),
                tunecontrol = tune.control(sampling = "cross",
                                           cross = kfold,
                                           best.model = TRUE))


  # Parameters selected:
  param = model$best.performance

  best.model <- model$best.model


  accuracy.kfold <- 100 * (1 - model$best.performance)

  accuracy.test <- mean(predict(best.model, test[, -p]) == test[[p]]) * 100

  accuracy.train <- mean(predict(best.model, train[, -p]) == train[[p]]) * 100



  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(accuracy.kfold = accuracy.kfold,
              accuracy.test = accuracy.test,
              accuracy.train = accuracy.train,
              labels.test = predict(best.model, test[, -p]),
              model = best.model,
              param = param))
}
