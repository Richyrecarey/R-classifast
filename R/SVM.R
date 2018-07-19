#' Apply svm algorithm
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return Support Vector Machine model for \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom e1071 tune
#' @importFrom e1071 svm

SVM <- function(train, test, kfold){

  p <- ncol(train)
  n <- nrow(train)

  # Calculous
  model <- e1071::tune(svm,
                train.x = train[, -p],
                train.y = train[, p],
                ranges = list(gamma = 2^(-2:2), cost = 2^(1:4)),
                tunecontrol = e1071::tune.control(sampling = "cross",
                                           cross = kfold,
                                           best.model = TRUE))


  # Parameters selected:
  param = model$best.parameters

  # Is it interest?
  best.model <- model$best.model


  accuracy.kfold <- 100 * (1 - model$best.performance)

  accuracy.test <- mean(predict(best.model, test[, -p]) == test[[p]]) * 100

  accuracy.train <- mean(predict(best.model, train[, -p]) == train[[p]]) * 100

  # Complete model in t
  data <- rbind(train, test)
  model <- e1071::svm(data[, -p],
                      data[[p]],
                      gamma = param[[1]],
                      cost = param[[2]])




  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(accuracy.kfold = accuracy.kfold,
              accuracy.test = accuracy.test,
              accuracy.train = accuracy.train,
              labels.test = predict(best.model, test[, -p]),
              model = model,
              param = param))
}
