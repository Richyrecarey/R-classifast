#' Apply kNN algorithm
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return K-Nearest Neighbours model for \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom class knn

kNN <- function(train, test, kfold, split){

    p <- ncol(train)
    n <- nrow(train)
    # How many actual registers are we gonna have when trainning with kfold?
    n.kfold <- length(split[[1]])
    # This function itself will calculate
    # Posible values of k
    if(n.kfold<30){
      k = seq(1, floor(n.kfold / 2))
    } else {
      k = seq.int(max(floor(sqrt(n.kfold) - log(n.kfold)^1.6), 1),
                  min(floor(sqrt(n.kfold) + log(n.kfold)^1.6), 2*sqrt(n.kfold)),
                  by = floor(log(n.kfold)))
    }


    # Wraper to use with lapply and vectorize the kfold
    wrapper <- function(index, data = train, k = k){
      # Training of the model for index[[i]]
      # We train with everything but the index "index"
      model <- class::knn(train[-index, -(p)],
                   train[index, -(p)],
                   train[-index, (p)],
                   k)
      # We check the accuracy predicting the data on the index "index"
      # i.e., the only one we left out of the training
      accuracy <- mean(train[index, p] == model)
    }

    # Initialization of reults for each k
    accuracy.each.k <- numeric(length = length(k))

    # Accuracy kfold for each posible k
    for (i in seq_along(k)){
      # For each k, we do cross validation to estimate kfold accuracy
      knnfold <- lapply(split, wrapper, data = train, k = k[i])
      # And, for each k, we put that estimated value onto the following vector
      accuracy.each.k[i] <- mean(unlist(knnfold))
    }

    # We choose the optimal k
    k.opt <- k[which.max(accuracy.each.k)]

    accuracy.kfold <- 100 * accuracy.each.k[which.max(accuracy.each.k)]




    # Test and train accuracy
    # Model with all the data
    model <- class::knn(train[, -(p)],
                 train[, -(p)],
                 train[, p],
                 k = k.opt)
    pred.test <- class::knn(train[, -(p)],
                     test[, -(p)],
                     train[, p],
                     k = k.opt)

    accuracy.test <- mean(pred.test == test[[p]])*100
    accuracy.train <- mean((model) == train[[p]])*100


  # The return:
  # Most important part: It must be the same in each method
  # Same structure:
  return(list(accuracy.kfold = accuracy.kfold,
              accuracy.test = accuracy.test,
              accuracy.train = accuracy.train,
              labels.test = pred.test))
}

