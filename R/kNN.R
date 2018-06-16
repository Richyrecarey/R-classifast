#' Apply knn algorithm
#'
#' @param train Data frame with train data
#' @param test A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' none
#' @importFrom class knn

kNN <- function(train, test, kfold, split){

    p <- ncol(train)
    n <- nrow(train)

    # This function itself will calculate
    # Posible values of k
    if(n<100){
      k = seq(1, 15)
    } else {
      k = seq.int(max(sqrt(n) - floor(log(n)^2), 1),
                  min(sqrt(n) + floor(log(n)^2), 2*sqrt(n)),
                  by = floor(log(n)))
    }


    # Wraper para utilizar con lapply y vectorizar el kfold
    wrapper <- function(index, data = train, k = k){
      # Entrenamos o modelo para index[[i]]
      model <- knn(train[-index, -(p)],
                   train[index, -(p)],
                   train[-index, (p)],
                   k)
      accuracy <- mean(train[index, p] == model)
    }

    # Initialization of reults for each k
    accuracy.each.k <- numeric(length = length(k))

    # Accuracy kfold para cada k posible
    for (i in seq_along(k)){

      knnfold <- lapply(split, wrapper, dat = train, k = k[i])
      accuracy.each.k[i] <- mean(unlist(knnfold))
    }

    # Escollemos o k.optimo
    k.opt <- k[which.max(accuracy.each.k)]

    accuracy.kfold <- 100 * accuracy.each.k[which.max(accuracy.each.k)]




    # Test and train accuracy
    # Model with all the data
    model <- knn(train[, -(p)],
                 train[, -(p)],
                 train[, p],
                 k = k.opt)
    pred.test <- knn(train[, -(p)],
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

