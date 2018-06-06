#' Compare different clasification methods on multivariante data
#'
#'
#'
#' @param x Data frame or matrix with multivariate data with n observations (rows) and p variables (cols)
#' @param y A factor with the labels of the rows of x
#' @param type Vector of the methods wanted. By default, "simple" gives you various lineal classifiers. Other posibilities are:
#'   "hard": All implemented classifiers (time consuming)
#' @param kfold Number of folds in the cross validation estimation  
#' 
#' @return Not yet
#' @examples Not yet
#' @export


classifast <- function(x, y, 
                       prob = 0.65, type = c("simple"), 
                       kfold = 10, cv.iter = 5, timing = TRUE){

  # Change the input x and y accordingly

  # Needed variables throughout the function:
  n <- nrow(x)
  p <- ncol(x)

  if(timing == TRUE){
    # ----------- Time Prediction
    # Predicted time, given "n" and "p" and the methods available
    # M.logistic: (p*6 + n*0.05) / 1000 seconds (fitted model:lm(t~p+n))
    time.mlog =  (p * 6 + .05 * n) / 1000

    time = time.mlog
    message("The expected computation time is ", time, " s")
  }

  # In case "y" is a data.frame, modify it accordingly
  if(is.data.frame(y)){
    y <- y[[1]]
  }

  # Change "y" to factor. If "y" was a factor inside the data.frame,
  # y[[1]] will keep being a "factor", so:
  if (!is.factor(y)){
    y <- factor(y, levels = paste(unique(y)))
  }

  # Levels of y?
  if(length(unique(y)) == 1){
    stop("The labels vector only has one level")
  }

  if(length(unique(y)) == 2){
    # Variable to trigger logistic or m.logistic
    # that also defines the classes of the object
    binary <- TRUE
    obj.class <- c("classifast", type, "binary")
  } else {
    # Otherwise:
    obj.class <- c("classifast", type)
    binary <- FALSE
  }

  # Check for lengths
  if(n != length(y)){
    stop("The labels vector must have the same length as the number of rows in your data")
  }

  # If "x" is a matrix, we turn it into d.f.
  if(class(x) == "matrix"){
    x <- data.frame(x)
  }
  

  # Creation of several character vectors needed
  # plus we change the names of the variables to V1, ..., Vp
  b1 <- paste0("V", 1:p,collapse = "+")
  b2 <- paste0("V", 1:p)

  # This formula y~V1+...+Vp will be usefull for some
  # of the classifiers
  formula <- paste("y ~ ",b1,sep = "")

  colnames(x) <- b2

  # We add the factor "y" to the data.frame "x"
  x$y <- y

  # At this point, we should have a dataframe "x" with p+1 cols
  # and "y" in the last one as a factor

  # Selection of train and test set
  # on the "x" data.frame, keeping "y" the whole time.
  train.n <- floor(prob * n)

  # Indexex for the train set
  train.i <- sample(n, train.n)

  # Indexes for the test set
  test.i <- c(1:n)[-train.i]

  # New sets, keeping the factor "y" of labels in them
  # For each, we take the rows "train.i" and "test.i"
  # This 2 data.frames will be given to all the subfunctions
  # This way we do the subsetting only once, in the main function
  x.train <- x[train.i, ]
  x.test <- x[test.i, ]

  # ------------------------- MODELS ------------------------------ #
  # For now, we will call all of them, in the future we will select
  # which ones we want. Nonetheless, the lineal classificator
  # wil always be called.
  # 
  # ORDER
  # 1. Logistic Regression (binary or multinomial)
  # 2. SVM


  ##################### LOGISTIC REGRESSION ####################
  # If the model is binary, we call logistic, otherwise m.logistic
  if(binary){
    logistic <- logistic(train = x.train,
                      test = x.test,
                      formula = formula)
  } else {

    # Otherwise, we fit a multinomial regression
    # Multinomial Logistic Regression
    logistic <- m.logistic(train = x.train,
                      test = x.test,
                      formula = formula)
  }
  ##############################################################
  

  ############################# EXTRA INFO #####################
  # Extra info from the classifiers that may be needed
  # We could've put here things like the "binary" or "simple" option
  # to pass them to the methods, but we chose to do it with classes
  info <- list(kfold = kfold,
               cv.iter = cv.iter)

  # ------------------------- OUTPUT ------------------------------- #
  
  # Invisible: Only if assigned you get this list of lists of info
  # "type" gives us which methods to use in summary and predict
  # "binary" tells us if the data is binary

 
  invisible(structure(list(logistic = logistic,
                           info = info),
                      class = obj.class))



  }


#' Summary method for the objetc with class "classifast"
#'
#'
#'
#' @param x Object of class "classifast"
#' @return Table with errors of different classifiers
#' @export


summary.classifast <- function(x){
  # At this point, we have the "classifast" objetct
  # We create the needed objets for printing

  # In n.simple and n.hard there are the future 
  # methods for use, on both binary and multilabel data
  # First the "simple", then the "hard".
  # For now theres only one in simple
  # This part should be independent, the rest
  # should depend only on tjis 6 lines:
  
  # Methods available as today:
  n.simple <- c("Logistic reg")
  n.hard <- c("ANN")
  


  if(inherits(x, "simple")){
    # Initialize
    error.test <- numeric(length(n.simple))
    error.train <- numeric(length(n.simple))

    # We build these vectors:
    for (i in seq_along(n.simple)){
      # These are the errores of each method in "n.simple"
      # or "n.hard". The order is as in "n.simple"!!!
      # Since we have different output for bina
      error.test[i] <- x[[i]]$error.test
      error.train[i] <- x[[i]]$error.train
    }
    # We build the dataframe
  } else {
    # If its not simple, is "hard"
    error.test <- numeric(length(n.simple) + length(n.hard))
    error.train <- numeric(length(n.simple) + length(n.hard))

    # We build these vectors throughout both sets of methods
    # So the first ones will be simple and the final ones hard
    for (i in seq_along(c(n.simple, n.hard))){
      # These are the errores of each method in "n.simple"
      # or "n.hard". The order is as in "n.simple"!!!
      # Since we have different output for bina
      error.test[i] <- x[[i]]$error.test
      error.train[i] <- x[[i]]$error.train
    }


  }

  results <- data.frame(methods = n.simple,
                        e1 = error.test,
                        e2 = error.train)
  colnames(results) <- c("Method",
                         "Test err %",
                         "Train err %")

  cat(paste0("Test error was approximated using ", x$info$kfold, "-fold validation"), "\n", "\n", "\n")
  print(results)
}



