#' Compare different clasification methods on multivariante data
#'
#'
#'
#' @param x Data frame or matrix with multivariate data with n observations (rows) and p variables (cols)
#' @param y A factor with the labels of the rows of x
#' @param method Vector of the methods wanted. By default, "simple" gives you various lineal classifiers. Other posibilities are:
#'   "hard": All implemented classifiers (time consuming)
#'   "log": Logistic or multinomial linear logistic regression
#'   "svm": Support Vector Machines with Radial Kernel
#'

#' @param kfold Number of folds in the cross validation estimation
#'
#' @return Not yet
#' @examples Not yet
#' @export


classifast <- function(x, y,
                       prob = 0.65, method = c("simple"),
                       kfold = floor(nrow(x) / 15),
                       cv.iter = 1, timing = TRUE){
  ##################### CHECK & TWEAK COMPATIBILITY OF INPUT ##################
  # Proper changes for methods: in "method" we have the methods wanted
  # to be computed. If "simple" is selected (default), we compute:
  if(method == "simple"){
    method = c("log")
  }

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

  # Proper naming of the cols of x, to be used by the methods
  colnames(x) <- b2

  # We add the factor "y" to the data.frame "x"
  x$y <- y
  ###############################################################

  # At this point, we should have a dataframe "x" with p+1 cols
  # and "y" in the last one as a factor

  ###################### DATA SPLIT #########################
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

  ##############################################################
  # Now we have to split the x.train dataset, with length train.n

  split.i <- sample(train.n, train.n)
  split <- split(split.i, ceiling(seq_along(split.i)/ floor(train.n / kfold) ))
  # Now we have a list with kfold lists with the index of the test set
  # For the CV. 




  ###############################################################

  # ------------------------- MODELS ------------------------------ #
  # We selected the methods wanted to be computed in the vector
  # "method", so in total length(method) methods will be computed

  # Models will be stored in this list
  output <- list()

  ##################### LOGISTIC REGRESSION ####################
  # Both options, for logistic and multinomial logistic regression
  # are inside the function "logistic.R"
  if("log" %in% method){
    model <- logistic(train = x.train,
                      test = x.test,
                      kfold = kfold,
                      split = split,
                      cv.iter = cv.iter,
                      formula = formula)

    # We add the list of the model, if selected, to the output
    # list "output" that has lists with each model.
    # Important: $ operator preserver the class list
    output$log <- model
  }
  ########################### SVM ############################


  ##############################################################


  ############################# EXTRA INFO #####################
  # Extra info from the classifiers that may be needed
  # for further methods
  output$info <- list(used.method = method,
               kfold = kfold,
               cv.iter = cv.iter)

  # ------------------------- OUTPUT ------------------------------- #
  # The objetc output will be a lists of lists with the methods, and
  # at the end, a list with "info"
  # Invisible: Only if assigned you get this list of lists of info
  #


  invisible(structure(output,
                      class = "classifast"))



}


#' Summary method for the objetc with class "classifast"
#'
#'
#'
#' @param x Object of class "classifast"
#' @return Table with accuracys of different classifiers
#' @export


summary.classifast <- function(x){
  # At this point, we have the "classifast" objetct
  # We create the needed objets for printing

  # Choosen methods
  method <-x$info$used.method


  # Initialize
  accuracy.kfold <- numeric(length(method))
  accuracy.test <- numeric(length(method))
  accuracy.train <- numeric(length(method))

  # We build these vectors:
  for (i in seq_along(method)){
    # These are the accuracyes of each method in "n.simple"
    # or "n.hard". The order is as in "n.simple"!!!
    # Since we have different output for bina
    accuracy.kfold[i] <- x[[i]]$accuracy.kfold
    accuracy.test[i] <- x[[i]]$accuracy.test
    accuracy.train[i] <- x[[i]]$accuracy.train
  }


  results <- data.frame(methods = method,
                        e1 = accuracy.kfold,
                        e2 = accuracy.test,
                        e3 = accuracy.train)

  colnames(results) <- c("Method",
                         "kf %",
                         "Test %",
                         "Train %")

  cat("\n", paste0("k-fold accuracy was approximated using ", x$info$kfold, "-fold validation"), "\n", "\n")

  cat("\n", "Accuracy (%) of the diferent methods used:", "\n", "\n")


  print(results)
}


#' Information about "classifast" implementation
#'
#' @return Bunch of information
#' @export
#' 
classifast_info <- function(){
  cat("Welcome to the package classifast.", "\n", "\n")

  cat("* Binary logistic regression uses glm()", "\n")
  cat("* Multinomial Logistic Regression uses multinom() from the package 'nnet' ", "\n")
}