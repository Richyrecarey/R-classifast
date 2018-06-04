#' Compare different clasification methods on multivariante data
#'
#' @param x Data frame or matrix with multivariate data with n observations (rows) and p variables (cols)
#' @param y A factor with the labels of the rows of x
#' @return Not yet
#' @examples Not yet
#' @export

classifast <- function(x, y, prob = 0.65, type = "simple"){

  # Change the input x and y accordingly

  # Needed variables throughout the function:
  n <- nrow(x)
  p <- ncol(x)

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

  # Multinomial Logistic Regression
  m.log <- m.logistic(train = x.train,
                      test = x.test,
                      formula = formula)

  # --------------------- OUTPUT ------------------- #
  return(structure(list(m.log = m.log),
                   class = "classifast"))
}

