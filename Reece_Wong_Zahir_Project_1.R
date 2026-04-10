# Libraries used
library(dplyr)
library(ggplot2)

# Helper function for getting weights using Tukey tri-cube function
# This function is vectorized
# Input:
# * u - a scaled distance between 0 and 1, inclusive
# Output:
# The weights according to the function
tricube_weight <- function(u) {
  # Compute and return value of the Tukey tri-cube function
  ifelse(abs(u) < 1, (1 - (abs(u))^3), 0)
}

# Inputs:
# 
# * x_train - a numeric vector of the predictor variable values for the training observations
# * y_train - a numeric vector of the response variable values for the training observations
#
# * x_test - a numeric vector of the predictor variable values for the testing observations
#          - if this is not provided, then the training data is automatically the testing data
# * y_test - a numeric vector of the response variable values for the testing observations
#          - if this is not provided, then the training data is automatically the testing data

# * degree should be 1 or 2 only
# * span can be any value in interval (0, 1) non-inclusive.
#
# If show.plot = TRUE, then the plot is shown
#
# Output is a named list containing:
# span: proportion of data used in each window (controls the bandwidth)
# degree: degree of the local polynomial
# N_train: total number of points in the training data set
# N_test: total number of points in the testing data set (if different from training)
# MSE: The Mean Squared Error for the Predictions
# loessplot: An object containing the ggplot
myloess <- function(x_train, y_train, x_test = NULL, y_test = NULL, 
                    span = 0.5, degree = 1, show.plot = TRUE){
  
  # Get the lengths of the testing and training data
  N_train <- length(x_train)
  # Length of NULL vector is 0, so this always works
  N_test <- length(x_test)
  # Number of points in each window
  window_points <- floor(N_train * span)
  # Store training data 
  train_df <- data.frame("xtrain" = x_train, "ytrain" = y_train)
  # Store testing data
  if (is.null(x_test)) {
    test_df <- data.frame("xtest" = x_train, "ytest" = y_train)
  }
  else { # x_test != NULL
    test_df <- data.frame("xtest" = x_test, "ytest" = y_test)
  }
  # Prepare storage for predicted values
  y_hat <- numeric(length(test_df$xtest))
  # Counter for storing yhats
  index <- 1
  # Do fit for each candidate point
  for (xtest_point in test_df$xtest) {
    # Compute distance
    train_df$distance <- abs(xtest_point - x_train)
    # Get points for the window
    subset <- train_df |> slice_min(order_by = distance, n = window_points, 
                                    with_ties = FALSE)
    # Scale distances
    subset$distance <- subset$distance/max(subset$distance)
    # Compute weights
    subset$weight <- tricube_weight(subset$distance)
    # Fit local model
    subset_lm <- lm(ytrain ~ poly(xtrain, degree), weights = weight,
                    data = subset)
    # Get yhat
    y_hat[index] <- predict(subset_lm, data.frame("xtrain" = xtest_point))
    # Increment index
    index <- index + 1
  }
  # Compute MSE
  MSE <- mean((y_hat - test_df$ytest)^2)
  # Store LOESS fit data, and sort it so it plots correctly
  loessfit <- data.frame("x" = test_df$xtest, "yhat" = y_hat) %>% arrange(x)
  # Create plot
  loessplot <- ggplot(test_df, aes(x = xtest, y = ytest)) + geom_point() + 
    theme_bw() + geom_line(data = loessfit, aes(x = x, y = yhat))
  # Show plot, if that option is selected
  if(show.plot) {
    print(loessplot)
  }
  # Return values
  return(list("span" = span, 
              "degree" = degree, 
              "N_train" = N_train, 
              "N_test" = N_test,
              "MSE" = MSE,
              "loessplot" = loessplot)) 
}

mykNN <- function(train, test, y_train, y_test, k = 3, weighted = TRUE){
  
  train <- as.matrix(train)
  test  <- as.matrix(test)
  
  # single test observation case, turn into row vector
  if(is.vector(test)){
    test <- matrix(test, nrow = 1)
  }


  # num observations
  n_test <- nrow(test)
  
  yhat <- vector(length = n_test)
  
  # classification or regression
  is_classification <- is.factor(y_train)
  
  # loop through each observation
  for(j in 1:n_test){
    
    # vectorized euclidean distance calculation for all training points
    dists <- sqrt(rowSums((train - matrix(test[j,], nrow=nrow(train), ncol=ncol(train), byrow=TRUE))^2))
    
    # order by smallest distanc
    idx <- order(dists)[1:k]
    
    # k nearest neighbor values
    nearest_y <- y_train[idx]
    
    # distances for the k nearest neighbors
    nearest_d <- dists[idx]
    
    # handle division by 0
    nearest_d <- pmax(nearest_d, 1e-10)
    
    
    # dwknn or knn
    if(weighted){
      w <- 1 / nearest_d
    } else {
      w <- rep(1, k)
    }
    
    if(is_classification){
      
      classes <- levels(y_train)
      scores <- rep(0, length(classes))
      
      # for each class get weighted value
      for(c in 1:length(classes)){
        scores[c] <- sum(w * (nearest_y == classes[c]))
      }
      
      # assign the one with heighest score
      yhat[j] <- classes[which.max(scores)]
    
      # regression case
    } else {
      # normalize
      w_norm <- w / sum(w)
      # calculate weighted average of neighbor values
      yhat[j] <- sum(w_norm * nearest_y)
    }
  }
  
  # classification
  if(is_classification){
    yhat <- factor(yhat, levels = levels(y_train))
    
    accuracy <- mean(yhat == y_test)
    error_rate <- 1 - accuracy
    conf_mat <- table(Predicted = yhat, Actual = y_test)
    
    return(list(
      yhat = yhat,
      accuracy = accuracy,
      error_rate = error_rate,
      confusion_matrix = conf_mat,
      k = k
    ))
  # regression
  } else {
    
    yhat <- as.numeric(yhat)
    residuals <- y_test - yhat
    SSE <- sum(residuals^2)
    n <- length(y_test)
    MSE <- SSE / n
    
    return(list(
      yhat = yhat,
      residuals = residuals,
      SSE = SSE,
      n = n,
      MSE = MSE,
      k = k
    ))
  }
}
