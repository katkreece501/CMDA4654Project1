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
    test_df <- data.frame("x" = x_train, "y" = y_train)
  }
  else { # x_test != NULL
    test_df <- data.frame("x" = x_test, "y" = y_test)
  }
  # Prepare storage for predicted values
  y_hat <- numeric(length(test_df$xtest))
  # Counter for storing yhats
  index <- 1
  # Do fit for each candidate point
  for (x in test_df$xtest) {
    # Compute distance
    train_df$distance <- abs(x - x_train)
    # Get points for the window
    subset <- train_df |> slice_min(order_by = distance, n = window_points, 
                                    with_ties = FALSE)
    # Scale distances
    subset$distance <- subset$distance/max(subset$distance)
    # Compute weights
    subset$weight <- tricube_weight(subset$distance)
    # Fit local model
    subset_lm <- lm(y ~ poly(xtrain, degree), weights = weight,
                    data = subset)
    # Get yhat
    y_hat[index] <- predict(subset_lm, data.frame("x" = x))
    # Increment index
    index <- index + 1
  }
  # Compute MSE
  MSE <- mean((y_hat - test_df$y)^2)
  # Store LOESS fit data, and sort it so it plots correctly
  loessfit <- data.frame("x" = test_df$x, "yhat" = y_hat) %>% arrange(x)
  # Create plot
  loessplot <- ggplot(test_df, aes(x = x, y = y)) + geom_point() + 
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
