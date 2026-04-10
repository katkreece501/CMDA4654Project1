library(ggplot2)
library(gganimate)
library(dplyr)
library(MASS) # For the mcycle dataset

# 
# * x - a numeric input vector
# * y - a numeric response
#
# Note span and degree are shown with their default values. (Read about this in the description)
# * degree should be 1 only, but if you want to get it to work with degree = 2, then that's great too
# * span can be any value in interval (0, 1) non-inclusive.
#
# filename - is the name of the final animation gif file
animated_loess <- function(x, y, span = 0.5, degree = 1, filename = "loessplot.gif") {
  df <- data.frame(x = x, y = y) %>% arrange(x)
  n <- nrow(df)
  k <- ceiling(span * n) # Number of points in the neighborhood
  
  eval_x <- sort(unique(x))
  
  frames_data <- data.frame()
  fitted_results <- data.frame()
  
  # loops through each evaluation point to calculate local fits
  for (i in seq_along(eval_x)) {
    target_x <- eval_x[i]
    
    df$dist <- abs(df$x - target_x)
    dist_k <- sort(df$dist)[k]
    
    #tricube weights: w = (1 - (dist/dist_k)^3)^3
    df$w <- ifelse(df$dist < dist_k, (1 - (df$dist / dist_k)^3)^3, 0)
    
    #fit local weighted regression
    if (degree == 1) {
      local_fit <- lm(y ~ x, data = df, weights = w)
    } else {
      local_fit <- lm(y ~ poly(x, 2, raw = TRUE), data = df, weights = w)
    }
    
    #prediction at current target
    pred_y <- predict(local_fit, newdata = data.frame(x = target_x))
    fitted_results <- rbind(fitted_results, data.frame(x = target_x, y = pred_y))
    
    # stores segment data for this frame
    neighborhood <- df[df$w > 0, ]
    line_x_seq <- seq(min(neighborhood$x), max(neighborhood$x), length.out = 50)
    line_y_seq <- predict(local_fit, newdata = data.frame(x = line_x_seq))
    
    frames_data <- rbind(frames_data, data.frame(
      line_x = line_x_seq,
      line_y = line_y_seq,
      target_x = target_x,
      frame = i
    ))
  }
  
  # fitted points moves as the sweep moves
  history_data <- data.frame()
  for (i in seq_along(eval_x)) {
    history_data <- rbind(history_data, fitted_results[1:i, ] %>% mutate(frame = i))
  }
  
  # Build animated point data with highlighting effect
  points_anim <- data.frame()
  trigger_width <- diff(range(x))/80 # makes the black points delayed and tighter than before
  highlight_window <- 2  # num of frames the point stays black
  
  for (i in seq_along(eval_x)) {
    target_x <- eval_x[i]
    
    temp <- df %>%
      mutate(
        frame = i,
        # distance from sweep line
        close_now = abs(x - target_x) < trigger_width #(diff(range(x)) / 30)
      )
    
    # past frames for persistence effect
    points_anim <- rbind(points_anim, temp)
  }
  
  # keeps points black for some time after being hit
  points_anim <- points_anim %>%
    group_by(x, y) %>%
    mutate(
      highlight = zoo::rollapply(close_now, width = highlight_window,
                                 FUN = any, fill = FALSE, align = "left")
    ) %>%
    ungroup()
  
  # Built the plot
  p <- ggplot() +
    geom_point(
      data = points_anim,
      aes(x = x, y = y, color = highlight, group = interaction(x, y)),
      alpha = 0.7
    ) +
    scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "black"), guide = "none") +
    # moving local regression line
    geom_line(data = frames_data, aes(x = line_x, y = line_y), color = "green", size = 1.2) +
    # LOESS curve
    geom_line(data = history_data, aes(x = x, y = y), color = "purple", size = 1) +
    # vertical indicator for the current evaluation point
    geom_vline(data = frames_data, aes(xintercept = target_x), linetype = "dashed", alpha = 0.4) +
    theme_minimal() +
    labs(
      title = paste0("LOESS Animation (Degree = ", degree, ", Span = ", span, ")"),
      x = "X Variable",
      y = "Y Variable"
    ) +
    transition_manual(frame)
  
  # Render and save the gif
  anim_save(filename, p, renderer = gifski_renderer())
  
  print(paste0("The animation was generated successfully and can be found in working directory as ", filename))
}
