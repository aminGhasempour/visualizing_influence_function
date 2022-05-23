library(scales)

# Tableau 20 colorscheme
tableau20 <- c('#4E79A7', # Blue
               '#A0CBE8', # Light Blue
               '#F28E2B', # Orange
               '#FFBE7D', # Light Orange
               '#59A14F', # Green
               '#8CD17D', # Light Green
               '#B6992D', # Yellow-Green
               '#F1CE63', # Yellow
               '#499894', # Yellow
               '#86BCB6', # Light Teal
               '#E15759', # Red
               '#FF9D9A', # Pink
               '#FABFD2', # Light Pink
               '#B07AA1', # Purple
               '#D4A6C8', # Light Purple
               '#9D7660', # Brown
               '#D7B5A6') # Light Orange)

# Plot path
# Function for plotting path between two distributions
plot_path <- function(x, p_x, p_x_tilde, ranges, epsilon = seq(0, 1, 0.1)) {
  
  # Plotting distributions
  col1 = tableau20[1]
  col2 = tableau20[3]
  x_range <- c(median(x) - 2, median(x) + 2)
  y_range <- c(0, max(max(p_x), max(p_x_tilde)))
  plot(
    x,
    p_x,
    type = "l",
    col = col1,
    xlim = ranges$x,
    ylim = ranges$y,
    lwd = 2
  )
  lines(x, p_x_tilde, col = col2, lwd = 2)
  lgnd <- c("p_x", "P~")
  col_lab <- c(col1, col2)
  
  # Plotting the path, \mathcal{P}_\epsilon
  trans <- div_gradient_pal(col1, high = col2, space = "Lab")
  cols <- trans(seq(0, 1, length.out = length(epsilon)))
  for (e in epsilon) {
    lines(
      x,
      (1 - e) * p_x + e * p_x_tilde,
      col = cols[e * 10],
      lwd = 1,
      lty = 1
    )
  }
}



# Plotting function for plotting curve of T vs eps
plot_curve <- function(x, y, one_step, k_fold = NULL, legend_pos, ylbl, xlbl="epsilon") {
  # col1: true curve, col2: linear approximation
  col1 = tableau20[1]
  col2 = tableau20[5]
  col3 = tableau20[6]
  
  # Plot the true curve
  plot(x, y,
       type="l",
       col=col1,
       lwd=2,
       lty=1,
       ylab=ylbl,
       xlab=xlbl,
       xlim=c(min(x) - 0.005 * max(x), max(x) + 0.005 * max(x)),
       ylim=c(min(min(y), one_step), max(max(y), one_step)),
       xaxs="i",
       yaxt = "n")
  
  # Show y ticks at the three relevant places
  axis(2,
       at = c(tail(y, 1), one_step, head(y, 1)),
       labels = round(c(tail(y, 1), one_step, head(y, 1)), digits=3), 
       las=1)
  
  # Plot points at T(P) and T(P~)
  points(c(max(x)), 
         c(tail(y, 1)),
         col=col1,
         pch=19,
         cex=2)
  
  # Plot linear approximation
  lines(c(min(x), max(x)), 
        c(one_step, tail(y, 1)),
        col=col2,
        lwd=2,
        lty=2)
  
  # Plot one-step k-fold
  if (!is.null(k_fold)) {
    lines(c(min(x), max(x)), 
          c(k_fold, tail(y, 1)),
          col=col3,
          lwd=2,
          lty=2)
    legend("right",
           legend = c("True curve", "One-step", "One-step k-fold"),  # Legend texts
           col = c(col1, col2, col3),
           lty=c(1,2,2),
           lwd=2
    )
  } else {
    legend("right",
           legend = c("True curve", "One-step"),  # Legend texts
           col = c(col1, col2),
           lty=c(1,2),
           lwd=2
    )
  }
  
}