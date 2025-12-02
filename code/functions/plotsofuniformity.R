plotsofuniformity <- function(pdens, figtitle, crit_value_vec, significance_level) {
  bin <- 10
  m <- length(pdens)
  
  # Select critical values and labels based on significance level
  if (significance_level == 1) {
    n_crit_value <- qnorm(0.01 / 2)
    crit_value <- crit_value_vec$ks[1]
    lbl <- "1% Critical Value"
  } else if (significance_level == 5) {
    n_crit_value <- qnorm(0.05 / 2)
    crit_value <- crit_value_vec$ks[2]
    lbl <- "5% Critical Value"
  } else if (significance_level == 10) {
    n_crit_value <- qnorm(0.1 / 2)
    crit_value <- crit_value_vec$ks[3]
    lbl <- "10% Critical Value"
  } else {
    stop("Choose a significance level from 1, 5, or 10.")
  }
  
  # ---------------------------
  # Histogram with confidence bands
  # ---------------------------
  breaks <- seq(0, 1, length.out = bin + 1)
  hist_counts <- hist(pdens, breaks = breaks, plot = FALSE)$counts
  bar_heights <- bin * hist_counts / m
  centers <- breaks[-(bin+1)]
  
  expected_height <- bin * (1 / bin)
  error_margin <- n_crit_value * sqrt((1/bin * (1 - 1/bin)) / m)
  upper_band <- bin * (1/bin + error_margin)
  lower_band <- bin * (1/bin - error_margin)
  
  # Plot histogram
  # barplot(height = bar_heights, names.arg = round(centers, 2), col = "lightblue",
  #         border = "gray", main = figtitle, ylab = "Frequency", xlab = "")
  # abline(h = expected_height, col = "red", lwd = 2)
  # abline(h = upper_band, col = "red", lty = 2, lwd = 2)
  # abline(h = lower_band, col = "red", lty = 2, lwd = 2)
  # box()
  # 
  # ---------------------------
  # ECDF vs uniform line with RS test bands
  # ---------------------------
  rvec <- seq(0, 1, by = 0.001)
  ecdf_vals <- sapply(rvec, function(r) mean(pdens < r))
  
  plot(rvec, ecdf_vals, type = "l", lwd = 2, col = "black",
       xlab = "", ylab = "", main = paste(figtitle))
  lines(rvec, rvec, col = "red", lwd = 2)
  lines(rvec, rvec + crit_value / sqrt(m), col = "red", lty = 2, lwd = 2)
  lines(rvec, rvec - crit_value / sqrt(m), col = "red", lty = 2, lwd = 2)
  legend("topleft", legend = c("FA-QR", "Theoretical", lbl),
         col = c("black", "red", "red"), lty = c(1, 1, 2), lwd = 2)
  box()
  grid()
}
