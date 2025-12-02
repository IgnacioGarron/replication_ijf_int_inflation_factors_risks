rs_test_one_step <- function(z, rvec) {
  
  # Implementing the test as in Theorem 2, which is applicable for
  # one-step-ahead density forecasts, the following two lines of code apply.
  # The critical values are as in Table 1 of the paper and correspond to 1% 5% and 10%,
  # respectively
  
  # z: T x 1 numeric vector (probability integral transforms)
  # rvec: 1 x m numeric vector of thresholds between 0 and 1
  
  T <- length(z)
  m <- length(rvec)
  
  # Matrix operations to create the test statistics
  cumcumz <- outer(z, rvec, "<") - matrix(rep(rvec, each = T), nrow = T)
  v <- colSums(cumcumz) / sqrt(T)
  
  KS <- max(abs(v))        # Kolmogorov-Smirnov test statistic
  CVM <- mean(v^2)         # Cramer-von-Mises test statistic
  
  # One-step-ahead critical values
  crit_value <- list(
    ks = c(1.61, 1.34, 1.21),   # 1%, 5%, 10%
    cvm = c(0.74, 0.46, 0.35)
  )
  
  QVrej_one_step_ahead <- (KS > crit_value$ks)
  CVMrej_one_step_ahead <- (CVM > crit_value$cvm)
  
 
  # Report results
  cat("Test statistics [KS, CVM]:\n")
  print(c(KS, CVM))
  
  cat("\nCritical Values (1%, 5%, 10%):\n")
  cat("One-step-ahead density calibration test\n")
  cat("Kolmogorov-Smirnov Test:\n")
  print(crit_value$ks)
  cat("Cramer-von-Mises Test:\n")
  print(crit_value$cvm)
  
  
  # Return results as a list
  return(list(
    crit_value = crit_value,
    test_statistics = c(KS = KS, CVM = CVM),
    QVrej_one_step = list(KS = QVrej_one_step_ahead, CVM = CVMrej_one_step_ahead)
  ))
}