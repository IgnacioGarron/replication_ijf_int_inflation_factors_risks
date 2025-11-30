# quantile-weighted versions of the con-tinuous ranked probability score Gneiting and R. Ranjan (2011)


# Continuous Ranked Probability Score (CRPS) for simulated forecast quantile 
qs <- function(y, quant, alpha=c()){
  J <- length(quant)
  if (length(alpha)==0){
    alpha <-  seq.int(0.5 * 1/J, 1 - 0.5 * 1/J, length.out = length(quant))
  }
  x <- sort(quant)
  ((y <  x) - alpha) * (x -  y) # Erase 2???
}


# Quantile weighted CRPS (T. Gneiting and R. Ranjan (2011):
#                         Comparing Density Forecasts Using Threshold-and
#                         Quantile-Weighted Scoring Rules, JBES)
crps_qw <- function(y, quant, w = w_c, alpha=c()){
  J <- length(quant)
  if (length(alpha)==0){
    alpha <-  seq.int(0.5 * 1/J, 1 - 0.5 * 1/J, length.out = length(quant))
  }
  x <- sort(quant)
   2 * w(alpha) * ((y <  x) - alpha) * (x -  y)
}


# Weighting functions proposed by Gneiting and Ranjan 2011
# Equal
w <- function(alpha) 1

## To emphasize the center
w_c <- function(alpha) alpha * (1-alpha)
#alpha <- seq(0,1, 0.01)
#plot(w_c(alpha), type = "l")

## To emphasize the tails
w_t <- function(alpha) (2 * alpha - 1)^2
#plot(w_t(alpha), type = "l")

## To emphasize the right tail 
w_r <- function(alpha) alpha^2 
#plot(w_r(alpha), type = "l")

## To emphasize the left tail
w_l <- function(alpha) (1-alpha)^2
#plot(w_l(alpha), type = "l")

