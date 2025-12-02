get_nonparametric_variance <- function(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela, OOSerror_modelb, scheme, epsilon, direct) {
  
#Get Dimension of x
dimx = NCOL(x_fullsamp)
  
#Get T
T = length(y_fullsamp)

#Get P
P = length(OOSerror_modelb)

#Get Residuals of Full Sample
if (dimx > 1) {
  df_fullsamp_a = data.frame("y" = y_fullsamp, "x" = x_fullsamp[,-2:-dimx])
}

df_fullsamp_b = data.frame("y" = y_fullsamp, "x" = x_fullsamp)
colnames(df_fullsamp_b)<-c("y",paste0('x.', 1:(ncol(df_fullsamp_b)-1)))

if (dimx  == 1) {
  reg_fullsamp_a = rq(y ~ 1, data = df_fullsamp_b, tau = alpha)
  reg_fullsamp_b = rq(y ~ x, data = df_fullsamp_b, tau = alpha)
} else if (dimx == 2) {
  reg_fullsamp_a = rq(y ~ x, data = df_fullsamp_a, tau = alpha)
  reg_fullsamp_b = rq(y ~ x.1 + x.2, data = df_fullsamp_b, tau = alpha)
} else if (dimx == 3) {
  reg_fullsamp_a = rq(y ~ x, data = df_fullsamp_a, tau = alpha)
  reg_fullsamp_b = rq(y ~ x.1 + x.2 + x.3, data = df_fullsamp_b, tau = alpha)
}else if (dimx == 4) {
  reg_fullsamp_a = rq(y ~ x, data = df_fullsamp_a, tau = alpha)
  reg_fullsamp_b = rq(y ~ x.1 + x.2 + x.3 + x.4, data = df_fullsamp_b, tau = alpha)
}

residuals_fullsamp_a = reg_fullsamp_a$residuals
residuals_fullsamp_b = reg_fullsamp_b$residuals

#J Selection Matrix
J = t(cbind(diag(dimx), c(1:dimx)*0))

#C_T 
C_T_a = 1.06*sd(residuals_fullsamp_a)*T^(-epsilon)

C_T_b = 1.06*sd(residuals_fullsamp_b)*T^(-epsilon)


#FROM MIKE'S NOTES FOR ESTIMATING ASYMPTOTIC VARIANCE PDF:

#STEP 1: Get Lamda_hh (based on P/R)
if (scheme == "recursive") {
  lamda_hh = 2*(1 -  (1/pi*log(1 + pi)))
} else if (scheme == "rolling" && pi <= 1) {
  lamda_hh = pi - (pi^2)/3
} else if (scheme == "rolling" && pi > 1) {
  lamda_hh = 1 - (3*pi)^-1
}



#STEP 2: Get B_2 (and B_1)
X2 = t(cbind(replicate(NROW(x_fullsamp), 1), x_fullsamp))
B_2_inv = 0
for (t in 1:T) {
    B_2_inv = B_2_inv + (abs(residuals_fullsamp_b[t]) < C_T_b)*(X2[, t]%*%t(X2[, t]))
}
B_2_inv = ((2*C_T_b*T)^-1)*B_2_inv;
#B_2 = inv(B_2_inv)
B_2 = solve(B_2_inv)
if (direct == 1) {
  B_1_inv = 0
  for (t in 1:T) {
    B_1_inv = B_1_inv + (abs(residuals_fullsamp_a[t]) < C_T_a)*(X2[-(dimx+1), t]%*%t(X2[-(dimx+1), t]))
  }
  B_1_inv = ((2*C_T_a*T)^-1)*B_1_inv;
  if (dimx > 1) {
    #B_1 = inv(B_1_inv)
    B_1 = solve(B_1_inv)
  } else {
    B_1 = B_1_inv^-1
  }
} else {
  if (dimx > 1) {
    B_1 = inv(t(J)%*%B_2_inv%*%J)
  } else {
    B_1 = (t(J)%*%B_2_inv%*%J)^-1
  }
}

#STEP 3: S_2hh, the long run variance of
#phi(error_fullsamp_modelc)*X2
phi = (alpha - (residuals_fullsamp_b < 0))
maxLag = floor(4*(T/100)^(2/9))
if (dimx == 1) {
EstCov = NeweyWest(lm(phi~X2[2:NROW(X2),]), lag = maxLag+h, prewhite = TRUE, adjust = TRUE)
} else if (dimx == 2) {
  EstCov = NeweyWest(lm(phi~X2[2,]+X2[3,]), lag = maxLag+h, prewhite = TRUE, adjust = TRUE)
} else if (dimx == 3) {
  EstCov = NeweyWest(lm(phi~X2[2,]+X2[3,]+X2[4,]), lag = maxLag+h, prewhite = TRUE, adjust = TRUE)
} else if (dimx == 4) {
  EstCov = NeweyWest(lm(phi~X2[2,]+X2[3,]+X2[4,]+X2[5,]), lag = maxLag+h, prewhite = TRUE, adjust = TRUE)
}


X2_mean = (X2%*%t(X2))/T
S_2hh = (X2_mean%*%EstCov%*%X2_mean)*T



#STEP 4: Get F_2 (and F_1)
#Next we need loop over each origin to get each component of F_2
X2_rt = t(cbind(replicate(NROW(x_rt), 1), x_rt))

F_2 = c(1:(dimx+1))*0
for (s in 1:P) {
    F_2 = F_2 + (X2_rt[, s])*((OOSerror_modelb[s] < 0) - alpha)
}
F_2 = (P^-1)*F_2;

if (direct == 1) {
  F_1 = c(1:dimx)*0
  for (s in 1:P) {
    F_1 = F_1 + (X2_rt[-(dimx+1), s])*((OOSerror_modela[s] < 0) - alpha)
  }
  F_1 = (P^-1)*F_1;
} else {
  F_1 = t(J)%*%F_2
}



#Step 5: Get Variance
zeros_mat = matrix(0, nrow=dimx+1, ncol=dimx)

V = (lamda_hh*cbind(t(F_1), t(-F_2))%*%rbind(cbind(B_1, t(zeros_mat)), cbind(zeros_mat, B_2))%*%rbind(cbind(t(J)%*%S_2hh%*%J, t(J)%*%S_2hh), cbind(S_2hh%*%J, S_2hh))%*%
    rbind(cbind(B_1, t(zeros_mat)), cbind(zeros_mat, B_2))%*%t(cbind(t(F_1), t(-F_2))))/P

  
}