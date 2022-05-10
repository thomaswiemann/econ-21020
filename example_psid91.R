# Case Study: 401(k) Retirement Savings ========================================
# Author: Thomas Wiemann 

# This script compute the CATE and ATE of 401(k) participation on net total
#     financial assets under a selection on observables assumption.
#
# For the data, see also: https://direct.mit.edu/rest/article/86/3/735/57586/The-Effects-of-401-K-Participation-on-the-Wealth

# Preliminaries ================================================================

# Implementation of CATE and ATE Estimators 

# Function to calculate the CATE
calc_cate <- function(y, w, x, x_val) {
  # Find treated/untreated individuals for x = x_val
  y_w1_x <-y[w == 1 & x == x_val]
  y_w0_x <-y[w == 0 & x == x_val]
  # Estimate conditional means
  mu_w1_x <- mean(y_w1_x)
  mu_w0_x <- mean(y_w0_x)
  # Estimate CATE
  cate_x <- mu_w1_x - mu_w0_x
  # Compute standard error 
  n <- length(y)
  p_w1_x <- mean(w == 1 & x == x_val)
  p_w0_x <- mean(w == 0 & x == x_val)
  se_cate_x <- sqrt((var(y_w1_x) / p_w1_x + 
                       var(y_w0_x) / p_w0_x) / n)
  # Return CATE and SE
  return(c(cate_x, se_cate_x))
}#CALC_CATE

# Function to calculate the ATE for binary X
calc_ate <- function(y, w, x) {
  # Estimate CATEs and P(X=1)
  cate_x1 <- calc_cate(y, w, x, 1)
  cate_x0 <- calc_cate(y, w, x, 0)
  p_x1 <- mean(x == 1); p_x0 <- 1 - p_x1
  # Estimate ATE
  ate <- cate_x1[1] * p_x1 + cate_x0[1] * p_x0
  # Compute standard error 
  n <- length(y)
  sgm2_ate  <- n * (cate_x1[2] * p_x1)^2 + 
    n * (cate_x0[2] * p_x0)^2 + 
    (cate_x1[1] - cate_x0[1])^2 * p_x1 * p_x0
  se_ate <- sqrt(sgm2_ate) / sqrt(n)
  # Return ATE and SE
  return(c(ate, se_ate))
}#CALC_ATE

# Function for balance test for binary X
test_SO <- function(x_tld, w, x) {
  # Calculate CATEs of w on x_tld
  cate_x1 <- calc_cate(x_tld, w, x, 1)
  cate_x0 <- calc_cate(x_tld, w, x, 0)
  # Calculate test statistic
  cates <- c(cate_x1[1], cate_x0[1])
  vars <- c(cate_x1[2], cate_x0[2])^2
  Tn <- cates %*% diag(1 / vars) %*% cates
  # Compute p-value
  pval <- pchisq(Tn, 2, lower.tail = FALSE)
  # Return output
  return(c(Tn, pval))
}#TEST_SO

# Data =========================================================================

# Load the psid91.csv data
df <- read.csv("data/psid91.csv")
n <- nrow(df)

# Define variables
y <- df$net_tfa # Net total financial assets
w <- df$p401 # Participated in a 401(k)
x <- df$educ >= 16 # Obtained a college degree
x_tld <- df$inc # Income

# CATE Estimation ==============================================================

# Estimate CATE and SE for college grads
cate_x1 <- calc_cate(y, w, x, 1)
cate_x1[1] # 29955.17
cate_x1[1] + c(-1, 1) * qnorm(0.975) * cate_x1[2] # 22240.44 37669.91

# Estimate CATE and SE for non-college grads
cate_x0 <- calc_cate(y, w, x, 0)
cate_x0[1] # 23694.85
cate_x0[1] + c(-1, 1) * qnorm(0.975) * cate_x0[2] # 20393.12 26996.59

# ATE Estimation ====================================================+==========

# Estimate CATE and SE for college grads
ate <- calc_ate(y, w, x)
ate[1] # 25262.62
ate[1] + c(-1, 1) * qnorm(0.975) * ate[2] # 22122.47 28402.76

# Check Common Support =========================================================

# Compute shares in the empirical distribution
mean(w == 1 & x == 1) # 0.08613212
mean(w == 0 & x == 1) # 0.1642965
mean(w == 1 & x == 0) # 0.1754917
mean(w == 0 & x == 0) # 0.5740797

# Balance Test =================================================================

# Check whether CATE of 401(k) on incomes are all zero
test_SO(x_tld, w, x) # 6.957109e+02 8.477902e-152