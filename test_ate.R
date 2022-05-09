
nobs <- 10000

p_x <- 0.7
p_w_x1 <- 0.7
p_w_x0 <- 0.3

x <- (runif(nobs) < p_x) * 1
w <- (runif(nobs) < (p_w_x1 * x + p_w_x0 * (1 - x))) * 1

y <- 0.5 + 1 * w  + 2.5 * w * x + -2 * x + (1 - 2 * runif(nobs)) 

x_val = 1
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
  se_ate <- sqrt(sgm2) / sqrt(n)
  # Return ATE and SE
  return(c(ate, se_ate))
}#CALC_ATE



ate_so <- function(y, w, x) {
  # Find treated/untreated individuals for each x
  y_w1_x1 <-y[w == 1 & x == 1]
  y_w0_x1 <-y[w == 0 & x == 1]
  y_w1_x0 <-y[w == 1 & x == 0]
  y_w0_x0 <-y[w == 0 & x == 0]
  
    # CATE & ATE Estimation
  mu_w1_x1 <- mean(y_w1_x1)
  mu_w0_x1 <- mean(y_w0_x1)
  mu_w1_x0 <- mean(y_w1_x0)
  mu_w0_x0 <- mean(y_w0_x0)
  
  p_x1 <- mean(x == 1)
  
  cate_x1 <- mu_w1_x1 - mu_w0_x1
  cate_x0 <- mu_w1_x0 - mu_w0_x0
  
  ate <- cate_x1 * p_x1 + cate_x0 * (1 - p_x1)
  return(c(ate, cate_x1, cate_x0))
}#ATE_SO

ate_res <- ate_so(y, w, x)
cate_x1 <- ate_res[2]
cate_x0 <- ate_res[3]

# ATE Variance
p_w1_x1 <- mean(w[x == 1] == 1) 
p_w1_x0 <- mean(w[x == 0] == 1) 

var_w1_x1 <- var(y[w == 1 & x == 1])
var_w0_x1 <- var(y[w == 0 & x == 1])
var_w1_x0 <- var(y[w == 1 & x == 0])
var_w0_x0 <- var(y[w == 0 & x == 0])

var_ate <- (var_w1_x1 / p_w1_x1 + var_w0_x1 / (1 - p_w1_x1)) * p_x1 + 
  (var_w1_x0 / p_w1_x0 + var_w0_x0 / (1 - p_w1_x0)) * (1 - p_x1) + 
  (cate_x1 - cate_x0)^2 * p_x1 * (1 - p_x1)
se_ate <- sqrt(var_ate) / sqrt(nobs)
se_ate

# Check with bootstrap
num_boot <- 10000
boot_mat <- matrix(0, num_boot, 1)
for (b in 1:num_boot) {
  # Draw bootstrap sample 
  sample_b <- sample(c(1:nobs), replace = TRUE)
  y_b <- y[sample_b]
  w_b <- w[sample_b]
  x_b <- x[sample_b]
  
  # Calculate ATE
  boot_mat[b] <- calc_ate(y_b, w_b, x_b)[1] #ate_so(y_b, w_b, x_b)[1]
}#FOR

sd(boot_mat)

calc_ate(y, w, x)

