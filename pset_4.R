# Problem Set 4 Solutions ======================================================
# Author: Thomas Wiemann

# Define a custom function to compute the ols estimates
my_simplecoef <- function(y, x) {
  # Compute and return estimates for alpha and beta
  beta <- cov(y, x) / var(x)
  alpha <- mean(y) - mean(x) * beta
  return(c(alpha, beta))
}#MY_SIMPLECOEF

# Problem 6 ====================================================================

# Load the bw06.csv data
dat <- read.csv("data/bw06.csv")
dat <- as.matrix(dat)

# Select variables
y <- dat[, "birthweight"]
w <- dat[, "cigsdaily"]
x <- cbind(1, dat[, c("boy", "age", "highschool",
                      "somecollege", "college")])

# Part a)
my_simplecoef(y, w)

# Part b) 

# Calculate BLP-coefficient
X <- cbind(w, x)
XX_inv <- solve(t(X) %*% X)
Xy <- t(X) %*% y
beta <- XX_inv %*% (Xy)
beta[1]

# Part f)
sum(x[, 2] == 0 & x[, 3] == 25 & x[, 6] == 1) # 283
sum(w == 20) # 43
sum(x[, 2] == 0 & x[, 3] == 25 & x[, 6] == 1 & w == 20) # 0
