# Case Study: Gneezy et al. (2019) =============================================
# Author: Thomas Wiemann 

# This script compute the ATE and its approximate sampling distribution using 
#     the data of Gneezy et al. (2019), as discussed in Lecture 5.

# Data =========================================================================

# Load the list.csv data
df <- read.csv("data/pisa19.csv")

# Define variables
y <- df$score
w <- df$incent

# Computation of the ATE and its Approximate Sampling Distribution =============

# Find treated and untreated individuals
y_1 <- y[w == 1]
y_0 <- y[w == 0]

# Compute conditional averages
mu_1 <- mean(y_1)
mu_0 <- mean(y_0)
ate <- mu_1 - mu_0
ate

# Compute standard error
n <- length(y)
p_1 <- mean(w == 1)
p_0 <- mean(w == 0)
se <- sqrt((var(y_1) / p_1 + var(y_0) / p_0) / n)
se

# Compute confidence set for pre-defined alpha
alpha <- 0.05
c_alpha <- qnorm(1 - alpha / 2)
conf <- ate + c(-1, 1) * c_alpha * se
conf

# Hypothesis Testing ===========================================================

# Compute test-statistic
ate_0 <- 0
Tn <- (mu_1 - mu_0 - ate_0) / se
Tn

# Compute p-value for H_0: mu_1 - mu_0 <= 0
1 - pnorm(Tn)

# Compute p-value for H_0: mu_1 - mu_0 =' 0
2 * (1 -  pnorm(abs(Tn)))

# Balance Test =================================================================

# Re-run previous code (except line 13!) but now consider
y <- df$age
