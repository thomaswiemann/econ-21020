# Case Study: STAR Data ========================================================
# Author: Thomas Wiemann 

# This script compute the BLP of average math score on student-teacher ratio 
#     using the STAR dataset. 
#
# See also: http://fmwww.bc.edu/ec-p/data/stockwatson/datasets.list.html

# Preliminaries ================================================================

# Load dependencies
library(ggplot2) # for neat plots

# Load custom utility functions 
source("_functions//fun_utilities.R") # imports save_img()

# Data =========================================================================

# Load the list.csv data
df <- read.csv("data/star98.csv")
n <- nrow(df)

# Define variables
y <- df$math_scr
w <- df$str

# Plot histogram of the student teacher ratio
plt <- ggplot() + 
  geom_histogram(aes(x = w, y =..density..), bins = 50) +
  ylab("Density") + 
  xlab("Student-Teacher Ratio") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20,  family="Times"))
save_img(plt, "figures/lectures/lecture_6B_starhist.pdf", w = 8, h = 7)

# Compute OLS Coefficient
beta <- cov(y, w) / var(w) # -1.938591
alpha <- mean(y) - mean(w) * beta # 691.4174

# Compute standard error for beta
epsilon <- y - alpha - w * beta
se_numer <- sqrt(mean(epsilon^2 * (w - mean(w))^2))
se <- (se_numer / var(w)) / sqrt(n) # 0.5191742






# Compute OLS Coefficient
beta <- cov(y, w) / var(w) 
alpha <- mean(y) - mean(w) * beta # 

# Compute standard error for beta
epsilon <- y - alpha - w * beta
se_numer <- sqrt(mean(epsilon^2 * (w - mean(w))^2))
se <- (se_numer / var(w)) / sqrt(n)


mean(epsilon^2 * w^2) - mean(epsilon * w)^2

mean((epsilon - mean(epsilon)))



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
