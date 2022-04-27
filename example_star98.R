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

# Load the star98.csv data
df <- read.csv("data/star98.csv")
n <- nrow(df)

# Define variables
y <- df$math_scr # Average math score
w <- df$str # Student-teacher ratio
x <- df$avginc # Average parental income

# Histogram ====================================================================

# Plot histogram of the student teacher ratio
plt <- ggplot() + 
  geom_histogram(aes(x = w, y =..density..), bins = 50) +
  ylab("Density") + 
  xlab("Student-Teacher Ratio") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20,  family="Times"))
save_img(plt, "figures/lectures/lecture_6B_starhist.pdf", w = 8, h = 7)

# BLP-Coefficient ==============================================================

# Compute OLS Coefficient
beta <- cov(y, w) / var(w) # -1.938591
alpha <- mean(y) - mean(w) * beta # 691.4174

# Compute standard error for beta
epsilon <- y - alpha - w * beta
se_numer <- sqrt(mean(epsilon^2 * (w - mean(w))^2))
se <- (se_numer / var(w)) / sqrt(n) # 0.5191742

# Compute 95% confidence interval
beta + c(-1, 1) * qnorm(0.975) * se

# Balance Test =================================================================

# Compute OLS estimate of income on student-teacher ratio
beta_x <- cov(x, w) / var(w) # -0.886878
alpha_x <- mean(x) - mean(w) * beta_x # 32.73525

# Compute standard error for beta_x
epsilon_x <- x - alpha_x - w * beta_x
se_numer_x <- sqrt(mean(epsilon_x^2 * (w - mean(w))^2))
se_x <- (se_numer_x / var(w)) / sqrt(n) # 0.2230173

# Compute the test statistic
Tn <- abs(beta_x) / se_x # 3.976722

# Compute the p-value
2 * (1 - pnorm(Tn)) # 6.987165e-05
