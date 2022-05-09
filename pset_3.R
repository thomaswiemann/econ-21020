# Problem Set 3 Solutions ======================================================
# Author: Thomas Wiemann 

# Preliminaries ================================================================

# Load dependencies
library(ggplot2) # for neat plots

# Load custom utility functions 
source("_functions//fun_utilities.R") # imports save_img()

# Problem 5 ====================================================================

# Define the Yitzhaki weights for U(-1, 1)
omega <- function(x) {
  return((3/4 * (1 - x^2) * ((-1 <= x) & (x <= 1))))
}#OMEGA

# Plot of the Yitzhaki weights for standard uniform
plt <- ggplot() + 
  geom_function(fun = omega, size = 1) +
  ylab(expression("Yitzhaki Weights "~omega~"(t)")) + 
  xlab("t") +
  xlim(c(-1.5, 1.5)) + 
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20, family="Times"))
save_img(plt, "figures/problem_sets/pset_3_yitzhakiweights.pdf", w = 8, h = 7)

# Problem 6 ====================================================================

# Load the ak91.csv data 
df <- read.csv("data/ak91.csv")
n <- nrow(df)

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# Part b)
beta <- cov(yrs_educ, wkly_wage) / var(yrs_educ) # 29.6224
alpha <- mean(wkly_wage) - mean(yrs_educ) * beta # 61.19537

# Part c)
alpha + 16 * beta # 535.1538

# Part d)
epsilon <- wkly_wage - alpha - yrs_educ *  beta
se <- (sd(epsilon * (yrs_educ - mean(yrs_educ))) / var(yrs_educ)) / 
  sqrt(n) # 0.2101698

# Part e)
Tn <- abs((beta - 31) / se) # 6.554683

# Part f)
2 * (1 - pnorm(Tn)) # 5.576029e-11

# Problem 7 ====================================================================

# Part a)

# Define a custom function to compute the ols estimates
my_simplecoef <- function(y, x) {
  # Compute and return estimates for alpha and beta
  beta <- cov(y, x) / var(x) 
  alpha <- mean(y) - mean(x) * beta 
  return(c(alpha, beta))
}#MY_SIMPLECOEF

# Test the function using your solution to Problem 6
coef <- my_simplecoef(wkly_wage, yrs_educ)
coef

# Part b)

# Define a custom function to compute the blp estimates
my_simpleblp <- function(coef, x) {
  # Compute and return BLP estimates
  blp <- coef[1] + coef[2] * x
  return(blp)
}#MY_SIMPLEBLP

# Test the function
mean(wkly_wage) - mean(my_simpleblp(coef, yrs_educ))

# Part c)

# Define a custom function to compute the standard error
my_simplese <- function(coef, y, x) {
  # Compute and return the standard error
  epsilon <- y - my_simpleblp(coef, x)
  se <- (sd(epsilon * (x - mean(x))) / var(x)) / sqrt(n)
  return(se)
}#MY_SIMPLESE

# Test the function using your solution to Problem 6
se <- my_simplese(coef, wkly_wage, yrs_educ)
se

# Part d)

# Define a custom function to compute the test stat and p-value
my_simpleteststat <- function(beta, se) {
  # Compute and return the test stat and p-value
  Tn <- abs(beta / se)
  pval <- 2 * (1 - pnorm(Tn))
  return(c(Tn, pval))
}#MY_SIMPLETESTSTAT

# Test the function using your solution to Problem 6
my_simpleteststat(coef[2] - 31, se)

# Part e)
my_simpleols <- function(y, x) {
  # Compute the the ols estimate, se, Tn, and p-val
  coef <- my_simplecoef(y, x)
  se <- my_simplese(coef, y, x) 
  test_stats <- my_simpleteststat(coef[2], se)
  # Combine results and return
  res <- round(c(coef[2], se, test_stats), 3)
  names(res) <- c("beta", "SE", "Tn", "p-val")
  return(res)
}#MY_SIMPLEOLS

# Test the function using your solution to Problem 6
my_simpleols(wkly_wage, yrs_educ)