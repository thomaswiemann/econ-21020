# Problem Set 1 Solutions ======================================================
# Author: Thomas Wiemann 

# Preliminaries ================================================================

# Load dependencies
library(ggplot2) # for neat plots

# Set random seed 
set.seed(5028)

# Load custom utility functions 
source("_functions//fun_utilities.R") # imports save_img()

# Problem 8 ====================================================================

# Part a)

# Generate a vector of n draws from a standard normal rv
n <- 10000
mu <- 0
sigma <- 1
x <- rnorm(n, mu, sigma)

# Plot a histogram of the draws using ggplot2
plt <- ggplot() + 
  geom_histogram(aes(x = x, y =..density..), binwidth = 0.2) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1) +
  ylab("Density") + 
  xlab("X") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20,  family="Times"))
save_img(plt, "figures/problem_sets/pset_1_normhist.pdf", w = 8, h = 7)

# Part b)

# Generate a vector of n draws from a uniform(-1, 1) rv
n <- 10000
min_y <- -1
max_y <- 1
y <- runif(n, min_y, max_y)

# Plot a histogram of the draws using ggplot2
plt <- ggplot() + 
  geom_histogram(aes(x = y, y =..density..), bins = 50) +
  stat_function(fun = dunif, args = list(min = -1, max = 1), size = 1) +
  ylab("Density") + 
  xlab("Y") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20,  family="Times"))
save_img(plt, "figures/problem_sets/pset_1_unifhist.pdf", w = 8, h = 7)

# Problem 9 ====================================================================

# Part b) 

# Define a custom function that returns a Bernoulli rv
my_rbernoulli <- function(n, p) {
  # Generate draws
  x <- (runif(n) <= p) * 1 # multiply by 1 to convert to numeric
  # Return draws
  return(x)
}#MY_RBERNOULLI

# Test the custom Bernoulli generator function
x <- my_rbernoulli(10000, 0.5)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 0.5

# Part c) 

# Define a custom function that returns a Bernoulli rv
my_rbinomial <- function(n, p, m) {
  # Generate Bernoulli draws
  x <- matrix(my_rbernoulli(n * m, p), nrow = n, ncol = m)
  # Sum across rows to generate Binomial draws
  x <- rowSums(x)
  # Return draws
  return(x)
}#MY_RBINOMIAL

# Test the custom Binomial generator function
x <- my_rbinomial(10000, 0.5, 10)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 5