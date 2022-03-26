# Example Problem Set Script ===================================================
# Author: Thomas Wiemann 
#
# This R script serves as an example for submitting code for the programming 
#     exercises of the course. Structuring your code well is important to avoid
#     errors and ensure readibility. Feel free to use this script as a template.
#
# There are various comments scattered throughout the script. These are meant to
#     help you get started but would be very excessive in an actual problem set.
#     For example, you don't need to explain what every section does using 
#     multi-line comments! 

# Preliminaries ================================================================

# Here, we'd typically load required packages or our own custom code stored in 
#     other R scripts, or set a random seed. Here, I'll also include the code
#     for installing the ggplot2 package (but you'll typically omit the code in
#     your script because packages only need to be installed once on a machine.)

# Install dependencies (only once; then delete)
install.packages("ggplot2")

# Load dependencies
library(ggplot2) # for neat plots

# Set random seed (to ensure replicability)
set.seed(2021)

# Problem 1 ====================================================================

# This section generates n = 1000 draws from a Chi^2 random variable with two 
#     degrees of freedom, and plots the corresponding histogram along with the 
#     true density function.

# Simulation parameters
n <- 10000 # number of random draws
df <- 2 # degrees of freedom

# Generate iid draws
x <- rchisq(n, df)

# Plot the histogram
ggplot() + 
  geom_histogram(aes(x = x, y =..density..), binwidth = 0.25) +
  stat_function(fun = dchisq, args = list(df = df), size = 1) +
  ylab("Density") + 
  xlab("X") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20,  family="Times"))

# Problem 2 ====================================================================

# This section create a simple function that takes a matrix X and a Boolean 
#     argument MARGIN. If MARGIN = 1, then the function calculates the mean of 
#     each row of X. If MARGIN = 2, the function calculates the mean of each 
#     columns of X.
#
# Note: The ``apply`` function in R generalizes row or column-wise operations.
#     It's (of course) much more efficient than the below custom function!

my_means <- function(X, MARGIN) {
  # Get the dimensions of X
  X_dim <- dim(X)
  
  # Transpose X if MARGIN == 1
  if (MARGIN == 1) {
    X <- t(X)
  }#IF
  
  # Calculate the averages
  X_means <- rep(0, X_dim[MARGIN])
  for (j in 1:X_dim[MARGIN]) {
    X_means[j] <- mean(X[, j])
  }#FOR
  
  # Return output
  return(X_means)
}#MY_MEANS

# To check the function, its a good idea to compare its output to the base R 
#     functions that do the same thing (but much more efficiently!). Let's 
#     generate a matrix filled with iid draws from N(0, 2).
X_rows <- 10
X_cols <- 100
X <- matrix(rnorm(X_rows * X_cols, 0, 2), X_rows, X_cols)

# First, confirm that the row-averages are the same.
my_means(X, 2)
colMeans(X)
apply(X, 2, mean)

# Second, confirm that the column-averages are the same.
my_means(X, 1)
rowMeans(X)
apply(X, 1, mean)

# Success!