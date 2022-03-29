# Lecture Plots ================================================================
# Author: Thomas Wiemann 

# A script containing code to generate the figures used in the lecture slides.

# Preliminaries ================================================================

# Load dependencies
library(ggplot2) # for neat plots

# Set random seed 
set.seed(9951)

# Load custom utility functions 
source("_functions//fun_utilities.R") # imports save_img()

# Lecture: Review of Probability Theory ========================================

# Plot of Normal Probability Density Functions 
plt <- ggplot() + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1, col = "blue") +
  annotate(geom="text", x=2.1, y=0.3, label="N(0, 1)",
           color="blue", size = 10) +
  stat_function(fun = dnorm, args = list(mean = -2, sd = 2), size = 1, col = "red") +
  annotate(geom="text", x=-4.6, y=0.185, label="N(-2, 4)",
           color="red", size = 10) +
  ylab("Density") + 
  xlab("x") +
  xlim(c(-6.5, 6.5)) + 
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20, family="Times"))
save_img(plt, "figures/lectures/lecture_2_normpdf.pdf", w = 8, h = 7)