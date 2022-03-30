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
save_img(plt, "figures/lectures/lecture_2A_normpdf.pdf", w = 8, h = 7)

# Plot of Chi^2 Density Functions 
plt <- ggplot() + 
  stat_function(fun = dchisq, args = list(df = 1), size = 1, col = "blue") +
  annotate(geom="text", x=6, y=0.3, label="chi^{2}~(1)", 
           parse = TRUE,  color="blue", size = 10) +
  stat_function(fun = dchisq, args = list(df = 10), size = 1, col = "red") +
  annotate(geom="text", x=14, y=0.125, label="chi^{2}~(10)", 
           parse = TRUE,  color="red", size = 10) +
  stat_function(fun = dchisq, args = list(df = 30), size = 1, col = "green") +
  annotate(geom="text", x=33, y=0.08, label="chi^{2}~(30)", 
           parse = TRUE,  color="green", size = 10) +
  ylab("Density") + 
  xlab("x") +
  xlim(c(0, 50)) + 
  theme_classic(base_size = 20) +
  theme(text = element_text(size = 20, family="Times"))
save_img(plt, "figures/lectures/lecture_2A_chi2pdf.pdf", w = 8, h = 7)
