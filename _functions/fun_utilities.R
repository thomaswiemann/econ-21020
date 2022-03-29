# Collection of Utility Functions ==============================================
# Author: Thomas Wiemann 

# Source script with simple utility functions.

# SAVE_IMG =====================================================================

# Function to save image as pdf
save_img <- function(plt, filename, w = 8, h = 7) {
  pdf(filename, w, h)
  plot(plt)
  dev.off()
}#SAVE_IMG