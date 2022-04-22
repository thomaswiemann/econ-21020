# Problem Set 2 Solutions ======================================================
# Author: Thomas Wiemann 

# Problem 5 ====================================================================

# Load the ak91.csv data 
df <- read.csv("data/ak91.csv")

# Store years od education and the weekly wage in seperate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# Find college graduates
has_college_degree <- yrs_educ == 16

# Part a) 

# Compute share of college and high school graduates
p_college <- mean(has_college_degree) # 0.1084857
p_college

# Part b)

# Compute conditional averages
mu_college <- mean(wkly_wage[has_college_degree]) # 594.4866
mu_college

# Part c)

# Compute standard errors using the formula in Problem 4 part l).
nobs <- nrow(df) # 329509
se_college <- sqrt(var(wkly_wage[has_college_degree]) / 
                     (nobs * p_college)) # 2.269014
se_college

# Part d)

# Construct symmetric two-sided confidence interval for college
c(mu_college - qnorm(1 - 0.05 / 2) * se_college, 
  mu_college + qnorm(1 - 0.05 / 2) * se_college) # [1] 590.0394 598.9338

# Problem 6 ====================================================================

# Part a)

# Define a custom function that returns a  two-sided confidence interval
my_confint <- function(mu_hat, se, alpha) {
  # Compute and return the confidence interval
  confint <- c(mu_hat - qnorm(1 - alpha / 2) * se, 
               mu_hat + qnorm(1 - alpha / 2) * se)
  return(confint)
}#MY_CONFINT

# Test the function
my_confint(mu_college, se_college, 0.01) # [1] 588.6420 600.3312

# Part b)

# Define a custom function that returns TRUE if mu_0 is not in confint
my_testrejects <- function(confint, mu_0) {
  # Check whether mu_0 is in confint
  is_in_confint <- confint[1] <= mu_0 &  mu_0 <= confint[2]
  # If mu_0 is in confit, don't reject. Else, reject. 
  is_rejected <- !is_in_confint
  # Return boolean 
  return(is_rejected)
}#MY_TESTREJECTS

# Check whether the test rejects on 1\% significance level
confint_01 <- my_confint(mu_college, se_college, 0.01)
my_testrejects(confint_01, 600) # [1] FALSE

# Check whether the test rejects on 10\% significance level
confint_10 <- my_confint(mu_college, se_college, 0.1)
my_testrejects(confint_10, 600) # [1] TRUE

# Part c)

# Define a custom function for a two-sided test
my_twosidedtest <- function(mu_hat, se, alpha, mu_0) {
  # Compute the two-sided confidence interval with correct significance level
  confint <- my_confint(mu_hat, se, alpha)
  # Check whether mu_0 is in the confidence interval
  is_rejected <- my_testrejects(confint, mu_0)
  # Construct test message 
  if (is_rejected) {
    message <- paste0("The test of `H0: mu = ", mu_0, 
                      "' against `H1: mu != ", mu_0, 
                      "' rejects at a ", round(100*alpha, 2), 
                      "% significance level.")
  } else {
    message <- paste0("The test of `H0: mu = ", mu_0, 
                      "' against `H1: mu != ", mu_0, 
                      "' does not reject at a ", round(100*alpha, 2), 
                      "% significance level.")
    
  }#IF
  # Print the message
  print(message)
}#MY_TWOSIDEDTEST

# Check whether the test rejects on 1\% significance level
my_twosidedtest(mu_college, se_college, 0.01, 600) # Should not reject

# Check whether the test rejects on 10\% significance level
my_twosidedtest(mu_college, se_college, 0.10, 600) # Should reject
