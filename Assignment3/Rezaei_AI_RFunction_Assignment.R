# BDA400 Assignment 3 - Prompting R Functions with AI
# Student: Parvaneh Rezaei
# AI Assistance Declaration:
# I used ChatGPT (GPT-5.6 Sol) for assistance with designing, refining,
# explaining, and testing an R function.
# Prompts used are documented below in this R script.
# I verified the outputs by running the function in RStudio with sample data,
# NA values, edge cases, non-numeric input, and by comparing the results
# with R's built-in boxplot.stats() function.
# All final calculations and testing were completed and verified by me.
# I am responsible for the accuracy and originality of this work.
# AI Assistance Declaration:
# I used ChatGPT (GPT-5.6 Sol) for assistance with prompt development,
# R function design, debugging suggestions, and writing assistance.
# I manually tested and verified the R code and outputs in RStudio.
# All final calculations and verification were completed by me.
# I am responsible for the accuracy and originality of this work.

# Original Task Description:
# Remove outliers from a numeric vector using the IQR method.

# Prompt 1:
# Write an R function that removes outliers from a numeric vector
# using the IQR method.

# Original AI-Generated Function:

remove_outliers_original <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  x[x >= lower_bound & x <= upper_bound]
}
# BDA400 Assignment 3 - Prompting R Functions with AI
# Student: Parvaneh Rezaei

# AI Assistance Declaration:
# I used ChatGPT (GPT-5.6 Sol) for assistance with prompt development,
# R function design, debugging suggestions, and writing assistance.
# I manually tested and verified the R code and outputs in RStudio.
# All final calculations and verification were completed by me.
# I am responsible for the accuracy and originality of this work.

# Original Task Description:
# Remove outliers from a numeric vector using the IQR method.

# Prompt 1:
# Write an R function that removes outliers from a numeric vector
# using the IQR method.

# Original AI-Generated Function:

remove_outliers_original <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  x[x >= lower_bound & x <= upper_bound]
}# Test the original function
test_data <- c(10, 15, 999, 20, 25)

remove_outliers_original(test_data)
# Improved Function:
remove_outliers <- function(x) {
  # Check that the input is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  # Remove missing values
  x <- x[!is.na(x)]
  
  # Calculate first and third quartiles
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  
  # Calculate the interquartile range
  IQR_value <- IQR(x)
  
  # Calculate lower and upper limits
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Return values that are not outliers
  return(x[x >= lower_bound & x <= upper_bound])
}
# Test improved function with NA value
test_data2 <- c(10, 15, 999, 20, 25, NA)
remove_outliers(test_data2)
# Compare with R's built-in boxplot.stats()
boxplot.stats(test_data2)$out
# Edge Case Tests

# Test data with no outliers
remove_outliers(c(10, 11, 12, 13, 14))

# Test data containing NA values
remove_outliers(c(10, NA, 15, 20, 25))

# Test invalid non-numeric input
try(remove_outliers(c("A", "B", "C")))

# ---------------------------------------------------------
# Line-by-Line Explanation of Final Function
# ---------------------------------------------------------

# remove_outliers <- function(x) {
# Creates a function named remove_outliers with input x.

# if (!is.numeric(x)) {
# Checks whether the supplied vector is numeric.

# stop("Input must be a numeric vector.")
# Stops the function and displays an error for non-numeric input.

# x <- x[!is.na(x)]
# Removes missing (NA) values from the input.

# Q1 <- quantile(x, 0.25)
# Calculates the first quartile (25th percentile).

# Q3 <- quantile(x, 0.75)
# Calculates the third quartile (75th percentile).

# IQR_value <- IQR(x)
# Calculates the interquartile range.

# lower_bound <- Q1 - 1.5 * IQR_value
# Calculates the lower limit for identifying outliers.

# upper_bound <- Q3 + 1.5 * IQR_value
# Calculates the upper limit for identifying outliers.

# return(x[x >= lower_bound & x <= upper_bound])
# Returns only values that fall within the acceptable range.

# ---------------------------------------------------------
# AI Prompts Used
# ---------------------------------------------------------

# Prompt 1:
# "Write an R function that removes outliers from a numeric vector
# using the IQR method."

# Prompt 2:
# "Revise the function to include comments, argument validation,
# and return a clean result."

# Prompt 3:
# "Explain what each line of the R code does and how you might
# test if it works correctly."

# Prompt 4:
# "Update the function to handle NA values."

# Prompt 5:
# "Optimize the function for efficiency and readability."

# Prompt 6:
# "How could you test this function with edge cases?"

# Prompt 7:
# "Summarize how this R function changed across revisions.
# Highlight what improvements were human-driven vs. AI-generated."
# ---------------------------------------------------------
# Appendix: Key AI Responses
# ---------------------------------------------------------

# Key Response 1:
# AI suggested an initial R function using Q1, Q3, and the IQR
# to calculate lower and upper outlier boundaries.

# Key Response 2:
# AI suggested improving the function by validating that the
# input is numeric and by handling missing (NA) values.

# Key Response 3:
# AI explained the purpose of each line of the final function
# and suggested testing the function with different inputs.

# Key Response 4:
# AI suggested edge-case testing with data containing no outliers,
# NA values, and non-numeric input.

# Key Response 5:
# AI suggested comparing the results with R's built-in
# boxplot.stats() function as an independent verification method.