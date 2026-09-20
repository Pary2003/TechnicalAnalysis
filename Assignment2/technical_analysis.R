# BDA400 Assignment 2
# Technical Analysis using R
# Student: Parvaneh Rezaei

library(quantmod)
library(TTR)

# Function to import stock data
import_stock_data <- function(symbol, start_date, end_date) {
  stock_data <- getSymbols(
    symbol,
    src = "yahoo",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
  
  return(stock_data)
} 
# Function to calculate statistics
calculate_statistics <- function(stock_data) {
  
  closing_prices <- Cl(stock_data)
  
  # 20-day moving average
  moving_average <- SMA(closing_prices, n = 20)
  
  # Mean
  mean_price <- mean(closing_prices, na.rm = TRUE)
  
  # Median
  median_price <- median(closing_prices, na.rm = TRUE)
  
  # Standard deviation
  sd_price <- sd(closing_prices, na.rm = TRUE)
  
  # Mode
  get_mode <- function(x) {
    x <- na.omit(as.numeric(x))
    values <- unique(x)
    values[which.max(tabulate(match(x, values)))]
  }
  
  mode_price <- get_mode(closing_prices)
  
  return(list(
    Moving_Average = moving_average,
    Mean = mean_price,
    Median = median_price,
    Mode = mode_price,
    Standard_Deviation = sd_price
  ))
}