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

# Display imported stock data
stock_data <- import_stock_data("AAPL", "2026-01-01", "2026-09-01")
head(stock_data)

# Calculate and display statistics
statistics <- calculate_statistics(stock_data)
print(statistics)

# Visualize closing prices
chartSeries(
  stock_data,
  name = "AAPL Stock Price",
  theme = "white"
)

# Visualize 20-day moving average
chartSeries(
  stock_data,
  name = "AAPL Stock Price with 20-Day Moving Average",
  theme = "white",
  TA = "addSMA(n = 20)"
)

# Read all stock symbols from portfolio.txt
symbols <- scan("Assignment2/portfolio.txt", what = character())

# Process all stocks in the portfolio
for (symbol in symbols) {
  
  stock_data <- import_stock_data(
    symbol,
    "2026-01-01",
    "2026-09-01"
  )
  
  cat("\nStock:", symbol, "\n")
  
  # Display imported stock data
  print(head(stock_data))
  
  # Calculate and display statistics
  statistics <- calculate_statistics(stock_data)
  print(statistics)
  
  # Display stock price with 20-day moving average
  chartSeries(
    stock_data,
    name = paste(symbol, "Stock Price with 20-Day Moving Average"),
    theme = "white",
    TA = "addSMA(n = 20)"
  )
}