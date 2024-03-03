library(dplyr)
library(ggplot2)

#Univariate Mean and Variance Analysis
#1. Univariate Analysis of Weekly Sales

#Procedure:

Walmart_sales$log_Weekly_Sales <- log(Walmart_sales$Weekly_Sales)

# Calculate mean and variance of log-transformed Weekly Sales
mean_log_sales <- mean(Walmart_sales$log_Weekly_Sales)
var_log_sales <- var(Walmart_sales$log_Weekly_Sales)

mean_log_sales
var_log_sales

# Histogram of log-transformed Weekly Sales
ggplot(Walmart_sales, aes(x = log_Weekly_Sales)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Log-transformed Weekly Sales",
       x = "Log(Weekly Sales)",
       y = "Frequency") +
  theme_minimal()

#Explanation:
  
#  The mean of the logarithm of weekly sales represents the average level of sales on a logarithmic scale.
#The variance of the logarithm of weekly sales indicates the spread or variability of sales values around the mean on a logarithmic scale.
#The histogram shows the distribution of the logarithm of weekly sales, giving insight into the pattern and shape of the sales data after logarithmic transformation.

#2. Multivariate Analysis of Weekly Sales by Store

#Procedure:

# Calculate mean and variance of log-transformed Weekly Sales by Store
store_sales_summary <- Walmart_sales %>%
  group_by(Store) %>%
  summarise(mean_log_Weekly_Sales = mean(log_Weekly_Sales),
            var_log_Weekly_Sales = var(log_Weekly_Sales))

store_sales_summary

# Scatter Plot of Mean vs. Variance of Log-transformed Weekly Sales by Store
ggplot(store_sales_summary, aes(x = mean_log_Weekly_Sales, y = var_log_Weekly_Sales)) +
  geom_point(color = "blue") +
  labs(title = "Mean vs. Variance of Log-transformed Weekly Sales by Store",
       x = "Mean Log(Weekly Sales)",
       y = "Variance Log(Weekly Sales)") +
  theme_minimal()

#Explanation:
  
#The scatter plot visualizes the relationship between the mean and variance of the logarithm of weekly sales for each store.
