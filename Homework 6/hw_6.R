library(dplyr)
library(psych)
library(GPArotation)

Walmart_sales <- read.csv("D:/MVA/Git/dataset/Walmart_sales.csv")

# Remove null values or empty spaces
Walmart_sales <- na.omit(Walmart_sales)

# Compute average values per store
store_avg <- Walmart_sales %>%
  group_by(Store) %>%
  summarise(
    Avg_Temperature = mean(Temperature, na.rm = TRUE),
    Avg_Fuel_Price = mean(Fuel_Price, na.rm = TRUE),
    Avg_CPI = mean(CPI, na.rm = TRUE),
    Avg_Unemployment = mean(Unemployment, na.rm = TRUE)
  )

# structure of the resulting dataframe
str(store_avg)

# parallel analysis to determine the number of factors
num_factor <- fa.parallel(store_avg[-1])

# results of parallel analysis to determine the number of factors
print(num_factor)

# Perform factor analysis with the determined number of factors
factor_analysis <- fa(store_avg[, c("Avg_Temperature", "Avg_Fuel_Price", "Avg_CPI", "Avg_Unemployment")], 
                      nfactors = 2, 
                      rotate = "varimax")

# factor analysis results
print(factor_analysis)

# factor loadings
factor_loadings <- factor_analysis$loadings
print(factor_loadings)

#Visualizations
fa.plot(factor_analysis)
fa.diagram(factor_analysis)
