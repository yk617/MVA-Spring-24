library(ggplot2)

setwd("D:/MVA/dataActual")

qualifying <- read.csv("qualifying.csv")
drivers <- read.csv("drivers.csv")
driver_standings <- read.csv("driver_standings.csv")
constructors <- read.csv("constructors.csv")
races <- read.csv("races.csv")
constructor_standings <- read.csv("constructor_standings.csv")

qualifying$q1 <- as.numeric(gsub("[^0-9.]", "", qualifying$q1))
qualifying$q2 <- as.numeric(gsub("[^0-9.]", "", qualifying$q2))
qualifying$q3 <- as.numeric(gsub("[^0-9.]", "", qualifying$q3))

# 1. Qualifying Times (q1, q2, q3) - Univariate Analysis
# Visualize the distribution of qualifying times for each session
#ggplot(qualifying, aes(x = q1)) +
#  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
#  labs(title = "Distribution of Qualifying Times (Q1)",
#       x = "Qualifying Time (seconds)", y = "Frequency")

# 1. Qualifying Times (q1, q2, q3) - Univariate Analysis
# Visualize the distribution of qualifying times for each session
ggplot(qualifying, aes(x = q1)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Qualifying Times (Q1)",
       x = "Qualifying Time (seconds)", y = "Frequency")

ggplot(qualifying, aes(x = q2)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Qualifying Times (Q2)",
       x = "Qualifying Time (seconds)", y = "Frequency")

ggplot(qualifying, aes(x = q3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Qualifying Times (Q3)",
       x = "Qualifying Time (seconds)", y = "Frequency")

merged_data <- merge(qualifying, driver_standings, by = c("raceId", "driverId"))
merged_data <- merge(merged_data, races, by = "raceId")

# Extract only numeric values from q1, q2, and q3 columns
merged_data$q1_numeric <- as.numeric(gsub(":", ".", merged_data$q1))
merged_data$q2_numeric <- as.numeric(gsub(":", ".", merged_data$q2))
merged_data$q3_numeric <- as.numeric(gsub(":", ".", merged_data$q3))

# Create violin plots to compare the distribution of qualifying times (q1, q2, q3) and driver points
ggplot(merged_data, aes(x = name, y = points)) +
  geom_violin() +
  labs(x = "Circuit Name", y = "Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggplot(merged_data, aes(x = name, y = q2_numeric)) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Distribution of Q2 Qualifying Time", x = "Circuit Name", y = "Qualifying Time (Q2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(merged_data, aes(x = name, y = q3_numeric)) +
  geom_violin(fill = "lightyellow") +
  labs(title = "Distribution of Q3 Qualifying Time", x = "Circuit Name", y = "Qualifying Time (Q3)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Driver Performance (Points) - Univariate Analysis
# Visualize the distribution of driver points
ggplot(driver_standings, aes(x = points)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Driver Points", x = "Driver Points", y = "Frequency")

#
ggplot(merged_data, aes(x = name, y = points)) +
  geom_violin(fill = "lightcoral") +
  labs(title = "Distribution of Driver Points", x = "Circuit Name", y = "Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Constructor Performance (Points) - Univariate Analysis
# Visualize the distribution of constructor points
ggplot(constructor_standings, aes(x = points)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Constructor Points", x = "Constructor Points", y = "Frequency")

merged_data <- merge(constructor_standings, constructors, by = "constructorId")

ggplot(merged_data, aes(x = name, y = points)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  coord_flip() +
  labs(title = "Distribution of Constructor Points", x = "Constructor", y = "Points")
