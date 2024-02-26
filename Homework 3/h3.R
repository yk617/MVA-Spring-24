library(ggplot2)
library(dplyr)

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

# 1. Qualifying Times (q1, q2, q3)
#Objective: Plot the distribution of Q1, Q2, and Q3 qualifying times by circuit, with each driver represented by their code.
merged_data <- merge(qualifying, driver_standings, by = c("raceId", "driverId"))
merged_data <- merge(merged_data, races, by = "raceId")

# Extract only numeric values from q1, q2, and q3 columns
merged_data$q1_numeric <- as.numeric(gsub(":", ".", merged_data$q1))
merged_data$q2_numeric <- as.numeric(gsub(":", ".", merged_data$q2))
merged_data$q3_numeric <- as.numeric(gsub(":", ".", merged_data$q3))

# Create histogram plots to compare the distribution of qualifying times (q1, q2, q3) and drivers
merged_data <- merge(merged_data, drivers[, c("driverId", "code")], by = "driverId", all.x = TRUE)

merged_data <- merged_data %>%
  select(-driverId)

names(merged_data)[names(merged_data) == "code"] <- "driverCode"

# Plot Q1 qualifying time by circuit
ggplot(merged_data, aes(x = name, y = q1_numeric, fill = factor(driverCode))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Q1 Qualifying Time",
       x = "Circuit Name",
       y = "Qualifying Time (Q1 in millisec)",
       fill = "Driver Code") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Q2 qualifying time by circuit
ggplot(merged_data, aes(x = name, y = q2_numeric, fill = factor(driverCode))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Q2 Qualifying Time",
       x = "Circuit Name",
       y = "Qualifying Time (Q2 in millisec)",
       fill = "Driver Code") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Q3 qualifying time by circuit
ggplot(merged_data, aes(x = name, y = q3_numeric, fill = factor(driverCode))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Q3 Qualifying Time",
       x = "Circuit Name",
       y = "Qualifying Time (Q3 in millisec)",
       fill = "Driver Code") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Explanation: These bar plots show the distribution of qualifying times for each driver at each circuit. By visualizing this data, we can identify trends and differences in performance between drivers across different circuits.

# 2. Driver Performance (Points)
#Objective: Show the total points earned by each driver at each circuit.
driver_points_per_circuit <- merged_data %>%
  group_by(name, driverCode) %>%
  summarise(total_points = sum(points)) %>%
  ungroup()

ggplot(driver_points_per_circuit, aes(x = name, y = total_points, fill = driverCode)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Points Earned by Each Driver per Circuit",
       x = "Circuit Name",
       y = "Total Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Explanation: This bar plot displays the total points earned by each driver at each circuit. It provides insights into the consistency and performance of drivers across different circuits. By examining this plot, we can identify which drivers performed well consistently and which circuits were more favorable for certain drivers.

# 3. Constructor Performance (Points)
# Objective: Visualize the distribution of constructor points by circuit.
constructor_circuit_points <- merged_data %>%
  group_by(name, constructorId) %>%
  summarise(total_points = sum(points)) %>%
  arrange(desc(total_points)) %>%
  left_join(constructors, by = "constructorId") 

ggplot(constructor_circuit_points, aes(x = name.x, y = total_points, fill = name.y)) +
  geom_bar(stat = "identity") +
  labs(title = "Constructor Points by Circuit",
       x = "Circuit Name",
       y = "Total Points",
       fill = "Constructor Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Explanation: This bar plot illustrates the total points earned by constructors at each circuit. It helps us understand the performance of constructors across different circuits and identifies circuits where certain constructors were more successful.
