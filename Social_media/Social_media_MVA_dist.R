library(readxl)

data <- read_excel("D:/MVA/combined/social_media_cleaned.xlsx")
data <- data[, -1]
distance = as.matrix(dist(scale(data)))
sum(distance[17,])

library(readxl)
data <- read_excel("D:/MVA/combined/social_media_cleaned.xlsx")
data <- data[, -1]
scale <- scale(data)

classcov <- cor(data)
classmean <- colMeans(data)

scale <- mahalanobis(data, classmean, classcov)
scale

print(scale[17])
