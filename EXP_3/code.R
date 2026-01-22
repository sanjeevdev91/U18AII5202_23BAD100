library(ggplot2)
library(GGally)
library(dplyr)

data <- read.csv("C:/Users/DAAI/Downloads/3.healthcare_data.csv", stringsAsFactors = FALSE)

data <- data %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(0, 30, 50, 80),
      labels = c("Young", "Middle-aged", "Senior")
    )
  )

ggpairs(
  data,
  columns = c("Age", "BMI", "Glucose_Level", "Blood_Pressure"),
  aes(color = AgeGroup, alpha = 0.7),
  title = "Multivariate Health Data Analysis – Roll No: 23BAD100"
)

cor(data[, c("Age", "BMI", "Glucose_Level", "Blood_Pressure")])
