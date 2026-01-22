library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/DAAI/Downloads/4.traffic_accidents.csv", stringsAsFactors = FALSE)

data$Severity <- factor(
  data$Severity,
  levels = c("Minor", "Serious", "Fatal"),
  ordered = TRUE
)

accident_freq <- data %>%
  group_by(Location, Accident_Type, Severity) %>%
  summarise(Frequency = n(), .groups = "drop")

ggplot(accident_freq, aes(
  x = Location,
  y = Frequency,
  color = Severity,
  size = Frequency,
  shape = Accident_Type
)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    values = c("Minor" = "green", "Serious" = "orange", "Fatal" = "red")
  ) +
  labs(
    title = "Traffic Accident Analysis – Roll No: 23BAD100",
    subtitle = "Color: Severity | Size: Frequency | Shape: Accident Type",
    x = "Location",
    y = "Accident Frequency"
  ) +
  theme_minimal()
