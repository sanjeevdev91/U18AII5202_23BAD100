library(GGally)
library(ggplot2)
library(lattice)

data <- read.csv("C:/Users/student/Downloads/6.retail_business.csv")

data$Customer_Segment <- as.factor(data$Customer_Segment)
data$Region <- as.factor(data$Region)

ggparcoord(
  data,
  columns = c(
    which(names(data) == "Sales"),
    which(names(data) == "Profit"),
    which(names(data) == "Discount")
  ),
  groupColumn = "Customer_Segment",
  scale = "uniminmax"
) +
  labs(title = "Parallel Coordinate Plot (Roll No: 23BAD100)") +
  theme_minimal()

ggplot(data, aes(x = Sales, y = Profit, size = Discount, color = Customer_Segment)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Bubble Chart: Sales vs Profit (Roll No: 23BAD100)",
    x = "Sales",
    y = "Profit"
  ) +
  theme_minimal()

xyplot(
  Profit ~ Sales | Region,
  data = data,
  type = c("p", "r"),
  layout = c(3, 2),
  main = "Trellis Display by Region (Roll No: 23BAD100)",
  xlab = "Sales",
  ylab = "Profit"
)
