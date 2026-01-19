library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("C:\\Users\\DAAI\\Downloads\\2.ecommerce_transactions.csv")

df <- na.omit(df)  # Remove missing rows

# Convert to factors where appropriate
df$Product_Category <- as.factor(df$Product_Category)
df$Payment_Mode <- as.factor(df$Payment_Mode)
df$Region <- as.factor(df$Region)

# 1) Average Transaction Amount per Product Category
product_avg <- df %>%
  group_by(Product_Category) %>%
  summarise(Avg_Amount = mean(Transaction_Amount))

ggplot(product_avg, aes(x = Product_Category, y = Avg_Amount, fill = Product_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg_Amount, 2)), vjust = -0.5) +
  labs(title = "Average Transaction Amount by Product Category", y = "Avg Transaction Amount") +
  theme_minimal()

# 2) Transaction Amount Trend by Payment Mode
payment_trend <- df %>%
  group_by(Payment_Mode) %>%
  summarise(Avg_Amount = mean(Transaction_Amount))

ggplot(payment_trend, aes(x = Payment_Mode, y = Avg_Amount, fill = Payment_Mode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg_Amount, 2)), vjust = -0.5) +
  labs(title = "Average Transaction Amount by Payment Mode", y = "Avg Transaction Amount") +
  theme_minimal()

# 3) Distribution of Transactions by Region (Pie chart)
region_counts <- df %>%
  count(Region)

ggplot(region_counts, aes(x = "", y = n, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Transaction Distribution by Region")
