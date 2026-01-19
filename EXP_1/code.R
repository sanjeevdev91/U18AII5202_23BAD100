# ----------------------------
# ROLL NO : 23BAD100
# ----------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("C:/Users/DAAI/Downloads/1.student_performance.csv")


df <- na.omit(df)
df$Subject <- as.factor(df$Subject)
df$Final_Grade <- as.factor(df$Final_Grade)

subject_avg <- df %>%
  group_by(Subject) %>%
  summarise(Mean = mean((Internal_Test1 + Internal_Test2) / 2))

ggplot(subject_avg, aes(x = Subject, y = Mean, fill = Subject)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Mean, 1)), vjust = -0.5) +
  labs(title = "Subject-wise Avg Marks", y = "Marks") +
  theme_minimal()

trend_data <- df %>%
  group_by(Subject) %>%
  summarise(
    Test1 = mean(Internal_Test1),
    Test2 = mean(Internal_Test2)
  ) %>%
  pivot_longer(
    cols = c(Test1, Test2),
    names_to = "Test",
    values_to = "Score"
  )

ggplot(trend_data, aes(x = Test, y = Score, group = Subject, color = Subject)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Trend: Test 1 vs Test 2") +
  theme_minimal()

grade_counts <- df %>% count(Final_Grade)

ggplot(grade_counts, aes(x = "", y = n, fill = Final_Grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Final Grade Distribution")


