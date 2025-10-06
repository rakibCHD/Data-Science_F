setwd("D:/archive(1)")
data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", stringsAsFactors = TRUE)
head(data)

library(ggplot2)
library(infotheo)

cat("\n Pearson Correlation:\n")
cat("Sales vs Profit:", cor(data$Sales, data$Profit, method = "pearson"), "\n")
cat("Discount vs Profit:", cor(data$Discount, data$Profit, method = "pearson"), "\n")

ggplot(data, aes(x = Sales, y = Profit)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Pearson Correlation: Sales vs Profit",
       x = "Sales", y = "Profit") +
  theme_minimal()


cat("\n Spearman Correlation:\n")
cat("Sales vs Profit:", cor(data$Sales, data$Profit, method = "spearman"), "\n")
cat("Discount vs Profit:", cor(data$Discount, data$Profit, method = "spearman"), "\n")

ggplot(data, aes(x = rank(Sales), y = rank(Profit))) +
  geom_point(color = "darkgreen") +
  labs(title = "Spearman Correlation: Ranked Sales vs Ranked Profit",
       x = "Ranked Sales", y = "Ranked Profit") +
  theme_minimal()


cat("\n Kendall Correlation:\n")
cat("Sales vs Profit:", cor(data$Sales, data$Profit, method = "kendall"), "\n")
cat("Discount vs Profit:", cor(data$Discount, data$Profit, method = "kendall"), "\n")

ggplot(data, aes(x = Sales, y = Profit)) +
  geom_jitter(color = "purple", width = 0.2, height = 0.2) +
  labs(title = "Kendall Correlation: Sales vs Profit (Jittered)",
       x = "Sales", y = "Profit") +
  theme_minimal()


cat("\n ANOVA (Does Region affect Profit?):\n")
anova_result <- aov(Profit ~ Region, data = data)
summary(anova_result)

ggplot(data, aes(x = Region, y = Profit, fill = Region)) +
  geom_boxplot() +
  labs(title = "ANOVA: Profit by Region",
       x = "Region", y = "Profit") +
  theme_minimal()


cat("\n Chi-Squared Test (Region vs Category):\n")
chisq_result <- chisq.test(table(data$Region, data$Category))
print(chisq_result)

ggplot(data, aes(x = Region, fill = Category)) +
  geom_bar(position = "dodge") +
  labs(title = "Chi-Square Test: Region vs Category",
       x = "Region", y = "Count") +
  theme_minimal()


library(infotheo)
library(ggplot2)

sales_disc <- discretize(data$Sales, disc = "equalfreq", nbins = 10)
profit_disc <- discretize(data$Profit, disc = "equalfreq", nbins = 10)
mi <- mutinformation(sales_disc, profit_disc)
cat("Mutual Information (Sales vs Profit):", mi, "\n")

discrete_df <- data.frame(
  Sales = as.factor(sales_disc[[1]]),
  Profit = as.factor(profit_disc[[1]])
)

heatmap_data <- as.data.frame(table(discrete_df))

ggplot(heatmap_data, aes(x = Sales, y = Profit, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Mutual Information: Discretized Sales vs Profit",
       x = "Discretized Sales", y = "Discretized Profit",
       fill = "Frequency") +
  theme_minimal()

