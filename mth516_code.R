# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(psych)
library(car)
library(stats)

# Load data
data <- read.csv("BankCustomerData_Dataset.csv")

# Data Validation and Basic Data Exploration
# Checking data structure
str(data)

print("Missing data by column:")
colSums(is.na(data))
data <- data[!duplicated(data), ]

#### plot representing house type in relation with martial status ####
ggplot(data, aes(x = MARITALSTATUS, fill = HOUSINGTYPE)) +
  geom_bar(position = "dodge") +
  labs(title = "Marital Status by Housing Type",
       x = "Marital Status",
       y = "Count") +
  theme_minimal()
# Summary statistics for numeric columns
summary_stats <- data %>%
  select(CHILDRENCOUNT, INCOMETOTAL, FAMSIZE) %>%
  summary()
print("Summary statistics for numeric columns:")
print(summary_stats)

# Descriptive Analysis and Visualizations
# Frequency table for categorical data
categorical_vars <- c("GENDER", "EDUCATIONLEVEL", "OWNPROPERTY", "MARITALSTATUS", "HOUSINGTYPE")

for (var in categorical_vars) {
  cat("\nFrequency distribution for", var, ":\n")
  print(table(data[[var]]))
}

# Visualization: Bar chart of EDUCATIONLEVEL by GENDER
ggplot(data, aes(x = GENDER, fill = EDUCATIONLEVEL)) +
  geom_bar(position = "dodge") +
  labs(title = "Education Level by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Visualization: Pie chart of OWNPROPERTY by GENDER
prop_data <- data %>%
  group_by(GENDER, OWNPROPERTY) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(prop_data, aes(x = "", y = percentage, fill = OWNPROPERTY)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ GENDER) +
  labs(title = "Percentage of Property Ownership by Gender", y = "Percentage", fill = "Owns Property")

# Histogram of Income by Gender
ggplot(data, aes(x = INCOMETOTAL, fill = GENDER)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(title = "Distribution of Income by Gender", x = "Income Total", y = "Frequency") +
  theme_minimal()

# Box plot of INCOMETOTAL by HOUSINGTYPE and GENDER
ggplot(data, aes(x = HOUSINGTYPE, y = INCOMETOTAL, fill = GENDER)) +
  geom_boxplot() +
  labs(title = "Income by Housing Type and Gender", x = "Housing Type", y = "Income Total") +
  theme_minimal()

# ANOVA Analysis

# One-Way ANOVA on Housing Type
#anova_housing <- aov(INCOMETOTAL ~ HOUSINGTYPE, data = data)
#summary(anova_housing)
#TukeyHSD(anova_housing)

# One-Way ANOVA with Blocking Variable (Credit Status)
#anova_blocking <- aov(INCOMETOTAL ~ HOUSINGTYPE + CREDITSTATUS, data = data)
#summary(anova_blocking)

# Post-Hoc Comparisons for Housing Type
#pairwise.t.test(data$INCOMETOTAL, data$HOUSINGTYPE, p.adjust.method = "bonferroni")

# One-Way ANOVA on Education Level
#anova_education <- aov(INCOMETOTAL ~ EDUCATIONLEVEL, data = data)
#summary(anova_education)
#TukeyHSD(anova_education)

# Non-parametric Tests
# Distribution Examination for Housing Type
#library(FSA)
#hist(data$INCOMETOTAL, main = "Distribution of Income by Housing Type", xlab = "Income Total")
#qqnorm(data$INCOMETOTAL)
#qqline(data$INCOMETOTAL)

# Kruskal-Wallis Test for Housing Type
#kruskal.test(INCOMETOTAL ~ HOUSINGTYPE, data = data)

# Kruskal-Wallis Test for Education Level
#kruskal.test(INCOMETOTAL ~ EDUCATIONLEVEL, data = data)
