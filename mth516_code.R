# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(psych)
library(car)
library(stats)
library(tseries)

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


# Runs Tests
rt_owncar<-runs.test(factor(data$OWNCAR))
print(rt_owncar)
#  not random

rt_ownprop<-runs.test(factor(data$OWNPROPERTY))
print(rt_ownprop)
# not random

data$CREDITSTATUS_BINARY <- ifelse(data$CREDITSTATUS == "0", 1, 0)
rt_credscore<-runs.test(factor(data$CREDITSTATUS_BINARY))
print(rt_credscore)
# not random


# chi square goodness of fit test 

# Gender

observed_counts <- table(data$GENDER)
expected_proportions <- c(0.5, 0.5) 

total_count <- sum(observed_counts)
expected_counts <- total_count * expected_proportions

chi_square_test <- chisq.test(observed_counts, p = expected_proportions)
print(chi_square_test)

# data does not follow an Bernoulli distribution.

# Load ggplot2 if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

gender_labels <- names(observed_counts)
expected_counts <- total_count * expected_proportions
plot_data <- data.frame(
  Gender = rep(gender_labels, 2),
  Count = c(observed_counts, expected_counts),
  Type = rep(c("Observed", "Expected"), each = length(gender_labels))
)

# Plot
ggplot(plot_data, aes(x = Gender, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Observed vs. Expected Counts of Gender",
       y = "Count") +
  theme_minimal()


# Load necessary package
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Define observed counts
observed_counts <- table(data$CREDITSTATUS)

# Define expected proportions (adjust these based on your expectations)
# Example: If we assume equal probability for 8 categories
expected_proportions <- rep(1 / length(observed_counts), length(observed_counts))

# Calculate expected counts
total_count <- sum(observed_counts)
expected_counts <- total_count * expected_proportions

# Perform Chi-Square Goodness of Fit test
chi_square_test_creditstatus <- chisq.test(observed_counts, p = expected_proportions)
print(chi_square_test_creditstatus)

#does not follow multinaulli dist.


# Prepare data for plotting
credit_labels <- names(observed_counts)
plot_data <- data.frame(
  CreditStatus = rep(credit_labels, 2),
  Count = c(observed_counts, expected_counts),
  Type = rep(c("Observed", "Expected"), each = length(credit_labels))
)

# Plot Observed vs Expected counts
ggplot(plot_data, aes(x = CreditStatus, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Observed vs. Expected Counts of CREDITSTATUS",
       y = "Count") +
  theme_minimal()

#most people have paid off loans, dont have loans or have loans pending from 0-29 days.
#lets remove these classes and see


# Filter out the categories "C" and "X" from CREDITSTATUS
filtered_data <- data[!(data$CREDITSTATUS %in% c("C", "X")), ]

observed_counts_filtered <- table(filtered_data$CREDITSTATUS)

expected_proportions_filtered <- rep(1 / length(observed_counts_filtered), length(observed_counts_filtered))

total_count_filtered <- sum(observed_counts_filtered)
expected_counts_filtered <- total_count_filtered * expected_proportions_filtered

chi_square_test_creditstatus_filtered <- chisq.test(observed_counts_filtered, p = expected_proportions_filtered)
print(chi_square_test_creditstatus_filtered)

credit_labels_filtered <- names(observed_counts_filtered)
plot_data_filtered <- data.frame(
  CreditStatus = rep(credit_labels_filtered, 2),
  Count = c(observed_counts_filtered, expected_counts_filtered),
  Type = rep(c("Observed", "Expected"), each = length(credit_labels_filtered))
)

library(ggplot2)
ggplot(plot_data_filtered, aes(x = CreditStatus, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Observed vs. Expected Counts of CREDITSTATUS (Excluding C and X)",
       y = "Count") +
  theme_minimal()

#still not multinaulli

# Filter out the categories "0", "C", and "X" from CREDITSTATUS
filtered_data <- data[!(data$CREDITSTATUS %in% c("0", "C", "X")), ]

observed_counts_filtered <- table(filtered_data$CREDITSTATUS)

expected_proportions_filtered <- rep(1 / length(observed_counts_filtered), length(observed_counts_filtered))

total_count_filtered <- sum(observed_counts_filtered)
expected_counts_filtered <- total_count_filtered * expected_proportions_filtered

# Perform Chi-Square Goodness of Fit test
chi_square_test_creditstatus_filtered <- chisq.test(observed_counts_filtered, p = expected_proportions_filtered)
print(chi_square_test_creditstatus_filtered)

credit_labels_filtered <- names(observed_counts_filtered)
plot_data_filtered <- data.frame(
  CreditStatus = rep(credit_labels_filtered, 2),
  Count = c(observed_counts_filtered, expected_counts_filtered),
  Type = rep(c("Observed", "Expected"), each = length(credit_labels_filtered))
)

library(ggplot2)
ggplot(plot_data_filtered, aes(x = CreditStatus, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Observed vs. Expected Counts of CREDITSTATUS (Filtered)",
       y = "Count") +
  theme_minimal()

#so it doesnt follow multinaulli at all
# lets try a poisson dist.

filtered_data_poisson <- data[data$CREDITSTATUS %in% c("0", "1", "2", "3", "4", "5"), ]

observed_counts_poisson <- table(filtered_data_poisson$CREDITSTATUS)

# Calculate the mean (λ) of the observed counts
lambda <- mean(as.numeric(names(observed_counts_poisson)) * observed_counts_poisson) / sum(observed_counts_poisson)

# Calculate expected counts based on Poisson distribution with mean λ
expected_counts_poisson <- sum(observed_counts_poisson) * dpois(as.numeric(names(observed_counts_poisson)), lambda)

chi_square_test_poisson <- chisq.test(observed_counts_poisson, p = expected_counts_poisson / sum(expected_counts_poisson))
print(chi_square_test_poisson)


#Doesn't follow poisson either

