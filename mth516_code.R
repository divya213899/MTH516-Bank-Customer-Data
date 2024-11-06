# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(psych)
library(car)
library(stats)
library(tseries)

# Load data
Data <- read.csv("BankCustomerData_Dataset.csv")

data<-Data

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


#Mann whitney test

data<-Data



#Car ownership vs income

# Data Preparation
# Convert OWNCAR to factor
data$OWNCAR <- factor(data$OWNCAR, levels = c("Y", "N"), labels = c("Owns Car", "Does Not Own Car"))

# Remove missing values
data_car <- data %>% filter(!is.na(INCOMETOTAL) & !is.na(OWNCAR))

# Outlier Removal Function
remove_outliers_iqr <- function(df, variable) {
  df %>%
    group_by(OWNCAR) %>%
    mutate(
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter((!!sym(variable)) >= Lower_Bound & (!!sym(variable)) <= Upper_Bound) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound)
}

# Remove outliers
data_car_no_outliers <- remove_outliers_iqr(data_car, "INCOMETOTAL")

# Summary Statistics
summary_stats_car <- data_car_no_outliers %>%
  group_by(OWNCAR) %>%
  summarise(
    Count = n(),
    Median = median(INCOMETOTAL),
    Mean = mean(INCOMETOTAL),
    SD = sd(INCOMETOTAL)
  )
print(summary_stats_car)

# Mann-Whitney U Test
income_own_car <- data_car_no_outliers %>% filter(OWNCAR == "Owns Car") %>% pull(INCOMETOTAL)
income_no_car <- data_car_no_outliers %>% filter(OWNCAR == "Does Not Own Car") %>% pull(INCOMETOTAL)

car_test <- wilcox.test(income_own_car, income_no_car, alternative = "two.sided")
print(car_test)

# Histogram
ggplot(data_car_no_outliers, aes(x = INCOMETOTAL, fill = OWNCAR)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6, boundary = 0) +
  labs(title = "Income Distribution by Car Ownership (Outliers Removed)",
       x = "Income Total",
       y = "Frequency") +
  scale_fill_manual(values = c("Owns Car" = "#4E79A7", "Does Not Own Car" = "#F28E2B")) +
  theme_minimal()

# Box Plot
ggplot(data_car_no_outliers, aes(x = OWNCAR, y = INCOMETOTAL, fill = OWNCAR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Income by Car Ownership (Outliers Removed)",
       x = "Car Ownership",
       y = "Income Total") +
  scale_fill_manual(values = c("Owns Car" = "#4E79A7", "Does Not Own Car" = "#F28E2B")) +
  theme_minimal()


# for most of the income groups more people dont own cars then the ones that own cars.
# This difference decreases as the income increases, but the total number with high income is also low.

# Gender vs Income

# Data Preparation
# Convert GENDER to factor
data$GENDER <- factor(data$GENDER, levels = c("M", "F"), labels = c("Male", "Female"))

# Remove missing values
data_gender <- data %>% filter(!is.na(INCOMETOTAL) & !is.na(GENDER))

# Outlier Removal Function
remove_outliers_iqr <- function(df, variable) {
  df %>%
    group_by(GENDER) %>%
    mutate(
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter((!!sym(variable)) >= Lower_Bound & (!!sym(variable)) <= Upper_Bound) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound)
}

# Remove outliers
data_gender_no_outliers <- remove_outliers_iqr(data_gender, "INCOMETOTAL")

# Summary Statistics
summary_stats_gender <- data_gender_no_outliers %>%
  group_by(GENDER) %>%
  summarise(
    Count = n(),
    Median = median(INCOMETOTAL),
    Mean = mean(INCOMETOTAL),
    SD = sd(INCOMETOTAL)
  )
print(summary_stats_gender)

# Mann-Whitney U Test
income_male <- data_gender_no_outliers %>% filter(GENDER == "Male") %>% pull(INCOMETOTAL)
income_female <- data_gender_no_outliers %>% filter(GENDER == "Female") %>% pull(INCOMETOTAL)

gender_test <- wilcox.test(income_male, income_female, alternative = "two.sided")
print(gender_test)

# Histogram
ggplot(data_gender_no_outliers, aes(x = INCOMETOTAL, fill = GENDER)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6, boundary = 0) +
  labs(title = "Income Distribution by Gender (Outliers Removed)",
       x = "Income Total",
       y = "Frequency") +
  scale_fill_manual(values = c("Male" = "#4E79A7", "Female" = "#F28E2B")) +
  theme_minimal()

# Box Plot
ggplot(data_gender_no_outliers, aes(x = GENDER, y = INCOMETOTAL, fill = GENDER)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Income by Gender (Outliers Removed)",
       x = "Gender",
       y = "Income Total") +
  scale_fill_manual(values = c("Male" = "#4E79A7", "Female" = "#F28E2B")) +
  theme_minimal()


# Both the mean and median income of male is more than female

#Property ownership vs income

# Data Preparation
# Convert OWNPROPERTY to factor
data$OWNPROPERTY <- factor(data$OWNPROPERTY, levels = c("Y", "N"), labels = c("Owns Property", "Does Not Own Property"))

# Remove missing values
data_property <- data %>% filter(!is.na(INCOMETOTAL) & !is.na(OWNPROPERTY))

# Outlier Removal Function
remove_outliers_iqr <- function(df, variable) {
  df %>%
    group_by(OWNPROPERTY) %>%
    mutate(
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter((!!sym(variable)) >= Lower_Bound & (!!sym(variable)) <= Upper_Bound) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound)
}

# Remove outliers
data_property_no_outliers <- remove_outliers_iqr(data_property, "INCOMETOTAL")

# Summary Statistics
summary_stats_property <- data_property_no_outliers %>%
  group_by(OWNPROPERTY) %>%
  summarise(
    Count = n(),
    Median = median(INCOMETOTAL),
    Mean = mean(INCOMETOTAL),
    SD = sd(INCOMETOTAL)
  )
print(summary_stats_property)

# Mann-Whitney U Test
income_own_property <- data_property_no_outliers %>% filter(OWNPROPERTY == "Owns Property") %>% pull(INCOMETOTAL)
income_no_property <- data_property_no_outliers %>% filter(OWNPROPERTY == "Does Not Own Property") %>% pull(INCOMETOTAL)

property_test <- wilcox.test(income_own_property, income_no_property, alternative = "two.sided")
print(property_test)

# Histogram
ggplot(data_property_no_outliers, aes(x = INCOMETOTAL, fill = OWNPROPERTY)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6, boundary = 0) +
  labs(title = "Income Distribution by Property Ownership (Outliers Removed)",
       x = "Income Total",
       y = "Frequency") +
  scale_fill_manual(values = c("Owns Property" = "#4E79A7", "Does Not Own Property" = "#F28E2B")) +
  theme_minimal()

# Box Plot
ggplot(data_property_no_outliers, aes(x = OWNPROPERTY, y = INCOMETOTAL, fill = OWNPROPERTY)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Income by Property Ownership (Outliers Removed)",
       x = "Property Ownership",
       y = "Income Total") +
  scale_fill_manual(values = c("Owns Property" = "#4E79A7", "Does Not Own Property" = "#F28E2B")) +
  theme_minimal()

# The mean income is less for people not owning property

# income vs education

# Convert EDUCATIONLEVEL to factor
data$EDUCATIONLEVEL <- as.factor(data$EDUCATIONLEVEL)

# Define higher education levels
higher_education <- c("Higher education", "Academic degree")

# Recode EDUCATIONLEVEL into two categories
data$EDUCATION_CAT <- ifelse(data$EDUCATIONLEVEL %in% higher_education, "Higher Education", "Secondary Education")
data$EDUCATION_CAT <- factor(data$EDUCATION_CAT, levels = c("Higher Education", "Secondary Education"))

# Remove missing values
data <- data %>% filter(!is.na(INCOMETOTAL) & !is.na(EDUCATION_CAT))

# Function to identify and remove outliers using IQR method
remove_outliers_iqr <- function(df, variable) {
  df %>%
    group_by(EDUCATION_CAT) %>%
    mutate(
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter( (!!sym(variable)) >= Lower_Bound & (!!sym(variable)) <= Upper_Bound ) %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound)
}

# Apply the outlier removal function
data_no_outliers <- remove_outliers_iqr(data, "INCOMETOTAL")

# Summary Statistics without outliers
summary_stats_education <- data_no_outliers %>%
  group_by(EDUCATION_CAT) %>%
  summarise(
    Count = n(),
    Median = median(INCOMETOTAL),
    Mean = mean(INCOMETOTAL),
    SD = sd(INCOMETOTAL)
  )
print(summary_stats_education)

# Data Preparation for Mann-Whitney U Test
income_higher_ed <- data_no_outliers %>% filter(EDUCATION_CAT == "Higher Education") %>% pull(INCOMETOTAL)
income_secondary_ed <- data_no_outliers %>% filter(EDUCATION_CAT == "Secondary Education") %>% pull(INCOMETOTAL)

# Mann-Whitney U Test
education_test <- wilcox.test(income_higher_ed, income_secondary_ed, alternative = "two.sided")
print(education_test)

# Histogram without outliers
ggplot(data_no_outliers, aes(x = INCOMETOTAL, fill = EDUCATION_CAT)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6, boundary = 0) +
  labs(title = "Income Distribution by Education Level (Outliers Removed)",
       x = "Income Total",
       y = "Frequency") +
  scale_fill_manual(values = c("Higher Education" = "#4E79A7", "Secondary Education" = "#F28E2B")) +
  theme_minimal()

# Box Plot without outliers
ggplot(data_no_outliers, aes(x = EDUCATION_CAT, y = INCOMETOTAL, fill = EDUCATION_CAT)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Income by Education Level (Outliers Removed)",
       x = "Education Level",
       y = "Income Total") +
  scale_fill_manual(values = c("Higher Education" = "#4E79A7", "Secondary Education" = "#F28E2B")) +
  theme_minimal()


#family size vs income 

# Data Preparation
# Remove missing values
data_famsize <- data %>% filter(!is.na(INCOMETOTAL) & !is.na(FAMSIZE))

# Calculate median family size
median_famsize <- median(data_famsize$FAMSIZE, na.rm = TRUE)

# Categorize FAMSIZE
data_famsize <- data_famsize %>%
  mutate(FAMSIZE_CAT = ifelse(FAMSIZE <= median_famsize, "Small Family", "Large Family")) %>%
  mutate(FAMSIZE_CAT = factor(FAMSIZE_CAT, levels = c("Small Family", "Large Family")))

# Outlier Removal Function
remove_outliers_iqr <- function(df, variable) {
  df %>%
    group_by(FAMSIZE_CAT) %>%
    mutate(
      Q1 = quantile(!!sym(variable), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(variable), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR
    ) %>%
    filter((!!sym(variable)) >= Lower_Bound & (!!sym(variable)) <= Upper_Bound) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR, -Lower_Bound, -Upper_Bound)
}

# Remove outliers
data_famsize_no_outliers <- remove_outliers_iqr(data_famsize, "INCOMETOTAL")

# Summary Statistics
summary_stats_famsize <- data_famsize_no_outliers %>%
  group_by(FAMSIZE_CAT) %>%
  summarise(
    Count = n(),
    Median = median(INCOMETOTAL),
    Mean = mean(INCOMETOTAL),
    SD = sd(INCOMETOTAL)
  )
print(summary_stats_famsize)

# Mann-Whitney U Test
income_small_family <- data_famsize_no_outliers %>% filter(FAMSIZE_CAT == "Small Family") %>% pull(INCOMETOTAL)
income_large_family <- data_famsize_no_outliers %>% filter(FAMSIZE_CAT == "Large Family") %>% pull(INCOMETOTAL)

famsize_test <- wilcox.test(income_small_family, income_large_family, alternative = "two.sided")
print(famsize_test)

# Histogram
ggplot(data_famsize_no_outliers, aes(x = INCOMETOTAL, fill = FAMSIZE_CAT)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6, boundary = 0) +
  labs(title = "Income Distribution by Family Size (Outliers Removed)",
       x = "Income Total",
       y = "Frequency") +
  scale_fill_manual(values = c("Small Family" = "#4E79A7", "Large Family" = "#F28E2B")) +
  theme_minimal()

# Box Plot
ggplot(data_famsize_no_outliers, aes(x = FAMSIZE_CAT, y = INCOMETOTAL, fill = FAMSIZE_CAT)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Income by Family Size (Outliers Removed)",
       x = "Family Size Category",
       y = "Income Total") +
  scale_fill_manual(values = c("Small Family" = "#4E79A7", "Large Family" = "#F28E2B")) +
  theme_minimal()
