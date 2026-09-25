# ==============================================================================
# Week 1 Task: Data Cleaning and Preliminary Analysis on Titanic Extended
# Target Environment: Posit Cloud / RStudio
# ==============================================================================

# 1. Package Installation and Setup
required_pkgs <- c("titanic", "tidyverse", "corrplot", "scales", "e1071")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(titanic)
library(tidyverse)
library(corrplot)
library(scales)
library(e1071)
library(dplyr)

# 2. Ingestion & Preliminary Inspection
data("titanic_train", package = "titanic")
df_raw <- as_tibble(titanic_train)

cat("--- Dimensions of Raw Data ---\n")
print(dim(df_raw))

cat("\n--- Structure of Dataset ---\n")
str(df_raw)

cat("\n--- Five-Number Summary of Raw Data ---\n")
summary(df_raw)

# Replace empty strings in character columns with standard NA
df_raw <- df_raw %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))

# Audit Missing Values
missing_audit <- df_raw %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Pct = (Missing_Count / nrow(df_raw)) * 100) %>%
  arrange(desc(Missing_Count))

cat("\n--- Missing Value Breakdown ---\n")
print(missing_audit)

# ------------------------------------------------------------------------------
# 3. Data Cleaning & Preprocessing
# ------------------------------------------------------------------------------
df_clean <- df_raw

# 3.1 Feature Engineering Before Imputation
# Extract honorific titles to improve Age imputation accuracy
df_clean <- df_clean %>%
  mutate(
    Title = str_extract(Name, "[A-Za-z]+(?=\\.)"),
    Title = case_when(
      Title %in% c("Mlle", "Ms") ~ "Miss",
      Title == "Mme" ~ "Mrs",
      Title %in% c("Don", "Sir", "Jonkheer", "Rev", "Dr", "Col", "Major", "Capt") ~ "Rare",
      Title %in% c("Lady", "Countess", "Dona") ~ "Rare",
      TRUE ~ Title
    )
  )

# 3.2 Missing Value Imputation
# A. Numeric (Age): Impute using Median grouped by Title
median_age_map <- df_clean %>%
  group_by(Title) %>%
  summarise(med_age = median(Age, na.rm = TRUE))

df_clean <- df_clean %>%
  left_join(median_age_map, by = "Title") %>%
  mutate(Age = if_else(is.na(Age), med_age, Age)) %>%
  select(-med_age)

# If any remain missing, fallback to global median
df_clean$Age[is.na(df_clean$Age)] <- median(df_clean$Age, na.rm = TRUE)

# B. Categorical (Embarked): Impute with mode ("S" - Southampton)
mode_embarked <- names(sort(table(df_clean$Embarked), decreasing = TRUE))[1]
df_clean <- df_clean %>%
  mutate(Embarked = if_else(is.na(Embarked), mode_embarked, Embarked))

# C. High Missingness Handling (Cabin has >77% missing)
# Convert Cabin into Deck indicator or "Unknown"
df_clean <- df_clean %>%
  mutate(Deck = if_else(is.na(Cabin), "Unknown", substr(Cabin, 1, 1)))

# 3.3 Outlier Handling (Fare column using IQR Capping / Winsorization)
q25_fare <- quantile(df_clean$Fare, 0.25, na.rm = TRUE)
q75_fare <- quantile(df_clean$Fare, 0.75, na.rm = TRUE)
iqr_fare <- q75_fare - q25_fare
upper_fare_bound <- q75_fare + 1.5 * iqr_fare

# Preserve original Fare, create Fare_Capped
df_clean <- df_clean %>%
  mutate(Fare_Capped = pmin(Fare, upper_fare_bound))

# 3.4 Feature Transformations and Scaling
# Log transform Fare (due to right-skew) + Min-Max + Z-Score Standardization
min_max <- function(x) (x - min(x)) / (max(x) - min(x))

df_clean <- df_clean %>%
  mutate(
    Fare_Log = log1p(Fare_Capped),
    Age_Standardized = as.vector(scale(Age)),
    Fare_Normalized = min_max(Fare_Capped)
  )

# 3.5 Categorical Encoding
# Convert categorical features to explicit factors and One-Hot encode Sex
df_clean <- df_clean %>%
  mutate(
    Sex_Male = if_else(Sex == "male", 1, 0),
    Pclass = factor(Pclass, ordered = TRUE, levels = c(3, 2, 1)),
    Survived = factor(Survived, levels = c(0, 1), labels = c("Died", "Survived"))
  )

cat("\n--- Cleaning Complete: Verified Post-Cleaning Missingness ---\n")
print(colSums(is.na(df_clean)))

# ------------------------------------------------------------------------------
# 4. Preliminary Visualizations (Exported as PNGs for Word)
# ------------------------------------------------------------------------------

# Figure 1: Outlier Treatment - Fare Before vs After Capping
p1 <- ggplot(df_raw, aes(y = Fare)) +
  geom_boxplot(fill = "#E74C3C", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Raw Fare: Severe Positive Outliers", y = "Fare ($)")

p2 <- ggplot(df_clean, aes(y = Fare_Capped)) +
  geom_boxplot(fill = "#2ECC71", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Cleaned Fare: 1.5x IQR Capping", y = "Fare ($)")

png("fig1_fare_outlier_treatment.png", width = 800, height = 400)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

# Figure 2: Univariate Distribution - Age Before & After Median Imputation
p3 <- ggplot() +
  geom_density(data = df_raw, aes(x = Age, fill = "Raw (Pre-Imputation)"), alpha = 0.4) +
  geom_density(data = df_clean, aes(x = Age, fill = "Cleaned (Imputed)"), alpha = 0.4) +
  scale_fill_manual(values = c("Raw (Pre-Imputation)" = "red", "Cleaned (Imputed)" = "blue")) +
  theme_minimal() +
  labs(title = "Age Distribution: Impact of Group-Median Imputation",
       x = "Age", y = "Density", fill = "Dataset")
ggsave("fig2_age_distribution.png", p3, width = 6, height = 4)

# Figure 3: Bivariate Exploratory Analysis - Survival Rate by Passenger Class & Sex
p4 <- ggplot(df_clean, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~Sex) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#7F8C8D", "#2980B9")) +
  theme_minimal() +
  labs(title = "Survival Proportions Across Passenger Class and Gender",
       x = "Passenger Class (1 = 1st, 3 = 3rd)", y = "Proportion", fill = "Status")
ggsave("fig3_bivariate_survival.png", p4, width = 7, height = 4.5)

# Figure 4: Correlation Matrix of Key Numerical Variables
num_data <- df_clean %>%
  transmute(
    Survived_Num = as.numeric(Survived) - 1,
    Pclass_Num = as.numeric(Pclass),
    Age,
    SibSp,
    Parch,
    Fare_Capped,
    Sex_Male
  )

cor_matrix <- cor(num_data, use = "complete.obs")

png("fig4_correlation_matrix.png", width = 600, height = 600)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Heatmap: Titanic Predictors", mar = c(0,0,1,0))
dev.off()

cat("\nPipeline complete. Visual assets exported successfully to the working directory.\n")

