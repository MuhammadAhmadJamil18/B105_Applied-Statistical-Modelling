# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date manipulation
library(ggplot2)    # For plotting
library(car)        # For regression diagnostics
library(broom)      # For tidying model outputs
library(knitr)      # For creating tables in the report

##Data Loading 
Walmart <- read.csv("C:/Users/PMYLS/Downloads/ahmed_105/archive (6)/Walmart.csv")

# 3. Data Preparation
# Convert Date column to Date type
Walmart <- Walmart %>%
  mutate(Date = dmy(Date))

# View the structure of the dataset
str(Walmart)

# Summary of the dataset
summary(Walmart)

# Check for missing values
missing_values <- sum(is.na(Walmart))
cat("Total missing values in the dataset:", missing_values, "\n")

# Check for duplicate rows
duplicate_rows <- Walmart %>% 
  duplicated() %>% 
  sum()
cat("Total duplicate rows in the dataset:", duplicate_rows, "\n")

# 4. Descriptive Statistics
# Summary statistics for numerical variables
numerical_summary <- Walmart %>% 
  select(-Date) %>% 
  summary()
kable(numerical_summary, caption = "Summary Statistics for Numerical Variables")

# Correlation matrix for numerical variables
cor_matrix <- Walmart %>%
  select(-Date) %>%
  cor()
kable(cor_matrix, digits = 2, caption = "Correlation Matrix")

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Convert the matrix to a data frame for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap with more vibrant colors
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") + # Use black text for contrast
  scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix for Numerical Variables", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizations
# Distribution of Weekly Sales
ggplot(Walmart, aes(x = Weekly_Sales)) +
  geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
  labs(title = "Distribution of Weekly Sales", x = "Weekly Sales", y = "Frequency") +
  theme_minimal()

# Time series plot of Weekly Sales
ggplot(Walmart, aes(x = Date, y = Weekly_Sales)) +
  geom_line(color = 'blue') +
  labs(title = "Weekly Sales Over Time", x = "Date", y = "Weekly Sales") +
  theme_minimal()

# Boxplot of Weekly Sales by Holiday Flag
ggplot(Walmart, aes(x = as.factor(Holiday_Flag), y = Weekly_Sales)) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = "Weekly Sales by Holiday Flag", x = "Holiday Flag", y = "Weekly Sales") +
  theme_minimal()

# Scatter plots for Weekly Sales against Economic Indicators
ggplot(Walmart, aes(x = Unemployment, y = Weekly_Sales)) +
  geom_point(alpha = 0.6, color = 'blue') +
  labs(title = "Weekly Sales vs Unemployment", x = "Unemployment", y = "Weekly Sales") +
  theme_minimal()

ggplot(Walmart, aes(x = CPI, y = Weekly_Sales)) +
  geom_point(alpha = 0.6, color = 'green') +
  labs(title = "Weekly Sales vs CPI", x = "CPI", y = "Weekly Sales") +
  theme_minimal()

# 5. Data Cleaning
# Identify outliers using the IQR method and remove them
Q1 <- quantile(Walmart$Weekly_Sales, 0.25)
Q3 <- quantile(Walmart$Weekly_Sales, 0.75)
IQR <- Q3 - Q1

Walmart_clean <- Walmart %>%
  filter(Weekly_Sales > (Q1 - 1.5 * IQR) & Weekly_Sales < (Q3 + 1.5 * IQR))

# Re-check summary statistics after cleaning
cleaned_summary <- Walmart_clean %>% 
  select(-Date) %>% 
  summary()
kable(cleaned_summary, caption = "Summary Statistics After Outlier Removal")

# Visualize the cleaned data
ggplot(Walmart_clean, aes(x = Weekly_Sales)) +
  geom_histogram(bins = 30, fill = 'purple', alpha = 0.7) +
  labs(title = "Distribution of Weekly Sales After Cleaning", x = "Weekly Sales", y = "Frequency") +
  theme_minimal()

# 6. Inferential Statistics

# Hypothesis 1: Effect of Holiday on Weekly Sales
# Perform a t-test to compare Weekly Sales during holidays vs non-holidays
t_test_result <- t.test(Weekly_Sales ~ Holiday_Flag, data = Walmart_clean)
kable(tidy(t_test_result), caption = "T-test Results: Holiday_Flag vs Weekly Sales")

# Interpretation:
# If p-value < 0.05, reject the null hypothesis, meaning that Holiday_Flag has a significant impact on Weekly Sales.

# Hypothesis 2: Impact of Economic Indicators on Weekly Sales
# Fit a linear regression model
# Perform a t-test to compare Weekly Sales during holidays vs non-holidays
t_test_result <- t.test(Weekly_Sales ~ Holiday_Flag, data = Walmart_clean)
kable(tidy(t_test_result), caption = "T-test Results: Holiday_Flag vs Weekly Sales")

model <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Holiday_Flag, data = Walmart_clean)

# Check regression assumptions

# 6.1 Linearity Assumption
plot(model, which = 1)  # Residuals vs Fitted
# Interpretation: Check if the residuals plot shows a random scatter, indicating linearity.

# 6.2 Normality of Residuals
plot(model, which = 2)  # Normal Q-Q
# Interpretation: Check if residuals follow a straight line, indicating normality.

# 6.3 Homoscedasticity (Constant Variance)
plot(model, which = 3)  # Scale-Location
# Interpretation: Check if the spread of residuals is constant across all levels of fitted values.

# 6.4 Check for Influential Points
plot(model, which = 4)  # Residuals vs Leverage
# Interpretation: Identify any potential outliers or high-leverage points that may influence the model.

# Summary of the regression model
model_summary <- summary(model)
kable(tidy(model), caption = "Regression Model Summary")

# Check Variance Inflation Factor (VIF) for multicollinearity
vif_values <- vif(model)
kable(data.frame(Variable = names(vif_values), VIF = vif_values), caption = "Variance Inflation Factor (VIF)")

# Interpretation of Model:
# - Significant p-values indicate that the corresponding variables have a statistically significant impact on Weekly Sales.
# - The R-squared value indicates the proportion of variance in Weekly Sales explained by the model.

# 7. Interpretation of Results

# Interpretation of T-test
if (t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis: Holiday_Flag has a significant impact on Weekly Sales.\n")
} else {
  cat("Fail to reject the null hypothesis: Holiday_Flag does not have a significant impact on Weekly Sales.\n")
}

# Interpretation of Regression Model
cat("Regression Model Interpretation:\n")
cat("R-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")

significant_vars <- tidy(model) %>%
  filter(p.value < 0.05) %>%
  pull(term)
cat("Significant Variables: ", paste(significant_vars, collapse = ", "), "\n")

# 8. Final Discussion
# Implications for Business:
# - The significant impact of holidays on sales suggests that targeted promotions during holidays could boost revenue.
# - The relationship between economic indicators and sales suggests that macroeconomic conditions should be considered in sales forecasting.
# - Seasonal trends and patterns should be analyzed further for more accurate predictions.

# Limitations and Future Work:
# - The analysis is limited to the available variables; adding more features (e.g., store locations, customer demographics) could improve the model.
# - A more advanced time series analysis could capture seasonal patterns and trends more effectively.
# - Future work could explore interaction effects between variables, such as how the impact of holidays varies with different economic conditions.

# 9. Save Script and Analysis
# Save your script to a GitHub repository and include the link in your report

# Save the cleaned dataset
write.csv(Walmart_clean, "Walmart_cleaned.csv", row.names = FALSE)

# Export the regression model summary as a CSV file
write.csv(tidy(model), "regression_model_summary.csv", row.names = FALSE)

# End of the script
