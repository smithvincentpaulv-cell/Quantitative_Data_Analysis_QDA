library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(stats)
library(rpart)
library(rpart.plot)
# Reading the datf20

datf20 <- read.csv("E:/draft_1.csv")
head(datf20,4)

# Structure of the datf20

str(datf20)

# Checking for missing values

missval <- colSums(is.na(datf20))
print("Missing Values:")
missval

# Checking for duplicated observations

duplrows <- datf20[duplicated(datf20), ]
print("Duplicated Rows:")
duplrows


# Converting date to a proper date format

datf20$date <- as.Date(as.character(datf20$date), format="%Y")

# Descriptive Summary

summary(datf20)


# Converting death_number to numeric

datf20$death_number <- as.factor(datf20$death_number)
datf20$death_number <- as.numeric(datf20$death_number)

# Convert 'percentage' columns to numeric
percentage_columns <- c("percentage_toddlers", "percentgae_adult", "percentage_oldage", "percentage_oldage.1",
                        "percentgae_otheridentities", "percentage_OtheridentitiesandatleastoneofEnglish",
                        "percentgae_AtleastoneofEnglish", "percentage_Verygoodhealth", "percentage_Goodhealth",
                        "percentage_Fairhealth", "percentage_Badhealth", "percentage_Verybadhealth",
                        "percentage_coviddeath")

datf20[percentage_columns] <- lapply(datf20[percentage_columns], function(x) as.numeric(sub("%", "", x)))

# EDA: Summary Statistics
summary(datf20)

# EDA: Visualization 1 - Scatter Plot for Very Good Health vs. Age
ggplot(datf20, aes(x = Age.Allusualresidentsmeasures.Value, y = Verygoodhealth)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Very Good Health vs. Age", x = "Age", y = "Very Good Health") +
  theme_minimal()

# EDA: Visualization 2 - Stacked Bar Plot for Health Categories by Region
health_columns <- c("Verygoodhealth", "Goodhealth", "Fairhealth", "Badhealth", "Verybadhealth")

datf20_long_health <- tidyr::gather(datf20, key = "Health_Category", value = "Value", all_of(health_columns))

ggplot(datf20_long_health, aes(x = geography, y = Value, fill = Health_Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Health Categories by Region", x = "Geography", y = "Count") +
  theme_minimal()

# EDA: Visualization 3 - Line Plot for Death Numbers Over Time
ggplot(datf20, aes(x = date, y = as.numeric(death_number), group = 1)) +
  geom_line(color = "red") +
  labs(title = "Death Numbers Over Time", x = "Date", y = "Death Numbers") +
  theme_minimal()

national_identity <- datf20[, c("NationalIdentity.Otheridentitiesonlymeasures.Value", 
                            "NationalIdentity.OtheridentitiesandatleastoneofEnglish.Welsh.Scottish.NorthernIrish.Britishonlymeasures.Value")]

national_identity <- colSums(national_identity)

names(national_identity) <- c("Other Identities Only", "At least one of English, Welsh, Scottish, Northern Irish, British Only")

pie(national_identity, labels = names(national_identity), main = "National Identity Distribution")


# Scatter Plot 1: Toddlers vs. Teenagers
plot(datf20$percentage_toddlers, datf20$percentage_teenagers,
     main = "Scatter Plot: Percentage Toddlers vs. Percentage Teenagers",
     xlab = "Percentage Toddlers", ylab = "Percentage Teenagers",
     col = "green", pch = 16)


# Correlation Analysis
correlation_matrix <- cor(datf20[, c("Age.Allusualresidentsmeasures.Value", "Toddlers", "Teenagers", "Adult", "Oldage",
                                 "Verygoodhealth", "Goodhealth", "Fairhealth", "Badhealth", "Verybadhealth",
                                 "death_number")])



# Visualization of Correlation Matrix
corrplot(correlation_matrix, method = "circle", tl.col = "black", tl.srt = 90)

# Print the correlation matrix
print(correlation_matrix)





# Convert factor columns to numeric
datf20[, 4:ncol(datf20)] <- sapply(datf20[, 4:ncol(datf20)], function(x) as.numeric(as.character(x)))

# Measure of Central Tendency
central_tendency <- sapply(datf20[, 4:ncol(datf20)], function(x) c(Min = min(x), Q1 = quantile(x, 0.25), Median = median(x), Mean = mean(x), Q3 = quantile(x, 0.75), Max = max(x)))


# Probability
probability <- sapply(datf20[, 4:ncol(datf20)], function(x) {
  prop_table <- table(x) / length(x)
  prop_table[order(-prop_table)]
})

# Print the results
print("Measure of Central Tendency:")
print(central_tendency)

print("Probability:")
print(probability)


# Non-Parametric Significance Hypothesis Test (Kolmogorov-Smirnov test)

ks_test_result <- ks.test(datf20$percentage_coviddeath, "pexp", 0.05)

cat("Kolmogorov-Smirnov Test Result:\n")
cat("D =", ks_test_result$statistic, "\n")
cat("p-value =", ks_test_result$p.value, "\n\n")

# Interpretation

alpha <- 0.05
if (ks_test_result$p.value < alpha) {
  cat("The null hypothesis is rejected. There is significant evidence of a difference.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence of a difference.\n")
}










#Fit ANOVA Model

anova_model <- aov(datf20$percentage_coviddeath ~ datf20$Teenagers, data = datf20)

# Summary of the ANOVA model

summary(anova_model)


# Interpretation

cat("ANOVA Results:\n")
cat("Overall p-value:", summary(anova_model)[[1]]$`Pr(>F)`[1], "\n")






















# Simple Linear Regression
simple_regression_model <- lm(datf20$percentage_coviddeath ~ datf20$Toddlers)

# Multiple Linear Regression
multiple_regression_model <- lm(percentage_coviddeath ~ Toddlers ++ percentage_toddlers + percentage_OtheridentitiesandatleastoneofEnglish + percentgae_AtleastoneofEnglish, data = datf20)

# Scatterplot for Simple Linear Regression
plot(datf20$percentage_toddlers, datf20$percentage_coviddeath, main = "Simple Linear Regression", xlab = "percentage_toddlers", ylab = "Percentage Covid Death")
abline(simple_regression_model, col = "red")

# Residuals vs Fitted Values Plot for Simple Regression
plot(simple_regression_model, which = 1)

# Normal Q-Q Plot for Residuals in Simple Regression
plot(simple_regression_model, which = 2)

# Residuals vs Fitted Values Plot for Multiple Regression
plot(multiple_regression_model, which = 1)

# Normal Q-Q Plot for Residuals in Multiple Regression
plot(multiple_regression_model, which = 2)

# Cook's Distance Plot for Multiple Regression
plot(multiple_regression_model, which = 4)

# Partial Regression Plots for Multiple Regression
crPlots(multiple_regression_model)
# Building a decision tree
decision_tree_model <- rpart(percentage_coviddeath ~ percentage_Fairhealth + Toddlers + Teenagers, data = datf20, method = "anova")
# Print the decision tree
print(decision_tree_model)


 #Linear Regression
linear_model <- lm(percentage_coviddeath ~ percentage_Badhealth + Toddlers+ Adult+ Oldage, data = datf20)

# Print the summary of the linear regression model
summary(linear_model)

# Extract actual and predicted values
actual_values <- datf20$percentage_coviddeath 
predicted_values <- predict(linear_model, newdata = datf20)

# Calculate R Squared Error
r_squared <- 1 - sum((actual_values - predicted_values)^2) / sum((actual_values - mean(actual_values))^2)
cat("R Squared Error:", r_squared, "\n")

# Calculate Mean Squared Error
mse <- mean((actual_values - predicted_values)^2)
cat("Mean Squared Error:", mse, "\n")


# Load the necessary libraries
library(factoextra)
library(cluster)

# Select numeric variables for clustering
numeric_variables_for_clustering <- datf20[, c("percentage_coviddeath", "Toddlers")]

# Check for missing values and handle them if needed
if (any(is.na(numeric_variables_for_clustering))) {
  numeric_variables_for_clustering <- na.omit(numeric_variables_for_clustering)
}

# Determine the optimal number of clusters using the "elbow" method
wss <- sapply(1:10, function(k) kmeans(numeric_variables_for_clustering, centers = k)$tot.withinss)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

# Choose the optimal number of clusters 
optimal_k <- 2

# Perform k-means clustering
kmeans_model <- kmeans(numeric_variables_for_clustering, centers = optimal_k)

# Add cluster assignment to the original dataframe
datf20$cluster <- as.factor(kmeans_model$cluster)

# Visualize the clusters
library(ggplot2)
ggplot(datf20, aes(x = percentage_coviddeath, y = Toddlers, color = cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering", x = "Percentage of COVID Deaths", y = "Number of Toddlers", color = "Cluster")

kmeans_model



# Print summaries
print(summary(simple_regression_model))
print(summary(multiple_regression_model))

