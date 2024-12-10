# Install packages if not already installed
# install.packages("dplyr")

library(dplyr)
library(plm)
library(stargazer)
## predicting doctor visits in a year 
# Load the dataset
data <- read.csv("C:/TERM 4/AEM/project/rwm5yr.csv")
data

# Simple Linear Regression using 'age' as a predictor
simple_model <- lm(docvis ~ age, data = data)

# Summary of the model
summary(simple_model)

# Predictions using the model
simple_predictions <- predict(simple_model, data)

# Evaluate model performance
mse_simple <- mean((data$docvis - simple_predictions)^2)
cat("MSE (Simple Linear Regression):", mse_simple, "\n")

# Multiple Linear Regression using all predictors (exclude 'docvis' from predictors)
multiple_model <- lm(docvis ~ educ + self + age + outwork + female + married + kids + hhninc , data = data)

# Summary of the model
summary(multiple_model)

# Predictions using the model
multiple_predictions <- predict(multiple_model, data)

# Evaluate model performance
mse_multiple <- mean((data$docvis - multiple_predictions)^2)
cat("MSE (Multiple Linear Regression):", mse_multiple, "\n")


# Convert the dataset into a panel data format
panel_data <- pdata.frame(data, index = c("id", "year"))

# Fixed effects model
fixed_model <- plm(docvis ~ age + female + outwork + married + kids + hhninc, data = panel_data, model = "within")

# Summary of the fixed effects model
summary(fixed_model)

# Random effects model
random_model <- plm(docvis ~ age + female + outwork + married + kids + hhninc, data = panel_data, model = "random")

# Summary of the random effects model
summary(random_model)

# Perform the Hausman test to compare the models
phtest(fixed_model, random_model)

library(stargazer)

# Compare the fixed effects and random effects models using stargazer
stargazer(fixed_model, random_model, type = "text", title = "Comparison of Fixed and Random Effects Models", 
          column.labels = c("Fixed Effects", "Random Effects"), 
          dep.var.labels = "Number of Doctor Visits (docvis)", 
          covariate.labels = c("Age", "Female", "Out of Work", "Married", "Kids", "Household Income"),
          omit.stat = c("f", "ser"), no.space = TRUE)
