# Load necessary libraries
library(ggplot2)
library(caret)

# Load the dataset
data(mtcars)

# Display the first few rows of the dataset
head(mtcars)
# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(mtcars$mpg, p = .8,  list = FALSE, times = 1)
trainData <- mtcars[trainIndex, ]
testData <- mtcars[-trainIndex, ]
# Train the linear regression model
lm_model <- lm(mpg ~ ., data = trainData)
# Make predictions on the test set
predictions <- predict(lm_model, newdata = testData)

# Calculate Mean Squared Error
mse <- mean((testData$mpg - predictions)^2)
cat("Mean Squared Error:", mse, "\n")

# Calculate R-squared
rsquared <- summary(lm_model)$r.squared
cat("R-squared:", rsquared, "\n")
# Plot actual vs. predicted values
ggplot() +
  geom_point(data = testData, aes(x = mpg, y = predictions), color = "blue") +
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "red") +
  labs(x = "Actual MPG", y = "Predicted MPG", title = "Actual vs. Predicted MPG") +
  theme_minimal()


