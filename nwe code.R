
# Load necessary libraries
library(caret)
library(dplyr)
library(ggplot2)

# Read data from the Github link
data <- read.csv("C:/Users/punit/OneDrive/Desktop/R/oulad-students.csv")
# View the data
View(data)

# Remove rows with missing values
data <- na.omit(data)

# Convert categorical variables to factors
data$code_module <- as.factor(data$code_module)
data$code_presentation <- as.factor(data$code_presentation)
data$gender <- as.factor(data$gender)
data$region <- as.factor(data$region)
data$highest_education <- as.factor(data$highest_education)
data$imd_band <- as.factor(data$imd_band)
data$age_band <- as.factor(data$age_band)
data$num_of_prev_attempts <- as.factor(data$num_of_prev_attempts)
data$disability <- as.factor(data$disability)
data$final_result <- as.factor(data$final_result)

# Exploratory Data Analysis (EDA)
# Example: Histogram of age_band
ggplot(data, aes(x = age_band)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age Bands")

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(120) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- train(final_result ~ ., data = train_data, method = "glm", family = "binomial")

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$final_result)

# Example: Plot of model coefficients
coefficients <- as.data.frame(coef(model$finalModel))
ggplot(coefficients, aes(x = rownames(coefficients), y = V1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Model Coefficients")

# Example: Plot of ROC curve
roc_curve <- roc(test_data$final_result, predictions, levels = c("Fail", "Pass"))
plot(roc_curve, col = "skyblue", lwd = 2, main = "ROC Curve")

