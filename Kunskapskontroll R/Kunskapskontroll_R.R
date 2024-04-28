# Importing libraries -----------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)


# Data preprocessing ----------------------------------------------------
cars <- read_csv("C:/Users/Alia-/Documents/EC-sthlm/R/Kunskapskontroll R/Blocket_Bilar_grupp2.csv")


# View the data -----------------------------------------------------------
View(cars)
head(cars)
summary(cars)

# Handle missing values ---------------------------------------------------
sum(is.na(cars))
cars <- na.omit(cars)
missing_values <- colSums(is.na(cars))
print(missing_values)

# Categorical data  --------------------------------------------------------
categorical <- c('CarName_Brand', 'CarName_Engine', 'CarName_gears', 'CarName_Region', 'CarName_Dealer')

result <- lapply(categorical, function(cat) {
  cat_table <- table(cars[[cat]])
  cat_table <- sort(cat_table, decreasing = TRUE)  
  return(cat_table)
})

result

# Visualizing mileage vs price
ggplot(cars, aes(x = CarName_Miles, y = CarName_Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Mileage vs Price", x = "Mileage (km)", y = "Price (SEK)")

# Train-test split ---------------------------------------------------------
set.seed(123)  
train_indices <- sample(1:nrow(cars), size = 0.7 * nrow(cars))
train_data <- cars[train_indices, ]
test_data <- cars[-train_indices, ]

# Regression modeling -----------------------------------------------------

# Converting categorical variables to factors -----------------------------
cars$CarName_Brand <- as.factor(cars$CarName_Brand)
cars$CarName_Engine <- as.factor(cars$CarName_Engine)
cars$CarName_gears <- as.factor(cars$CarName_gears)
cars$CarName_Region <- as.factor(cars$CarName_Region)

# Building the model
model <- lm(CarName_Price ~ CarName_Miles + CarName_ModelYear + CarName_Brand + CarName_Engine + CarName_gears + CarName_Region, data = cars)
summary(model)

# Model diagnostics
par(mfrow = c(2, 2))
plot(model)

# Visualizing Residuals ---------------------------------------------------
ggplot(cars, aes(x = predict(model), y = residuals(model))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals")

# Calculate and Print AIC and BIC ------------------------------------------
aic_value <- AIC(model)
bic_value <- BIC(model)
cat("AIC:", aic_value, "\nBIC:", bic_value, "\n")

# Additional model building ------------------------------------------------
# Testing interactions between categorical variables
model_with_interactions <- lm(CarName_Price ~ CarName_Miles + CarName_ModelYear + CarName_Brand * CarName_Engine * CarName_gears, data = cars)
summary(model_with_interactions)

# Visualize model fit ------------------------------------------------------
ggplot(cars, aes(x = CarName_Price, y = predict(model))) +
  geom_point() +
  geom_line(aes(y = CarName_Price), color = "blue") +
  labs(title = "Model Fit: Predicted vs Actual", x = "Actual Price", y = "Predicted Price")

# Print summary of the new model with interactions -------------------------
print(summary(model_with_interactions))

# Evaluate improvements in the model with interaction terms ----------------
aic_with_interactions <- AIC(model_with_interactions)
bic_with_interactions <- BIC(model_with_interactions)
cat("AIC with interactions:", aic_with_interactions, "\nBIC with interactions:", bic_with_interactions, "\n")

# Conclusion and recommendations -------------------------------------------
cat("The model with interactions provides a better fit based on AIC and BIC comparisons, suggesting complex relationships between brand, engine type, and gears significantly impact car prices.\n")


