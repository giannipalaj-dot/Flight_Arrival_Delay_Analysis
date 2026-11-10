# =============================================================================
# GIANNI PALAJ | FLIGHT ARRIVAL DELAY ANALYSIS
# Multiple linear regression model to estimate flight arrival delays.
# Dataset: Chapter_06_flight_delay.csv
# =============================================================================

# 1. SETUP ---------------------------------------------------------------------

# Set the working directory to the folder containing the CSV file, if needed.
# setwd("C:/Users/giann/Desktop/BUA 650")

library(psych)
library(moments)
library(caTools)
library(stargazer)
library(car)

data1 <- read.csv("Chapter_06_flight_delay.csv", header = TRUE)

# 2. DATA REVIEW ----------------------------------------------------------------

cat("\n========== DATA OVERVIEW ==========\n")
str(data1)
head(data1, 10)
names(data1)

cat("\nMissing values in dataset:", sum(is.na(data1)), "\n")
cat("\nMissing values by variable:\n")
print(colSums(is.na(data1)))

# Arrival-delay distribution
cat("\n========== ARRIVAL DELAY SUMMARY ==========\n")
describe(data1$Arr_Delay)

cat("\n========== JARQUE-BERA TEST ==========\n")
jarque.test(data1$Arr_Delay)

par(mfrow = c(1, 2))
hist(data1$Arr_Delay, breaks = 30,
     main = "Arrival Delay Distribution",
     xlab = "Arrival delay (minutes)",
     col = "lightblue", freq = FALSE)
curve(dnorm(x, mean = mean(data1$Arr_Delay, na.rm = TRUE),
            sd = sd(data1$Arr_Delay, na.rm = TRUE)),
      add = TRUE, col = "red", lwd = 2)
boxplot(data1$Arr_Delay,
        main = "Arrival Delay Boxplot",
        ylab = "Arrival delay (minutes)",
        col = "lightgreen")
par(mfrow = c(1, 1))

# 3. CORRELATION ANALYSIS -------------------------------------------------------

# Use numeric columns for the correlation analysis.
numeric_data <- data1[, sapply(data1, is.numeric), drop = FALSE]

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

cat("\n========== CORRELATION MATRIX ==========\n")
print(round(cor_matrix, 2))

cat("\n========== CORRELATIONS WITH ARRIVAL DELAY ==========\n")
cor_with_delay <- sort(cor_matrix[, "Arr_Delay"], decreasing = TRUE)
print(round(cor_with_delay, 3))

# Selected relationships with arrival delay
par(mfrow = c(2, 2))

plot(data1$Arr_Delay, data1$Number_of_flights,
     main = "Arrival Delay vs. Number of Flights",
     xlab = "Actual arrival delay (minutes)",
     ylab = "Number of flights",
     pch = 20, col = rgb(0, 0, 1, 0.3))

plot(data1$Arr_Delay, data1$Baggage_loading_time,
     main = "Arrival Delay vs. Baggage Loading",
     xlab = "Actual arrival delay (minutes)",
     ylab = "Baggage loading time",
     pch = 20, col = rgb(0, 0, 1, 0.3))

plot(data1$Arr_Delay, data1$Security_o,
     main = "Arrival Delay vs. Security",
     xlab = "Actual arrival delay (minutes)",
     ylab = "Security",
     pch = 20, col = rgb(0, 0, 1, 0.3))

plot(data1$Arr_Delay, data1$Support_Crew_Available,
     main = "Arrival Delay vs. Support Crew",
     xlab = "Actual arrival delay (minutes)",
     ylab = "Support crew available",
     pch = 20, col = rgb(0, 0, 1, 0.3))

par(mfrow = c(1, 1))

# 4. TRAINING AND TEST DATA ----------------------------------------------------

# A fixed seed makes the split reproducible.
set.seed(1000)

split_index <- sample.split(data1$Arr_Delay, SplitRatio = 0.70)
train_data <- data1[split_index, ]
test_data <- data1[!split_index, ]

cat("\nTraining observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")
cat("Training mean arrival delay:",
    round(mean(train_data$Arr_Delay, na.rm = TRUE), 2), "\n")
cat("Test mean arrival delay:",
    round(mean(test_data$Arr_Delay, na.rm = TRUE), 2), "\n")

# 5. REGRESSION MODELS ----------------------------------------------------------

# Full model: includes the available operational and flight-related predictors.
model <- lm(
  Arr_Delay ~ Baggage_loading_time +
    Number_of_flights +
    Late_Arrival_o +
    Airport_Distance +
    Weather +
    Security_o +
    Cleaning_o +
    Fueling_o +
    Support_Crew_Available,
  data = train_data
)

cat("\n========== FULL REGRESSION MODEL ==========\n")
summary(model)

stargazer(
  model,
  type = "text",
  title = "Flight Arrival Delay: Full Regression Model",
  dep.var.labels = "Arrival delay (minutes)",
  ci = TRUE,
  ci.level = 0.95,
  single.row = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

# Reduced model: focuses on the predictors selected for this analysis.
model_sig <- lm(
  Arr_Delay ~ Airport_Distance + Number_of_flights + Weather +
    Support_Crew_Available + Baggage_loading_time + Late_Arrival_o,
  data = train_data
)

cat("\n========== REDUCED REGRESSION MODEL ==========\n")
summary(model_sig)

stargazer(
  model_sig,
  type = "text",
  title = "Flight Arrival Delay: Reduced Regression Model",
  dep.var.labels = "Arrival delay (minutes)",
  ci = TRUE,
  ci.level = 0.95,
  single.row = TRUE
)

cat("\n========== FULL VS. REDUCED MODEL ==========\n")
stargazer(
  model, model_sig,
  type = "text",
  title = "Regression Model Comparison",
  column.labels = c("Full Model", "Reduced Model"),
  dep.var.labels = "Arrival delay (minutes)",
  keep.stat = c("n", "rsq", "adj.rsq"),
  star.cutoffs = c(0.05, 0.01, 0.001)
)

cat("\nFull model R-squared:",
    round(summary(model)$r.squared, 4), "\n")
cat("Full model adjusted R-squared:",
    round(summary(model)$adj.r.squared, 4), "\n")
cat("Reduced model R-squared:",
    round(summary(model_sig)$r.squared, 4), "\n")
cat("Reduced model adjusted R-squared:",
    round(summary(model_sig)$adj.r.squared, 4), "\n")

# 6. MODEL PERFORMANCE ----------------------------------------------------------

pred_train <- predict(model_sig, newdata = train_data)
pred_test <- predict(model_sig, newdata = test_data)

# Training performance
rmse_train <- sqrt(mean((train_data$Arr_Delay - pred_train)^2, na.rm = TRUE))
mae_train <- mean(abs(train_data$Arr_Delay - pred_train), na.rm = TRUE)

# Test performance
rmse_test <- sqrt(mean((test_data$Arr_Delay - pred_test)^2, na.rm = TRUE))
mae_test <- mean(abs(test_data$Arr_Delay - pred_test), na.rm = TRUE)

ss_res <- sum((test_data$Arr_Delay - pred_test)^2, na.rm = TRUE)
ss_tot <- sum((test_data$Arr_Delay -
                 mean(test_data$Arr_Delay, na.rm = TRUE))^2, na.rm = TRUE)
r2_test <- 1 - (ss_res / ss_tot)

cat("\n========== MODEL PERFORMANCE ==========\n")
cat("\nTraining set:\n")
cat("RMSE:", round(rmse_train, 2), "minutes\n")
cat("MAE:", round(mae_train, 2), "minutes\n")
cat("R-squared:", round(summary(model_sig)$r.squared, 4), "\n")

cat("\nTest set:\n")
cat("RMSE:", round(rmse_test, 2), "minutes\n")
cat("MAE:", round(mae_test, 2), "minutes\n")
cat("R-squared:", round(r2_test, 4), "\n")
cat("Variance explained on test data:",
    round(r2_test * 100, 1), "%\n")

# Actual versus predicted arrival delays
par(mfrow = c(1, 2))

plot(test_data$Arr_Delay, pred_test,
     main = "Actual vs. Predicted Arrival Delays",
     xlab = "Actual delay (minutes)",
     ylab = "Predicted delay (minutes)",
     pch = 20, col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)
legend("topleft",
       legend = c("Perfect prediction", "Model predictions"),
       col = c("red", rgb(0, 0, 1, 0.5)),
       lty = c(1, NA), pch = c(NA, 20), bty = "n")

# Test-set residual distribution
residuals_test <- test_data$Arr_Delay - pred_test

hist(residuals_test, breaks = 30,
     main = "Test-Set Residual Distribution",
     xlab = "Residual (actual - predicted minutes)",
     col = "lightblue")
abline(v = 0, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

# 7. REGRESSION DIAGNOSTICS -----------------------------------------------------

cat("\n========== REGRESSION DIAGNOSTICS ==========\n")

par(mfrow = c(2, 2))
plot(model_sig)
par(mfrow = c(1, 1))

# Durbin-Watson test
cat("\n========== DURBIN-WATSON TEST ==========\n")
dw_test <- durbinWatsonTest(model_sig)
print(dw_test)

# Non-constant variance test
cat("\n========== BREUSCH-PAGAN / NON-CONSTANT VARIANCE TEST ==========\n")
ncv_test <- ncvTest(model_sig)
print(ncv_test)

# Variance inflation factors
cat("\n========== VARIANCE INFLATION FACTORS ==========\n")
vif_values <- vif(model_sig)
print(round(vif_values, 2))

# 8. FINAL PROJECT SUMMARY ------------------------------------------------------

coef_summary <- summary(model_sig)$coefficients
sig_predictors <- rownames(coef_summary)[coef_summary[, "Pr(>|t|)"] < 0.05]
sig_predictors <- sig_predictors[sig_predictors != "(Intercept)"]

cat("\n========== FINAL MODEL SUMMARY ==========\n")
cat("\nPredictors significant at the 5% level:\n")

if (length(sig_predictors) == 0) {
  cat("No predictors were statistically significant at the 5% level.\n")
} else {
  for (pred in sig_predictors) {
    coef_val <- coef_summary[pred, "Estimate"]
    p_val <- coef_summary[pred, "Pr(>|t|)"]
    cat(pred, ": coefficient =", round(coef_val, 3),
        "| p-value =", format(p_val, scientific = FALSE), "\n")
  }
}

cat("\nModel performance:\n")
cat("Training R-squared:", round(summary(model_sig)$r.squared, 3), "\n")
cat("Test R-squared:", round(r2_test, 3), "\n")
cat("Test RMSE:", round(rmse_test, 2), "minutes\n")
cat("Test MAE:", round(mae_test, 2), "minutes\n")

cat("\nAnalysis complete: flight arrival delay model evaluated.\n")
