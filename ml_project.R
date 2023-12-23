library(ggplot2)
library(rpart)
library(rpart.plot)
library(forecast)

file_path <- "C:/Users/Ali Brothers/Downloads/RE_data_upd.csv"
df <- read.csv(file_path)
print(head(df))

num_rows <- nrow(df)
num_columns <- ncol(df)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_columns, "\n")

str(df)

missing_values <- colSums(is.na(df))
missing_values

column_name <- "Fossil.fuel.energy.consumption....of.total."
if (column_name %in% names(df)) {
  if (any(!is.na(df[[column_name]]))) {
    if (!is.numeric(df[[column_name]])) {
      df[[column_name]] <- as.numeric(as.character(df[[column_name]]))
    }
    if (any(is.na(df[[column_name]]))) {
      column_mean <- mean(df[[column_name]], na.rm = TRUE)
      df[[column_name]] <- ifelse(is.na(df[[column_name]]), column_mean,
                                  df[[column_name]])
    }
  } else {
    print("All non-missing values in the column are missing.")
  }
} else {
  print("Column not found in the data frame.")
}
print(df)

summary(df[[column_name]])

mean_values <- colMeans(df[, 2:ncol(df)], na.rm = TRUE)
median_values <- sapply(df[, 2:ncol(df)], median, na.rm = TRUE)
mode_values <- sapply(df[, 2:ncol(df)], function(x) {
  ifelse(length(unique(x[!is.na(x)])) > 1, as.numeric(names(sort(table(x), decreasing = TRUE)[1])), NA)
})
std_dev_values <- apply(df[, 2:ncol(df)], 2, sd, na.rm = TRUE)
skewness_values <- sapply(df[, 2:ncol(df)], skewness, na.rm = TRUE)
kurtosis_values <- sapply(df[, 2:ncol(df)], kurtosis, na.rm = TRUE)


summary_stats <- data.frame(
  Mean = mean_values,
  Median = median_values,
  Mode = mode_values,
  Std_Dev = std_dev_values,
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

print(summary_stats)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(summary_stats$Skewness, main = 'Distribution of Skewness', col = 'skyblue',
     xlab = 'Skewness', xlim = range(summary_stats$Skewness), prob = TRUE)
lines(density(summary_stats$Skewness), col = 'red', lwd = 2)

hist(summary_stats$Kurtosis, main = 'Distribution of Kurtosis', col = 'skyblue', xlab = 'Kurtosis', xlim = range(summary_stats$Kurtosis), prob = TRUE)
lines(density(summary_stats$Kurtosis), col = 'red', lwd = 2)
par(mfrow = c(1, 1))

column_name_mapping <- c('Time' = 'Time',
                         'Renewable energy consumption (% of total final energy consumption)' = 'REC',
                         'Renewable electricity output (% of total electricity output)' = 'REC output',
                         'Electricity production from renewable sources, excluding hydroelectric (% of
total)' = 'EPES',
                         'Fossil fuel energy consumption (% of total)' = 'FFES',
                         'Electric power consumption (kWh per capita)' = 'EPC (kWh per capita)')

numeric_columns <- sapply(df, is.numeric)
numeric_df <- df[, numeric_columns]
correlation_matrix <- cor(numeric_df)
colnames(correlation_matrix) <- rownames(correlation_matrix) <- column_name_mapping

heatmap(correlation_matrix,
        col = colorRampPalette(c("blue", "white", "red"))(50),
        main = 'Correlation Matrix',
        cexRow = 0.8, cexCol = 0.8, margins = c(10, 10),
        labRow = names(correlation_matrix),
        labCol = names(correlation_matrix))


numeric_columns <- df[sapply(df, is.numeric)]
quantiles_iqr <- lapply(numeric_columns, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  outliers <- (x < (Q1 - 1.5 * IQR_value)) | (x > (Q3 + 1.5 * IQR_value))
  return(list(Q1 = Q1, Q3 = Q3, IQR = IQR_value, outliers = outliers))
})

print(quantiles_iqr)
any_outliers <- Reduce(`|`, lapply(quantiles_iqr, function(col) col$outliers))
df_no_outliers <- df[!any_outliers, ]
cat("Original Data Frame:\n")
print(df)
cat("Data Frame without Outliers:\n")
print(df_no_outliers)


developing_countries <- c('Nepal', 'New Zealand', 'Sri Lanka')
underdeveloped_countries <- c('Australia', 'Argentina', 'Bahrain', 'Pakistan', 'Qatar','United States','United Arab Emirates')
developing_nations <- df[df$`Country.Name` %in% developing_countries, ]
underdeveloped_nations <- df[df$`Country.Name` %in% underdeveloped_countries, ]
if (nrow(developing_nations) == 0 || nrow(underdeveloped_nations) == 0) {
  print("No matching countries found in the original dataset.")
} else {
  t_test_result <- t.test(
    developing_nations$`Renewable.energy.consumption....of.total.final.energy.consumption.`,
    underdeveloped_nations$`Renewable.energy.consumption....of.total.final.energy.consumption.`
  )
  cat("Hypothesis 1: T-statistic =", t_test_result$statistic, ", p-value =", t_test_result$p.value, "\n")
}
combined_data <- rbind(
  transform(developing_nations, Country_Type = "Developing"),
  transform(underdeveloped_nations, Country_Type = "Underdeveloped")
)

ggplot(combined_data, aes(x = Country_Type, y = `Renewable.energy.consumption....of.total.final.energy.consumption.`)) +
  geom_boxplot(fill = 'skyblue', color = 'black') +
  
  labs(title = 'Comparison of Renewable Energy Consumption between Developing
and Underdeveloped Nations',
       x = 'Country Type',
       y = 'Renewable Energy Consumption (%)') +
  theme_minimal()

correlation_result <- cor.test(
  df$`Renewable.electricity.output....of.total.electricity.output.`,
  df$`Renewable.energy.consumption....of.total.final.energy.consumption.`,
  method = "spearman")
cat("Hypothesis 2: Spearman correlation coefficient =",
    correlation_result$estimate, ", p-value =", correlation_result$p.value, "\n")

ggplot(df, aes(x = `Renewable.electricity.output....of.total.electricity.output.`, y =
                 `Renewable.energy.consumption....of.total.final.energy.consumption.`)) +
  geom_point(color = 'blue') +
  labs(title = 'Scatter Plot of Renewable Electricity Output vs. Renewable Energy
Consumption',
       x = 'Renewable electricity output (% of total electricity output)',
       y = 'Renewable energy consumption (% of total final energy consumption)') +
  theme_minimal()

X <- df[, c('Renewable.electricity.output....of.total.electricity.output.',
            'Fossil.fuel.energy.consumption....of.total.',
            'Electricity.production.from.renewable.sources..excluding.hydroelectric....of.total.',
            'Electric.power.consumption..kWh.per.capita.')]
y <- df$`Renewable.energy.consumption....of.total.final.energy.consumption.`
X <- cbind(1, X)
model <- lm(y ~ ., data = as.data.frame(X))

y_pred <- predict(model, newdata = as.data.frame(X))


ggplot(data = as.data.frame(cbind(y, y_pred)), aes(x = y, y = y_pred)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  labs(title = 'Actual vs. Predicted Values for Multiple Regression',
       x = 'Actual Values',
       y = 'Predicted Values') +
  theme_minimal()

mse <- mean((y - y_pred)^2)
rmse <- sqrt(mse)
rsquared <- summary(model)$r.squared
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsquared, "\n")

df_tree <- df
new_names <- c("Target", "Renew_cons", "Renew_elec", "Fossil_fuel", "Elec_prod_renew", "Elec_power_capita")
colnames(df_tree) <- new_names

tree_model <- rpart(Target ~ Renew_cons + Renew_elec + Fossil_fuel +
                      Elec_prod_renew + Elec_power_capita,
                    data = df_tree, method = "class")

par(mar = c(0, 2, 0, 5) )
prp(tree_model, box.col = "lightblue", branch.lty = 2, branch.col = "darkgreen", shadow.col = "gray", fallen.leaves = TRUE, trace = TRUE)


df$Time <- as.Date(paste0(df$Time, "-01-01"))
ts_data <- ts(df$`Renewable.energy.consumption....of.total.final.energy.consumption.`, frequency = 1, start = min(df$Time))
model_consumption <- auto.arima(ts_data)
forecast_consumption <- forecast(model_consumption, h = 5)
plot(forecast_consumption, main = 'Renewable Energy Consumption Forecasting',
     xlab = 'Time', ylab = 'Renewable Energy Consumption(%)')
plot_consumption <- ggplot(df, aes(x = Time, y = `Renewable.energy.consumption....of.total.final.energy.consumption.`, color = `Country.Name`)) +
  geom_line() +
  labs(title = 'Renewable Energy Consumption Over Time',
       x = 'Time',
       y = 'Renewable Energy Consumption (%)') +
  theme_minimal()

plot_consumption <- ggplot(df, aes(x = Time, y = `Renewable.energy.consumption....of.total.final.energy.consumption.`, color = `Country.Name`)) +
  geom_line() +
  labs(title = 'Renewable Energy Consumption Over Time',
       x = 'Time',
       y = 'Renewable Energy Consumption (%)') +
  theme_minimal()

print(plot_consumption)

plot_output <- ggplot(df, aes(x = Time, y = `Renewable.electricity.output....of.total.electricity.output.`, color = `Country.Name`)) +
  geom_line() +
  labs(title = 'Renewable Electricity Output Over Time',
       x = 'Time',
       y = 'Renewable Electricity Output (%)') +
  theme_minimal()

print(plot_output)

