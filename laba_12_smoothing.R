# Load necessary libraries
library(plotrix)
library(moments)
library(ggplot2)
library(forecast)
library(zoo)

data <- read.csv("missile_attacks_daily.csv", header = TRUE, sep = ",")

# Convert time_start to Date type
data$time_start <- as.Date(data$time_start)

# Filter data for the year 2023
data_2023 <- subset(data, format(time_start, "%Y") == "2023" & !is.na(launched))

# Create a new column for the week number
data_2023$week <- format(data_2023$time_start, "%Y-%U")

# Count the number of attacks per week
weekly_attacks <- data_2023 %>%
  group_by(week) %>%
  summarise(attacks = n())

# Plot unsmoothed values
unsmoothed_plot <- ggplot(weekly_attacks, aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = attacks)) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "Частота атак по тижнях у 2023 році (Unsmoothed)",
       x = "Тиждень",
       y = "Кількість атак") +
  theme_minimal()

# Save the unsmoothed plot to a file
ggsave("unsmoothed_attacks_2023.png", plot = unsmoothed_plot)

# Display non-smoothed statistics
cat("Non-smoothed statistics:\n")
print(weekly_attacks)
cat("\n")

# Define different window sizes (W values) for Kendall smoothing
window_sizes <- c(3, 5, 7)

# Function to apply Kendall smoothing and return a matrix
apply_kendall_smoothing <- function(data, window_size) {
  smoothed_attacks <- zoo::rollmean(data$attacks, k = window_size, fill = NA, align = "right")
  return(data.frame(week = data$week, attacks = data$attacks, smoothed_attacks = smoothed_attacks))
}

# Apply Kendall smoothing for each window size and store results in a list
kendall_smoothed_results <- lapply(window_sizes, function(w) apply_kendall_smoothing(weekly_attacks, w))

# Function to apply exponential smoothing and return a matrix
apply_exponential_smoothing <- function(data, alpha) {
  smoothed_attacks <- forecast::ses(data$attacks, alpha = alpha, initial = "simple")$fitted
  return(data.frame(week = data$week, attacks = data$attacks, smoothed_attacks = smoothed_attacks))
}

# Define different alpha values for exponential smoothing
alpha_values <- c(0.2, 0.5, 0.8)

# Apply exponential smoothing for each alpha value and store results in a list
exponential_smoothed_results <- lapply(alpha_values, function(alpha) apply_exponential_smoothing(weekly_attacks, alpha))

# Function to apply Pollard smoothing and return a matrix
apply_pollard_smoothing <- function(data, window_size) {
  smoothed_attacks <- zoo::rollapply(data$attacks, width = window_size, FUN = function(x) mean(x, trim = 0.1), fill = NA, align = "right")
  return(data.frame(week = data$week, attacks = data$attacks, smoothed_attacks = smoothed_attacks))
}

# Function to apply median filtering and return a matrix
apply_median_filtering <- function(data, window_size) {
  smoothed_attacks <- zoo::rollapply(data$attacks, width = window_size, FUN = median, fill = NA, align = "right")
  return(data.frame(week = data$week, attacks = data$attacks, smoothed_attacks = smoothed_attacks))
}

# Apply median filtering for each window size and store results in a list
median_filtered_results <- lapply(window_sizes, function(w) apply_median_filtering(weekly_attacks, w))

# Apply Pollard smoothing for each window size and store results in a list
pollard_smoothed_results <- lapply(window_sizes, function(w) apply_pollard_smoothing(weekly_attacks, w))

# Display matrices for different smoothing methods
for (i in 1:length(window_sizes)) {
  cat("Kendall Smoothing Matrix for W =", window_sizes[i], "\n")
  print(kendall_smoothed_results[[i]])
  cat("\n")
}

for (i in 1:length(alpha_values)) {
  cat("Exponential Smoothing Matrix for alpha =", alpha_values[i], "\n")
  print(exponential_smoothed_results[[i]])
  cat("\n")
}

for (i in 1:length(window_sizes)) {
  cat("Pollard Smoothing Matrix for W =", window_sizes[i], "\n")
  print(pollard_smoothed_results[[i]])
  cat("\n")
}

for (i in 1:length(window_sizes)) {
  cat("Median Filtering Matrix for W =", window_sizes[i], "\n")
  print(median_filtered_results[[i]])
  cat("\n")
}

# Build correlation table for each smoothing method
correlation_table <- weekly_attacks %>%
  select(week, attacks) %>%
  mutate(across(everything(), as.character)) %>%
  rename(Original = attacks)

# Calculate correlation for each value
correlation_values <- data.frame(Week = weekly_attacks$week)
for (i in 1:length(window_sizes)) {
  smoothed_col <- paste0("Kendall_Smoothed_W_", window_sizes[i])
  correlation_values[[smoothed_col]] <- cor(weekly_attacks$attacks, kendall_smoothed_results[[i]]$smoothed_attacks, use = "complete.obs")
}

for (i in 1:length(alpha_values)) {
  smoothed_col <- paste0("Exponential_Smoothed_alpha_", alpha_values[i])
  correlation_values[[smoothed_col]] <- cor(weekly_attacks$attacks, exponential_smoothed_results[[i]]$smoothed_attacks, use = "complete.obs")
}

for (i in 1:length(window_sizes)) {
  smoothed_col <- paste0("Pollard_Smoothed_W_", window_sizes[i])
  correlation_values[[smoothed_col]] <- cor(weekly_attacks$attacks, pollard_smoothed_results[[i]]$smoothed_attacks, use = "complete.obs")
}

for (i in 1:length(window_sizes)) {
  smoothed_col <- paste0("Median_Filtered_W_", window_sizes[i])
  correlation_values[[smoothed_col]] <- cor(weekly_attacks$attacks, median_filtered_results[[i]]$smoothed_attacks, use = "complete.obs")
}

# Display correlation table
cat("Correlation Table:\n")
print(correlation_table)
cat("\n")

# Display correlation values
cat("Correlation Values:\n")
print(correlation_values)

# Plot smoothed attacks for Kendall smoothing
for (i in 1:length(window_sizes)) {
  smoothed_plot <- ggplot(kendall_smoothed_results[[i]], aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = smoothed_attacks)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    labs(title = paste("Згладжена частота атак по тижнях у 2023 році (Kendall, W =", window_sizes[i], ")"),
         x = "Тиждень",
         y = "Згладжена кількість атак") +
    theme_minimal()
  
  # Save the smoothed plot to a file
  ggsave(paste0("kendall_smoothed_attacks_2023_W_", window_sizes[i], ".png"), plot = smoothed_plot)
  
  # Display the smoothed plot
  print(smoothed_plot)
}

# Plot smoothed attacks for Exponential smoothing
for (i in 1:length(alpha_values)) {
  smoothed_plot <- ggplot(exponential_smoothed_results[[i]], aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = smoothed_attacks)) +
    geom_point(color = "green") +
    geom_line(color = "green") +
    labs(title = paste("Згладжена частота атак по тижнях у 2023 році (Exponential, alpha =", alpha_values[i], ")"),
         x = "Тиждень",
         y = "Згладжена кількість атак") +
    theme_minimal()
  
  # Save the smoothed plot to a file
  ggsave(paste0("exponential_smoothed_attacks_2023_alpha_", alpha_values[i], ".png"), plot = smoothed_plot)
  
  # Display the smoothed plot
  print(smoothed_plot)
}

# Plot smoothed attacks for Pollard smoothing
for (i in 1:length(window_sizes)) {
  smoothed_plot <- ggplot(pollard_smoothed_results[[i]], aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = smoothed_attacks)) +
    geom_point(color = "purple") +
    geom_line(color = "purple") +
    labs(title = paste("Згладжена частота атак по тижнях у 2023 році (Pollard, W =", window_sizes[i], ")"),
         x = "Тиждень",
         y = "Згладжена кількість атак") +
    theme_minimal()
  
  # Save the smoothed plot to a file
  ggsave(paste0("pollard_smoothed_attacks_2023_W_", window_sizes[i], ".png"), plot = smoothed_plot)
  
  # Display the smoothed plot
  print(smoothed_plot)
}

# Plot smoothed attacks for Median filtering
for (i in 1:length(window_sizes)) {
  smoothed_plot <- ggplot(median_filtered_results[[i]], aes(x = as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), y = smoothed_attacks)) +
    geom_point(color = "orange") +
    geom_line(color = "orange") +
    labs(title = paste("Згладжена частота атак по тижнях у 2023 році (Median, W =", window_sizes[i], ")"),
         x = "Тиждень",
         y = "Згладжена кількість атак") +
    theme_minimal()
  
  # Save the smoothed plot to a file
  ggsave(paste0("median_filtered_attacks_2023_W_", window_sizes[i], ".png"), plot = smoothed_plot)
  
  # Display the smoothed plot
  print(smoothed_plot)
}
