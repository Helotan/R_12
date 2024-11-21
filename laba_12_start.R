
data <- read.csv ("missile_attacks_daily.csv", header = TRUE, sep = ",")


# Convert time_start to Date type
data$time_start <- as.Date(data$time_start)


# Filter data for the year 2024
data_2023 <- subset(data, format(time_start, "%Y") == "2023" & launched > 0)


# Plot time_start vs launched
plot(data_2023$time_start, data_2023$launched,
     xlab = "Час атаки",
     ylab = "Кількість запущених ракет",
     main = "Ракетні удари протягом 2023 року",
     pch = 19,
     col = "blue")


model <- lm(launched ~ time_start, data = data_2023)
abline(model, col = "red", lwd = 2)

# Count attacks per target
target_counts <- table(data_2023$target)
par(mar = c(18, 0, 0, 0))
# Sort target counts in descending order
target_counts <- sort(target_counts, decreasing = TRUE)
# Plot histogram
bar_positions <- barplot(target_counts,
       main = "Атаки по областях за 2023 рік",
       cex.names = 1,
       ylab = "Кількість ракетних ударів",
       col = "lightblue",
       las = 2)


mtext("Область/регіон атаки", side = 1, line = -4)
mtext("Кількість ракетних ударів", side = 2, line = -1)
text(x = bar_positions, y = target_counts, label = target_counts, pos = 3, cex = 0.8, col = "black")
