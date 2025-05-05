#Code for Figure 3a.
#Written by Utsav Biswas in May 2024.
#Code to simulate sample mean & variance vs sample size for normal distribution.
#Plotting 30 such lines (in grey) and then put an envelop (values +-2SD) around the results.
#Final modification in April 2025.

library(ggplot2)
library(gridExtra)

# Defining the range of sample sizes
nd_sample_sizes <- seq(10, 400, 10)   # Sample sizes from 10 to 400 in steps of 10
num_lines <- 30  # Number of lines to plot per plot

# Generating empty vectors to store the sample means and variances
all_means <- list()
all_variances <- list()
mean_envelopes <- data.frame()
variance_envelopes <- data.frame()

# Generating random samples and calculating the mean and variance for each sample size
for (size in nd_sample_sizes) {
  means <- numeric(num_lines)
  variances <- numeric(num_lines)
  
  for (j in seq_len(num_lines)) {
    nd_sample <- rnorm(size, mean = 1, sd = 1)  # Generating random sample from normal distribution
    means[j] <- mean(nd_sample)
    variances[j] <- var(nd_sample)
  }
  
  # Storing the results in lists
  all_means[[as.character(size)]] <- means
  all_variances[[as.character(size)]] <- variances
  
  # Calculating mean and theoretical 95% confidence interval (CI) for envelopes
  mean_data <- data.frame(
    SampleSize = size,
    MeanMean = 1,
    MeanSD = 1.96 * (1 / sqrt(size))
  )
  
  variance_data <- data.frame(
    SampleSize = size,
    VarMean = 1,
    VarLower = (size - 1) / qchisq(0.975, size - 1),
    VarUpper = (size - 1) / qchisq(0.025, size - 1)
  )
  
  mean_envelopes <- rbind(mean_envelopes, mean_data)
  variance_envelopes <- rbind(variance_envelopes, variance_data)
}

# Creating data frames for plotting
mean_data <- do.call(rbind, lapply(names(all_means), function(size) {
  data.frame(
    SampleSize = as.numeric(size),
    SampleMean = all_means[[size]],
    Line = factor(1:num_lines)
  )
}))

variance_data <- do.call(rbind, lapply(names(all_variances), function(size) {
  data.frame(
    SampleSize = as.numeric(size),
    SampleVariance = all_variances[[size]],
    Line = factor(1:num_lines)
  )
}))

###############################################################################
# Plotting sample means vs sample size with theoretical 95% confidence envelopes
mean_plot <- ggplot() +
  geom_line(data = mean_data, aes(x = SampleSize, y = SampleMean, color = Line, group = Line), size = 0.8, colour = "#fdd0a2") +
  geom_line(data = mean_envelopes, aes(x = SampleSize, y = MeanMean - MeanSD), linetype = "solid", size = 2, color = "#fb8072") +
  geom_line(data = mean_envelopes, aes(x = SampleSize, y = MeanMean + MeanSD), linetype = "solid", size = 2, color = "#fb8072") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
  labs(title = "normal distribution", x = "", y = "sample mean") +
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2), minor_breaks = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 52),
    axis.text.y = element_text(face = "bold", size = 50),
    axis.text.x = element_text(face = "bold", size = 50),
    axis.title = element_text(size = 50),
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.8),  # Adjusting grid lines' color and width
    panel.grid.minor = element_line(color = "grey90", size = 0.5),  # Minor grid lines' appearance
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.25, "cm")
  )

###############################################################################
# Plotting sample variances vs sample size with theoretical 95% confidence envelopes
variance_plot <- ggplot() +
  geom_line(data = variance_data, aes(x = SampleSize, y = SampleVariance, color = Line, group = Line), size = 0.8, colour = "#cab2d6") +
  geom_line(data = variance_envelopes, aes(x = SampleSize, y = VarLower), linetype = "solid", size = 2, color = "#6a3d9a") +
  geom_line(data = variance_envelopes, aes(x = SampleSize, y = VarUpper), linetype = "solid", size = 2, color = "#6a3d9a") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
  labs(title = " ", x = "sample size", y = "sample variance") +
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2), minor_breaks = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 52),
    axis.text.y = element_text(face = "bold", size = 50),
    axis.text.x = element_text(face = "bold", size = 50),
    axis.title = element_text(size = 50),
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.8),  # Adjusting grid lines' color and width
    panel.grid.minor = element_line(color = "grey90", size = 0.5),  # Minor grid lines' appearance
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.25, "cm")
  )

# Displaying the plots side by side
grid.arrange(mean_plot, variance_plot, ncol = 1)

#Exported as TIFF at 1500x1346 pixels.