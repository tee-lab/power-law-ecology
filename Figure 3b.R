#Code for Figure 3b.
#Code to simulate sample mean & variance vs sample size for exponential distribution. 
#Plotting 30 such lines (in grey) and then put an envelop (values +-2SD) around the results.
#Final modification in April 2025.

library(ggplot2)
library(gridExtra)

# Defining the range of sample sizes
ed_sample_sizes <- seq(10, 400, 10)  # Sample sizes from 10 to 400 in steps of 10

# Number of lines to plot per plot
num_lines <- 30

# Empty lists to store sample means and variances
all_means <- list()
all_variances <- list()
mean_envelopes <- data.frame()
variance_envelopes <- data.frame()

# Generating random samples and calculate the mean and variance for each sample size
for (size in ed_sample_sizes) {
  means <- numeric(num_lines)
  variances <- numeric(num_lines)
  
  for (j in seq_len(num_lines)) {
    ed_sample <- rexp(size, rate = 1)  # Generating random sample from exponential distribution
    means[j] <- mean(ed_sample)
    variances[j] <- var(ed_sample)
  }
  
  # Storing the results in lists
  all_means[[as.character(size)]] <- means
  all_variances[[as.character(size)]] <- variances
  
  # Calculating mean and theoretical envelope for means
  mean_data <- data.frame(
    SampleSize = size,
    MeanMean = mean(means),
    MeanLower = 1 - 2 / sqrt(size),  # 95% CI lower bound
    MeanUpper = 1 + 2 / sqrt(size)   # 95% CI upper bound
  )
  mean_envelopes <- rbind(mean_envelopes, mean_data)
  
  # Calculating mean and theoretical envelope for variances
  var_data <- data.frame(
    SampleSize = size,
    VarMean = mean(variances),
    VarLower = (1 - 2 / sqrt(size))^2,  # 95% CI lower bound
    VarUpper = (1 + 2 / sqrt(size))^2   # 95% CI upper bound
  )
  variance_envelopes <- rbind(variance_envelopes, var_data)
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
# Plotting sample means vs sample size with theoretical envelopes
mean_plot <- ggplot() +
  geom_line(data = mean_data, aes(x = SampleSize, y = SampleMean, color = Line, group = Line), size = 0.5, color = "#fdd0a2") +
  geom_line(data = mean_envelopes, aes(x = SampleSize, y = MeanLower), linetype = "solid", size = 2, color = "#fb8072") +
  geom_line(data = mean_envelopes, aes(x = SampleSize, y = MeanUpper), linetype = "solid", size = 2, color = "#fb8072") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
  labs(title = "Exponential",
       x = "", y = "") +
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2), minor_breaks = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.text.y = element_text(face = "bold", size = 30),
    axis.text.x = element_text(face = "bold", size = 30),
    axis.title = element_text(size = 50),
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.8),  # Adjusting grid lines' color and width
    panel.grid.minor = element_line(color = "grey90", size = 0.5),  # Minor grid lines' appearance
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.25, "cm")
  )

###############################################################################
# Plotting sample variances vs sample size with theoretical envelopes
variance_plot <- ggplot() +
  geom_line(data = variance_data, aes(x = SampleSize, y = SampleVariance, color = Line, group = Line), size = 0.5, color = "#cab2d6") +
  geom_line(data = variance_envelopes, aes(x = SampleSize, y = VarLower), linetype = "solid", size = 2, color = "#6a3d9a") +
  geom_line(data = variance_envelopes, aes(x = SampleSize, y = VarUpper), linetype = "solid", size = 2, color = "#6a3d9a") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1.5) +
  labs(title = "",
       x = "sample size", y = "") +
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2), minor_breaks = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.text.y = element_text(face = "bold", size = 30),
    axis.text.x = element_text(face = "bold", size = 30),
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

#Can export the plots separately also
mean_plot
variance_plot

#Exported as TIFF at 1500x1346 pixels.
