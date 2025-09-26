#Code for Figure 3c.
#Code to simulate sample mean & variance vs sample size for power-law distribution.
#Final modification in April 2025.

library(poweRlaw)
library(ggplot2)
library(gridExtra)

# Defining the range of sample sizes
pld_sample_sizes <- seq(10, 400, 10)

# Generating empty vectors to store the sample means and variances
pld_sample_means <- numeric(length(pld_sample_sizes))
pld_sample_var <- numeric(length(pld_sample_sizes))

# Generating random samples and calculating the mean and variance for each sample size
for (i in seq_along(pld_sample_sizes)) {
  pld_sample <- rpldis(pld_sample_sizes[i], 10, 1.5)  # Generating random sample from power-law distribution
  pld_sample_means[i] <- mean(pld_sample)  # Calculating and storing the sample mean
  pld_sample_var[i] <- var(pld_sample)  # Calculating and storing the sample variance
}

# Creating a data frame to hold the results
data <- data.frame(
  SampleSize = pld_sample_sizes,
  SampleMean = pld_sample_means,
  SampleVariance = pld_sample_var)

###############################################################################
# Plotting sample mean vs sample size using ggplot2
mean_plot <- ggplot(data, aes(x = SampleSize, y = SampleMean)) +
  geom_line(color = "#fb8072", size = 2) +
  labs(title = "Power-law (β = 1.5)",
       x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 44),  # Increase title font size
    axis.text = element_text(face = "bold", size = 30),  # Increase axis text font size
    axis.title = element_text(size = 50),  # Increase axis title font size
    panel.grid.major = element_line(color = "grey80", size = 0.8),  # Adjust major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.5),  # Adjust minor grid lines
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.25, "cm")
  ) +
  xlim(10, max(pld_sample_sizes))  # Set x-axis limits

###############################################################################
# Plotting sample variance vs sample size using ggplot2
variance_plot <- ggplot(data, aes(x = SampleSize, y = SampleVariance)) +
  geom_line(color = "#6a3d9a", size = 2) +
  labs(title = "",
       x = "sample size", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),  # Increase title font size
    axis.text = element_text(face = "bold", size = 30),  # Increase axis text font size
    axis.title = element_text(size = 50),  # Increase axis title font size
    panel.grid.major = element_line(color = "grey80", size = 0.8),  # Adjust major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.5),  # Adjust minor grid lines
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.25, "cm")
  ) +
  xlim(10, max(pld_sample_sizes))  # Set x-axis limits

# Displaying the plots side by side
grid.arrange(mean_plot, variance_plot, ncol = 1)

#Can export the plots separately also
mean_plot
variance_plot

# Exported as TIFF at 1500x1346 pixels.
