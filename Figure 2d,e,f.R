#Code for Figures 2d,e,f
#Written by Utsav Biswas. Final edit in May 2025.

###############################################################################
library(ggplot2)
#----
#Generating x values for the normal distribution
x_normal <- seq(0.01, 10000, length.out = 100000)
y_norm <- dnorm(x_normal, mean = 0, sd = 1)
y_normal <- 2*y_norm #Since we are going to plot only for +x values, for normal distribution, 
#the y values need to be multiplied by 2, so that the net probability remains same.
x_normal_log <-log(x_normal[1:380])
y_normal_log <-log(y_normal[1:380])

#----
#Generating x values for the exponential distribution
x_exponential <- seq(0.01, 10000, length.out = 100000)
lambda <- 1
y_exponential <- dexp(x_exponential, rate = lambda)
x_exponential_log <-log(x_exponential[1:380])
y_exponential_log <-log(y_exponential[1:380])

#----
#Generating x values for the power law distribution
xmin <- 1
x_max <- 10000
alpha <- 2
x_powerlaw <- seq(xmin, x_max, length.out = 100000)
y_powerlaw <- (alpha - 1) / xmin * (x_powerlaw / xmin)^(-alpha)
#Normalizing the power-law density values
y_powerlaw <- y_powerlaw / sum(y_powerlaw * diff(x_powerlaw)[1])

x_powerlaw_log <-log(x_powerlaw[1:380])
y_powerlaw_log <-log(y_powerlaw[1:380])
 
#----
#Creating data frames for all distributions
data_normal <- data.frame(x = x_normal, y = y_normal, Distribution = "Gaussian")
data_exponential <- data.frame(x = x_exponential, y = y_exponential, Distribution = "Exp")
data_powerlaw <- data.frame(x = x_powerlaw, y = y_powerlaw, Distribution = "Power-law")

#Creating data frames for all distributions (for log values)
data_normal_log <- data.frame(x = x_normal_log, y = y_normal_log, Distribution = "Gaussian")
data_exponential_log <- data.frame(x = x_exponential_log, y = y_exponential_log, Distribution = "Exp")
data_powerlaw_log <- data.frame(x = x_powerlaw_log, y = y_powerlaw_log, Distribution = "Power-law")

#---- 
#Combining the data frames
data <- rbind(data_normal, data_exponential, data_powerlaw)

#Combining the data frames (for log values)
data_log <- rbind(data_normal_log, data_exponential_log, data_powerlaw_log)

#----
#Setting the levels of the Distribution factor to ensure correct legend order
data$Distribution <- factor(data$Distribution, levels = c("Gaussian", "Exp", "Power-law"))

#Setting the levels of the Distribution factor to ensure correct legend order (for log values)
data_log$Distribution <- factor(data_log$Distribution, levels = c("Gaussian", "Exp", "Power-law"))

###############################################################################
#Plotting using ggplot2 [Figure 2d]
ggplot(data, aes(x = x, y = y, color = Distribution, linetype = Distribution)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Gaussian" = "brown",
                                "Exp" = "darkgreen",
                                "Power-law" = "blue")) +
  scale_linetype_manual(values = c("Gaussian" = "solid", 
                                   "Exp" = "dashed", 
                                   "Power-law" = "dotted")) +  # Custom line types
  labs(x = "x", y = "Probability Density", title = "") +
  theme_minimal() +
  xlim(0, 10) +
  ylim(0, 0.45) +
  theme(legend.position = "none",  # Remove legend
        axis.text = element_text(face = "bold", size = 50),
        axis.title = element_text(size = 50))

###############################################################################
#Plotting using ggplot2  [Figure 2e]
ggplot(data, aes(x = x, y = y, color = Distribution, linetype = Distribution)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Gaussian" = "brown",
                                "Exp" = "darkgreen",
                                "Power-law" = "blue")) +
  scale_linetype_manual(values = c("Gaussian" = "solid", 
                                   "Exp" = "dashed", 
                                   "Power-law" = "dotted")) +  # Custom line types
  labs(x = "x", y = "Probability Density", title = "") +
  theme_minimal() +
  xlim(5, 10) + 
  ylim(0, 0.041) +
  theme(legend.position = "top",
        axis.text = element_text(face = "bold", size = 50),  
        axis.title = element_text(size = 50),  
        legend.text = element_text(size = 36)) +  # Adjust legend text size
  guides(color = guide_legend(title = NULL),  # Remove color legend title
         linetype = guide_legend(title = NULL))  # Remove linetype legend title

###############################################################################
#Plotting using ggplot2 [Figure 2f] [log-log plot] [log gives natural log]
#Plotting the first 100 values only

ggplot(data_log, aes(x = x, y = y, color = Distribution, linetype = Distribution)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Gaussian" = "brown",
                                "Exp" = "darkgreen",
                                "Power-law" = "blue")) +
  scale_linetype_manual(values = c("Gaussian" = "solid", 
                                   "Exp" = "dashed", 
                                   "Power-law" = "dotted")) +  # Custom line types
  labs(x = "log(x)", y = "log(Probability Density)", title = "") +
  theme_minimal() +
  xlim(1, 3.7) + 
  ylim(-25, 0) +
  theme(legend.position = "none",  # Remove legend
        axis.text = element_text(face = "bold", size = 50),
        axis.title = element_text(size = 44))
