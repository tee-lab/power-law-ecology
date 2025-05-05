#Code to create plots of 3 distributions [Figure 2a]
#Written by Utsav Biswas. Final edit in May 2025.

###############################################################################
#Normal Distribution [Figure 2a.1]
library(ggplot2)
#Defining the mean and standard deviation
mean_value <- 1
sd_value <- 1
x <- seq(-3, 5, length.out = 100)
y <- dnorm(x, mean = mean_value, sd = sd_value)
data <- data.frame(x = x, y = y)
#Plotting the normal distribution using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "darkorange", alpha = 0.5) +  
  geom_line(color = "brown", size = 2.5) +   
  labs(title = "normal distribution",
       x = "x",
       y = "Probability Density") +
  theme_classic() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  
    axis.text = element_text(face = "bold", size = 28),  
    axis.title = element_text(size = 28),  
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),  
    panel.grid.minor = element_line(size = 0.7, linetype = 'dotted', colour = "grey")  
  ) +
  xlim(-3, 5)  

###############################################################################
#Exponential Distribution [Figure 2a.2]
#Defining the rate parameter for the exponential distribution
lambda <- 1  # When lambda = 1, mean = 1/lambda = 1, sd = 1/lambda = 1
x <- seq(0, 5, length.out = 100)
y <- dexp(x, rate = lambda)
data <- data.frame(x = x, y = y)
#Plotting the exponential distribution using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightgreen", alpha = 0.5) +  
  geom_line(color = "darkgreen", size = 2.5) +   
  labs(title = "exponential distribution",
       x = "x",
       y = "Probability Density") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  
    axis.text = element_text(face = "bold", size = 28),  
    axis.title = element_text(size = 28),  
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),  
    panel.grid.minor = element_line(size = 0.7, linetype = 'dotted', colour = "grey")  
  ) +
  xlim(0, 5)  

###############################################################################
#Powerlaw Distribution [Figure 2.a.3]
beta <- 2 #Power-law exponent
x <- seq(1, 5, length.out = 100)
y <- x^(-beta)
#Normalizing the y values to make it a proper probability density function
y <- y / sum(y)
data <- data.frame(x = x, y = y)

#Plotting the power-law distribution using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_area(fill = "lightblue", alpha = 0.5) +  
  geom_line(color = "blue", size = 2.5) +       
  labs(title = "power-law distribution",
       x = "x",
       y = "Probability Density") +
  theme_classic() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  
    axis.text = element_text(face = "bold", size = 28),  
    axis.title = element_text(size = 28),  
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),  
    panel.grid.minor = element_line(size = 0.7, linetype = 'dotted', colour = "grey")  
  )