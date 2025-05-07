#Code for Figure 4a
#To simulate a two-dimensional random walk in continuous space and time
#Written by Utsav Biswas in June 2024.
#Final edit in Jan 2025.

library(animation)
#Parameters
tt <- 300   #Total simulation time 
tau <- 10   #Average waiting time
L <- 1      #Max jump length

N <- 10 * tt / tau  #Rough expected number of jumps  #300 steps

#Declaring variables
x <- numeric(N)
y <- numeric(N)

theta <- numeric(N)  #Turning angle
jump <- numeric(N)   #Jump lengths
t <- numeric(N)      #Waiting times

#Initial location
x[1] <- 0
y[1] <- 0

#Plotting window
plot(x[1], y[1], type = 'p', xlim = c(-10, 10), ylim = c(-10, 10), xlab = 'x', ylab = 'y', 
     main = 'Random Walk', las = 1, col = 'black', pch = 18, cex = 1.8, axes = TRUE, cex.lab=1.8, cex.axis=1.8, cex.main=1.8)
box()
grid()

#Simulating random walk
for (i in 2:N) {
  
  #Generating random turning angle and jump length
  theta[i-1] <- runif(1, -pi, pi)
  jump[i-1] <- runif(1, 0, L)
  
  #New location based on turning angle and jump length
  x[i] <- x[i-1] + jump[i-1] * cos(theta[i-1])
  y[i] <- y[i-1] + jump[i-1] * sin(theta[i-1])
  
  #Adjusting animation speed
  Sys.sleep(0.1)
  
  #Plotting the animation
  lines(x[(i-1):i], y[(i-1):i], col = 'blue', lwd = 2)
  #points(x[i], y[i], pch = 20, col = 'darkgreen', cex=0.7)
  
  #Highlighting the final point
  if (i == N) {
    points(x[N], y[N], pch = 20, col = 'darkviolet', cex = 2)
  }
  
  #Adding a legend on the first iteration
  if (i == 2) {
    legend('topright', legend = c('Path', 'Final point'),
           col = c('blue', 'darkviolet'), lty = c(1, NA), 
           pch = c(NA, 20), pt.cex = c(2, 2), bty = 'n', lwd=c(2, NA), cex=1.5)
  }
}
