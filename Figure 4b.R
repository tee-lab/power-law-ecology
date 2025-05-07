#Code for Figure 4b
#To simulate two-dimensional Levy walk
#Written by Utsav Biswas in June 2024.
#Final edit in Jan 2025.

library(poweRlaw)
library(animation)

#Parameters
tt <- 300   #Total simulation time 
tau <- 10   #Average waiting time
L <- 1      #Max jump length

N <- 10 * tt / tau  # (rough) Expected number of jumps.

#Declaring variables
x <- numeric(N)
y <- numeric(N)

theta <- numeric(N)  #Turning angle
jump <- numeric(N)   #Jump lengths
t <- numeric(N)      #Waiting times

#Initial location
x[1] <- 0
y[1] <- 0

#Setting up the plot window
plot(x[1], y[1], type='p', xlim=c(-300,300), ylim=c(-300,300), xlab='x', ylab='y', 
     main='Levy Walk', las = 1, col = 'black', pch = 18, cex = 1.8, axes = TRUE, cex.lab=1.8, cex.axis=1.8, cex.main=1.8)
box()
grid()

#Function to generate Levy-distributed jump lengths
generate_levy_jump <- function(mu, L) {
  return(L / runif(1)^(1 / mu))
}

#Simulating Levy walk
for (i in 2:N) {
  
  theta[i-1] <- runif(1, -pi, pi)  #Random uniform turning angle
  jump[i-1] <- generate_levy_jump(mu = 1, L = L)  #Levy-distributed jump length
  
  #New location based on turning angle and jump length
  x[i] <- x[i-1] + jump[i-1] * cos(theta[i-1])
  y[i] <- y[i-1] + jump[i-1] * sin(theta[i-1])
  
  Sys.sleep(0.1)  #Adjusting animation speed
  lines(x[(i-1):i], y[(i-1):i], col='blue', lwd=2)
  #points(x[i], y[i], pch=20, col='darkgreen', cex=0.5)
  if (i == N) {
    points(x[N], y[N], pch=20, col='darkviolet', cex=2)
  }
  
  if (i == 2) {
    legend('topright', legend = c('Path', 'Final point'),
           col = c('blue', 'darkviolet'), lty = c(1, NA), 
           pch = c(NA, 20), pt.cex = c(2, 2), bty = 'n', lwd=c(2,NA), cex=1.5)
  }
}
