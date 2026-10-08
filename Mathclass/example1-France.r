# Model France's population using interactive carrying capacities

library(deSolve)
library(manipulate)

# Set working directory
path = '/home/vuddameri/MathBiology/Mathclass'
setwd(path)
# Read observed population
fname <- 'Francepop.csv'
france <- read.csv(fname)
head(france)

#make a plot
plot(france$Year,france$FrancePopM,
     xlab='Year',
     ylab='Population (M)',
     pch=10,col='blue',
     main='Population of France')
grid()
  
# Initial population in millions
P0 <- france$FrancePopM[1]

# Logistic growth with changing carrying capacity
logistic <- function(t, state, parameters) {
  P <- state[1]
  K <- ifelse(t < 1950, parameters["K1"], parameters["K2"])
  r <- parameters["r"]
  dP <- r * P * (1 - P/K)
  list(c(dP))
}

# Interactive numerical simulation
manipulate({
parameters <- c(r = r, K1 = K1, K2 = K2)
times <- seq(1800, 2020, by = 1)
out <- ode(y = c(P = P0),times = times,
    func = logistic,
    parms = parameters)
# Make new plots
plot(out[, "time"], out[, "P"],
       type = "l",
       col = "blue",
       lwd = 2,
       ylim = c(20, 80),
       xlab = "Year",
       ylab = "Population (millions)",
       main = "France Population: Step Change in Carrying Capacity")
  
  points(france$Year,
         france$FrancePopM,
         pch = 10,
         col = "red")
  abline(v = 1950, lty = 2, col = "gray40")
  grid()
  legend("topleft",
         legend = c("Logistic Model", "Observed Population"),
         col = c("blue", "red"),
         lty = c(1, NA),
         pch = c(NA, 19),
         bty = "n")
  
},
r = slider(0.001, 0.05, initial = 0.015, step = 0.001),
K1 = slider(30, 70, initial = 45, step = 1),
K2 = slider(50, 120, initial = 80, step = 1)
)