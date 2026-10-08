# Numerically solve the Lotka-Volterra model with interactive parameters
# R = Rabbits; F = Foxes
library(deSolve)
library(rootSolve)
library(manipulate)

LV <- function(t, state, parms) {
  R <- state[1]
  F <- state[2]
  with(as.list(parms), {
    dR <- a * R - b * R * F
    dF <- c * R * F - d * F
    list(c(dR, dF))
  })
}

# Function for finding steady-state roots
LVroot <- function(state, parms) {
  R <- state[1]
  F <- state[2]
  with(as.list(parms), {
    dR <- a * R - b * R * F
    dF <- c * R * F - d * F
    c(dR, dF)
  })
}

manipulate({
  parms <- c(a = a, b = b, c = c, d = d)
  state <- c(R = 40, F = 9)
  times <- seq(0, 50, by = 0.05)
  out <- as.data.frame(
    ode(state, times, LV, parms)
  )
  # Numerically calculate the coexistence equilibrium
  root <- multiroot(
    f = LVroot,
    start = c(R = 20, F = 10),
    parms = parms
  )
  Rstar <- root$root[1]
  Fstar <- root$root[2]
  # Display population dynamics and phase plane
  par(mfrow = c(1, 2))
  matplot(out$time,
          out[, c("R", "F")],
          type = "l",
          lty = 1,
          lwd = 2,
          col = c("blue", "red"),
          xlab = "Time (years)",
          ylab = "Population",
          main = "Predator-Prey Cycles")
  legend("topright",
         legend = c("Rabbits", "Foxes"),
         col = c("blue", "red"),
         lty = 1,
         bty = "n")
  grid()
  
  # Phase-plane trajectory
  plot(out$R, out$F,
       type = "l",
       lwd = 2,
       col = "darkgreen",
       xlab = "Rabbits",
       ylab = "Foxes",
       main = "Phase Plane")
  
  points(Rstar, Fstar,
         pch = 19,
         col = "red",
         cex = 1.5)
  
  abline(v = Rstar, lty = 2, col = "blue")
  abline(h = Fstar, lty = 2, col = "red")
  grid()
  par(mfrow = c(1, 1))
  cat("Coexistence equilibrium:\n")
  cat("Rabbits =", round(Rstar, 2), "\n")
  cat("Foxes =", round(Fstar, 2), "\n")
},
a = slider(0.5, 2.0, initial = 1.0, step = 0.1),
b = slider(0.02, 0.2, initial = 0.10, step = 0.01),
c = slider(0.02, 0.15, initial = 0.075, step = 0.005),
d = slider(0.5, 3.0, initial = 1.5, step = 0.1)
)