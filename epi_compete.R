## Load deSolve package
library(deSolve)
library(ggplot2)
library(tidyr)

## ## This here would be the most basic SIR 
## sir <- function(time, state, parameters) {
##     with(as.list(c(state, parameters)), {
##         dS <- -beta * S * I
##         dI <-  beta * S * I - gamma * I
##         dR <-                 gamma * I
##         return(list(c(dS, dI, dR)))
##     })
## }


## ## This here would be a SIR with recovered becoming infected at a
## ## rate depending on their primary virulence and a immunization
## ## factor x and with death and birth

RECsir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
        dS <- birth - beta * S * I - death * S
        dI <-  beta * S * I + x/beta * R * I  - gamma * I - death * I
        dR <-                    gamma * I - x/beta * R * I -  death * R
        return(list(c(dS, dI, dR)))
    })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected
## 0.000001, Recovered 0
init       <- c(S = 1-1e-6, I = 1e-6, R = 0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1.9, gamma = 0.14286, x = 0.5,
                death = 0.01, birth = 0.01)
## Time frame
times      <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = RECsir,
           parms = parameters)
## change to data frame
out <- as.data.frame(out)

lout <- pivot_longer(out, cols=c("S", "I", "R"), names_to="state")

ggplot(lout, aes(x = time, y = value, color = state)) +
    geom_line()

