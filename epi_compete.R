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
        dI <-  beta * S * I + p/beta * R * I  - gamma * I - death * I
        dR <-                    gamma * I - p/beta * R * I -  death * R
        return(list(c(dS, dI, dR)))
    })
}

COMPsir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
        dS <- birth - beta1 * S * I1 - beta2 * S * I2  - death * S
        ## first species
        dI1 <-  beta1 * S * I1 + p1/beta1 * R1 * I1 -
            gamma * I1 - death * I1  ## +
        ### y/beta2 * R2 * I1
        dR1 <- gamma * I1 - p1/beta1 * R1 * I1 -
                   death * R1	 ## +, 
        ##  y/beta2 * R2 * I1
        
        ## second species
        dI2 <-  beta2 * S * I2 + p2/beta2 * R2 * I2  -
            gamma * I2 - death * I2 ## +
        ## p2/beta2 * R1 * I2 
        
        dR2 <- gamma * I2 - p2/beta2 * R2 * I2 - 
            death * R2 ## +
        ## p1/beta1 * R1 * I2

        ## ## both infections
        ## dI12 <-  x/beta1 * R2 * I1 + y/beta2 * R1 * I2 - 
        ##     gamma * I12 - death * I12  ## +
        ## ### y/beta2 * R2 * I1
        ## dR12 <- gamma * I12 - y/beta2 * R12 * I2 - x/beta1 * R12 * I1 -
        ##     death * R12 ## +
        return(list(c(dS, dI1, dR1, dI2, dR2))) ##, dI12, dR12)))
    })
}


### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected
## 0.000001, Recovered 0

initREC       <- c(S = 1-1e-6, I = 1e-6,  R = 0)##,

initCOMP       <- c(S = 1-1e-6, I1 = 1e-6, I2 = 1e-6, R1 = 0, R2 = 0)##,
## R12 = 0, I12=0) beta1: infection parameter; gamma: recovery
## parameter; p1 and p2 immune protection parameter

parametersREC <- c(beta = 0.95, gamma = 0.2, p=0.8,
                   death = 0.01, birth = 0.01)

parametersCOMP <- c(beta1 = 0.84, beta2 = 0.84, gamma = 0.2, p1 = 0.8, p2=0.8,
                death = 0.01, birth = 0.01)
## Time frame
times      <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
outREC <- ode(y = initREC, times = times, func = RECsir,
              parms = parametersREC)

## Solve using ode (General Solver for Ordinary Differential Equations)
outCOMP <- ode(y = initCOMP, times = times, func = COMPsir,
              parms = parametersCOMP)


## change to data frame
outREC <- as.data.frame(outREC)

loutREC <- pivot_longer(outREC, cols=c("S", "I", "R"),
                        names_to="state")


outCOMP <- as.data.frame(outCOMP)

loutCOMP <- pivot_longer(outCOMP, cols=c("S", "I1", "R1", "I2", "R2"), ##, "R12", "I12"),
                     names_to="state")


ggplot(loutREC, aes(x = time, y = value, color = state)) +
    geom_line()



ggplot(loutCOMP, aes(x = time, y = value, color = state)) +
    geom_line()



