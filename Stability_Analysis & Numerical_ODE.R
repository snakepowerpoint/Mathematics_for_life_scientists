######################################################################
### Stability Analysis (Prey & Predator interaction)
######################################################################
library(deSolve)

### Example function
LVmod = function(Time, State, Pars){
    with(as.list(c(State, Pars)), {
        dPrey = r*Prey - aPrey*Predator
        dPredator = -d*Predator + b*a*Prey*Predator
        return(list(c(dPrey, dPredator)))
    })
}

### Homework function
Mymod = function(time, state, pars){
    with(as.list(c(state, pars)), {
        dPrey = a*(1 - Prey/k)*Prey - b*Prey*Predator
        dPredator = c*Prey*Predator - d*Predator
        return(list(c(dPrey, dPredator)))
    })
}

pars = c(a = 5, c = 0.01, d = 0.5, b = 0.1, k = 1000000)
yini = c(Prey = 200, Predator = 20)
times = seq(0, 100, by = 1)
out = as.data.frame(ode(func = Mymod, y = yini, parms = pars, times = times))

## plot two time series together
matplot(out[,1], out[,2:3], type="l", xlab="Time", ylab="Abundance",
        main = "Lotka-Volterra", lwd = 2, col=c(4,2))
legend("topright", c("prey", "predator"), col = c(4,2), lty = 1:2)

## phase plot
plot(out[, 2], out[, 3], type="l", xlab="Prey", ylab="Predator", col=4)
points(500,50,pch="*",col=4)

######################################################################
### Numerical ODE (ordinary differential equation)
######################################################################

Mymod1 = function(time, state, pars){
    with(as.list(c(state, pars)), {
        dPrey1 = Prey1*(1 - Prey1) - Prey2*a1*Prey1/(1 + b1*Prey1)
        dPrey2 = Prey2*a1*Prey1/(1 + b1*Prey1) - Predator*a2*Prey2/(1 + b2*Prey2) - 
            Prey2*d1
        dPredator = Predator*a2*Prey2/(1 + b2*Prey2) - Predator*d2
        return(list(c(dPrey1, dPrey2, dPredator)))
    })
}

pars = c(a1 = 5, b1 = 3.3, a2 = 0.1, b2 = 2, d1 = 0.4, d2 = 0.01)
yini = c(Prey1 = 0.5, Prey2 = 0.15, Predator = 5)
times = seq(0, 500, by = 1)
out1 = as.data.frame(ode(func = Mymod1, y = yini, parms = pars, times = times))

## plot several time series together
matplot(out1[,1], out1[,2:4], type="l", xlab="Time", ylab="Abundance",
        main = "Lotka-Volterra", lwd = 2, col=c(1,2,3), lty=1:3)
legend("topright", c("x", "y", "z"), col = c(1,2,3), lty = 1:3,
       bg="transparent", cex=0.7)

## phase plot (3D plot)
library(rgl)
plot3d(out1[,2], out1[,3], out1[,4], col=1:3, type="l", xlab="x", 
       ylab="y", zlab="z")
rgl.snapshot(filename="ODE.png", fmt="png")
rgl.postscript("ODE.pdf", fmt="pdf")


