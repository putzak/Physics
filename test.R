rm(list = ls()); ##Clear Memory
library(deSolve)
library(graphics)

# Product
pd <- function(x, y) {
  pdct <- x*y
  return(pdct)
}

# Integrate
# dim(numsol1)
# matplot(numsol1[,1],numsol1[,2:3],type="l")
# matplot(numsol1[,1],numsol1[,2:3],type="l",xlab="time t",ylab="u(t),v(t)")
int <- function(t,x,parameters) {
  a = parameters[1];
  b = parameters[2];
  p = parameters[3];
  d = parameters[4];
  odeVec = rep(0,2);
  odeVec[1] = -x[1] + a/(1+ x[2]^b);
  odeVec[2] = -x[2] + p/(1+x[1]^d);
  return(list(odeVec))
}
numsol1 <- lsoda(y=c(0.2,0.1),times=seq(0,50,by=0.2),func=int,parms=c(3,2,3,2))