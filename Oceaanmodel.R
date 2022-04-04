library(ggplot2)

# Oceaanmodel opwarming door stralingsforcering
# Variabelen
S <- 0.8
dI <- 1.1
C_s <- 3.84^8
tau <- S*C_s

# Functie 10.22
dT_s <- function(S, dI, t){
  S*dI*(1-exp(-t/tau))
}

# Functie 10.25 !! Waarde in boek klopt niet !!
# 1850-1950
ddI <- 0.4/100
t <- 1850
t_1 <- 2010

integrand <- function(t){
  ddI*(1-exp((-t_1-t)/tau))
}

int <- integrate(integrand, lower=1850, upper=1950)

dT_s1 <- function(S){
  S*int[[1]]
}

intdT_s1 <- dT_s1(0.8)

# 1950-2010
ddI <- 1.1/60
t2 <- 1950
t_2 <- 2010

integrand2 <- function(t2){
  ddI*(1-exp((-t_2-t2)/tau))
}

int2 <- integrate(integrand2, lower=1950, upper=2010)

dT_s2 <- function(S){
  S*int2[[1]]
}

intdT_s2 <- dT_s2(0.8)
intT_stot <- (intdT_s1 + intdT_s2)

# Plot
x_axis <- seq(1,200000, by=1000)
y_axis <- dT_s(0.8,1.1,x_axis)

x_axis1 <- c(1850,1950,2010)
y_axis1 <- c(0,intdT_s1,intT_stot)

df <- data.frame(
  t = x_axis,
  temp = y_axis
)

df1 <- data.frame(
  Jaar <- x_axis1,
  Temperatuur <- y_axis1
)

ggplot(df, aes(t/1e4,temp)) +
  xlab("*10^4 Time (s)") +
  ylab("Temp (K)") +
  geom_line(color = 'red') +
  ggtitle("Warming ocean") +
  theme (
    plot.title = element_text(color = 'blue', size = 14)
  )

ggplot(df1, aes(Jaar,Temperatuur)) +
  xlab("Jaar") +
  ylab("Temp (K)") +
  geom_line(color = 'red') +
  ggtitle("Warming ocean") +
  theme (
    plot.title = element_text(color = 'blue', size = 14)
  )