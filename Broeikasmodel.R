# Broeikasmodel H10 Natuurkunde voor milieuwetenschappen
# Variabelen
t_a    <-  0.58
a_s    <-  0.15
a_a    <-  0.23
S      <-  1366
So4    <-  342
t_ainf <-  0.1
c      <-  7.82
sigma  <-  5.67*10**-8
T_a    <-  242
f      <-  32.8
T_sder <- (((1-a_a-a_s*t_a)*So4-sigma*(T_a^4))/(sigma*t_ainf))^(1/4)

# Formules
bk_model <- function(T_sder, f) {
  (t_a-a_s*t_a)*So4-c*(T_sder-T_a-f)-sigma* T_sder^4+sigma*(T_a+f)^4
}

bk_model1 <- function(T_a,T_s,f) {
  (t_a-a_s*t_a)*So4-c*(T_s-T_a-f)-sigma*T_s^4+sigma*(T_a+f)^4
}

bk_model2 <- function(T_s,T_a) {
  So4*(1-a_a-a_s*t_a)-t_ainf*sigma*T_s^4-sigma*T_a^4
}

# Plotting
 
x_axis <- 270:300
y_axis <- bk_model(270:300,32.8)
x_axis1 <- 30:35
y_axis1 <- bk_model(288,30:35)

plot(x_axis, y_axis, type = "l",
     main = "Deviation from validity",
     xlab = "T_s",
     ylab = "Delta",
     col = "red",
     )

plot(x_axis1, y_axis1, type = "l",
     main = "Deviation from validity",
     xlab = "T_s",
     ylab = "Delta",
     col = "red"
)