# Radiative forcing
# Load data
CO2dat <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_gl.csv", 
                   sep = ",", dec = ".", comment.char = "#", header = TRUE)
CH4dat <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/ch4/ch4_mm_gl.csv", 
                   sep = ",", dec = ".", comment.char = "#", header = TRUE)
N2Odat <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/n2o/n2o_mm_gl.csv", 
                   sep = ",", dec = ".", comment.char = "#", header = TRUE)

# Reference values 1750
CO2ref <- 278
CH4ref <- 722
N2Oref <- 270

# Actual values
CO2act <- tail(CO2dat$average, n = 1)
CH4act <- tail(CH4dat$average, n = 1)
N2Oact <- tail(N2Odat$average, n = 1)

# Calculation
QCO2 <- 5.35 * log(CO2act / CO2ref)
QCH4 <- 0.036 * log(sqrt(CH4act) - sqrt(CH4ref))
QN2O <- 0.12 * log(sqrt(N2Oact) - sqrt(N2Oref))
QTOT <- sum(QCO2, QCH4, QN2O)

# Plot
barplot(
    c(QCO2, QCH4, QN2O), 
    names.arg = c("CO2", "CH4", "N2O"), 
    col = c("lightyellow", "lightgreen", "lightblue"), 
    ylab = expression("Radiative forcing Q (W m"^-2*")"),
    main = paste("Total Radiative forcing = ", QTOT)
    )