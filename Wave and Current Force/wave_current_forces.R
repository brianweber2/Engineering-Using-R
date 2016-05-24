## This script calculates wave and curruent forces on the Tetra Arapaho Barge

### Define the input parameters ###

# Barge Inputs
bargeLength <- 106.68 # Barge length in meters
bargeDraft <- 4.88 # Barge draft in meters
subALong <- 496.47 # Barge submerged area in the longitudinal direction
Cd <- 1.9 # drag coefficient per DNV-RP-C205
Ca <- 1 # added mass coefficient per DNV-RP-C205

# Environmental Inputs
dmax <- 24.39 # maximum water depth in meters
rhoSW <- 1025 # density of seawater in kg/m^3
Hs <- 3.05 # significant wave height in meters
Tp <- 9.8 # peak wave period in seconds
Uc_all <- c(0.5144,0.7717, 1.0289) # current velocity cases at SWL in m/s
g <- 9.81 # acceleration due to gravity in m/s^2


### Wave Calculation ###
c <- 1.33 * sqrt(g*dmax) # wave celerity in m/s
lambda <- c * Tp # wave length in meters
k <- (2*pi)/lambda # wave number in 1/m

# Horizontal particle velocity for graph
# Uw_z <- ((pi*Hs)/Tp)*exp(k*z) + 0.75*((pi*Hs)/Tp)*((pi*Hs)/lambda)*(cosh(2*k*(z + dmax))/sinh(k*dmax)^4)
# Horizontal particle acceleration for graph
# Aw_z <- ((2*pi^2 * Hs)/Tp^2)*exp(k*z) + 3*((pi^2*Hs)/Tp^2)*((pi*Hs)/lambda)*(cosh(2*k*(z+dmax))/sinh(k*dmax)^4)

Uw_z_f <- function(z) {
    ((pi*Hs)/Tp)*exp(k*z) + 0.75*((pi*Hs)/Tp)*((pi*Hs)/lambda)*(cosh(2*k*(z + dmax))/sinh(k*dmax)^4)
}

Aw_z_f <- function(z) {
    ((2*pi^2 * Hs)/Tp^2)*exp(k*z) + 3*((pi^2*Hs)/Tp^2)*((pi*Hs)/lambda)*(cosh(2*k*(z+dmax))/sinh(k*dmax)^4)
}

Fw <- function(z) {
    (rhoSW*(1+Ca)*subALong*Aw_z_f(z)) + (rhoSW/2)*(Uw_z_f(z))^2*Cd*bargeLength
}


### Wave and Current Forces in Metric Tons ###
for (i in 1:length(Uc_all)) {
    Fwc_all <- function(z) {
        (rhoSW*(1+Ca)*subALong*Aw_z_f(z)) + (rhoSW/2)*(Uw_z_f(z)+Uc_all[i])^2*Cd*bargeLength
    }
    print(as.numeric(unlist(integrate(Fwc_all, lower = -bargeDraft, upper = 0))[1]) / (1000*9.81))
}


### Wave Forces in Metric Tons ###
Fwave <- as.numeric(unlist(integrate(Fw, lower = -bargeDraft, upper = 0))[1]) / (1000 *9.81) # wave force in Metric Tons
Fwave


