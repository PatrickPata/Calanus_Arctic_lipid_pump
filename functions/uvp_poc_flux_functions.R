# Functions for POC Flux calculation from UVP particle profiles
# P. Pata
# November 21, 2025

# # Functions for calculating and modeling the POC flux at each depth
# The PSD is based on the LPM (large particulate material) concentrations (# L-1) by size classes.
#   - In the calculatePocFlux function: the psd and binMiddle inputs are arrays for each depth bin. The zeros are not removed since these are summed anyway.
#   - In the calculateModelledPsd: observedVol is a single value, and observedN/binWidth/binMiddle are arrays
#   
#   alpha: particle dominance (smaller value, larger particles dominate)
#   lambda: the particle size (mm) where power law is truncated

# Function modified from Rufas_etal_2024_GRL/code/processPocFluxFromUVP.m
# The value returned has units of:  mg C m-2 d-1
calculatePOCFlux <- function(psd, binMiddle) {
  # Input:
  #   psd, # part L-1
  #   binMiddle, mm
  # 
  # Output:
  #   pocFlux, mg C m-2 d-1
  
  particleRadius <- (binMiddle / ((4/3) * pi))^(1/3)
  particleEsd <- particleRadius * 2
  
  # Flux factor function from Eq. 5 in Bisson et al. (2022)
  # The parameter values are based on the Cael et al. (2021) size-sinking speed scaling relationship.
  fluxFactor <- function(d) 2.8 * d^2.24
  
  pocFlux <- sum(psd * fluxFactor(particleEsd), na.rm = TRUE)
  
  return(pocFlux)
}

# For the truncated power law method in Bisson et al. (2022)
# NOTE observedN is the concentration * volume sampled

# library(stats)
calculateModelledPsd <- function(observedN, observedVol, binWidth, binMiddle) {
  # Modelled particle number (# part./um), Eq. 1 in Bisson et al. (2022)
  modelledN <- function(x, d) {
    x[1] * d^-x[2] * exp(-d / x[3])
  }
  
  # log10 transform observedN and divide by bin width
  observedN_log10 <- log10(observedN / binWidth)
  observedN_log10 <- unlist(observedN_log10)
  
  # Only consider populated size classes and remove the ones that are not
  bad <- which(is.infinite(observedN_log10))
  observedN_log10_good <- observedN_log10[-bad]
  binMiddle_good <- binMiddle[-bad]
  
  # Initial guess and bounds for model parameters
  # PP: Originally, x0 is c(40, 4, 0.1) but I changed it to c(10, 4, 0.1) to remove the error "Error in optim(par = x0, fn = function(x) { :  L-BFGS-B needs finite values of 'fn'"
  #       C1    alpha lambda
  x0 <- c(10, 4, 0.1) # # part L-1 / unitless / mm
  lower <- c(-Inf, 0, min(binMiddle_good))
  upper <- c(Inf, 6, max(binMiddle_good))
  
  # PP: In the original code, this is the fminsearchbnd() Matlab function.
  # Optimise model parameters using optim with box constraints (L-BFGS-B)
  opt <- stats::optim(
    par = x0,
    fn = function(x) {
      sum((observedN_log10_good - log10(modelledN(x, binMiddle_good)))^2)
    },
    method = "L-BFGS-B", 
    lower = lower,
    upper = upper
  )
  
  C1 <- opt$par[1]
  alpha <- opt$par[2]
  lambda <- opt$par[3]
  
  estimNbl <- (modelledN(opt$par, binMiddle) / observedVol) * binWidth
  
  # Remove particle size classes that are not in the observed set
  estimNbl[bad] <- NA
  
  list(estimNbl = estimNbl, C1 = C1, alpha = alpha, lambda = lambda)
}

generateRandomSamplesOfParticleNumber <- function(observedNbl, observedVol, 
                                                  nIterations = 100, 
                                                  N_sizebins = 26) {
  # Assume observedNbl follows a negative binomial distribution and draw nIterations 
  # samples from it to obtain simulatedN
  
  simulatedN <- matrix(NA, nrow = nIterations, ncol = N_sizebins)
  simulatedNbl <- matrix(NA, nrow = nIterations, ncol = N_sizebins)
  
  for (iSc in 1:N_sizebins) {
    if (observedNbl[iSc] > 0) {
      size_param <- as.numeric(observedNbl[iSc] + 1/2)
      prob_param <- 1 / (observedVol + 1)
      simulatedN[, iSc] <- rnbinom(nIterations, size = size_param, prob = prob_param)
      simulatedNbl[, iSc] <- simulatedN[, iSc] / observedVol
    }
  }
  
  return(simulatedNbl)
}
