.SlopeEquivalentInitialSpreadIndex.Fuel <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  #ISI with 0 wind on level grounds
  ISZ <- InitialSpreadIndex(FFMC, 0)
  #Surface spread rate with 0 wind on level ground
  RSZ <- .RateOfSpread(this, ISZ, BUI=-1, FMC, SFC, PC, PDF, CC, CBH)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF <- log(max(0.01, 1 - (RSF / this[["a"]])**(1 / this[["c0"]]))) / (-this[["b"]])
  # s <- 1 - (RSF / this[["a"]])**(1 / this[["c0"]])
  # ISF <- ifelse(s >= 0.01, log(s), log(0.01)) / (-this[["b"]])
  return(ISF)
}
.SlopeEquivalentInitialSpreadIndex..FuelMixedwood <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  #When calculating the M1/M2 types, we are going to calculate for both C2
  # and D1 types, and combine
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- .SlopeEquivalentInitialSpreadIndex(.C2, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
  ISF_D1 <- .SlopeEquivalentInitialSpreadIndex(.D1, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
  #Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
  ISF <- PC / 100 * ISF_C2 + (1 - PC / 100) * ISF_D1
  return(ISF)
}
.SlopeEquivalentInitialSpreadIndex..FuelGrass <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  #ISI with 0 wind on level grounds
  ISZ <- InitialSpreadIndex(FFMC, 0)
  #Surface spread rate with 0 wind on level ground
  RSZ <- .RateOfSpread(this, ISZ, BUI=-1, FMC, SFC, PC, PDF, CC, CBH)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  #Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
  CF <- ifelse(CC < 58.8,
               0.005 * (exp(0.061 * CC) - 1),
               0.176 + 0.02 * (CC-58.8))
  #Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
  ISF <- log(max(0.01, (1 - (RSF / (CF * this[["a"]]))**(1 / this[["c0"]])))) / (-this[["b"]])
}
