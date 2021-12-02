.SlopeEquivalentInitialSpreadIndex.Fuel <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  # # d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
  # #        "S1", "S2", "S3", "O1A", "O1B")
  # # a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190, 
  # #        250)
  # # b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0, 
  # #        0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
  # # c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 
  # #         3.2, 1.4, 1.7)
  # # names(a) <- names(b) <- names(c0) <- d
  # # stopifnot(this$name %in% d)
  # # # print(this)
  # # if (is.na(this$name))
  # # {
  # #   print(this)
  # # }
  # # if (!(this$a == a[this$name]))
  # # {
  # #   print(this$name)
  # # }
  # # stopifnot(this$a == a[this$name])
  # # stopifnot(this$b == b[this$name])
  # # stopifnot(this$c0 == c0[this$name])
  # FUELTYPE <- this$name
  # NoBUI <- -1
  # #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  # SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  # #ISI with 0 wind on level grounds
  # ISZ <- InitialSpreadIndex(FFMC, 0)
  # #Surface spread rate with 0 wind on level ground
  # RSZ <- RateOfSpread(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  # RSF <- RSZ * SF
  # #setup some reference vectors
  # d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
  #        "S1", "S2", "S3", "O1A", "O1B")
  # a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190,
  #        250)
  # b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0,
  #        0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
  # c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7,
  #         3.2, 1.4, 1.7)
  # names(a) <- names(b) <- names(c0) <- d
  # 
  # #initialize some local vars
  # RSZ <- -99
  # RSF_C2 <- -99
  # RSF_D1 <- -99
  # RSF_M3 <- -99
  # RSF_M4 <- -99
  # CF <- -99
  # ISF <- -99
  # ISF_C2 <- -99
  # ISF_D1 <- -99
  # ISF_M3 <- -99
  # ISF_M4 <- -99
  # 
  # #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalend ISI
  # ISF <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1",
  #                               "S1", "S2", "S3"),
  #               ifelse((1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) >= 0.01,
  #                      log(1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
  #                      log(0.01)/(-b[FUELTYPE])),
  #               ISF)
  # 
  # #When calculating the M1/M2 types, we are going to calculate for both C2
  # # and D1 types, and combine
  # #Surface spread rate with 0 wind on level ground
  # RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
  #               RateOfSpread("C2", ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, 
  #                        CC, CBH),
  #               RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
  # RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
  # RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
  #               RateOfSpread("D1", ISZ, BUI = NoBUI, FMC, SFC, PC, 
  #                        PDF, CC, CBH),RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  # RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
  # RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
  #                  log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]), 
  #                  ISF_C2)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["C2"]]),
  #                  ISF_C2)
  # RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
  #                  log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
  # ISF <- ifelse(FUELTYPE %in% c("M1", "M2"), PC / 100 * ISF_C2 + 
  #                 (1 - PC / 100) * ISF_D1, 
  #               ISF)
  # 
  # #Set % Dead Balsam Fir to 100%
  # PDF100 <- 100
  # #Surface spread rate with 0 wind on level ground
  # RSZ <- ifelse(FUELTYPE %in% c("M3"), 
  #               RateOfSpread("M3", ISI = ISZ, BUI = NoBUI, FMC, SFC, 
  #                        PC, PDF100, CC, CBH), 
  #               RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  # RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
  # #Surface spread rate with 0 wind on level ground, using D1
  # RSZ <- ifelse(FUELTYPE %in% c("M3"), 
  #               RateOfSpread("D1", ISZ, BUI = NoBUI, FMC, SFC, PC, 
  #                        PDF100, CC, CBH), 
  #               RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  # RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
  # RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
  #                  log(1 - (RSF_M3/a[["M3"]])**(1/c0[["M3"]]))/(-b[["M3"]]),ISF_M3)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["M3"]]),
  #                  ISF_M3)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  # RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
  #                  log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  # ISF <- ifelse(FUELTYPE %in% c("M3"), 
  #               PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1, 
  #               ISF)
  # #Surface spread rate with 0 wind on level ground, using M4
  # RSZ <- ifelse(FUELTYPE %in% c("M4"), 
  #               RateOfSpread("M4", ISI = ISZ, BUI = NoBUI, FMC, SFC, 
  #                        PC, PDF100, CC, CBH), 
  #               RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
  # RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
  # #Surface spread rate with 0 wind on level ground, using M4
  # RSZ <- ifelse(FUELTYPE %in% c("M4"), 
  #               RateOfSpread("D1", ISZ, BUI = NoBUI, FMC, SFC, PC, 
  #                        PDF100, CC, CBH), 
  #               RSZ)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  # RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF,RSF_D1)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  # RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
  #                  log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
  #                  ISF_M4)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["M4"]]),
  #                  ISF_M4)
  # #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  # RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
  #                  log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  # ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
  #                  log(0.01) / (-b[["D1"]]),
  #                  ISF_D1)
  # #Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
  # ISF <- ifelse(FUELTYPE %in% c("M4"), PDF / 100 * ISF_M4 + (1 - PDF / 100.) * 
  #                 ISF_D1, 
  #               ISF)
  # #Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
  # CF <- ifelse(FUELTYPE %in% c("O1A", "O1B"), 
  #              ifelse(CC < 58.8, 0.005 * (exp(0.061 * CC) - 1), 
  #                     0.176 + 0.02 * (CC-58.8)),
  #              CF)
  # #Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
  # ISF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
  #               ifelse((1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
  #                      log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) / 
  #                        (-b[FUELTYPE]),
  #                      log(0.01) / (-b[FUELTYPE])),
  #               ISF)
  # SFold <- SF
  # ISZold <- ISZ
  # RSZold <- RSZ
  # RSFold <- RSF
  # ISFold <- ISF
  # SF <- NA
  # ISZ <- NA
  # RSZ <- NA
  # RSF <- NA
  # ISF <- NA
  ############################################
  #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  stopifnot(SF >= 0)
  #ISI with 0 wind on level grounds
  ISZ <- InitialSpreadIndex(FFMC, 0)
  stopifnot(ISZ >= 0)
  #Surface spread rate with 0 wind on level ground
  RSZ <- .RateOfSpread(this, ISZ, BUI=-1, FMC, SFC, PC, PDF, CC, CBH)
  if (!is.na(RSZ) && RSZ < 0)
  {
    print(this)
    print(RSZ)
    stopifnot(RSZ >= 0)
  }
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  if (!is.na(RSF) && RSF < 0)
  {
    print(this)
    print(RSF)
    stopifnot(RSF >= 0)
  }
  #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF <- log(max(0.01, 1 - (RSF / this$a)**(1 / this$c0))) / (-this$b)
  if (!is.na(ISF) && ISF < 0)
  {
    print(this)
    print(ISF)
    stopifnot(ISF >= 0)
  }
  # s <- 1 - (RSF / this$a)**(1 / this$c0)
  # ISF <- ifelse(s >= 0.01, log(s), log(0.01)) / (-this$b)
  # eq <- function(a, b)
  # {
  #   return (a==b || (is.na(a) && is.na(b)))
  # }
  # if (!(eq(SF, SFold) && eq(ISZ, ISZold)
  #       && eq(RSZ, RSZold)
  #       && eq(RSF, RSFold) && eq(ISF, ISFold)))
  # {
  #   print(this$name)
  #   print(c(SF, ISZ, RSZ, RSF, ISF))
  #   print(c(SFold, ISZold, RSZold, RSFold, ISFold))
  # }
  # stopifnot(eq(SF, SFold) && eq(ISZ, ISZold)
  #           && eq(RSZ, RSZold)
  #           && eq(RSF, RSFold) && eq(ISF, ISFold))
  if (!is.na(ISF) && ISF < 0)
  {
    print(this)
    print(ISF)
    stopifnot(ISF >= 0)
  }
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
  if (!is.na(ISF) && ISF < 0)
  {
    print(this)
    print(ISF)
    stopifnot(ISF >= 0)
  }
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
  ISF <- log(max(0.01, (1 - (RSF / (CF * this$a))**(1 / this$c0)))) / (-this$b)
  if (!is.na(ISF) && ISF < 0)
  {
    print(this)
    print(ISF)
    stopifnot(ISF >= 0)
  }
  return(ISF)
}
