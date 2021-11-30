test_that("SlopeEquivalentWindSpeed", {
  fctSlopeWSE <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                          CC, CBH, ISI, output = "RAZ")
  {
    ISF <- .SlopeEquivalentInitialSpreadIndex(FUELS[[FUELTYPE]], FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
    if (is.na(ISF) || -99.0 == ISF)
    {
      return(NA)
    }
    #Eq. 46 (FCFDG 1992)
    m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
    #Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
    fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
    #Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
    WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
    #Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(WSE > 40 & ISF < (0.999 * 2.496 * fF),
                  28 - (1 / 0.0818 * log(1 - ISF/ ( 2.496 * fF))),
                  WSE)
    #Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
    WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
    return(WSE)
  }
  # HACK: use extra variables so we generate same rows as SlopeAdjust test
  checkData('SlopeEquivalentWindSpeed',
            fctSlopeWSE,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(FFMC=FFMC),
                 data.table(BUI=BUI),
                 data.table(WS=WS),
                 data.table(WAZ=WAZ),
                 data.table(GS=GS),
                 data.table(SAZ=SAZ),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(PC=PC),
                 data.table(PDF=PDF),
                 data.table(CC=CC),
                 data.table(CBH=CBH),
                 data.table(ISI=ISI),
                 data.table(output = "RAZ")))
})
