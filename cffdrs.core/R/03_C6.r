.C6 <- structure(.Data=list(name="C6",
                            a=30,
                            b=0.08,
                            c0=3.0,
                            BUIo=62,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0149,
                            sfcC=2.48,
                            CBH=7,
                            CFL=1.8),
                 class=c(".C6", ".FuelClosed", "Fuel", ".FuelBase")
)
.CrownBaseHeight..C6 <- function(this, CBH, SD, SH)
{
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH),
                ifelse(SD > 0 & SH > 0,
                       -11.2 + 1.06 * SH + 0.0017 * SD,
                       this$CBH),
                CBH)
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  return(CBH)
}

.BEcalc <- function(FUELTYPE, BUI) {
  
  #Fuel Type String represenations
  d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3",
         "M4","S1", "S2", "S3", "O1A", "O1B")
  #The average BUI for the fuel type - as referenced by the "d" list above
  BUIo <- c(72, 64, 62, 66, 56, 62, 106, 32, 50, 50, 50, 50, 38, 63, 31, 01, 
            01)
  #Proportion of maximum possible spread rate that is reached at a standard BUI
  Q <- c(0.9, 0.7, 0.75, 0.8, 0.8, 0.8, 0.85, 0.9, 0.8, 0.8, 0.8, 0.8, 0.75, 
         0.75, 0.75, 1.0, 1.0)
  names(BUIo) <- names(Q)<-d
  
  #Eq. 54 (FCFDG 1992) The Buildup Effect
  BE<- ifelse(BUI > 0 & BUIo[FUELTYPE] > 0,
              exp(50 * log(Q[FUELTYPE]) * (1 / BUI - 1 / BUIo[FUELTYPE])), 1)
  
  return(as.numeric(BE))
}
.RateOfSpread..C6 <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
  RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
  RSC <- CrownRateOfSpreadC6(ISI, FMC)
  CSI <- .CriticalSurfaceIntensity(this, FMC, CBH)
  #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
  RSO <- CSI / (300 * SFC)
  CFB <- ifelse(RSC > RSS, .CrownFractionBurned(this, RSS, RSO), 0)
  ROS <- RateOfSpreadC6(RSC, RSS, CFB)
  ROS <- ifelse(ROS <= 0, 0.000001, ROS)
  return(ROS)
  return(ROS)
}
.FireBehaviourPrediction..C6 <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA)
{
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  SFC <- TFC <- HFI <- CFB <- ROS <- 0
  RAZ <- -999
  if (output == "SECONDARY" | output == "ALL" | output == "S" |
      output == "A") {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <-
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- 0
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- -999
  }
  CBH <- .CrownBaseHeight(this, CBH, SD, SH)
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), this$CFL, CFL)
  FMC <- ifelse(FMC <= 0 | FMC > 120 | is.na(FMC),
                .FoliarMoistureContent(this, LAT, LONG, ELV, DJ, D0),
                FMC)
  ############################################################################
  #                         END
  ############################################################################
  #Calculate Surface fuel consumption (SFC)
  SFC <- .SurfaceFuelConsumption(this, FFMC, BUI, PC, GFL)
  #Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  SLOPE_ADJUST <- .SlopeAdjust(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
  #Calculate the net effective windspeed (WSV)
  # WSV0 <- SLOPE_ADJUST[["WSV"]]
  WSV0 <- SLOPE_ADJUST$WSV
  WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
  #Calculate the net effective wind direction (RAZ)
  # RAZ0 <- SLOPE_ADJUST[["RAZ"]]
  RAZ0 <- SLOPE_ADJUST$RAZ
  RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
  #Calculate or keep Initial Spread Index (ISI)
  ISI <- ifelse(ISI > 0, ISI, InitialSpreadIndex(FFMC, WSV, TRUE))
  # Calculate the Rate of Spread (ROS) and Crown Fraction Burned (CFB)
  # C6 has different calculations
  # ROS <- .RateOfSpread(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  # HACK: use ifelse for now to keep old behaviour
  RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
  RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
  RSC <- CrownRateOfSpreadC6(ISI, FMC)
  CSI <- .CriticalSurfaceIntensity(this, FMC, CBH)
  #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
  RSO <- CSI / (300 * SFC)
  CFB <- ifelse(RSC > RSS, .CrownFractionBurned(this, RSS, RSO), 0)
  ROS <- RateOfSpreadC6(RSC, RSS, CFB)
  #Calculate Total Fuel Consumption (TFC)
  TFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, CFB, PC, PDF), SFC)
  #Calculate Head Fire Intensity(HFI)
  HFI <- FireIntensity(TFC, ROS)
  #Adjust Crown Fraction Burned
  CFB <- ifelse(HR < 0, -CFB, CFB)
  #Adjust RAZ
  RAZ <- RAZ * 180/pi
  RAZ <- ifelse(RAZ == 360, 0, RAZ)
  #Calculate Fire Type (S = Surface, C = Crowning, I = Intermittent Crowning)
  FD <- ifelse(CFB < 0.1, "S", ifelse(CFB >= 0.9, "C", "I"))
  #Calculate Crown Fuel Consumption(CFC)
  CFC <- .CrownFuelConsumption(this, CFL, CFB, PC, PDF)
  #Calculate the Secondary Outputs
  if (output == "SECONDARY" | output == "ALL" | output == "S" |
      output == "A") {
    #Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
    SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS/100)^1.2))
    CSI <- .CriticalSurfaceIntensity(this, FMC, CBH)
    #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
    RSO <- CSI / (300 * SFC)
    #Calculate The Buildup Effect
    BE <- .BuildupEffect(this, BUI)
    #Calculate length to breadth ratio
    LB <- .LengthToBreadthRatio(this, WSV)
    LBt <- ifelse(ACCEL == 0, LB, .LengthToBreadthRatioAtTime(this, LB, HR, CFB))
    #Calculate Back fire rate of spread (BROS)
    BROS <- .BackRateOfSpread(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
    #Calculate Flank fire rate of spread (FROS)
    FROS <- FlankRateOfSpread(ROS, BROS, LB)
    #Calculate the eccentricity
    E <- sqrt(1 - 1/LB/LB)
    #Calculate the rate of spread towards angle theta (TROS)
    TROS <- ROS * (1 - E)/(1 - E * cos(THETA - RAZ))
    #Calculate rate of spread at time t for Flank, Back of fire and at angle
    #  theta.
    ROSt <- ifelse(ACCEL == 0, ROS, .RateOfSpreadAtTime(this, ROS, HR, CFB))
    BROSt <- ifelse(ACCEL == 0, BROS, .RateOfSpreadAtTime(this, BROS, HR, CFB))
    FROSt <- ifelse(ACCEL == 0, FROS, FlankRateOfSpread(ROSt, BROSt, LBt))
    #Calculate rate of spread towards angle theta at time t (TROSt)
    TROSt <- ifelse(ACCEL == 0, TROS,
                    ROSt * (1 - sqrt(1 - 1 / LBt / LBt)) /
                      (1 - sqrt(1 - 1 / LBt / LBt) * cos(THETA - RAZ)))
    # FIX: C6 is always 0
    #Calculate Crown Fraction Burned for Flank, Back of fire and at angle theta.
    FCFB <- 0
    BCFB <- 0
    TCFB <- 0
    # if (CFL != 0 && !(this$name == "C6"))
    # {
    #   FCFB <- CrownFractionBurned(FROS, RSO)
    #   BCFB <- CrownFractionBurned(BROS, RSO)
    #   TCFB <- CrownFractionBurned(TROS, RSO)
    # }
    #Calculate Total fuel consumption for the Flank fire, Back fire and at
    #  angle theta
    FTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, FCFB, PC, PDF), SFC)
    BTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, BCFB, PC, PDF), SFC)
    TTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, TCFB, PC, PDF), SFC)
    #Calculate the Fire Intensity at the Flank, Back and at angle theta fire
    FFI <- FireIntensity(FTFC, FROS)
    BFI <- FireIntensity(BTFC, BROS)
    TFI <- FireIntensity(TTFC, TROS)
    #Calculate Rate of spread at time t for the Head, Flank, Back of fire and
    #  at angle theta.
    HROSt <- ifelse(HR < 0, -ROSt, ROSt)
    FROSt <- ifelse(HR < 0, -FROSt, FROSt)
    BROSt <- ifelse(HR < 0, -BROSt, BROSt)
    TROSt <- ifelse(HR < 0, -TROSt, TROSt)

    #Calculate the elapsed time to crown fire initiation for Head, Flank, Back
    # fire and at angle theta. The (a# variable is a constant for Head, Flank,
    # Back and at angle theta used in the *TI equations)
    # NOTE: old version used non-constant equation for every FUELTYPE
    TI <- log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha(this, CFB))
    FTI <- log(ifelse(1 - RSO/FROS > 0, 1 - RSO/FROS, 1))/(-.Alpha(this, FCFB))
    BTI <- log(ifelse(1 - RSO/BROS > 0, 1 - RSO/BROS, 1))/(-.Alpha(this, BCFB))
    TTI <- log(ifelse(1 - RSO/TROS > 0, 1 - RSO/TROS, 1))/(-.Alpha(this, TCFB))

    #Fire spread distance for Head, Back, and Flank of fire
    DH <- ifelse(ACCEL == 1, .DistanceAtTime(this, ROS, HR, CFB), ROS * HR)
    DB <- ifelse(ACCEL == 1, .DistanceAtTime(this, BROS, HR, CFB), BROS * HR)
    DF <- ifelse(ACCEL == 1, (DH + DB)/(LBt * 2), (DH + DB)/(LB * 2))
  }
  #if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  #if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  if (output == "PRIMARY" | output == "P") {
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD=as.character(FD), HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC,
                TFC=SFC)
  }
  #If Secondary is selected, wrap the secondary outputs into a data frame
  #  and return them.
  else if (output == "SECONDARY" | output == "S") {
    FBP <- list(ID=ID, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO,
                CSI=CSI, FROS=FROS, BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB,
                FFI=FFI, BFI=BFI, FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV,
                DH=DH, DB=DB, DF=DF, TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  #If all outputs are selected, then wrap all outputs into a data frame and
  #  return it.
  else if (output == "ALL" | output == "A") {
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD=as.character(FD), HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC,
                TFC=TFC, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO, CSI=CSI, FROS=FROS,
                BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB, FFI=FFI, BFI=BFI,
                FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV, DH=DH, DB=DB, DF=DF,
                TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  return(list(FBP))
}
