library(data.table)
library(testthat)
PATH <- '../nrcan-cfs-fire/cffdrs.core/tests/data/'
DESIRED_ROWS <- 5000

DAY <- seq(0, 366)
PERCENT <- seq(0, 100)
RADIANS <- seq(-360, 360, by=0.1) * pi/180
ZERO_OR_ONE <- list(0, 1)

ACCEL <- ZERO_OR_ONE
ASPECT <- RADIANS
BOOL <- c(TRUE, FALSE)
BUI <- seq(0, 1000, by=0.1)
BUIEFF <- ZERO_OR_ONE
CBH <- seq(0, 200, by=0.1)
CC <- PERCENT
CFB <- seq(-1, 2, by=0.01)
CFL <- seq(-10, 4000, by=0.1)
D0 <- DAY
DC <- seq(0, 1000, by=0.1)
DMC <- seq(0, 1000, by=0.1)
DJ <- DAY
ELV <- seq(0, 10000)
FC <- seq(-10, 20000)
FFMC <- seq(0, 101, by=0.1)
FMC <- seq(0, 500, by=0.1)
FUELTYPE=c("NF", "WA", "C1", "C2", "C3", "C4", "C5", "C6", "C7",
           "D1", "M1", "M2", "M3", "M4", "S1", "S2",
           "S3", "O1A", "O1B")
GFL <- seq(0, 100)
GS <- seq(0, 200)
HR <- seq(0, 366 * 24) * 60
ISI <- seq(0, 300, by=0.1)
LAT <- seq(-90, 90, by=0.1)
LB <- seq(-1, 1.1, by=0.01)
# FIX: for some reason the original package just makes negatives positive
LONG <- abs(seq(-180, 360, by=0.1))
MON <- seq(1, 12)
PC <- PERCENT
PDF <- PERCENT
PREC <- seq(-10, 300, by=0.01)
RH <- seq(-10, 110, by=0.01)
ROS <- seq(0, 600, by=0.01)
SAZ <- RADIANS + pi
SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
SD <- append(list(-999), seq(0, 100))
SFC <- seq(0, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by=0.1)
WD <- RADIANS
WS <- seq(0, 300, by=0.1)
THETA <- seq(-360, 360, by=0.01)
WSV <- seq(-10, 500, by=0.1)
WAZ <- RADIANS + pi
WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)

pickRows <- function(d1, num_rows=DESIRED_ROWS)
{
  d1 <- data.table(d1)
  #print(d1)
  #print(nrow(d1))
  #print(MAX_ROWS)
  old_names <- colnames(d1)
  while (nrow(d1) > num_rows)
  {
    #print('loop')
    #print(seq(1, nrow(d1), by=3))
    #print(d1[seq(1, nrow(d1), by=3), ])
    #print('assign')
    d1 <- data.table(d1[seq(1, nrow(d1), by=3), ])
    #print('end loop')
    #print(nrow(d1))
    stopifnot(!is.null(nrow(d1)))
    colnames(d1) <- old_names
  }
  #print('return')
  return(d1)
}

makeInput <- function(arguments)
{
  #print(arguments)
  d1 <- pickRows(arguments[[1]])
  if (1 < length(arguments))
  {
    for (n in 2:length(arguments))
    {
      #print(n)
      #print(arguments[[n]])
      d2 <- pickRows(arguments[[n]], ceiling(3 * DESIRED_ROWS / nrow(d1)))
      d1 <- pickRows(merge(data.frame(d1), data.frame(d2), by=NULL))
    }
  }
  return(d1)
}

makeData <- function(name, fct, arguments)
{
  i <- makeInput(arguments)
  #i[, c(name)] <- do.call(fct, i)
  r <- list(do.call(fct, i[1, ]))
  isRow <- length(r[[1]]) > 1
  if (isRow)
  {
    r <- r[[1]]
    for (n in 2:nrow(i))
    {
      r2 <- do.call(fct, i[n, ])
      r <- rbind(r, r2)
    }
    return(r)
  }
  else
  {
    for (n in 2:nrow(i))
    {
      r <- append(r, do.call(fct, i[n, ]))
    }
    i[, c(name)] <- unlist(r)
    return(i)
  }
}

saveResults <- function(name, data)
{
  write.csv(data, paste0(PATH, name, '.csv'), row.names=FALSE)
}

saveData <- function(name, fct, arguments)
{
  saveResults(name, makeData(name, fct, arguments))
  print(paste0("Checking ", name))
  checkData(name, fct, arguments)
}

checkResults <- function(name, df1)
{
  df1 <- data.table(df1)
  df2 <- data.table(read.csv(paste0(PATH, name, '.csv')))
  expect_equal(colnames(df1), colnames(df2))
  for (n in sort(colnames(df1)))
  {
    test_that(paste0(name, '$', n), {
      actual <- unlist(df1[[n]])
      expected <- unlist(df2[[n]])
      expect_equal(actual, expected)
    })
  }
}

checkData <- function(name, fct, arguments)
{
  df1 <- makeData(name, fct, arguments)
  df2 <- data.table(read.csv(paste0(PATH, name, '.csv')))
  #print(df1[[name]])
  #print(as.numeric(df1[[name]]))
  #print(df2[[name]])
  #print(as.numeric(df2[[name]]))
  #expect_equal(as.numeric(df1[[name]]), as.numeric(df2[[name]]))
  actual <- df1[[name]]
  expected <- df2[[name]]
  expect_equal(actual, expected)
}

saveData('BackRateOfSpread',
         cffdrs:::.BROScalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(FFMC=FFMC),
              data.table(BUI=BUI),
              data.table(WSV=WSV),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(PC=PC),
              data.table(PDF=PDF),
              data.table(CC=CC),
              data.table(CBH=CBH)))
saveData('BuildupEffect',
         cffdrs:::.BEcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(BUI=BUI)))
saveData('BuildupIndex',
         cffdrs:::.buiCalc,
         list(data.table(dmc=DMC),
              data.table(dc=DC)))
saveData('CriticalSurfaceIntensity',
         cffdrs:::.CFBcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(ROS=ROS),
              data.table(CBH=CBH),
              data.table(option=c("CSI"))))
saveData('CriticalSurfaceRateOfSpread',
         cffdrs:::.CFBcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(ROS=ROS),
              data.table(CBH=CBH),
              data.table(option=c("RSO"))))
saveData('CrownFractionBurned',
         cffdrs:::.CFBcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(ROS=ROS),
              data.table(CBH=CBH)))
saveData('CrownFuelConsumption',
         cffdrs:::.TFCcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(CFL=CFL),
              data.table(CFB=CFB),
              data.table(SFC=SFC),
              data.table(PC=PC),
              data.table(PDF=PDF),
              data.table(option="CFC")))
saveData('DistanceAtTime',
         cffdrs:::.DISTtcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(ROSeq=ROS),
              data.table(HR=HR),
              data.table(CFB=CFB)))
saveData('DroughtCode',
         cffdrs:::.dcCalc,
         list(data.table(dc_yda=DC),
              data.table(temp=TEMP),
              data.table(rh=RH),
              data.table(prec=PREC),
              data.table(lat=LAT),
              data.table(mon=MON),
              data.table(lat.adjust=BOOL)))
saveData('DuffMoistureCode',
         cffdrs:::.dmcCalc,
         list(data.table(dmc_yda=DMC),
              data.table(temp=TEMP),
              data.table(rh=RH),
              data.table(prec=PREC),
              data.table(lat=LAT),
              data.table(mon=MON),
              data.table(lat.adjust=BOOL)))
test_fbp <- read.csv2('data/test_fbp.csv', sep=';')
test_fbp$FFMC <- as.numeric(test_fbp$FFMC)
test_fbp$hr <- as.numeric(test_fbp$hr)
test_fbp$WS <- as.numeric(test_fbp$WS)
test_fbp$GFL <- as.numeric(test_fbp$GFL)
test_fbp$CBH <- as.numeric(test_fbp$CBH)
test_fbp$CFL <- as.numeric(test_fbp$CFL)
saveResults('FireBehaviourPrediction_test_fbp',
            cffdrs:::.FBPcalc(test_fbp, "A"))
fctFBP <- function(ID, FUELTYPE, FFMC, BUI, WS, WD, FMC, GS, LAT, LONG, ELV, DJ, D0,
                   SD, SH, HR, PC, PDF, GFL, CC, THETA, ACCEL, ASPECT, BUIEFF,
                   CBH, CFL, ISI)
{
  input <- data.frame(ID=ID,
                      FUELTYPE=FUELTYPE,
                      FFMC=FFMC,
                      BUI=BUI,
                      WS=WS,
                      WD=WD,
                      FMC=FMC,
                      GS=GS,
                      LAT=LAT,
                      LONG=LONG,
                      ELV=ELV,
                      DJ=DJ,
                      D0=D0,
                      SD=SD,
                      SH=SH,
                      HR=HR,
                      PC=PC,
                      PDF=PDF,
                      GFL=GFL,
                      CC=CC,
                      THETA=THETA,
                      ACCEL=ACCEL,
                      ASPECT=ASPECT,
                      BUIEFF=BUIEFF,
                      CBH=CBH,
                      CFL=CFL,
                      ISI=ISI)
  return(cffdrs:::.FBPcalc(input=input, output="S"))
}
saveData('FireBehaviourPrediction',
         fctFBP,
         list(data.table(ID=1),
              data.table(FUELTYPE=FUELTYPE),
              data.table(FFMC=FFMC),
              data.table(BUI=BUI),
              data.table(WS=WS),
              data.table(WD=WD),
              data.table(FMC=FMC),
              data.table(GS=GS),
              data.table(LAT=LAT),
              data.table(LONG=LONG),
              data.table(ELV=ELV),
              data.table(DJ=DJ),
              data.table(D0=D0),
              data.table(SD=SD),
              data.table(SH=SH),
              data.table(HR=HR),
              data.table(PC=PC),
              data.table(PDF=PDF),
              data.table(GFL=GFL),
              data.table(CC=CC),
              data.table(THETA=THETA),
              data.table(ACCEL=ACCEL),
              data.table(ASPECT=ASPECT),
              data.table(BUIEFF=BUIEFF),
              data.table(CBH=CBH),
              data.table(CFL=CFL),
              data.table(ISI=ISI)))
saveData('FireIntensity',
         cffdrs:::.FIcalc,
         list(data.table(FC=FC),
              data.table(ROS=ROS)))
saveData('FireWeatherIndex',
         cffdrs:::.fwiCalc,
         list(data.table(isi=ISI),
              data.table(bui=BUI)))
saveData('FineFuelMoistureCode',
         cffdrs:::.ffmcCalc,
         list(data.table(ffmc_yda=FFMC),
              data.table(temp=TEMP),
              data.table(rh=RH),
              data.table(ws=WS),
              data.table(prec=PREC)))
saveData('FlankRateOfSpread',
         cffdrs:::.FROScalc,
         list(data.table(ROS=ROS),
              data.table(BROS=ROS),
              data.table(LB=LB)))
saveData('FoliarMoistureContent',
         cffdrs:::.FMCcalc,
         list(data.table(LAT=LAT),
              data.table(LONG=LONG),
              data.table(ELV=ELV),
              data.table(DJ=DJ),
              data.table(D0=D0)))
saveData('InitialSpreadIndex',
         cffdrs:::.ISIcalc,
         list(data.table(ffmc=FFMC),
              data.table(ws=WS),
              data.table(fbpMod=BOOL)))
saveData('C6IntermediateSurfaceRateOfSpread',
         cffdrs:::.C6calc,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("RSI"))))
saveData('C6CrownRateOfSpread',
         cffdrs:::.C6calc,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("RSC"))))
fctRSSC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
{
  stopifnot("C6" == FUELTYPE)
  RSI <- cffdrs:::.C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  RSS <- RSI * cffdrs:::.BEcalc(FUELTYPE, BUI)
  return(RSS)
}
saveData('C6SurfaceRateOfSpread',
         fctRSSC6,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("RSI"))))
saveData('C6CrownFractionBurned',
         cffdrs:::.C6calc,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("CFB"))))
fctCSIC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
{
  stopifnot("C6" == FUELTYPE)
  stopifnot("RSC" == option)
  CSI <- cffdrs:::.CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, "CSI")
  return(CSI)
}
saveData('C6CriticalSurfaceIntensity',
         fctCSIC6,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("RSC"))))
fctRSOC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
{
  stopifnot("C6" == FUELTYPE)
  stopifnot("RSC" == option)
  RSO <- cffdrs:::.CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, "RSO")
  return(RSO)
}
saveData('C6CriticalSurfaceRateOfSpread',
         fctRSOC6,
         list(data.table(FUELTYPE=c("C6")),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(CBH=CBH),
              data.table(ROS=ROS),
              data.table(CFB=CFB),
              data.table(RSC=ROS),
              data.table(option=c("RSC"))))
saveData('LengthToBreadthRatio',
         cffdrs:::.LBcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(WSV=WSV)))
saveData('LengthToBreadthRatioAtTime',
         cffdrs:::.LBtcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(LB=LB),
              data.table(HR=HR),
              data.table(CFB=CFB)))
saveData('RateOfSpread',
         cffdrs:::.ROScalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(ISI=ISI),
              data.table(BUI=BUI),
              data.table(FMC=FMC),
              data.table(SFC=SFC),
              data.table(PC=PC),
              data.table(PDF=PDF),
              data.table(CC=CC),
              data.table(CBH=CBH)))
saveData('RateOfSpreadAtTheta',
         cffdrs:::.ROSthetacalc,
         list(data.table(ROS=ROS),
              data.table(FROS=ROS),
              data.table(BROS=ROS),
              data.table(THETA=THETA)))
saveData('RateOfSpreadAtTime',
         cffdrs:::.ROStcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(ROSeq=ROS),
              data.table(HR=HR),
              data.table(CFB=CFB)))
saveData('SlopeAdjustRAZ',
         cffdrs:::.Slopecalc,
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
saveData('SlopeAdjustWSV',
         cffdrs:::.Slopecalc,
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
              data.table(output = "WSV")))
fctSlopeISI <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ") {
  # output options include: RAZ and WSV
  
  #check for valid output types
  validOutTypes = c("RAZ", "WAZ", "WSV")
  if(!(output %in% validOutTypes)){
    stop(paste("In 'slopecalc()', '",output, "' is an invalid 'output' type.", 
               sep=""))
  }
  
  NoBUI <- rep(-1,length(FFMC))
  #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  #ISI with 0 wind on level grounds
  ISZ <- cffdrs:::.ISIcalc(FFMC, 0)
  #Surface spread rate with 0 wind on level ground
  RSZ <- cffdrs:::.ROScalc(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  #setup some reference vectors
  d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
         "S1", "S2", "S3", "O1A", "O1B")
  a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190, 
         250)
  b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0, 
         0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
  c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 
          3.2, 1.4, 1.7)
  names(a) <- names(b) <- names(c0) <- d
  
  #initialize some local vars
  RSZ <- rep(-99,length(FFMC))
  RSF_C2 <- rep(-99,length(FFMC))
  RSF_D1 <- rep(-99,length(FFMC))
  RSF_M3 <- rep(-99,length(FFMC))
  RSF_M4 <- rep(-99,length(FFMC))
  CF <- rep(-99,length(FFMC))
  ISF <- rep(-99,length(FFMC))
  ISF_C2 <- rep(-99,length(FFMC))
  ISF_D1 <- rep(-99,length(FFMC))
  ISF_M3 <- rep(-99,length(FFMC))
  ISF_M4 <- rep(-99,length(FFMC))
  
  #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalend ISI
  ISF <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", 
                                "S1", "S2", "S3"),
                ifelse((1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) >= 0.01,
                       log(1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
                       log(0.01)/(-b[FUELTYPE])),
                ISF)
  
  #When calculating the M1/M2 types, we are going to calculate for both C2
  # and D1 types, and combine
  #Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
                cffdrs:::.ROScalc(rep("C2", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, 
                                  CC, CBH),
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
  RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
                cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                                  PDF, CC, CBH),RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
                   log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]), 
                   ISF_C2)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
                   log(0.01) / (-b[["C2"]]),
                   ISF_C2)
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
  ISF <- ifelse(FUELTYPE %in% c("M1", "M2"), PC / 100 * ISF_C2 + 
                  (1 - PC / 100) * ISF_D1, 
                ISF)
  
  #Set % Dead Balsam Fir to 100%
  PDF100 <- rep(100, length(ISI))
  #Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M3"), 
                cffdrs:::.ROScalc(rep("M3", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC, 
                                  PC, PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
  #Surface spread rate with 0 wind on level ground, using D1
  RSZ <- ifelse(FUELTYPE %in% c("M3"), 
                cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                                  PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
                   log(1 - (RSF_M3/a[["M3"]])**(1/c0[["M3"]]))/(-b[["M3"]]),ISF_M3)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
                   log(0.01) / (-b[["M3"]]),
                   ISF_M3)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  ISF <- ifelse(FUELTYPE %in% c("M3"), 
                PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1, 
                ISF)
  #Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"), 
                cffdrs:::.ROScalc(rep("M4", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC, 
                                  PC, PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
  RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
  #Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"), 
                cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                                  PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF,RSF_D1)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
                   log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
                   ISF_M4)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
                   log(0.01) / (-b[["M4"]]),
                   ISF_M4)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
  ISF <- ifelse(FUELTYPE %in% c("M4"), PDF / 100 * ISF_M4 + (1 - PDF / 100.) * 
                  ISF_D1, 
                ISF)
  #Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
  CF <- ifelse(FUELTYPE %in% c("O1A", "O1B"), 
               ifelse(CC < 58.8, 0.005 * (exp(0.061 * CC) - 1), 
                      0.176 + 0.02 * (CC-58.8)),
               CF)
  #Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
  ISF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
                ifelse((1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
                       log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) / 
                         (-b[FUELTYPE]),
                       log(0.01) / (-b[FUELTYPE])),
                ISF)
  return(ISF)
}
saveData('SlopeEquivalentInitialSpreadIndex',
         fctSlopeISI,
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
fctSlopeWSE <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ")
{
  ISF <- fctSlopeISI(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC,
                     PDF, CC, CBH, ISI, output)
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
saveData('SlopeEquivalentWindSpeed',
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
fctSlopeWSX <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ")
{
  WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                     CC, CBH, ISI, output)
  #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  return(WSX)
}
fctSlopeWSY <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output)
{
  WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                     CC, CBH, ISI, output)
  #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  return(WSY)
}
fctSlopeWSV <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output)
{
  WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                     CC, CBH, ISI, output)
  #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  #Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  return(WSV)
}
fctSlopeRAZ <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output)
{
  WSE <- fctSlopeWSE(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                     CC, CBH, ISI, output)
  #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  #Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  #Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
  RAZ <- acos(WSY / WSV)
  #Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
  # directions
  RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
  return(RAZ)
}
saveData('SlopeAdjustWSX',
         fctSlopeWSX,
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
saveData('SlopeAdjustWSY',
         fctSlopeWSY,
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
saveData('SlopeAdjust_WSV',
         fctSlopeWSV,
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
saveData('SlopeAdjust_RAZ',
         fctSlopeRAZ,
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
saveData('SurfaceFuelConsumption',
         cffdrs:::.SFCcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(FFMC=FFMC),
              data.table(BUI=BUI),
              data.table(PC=PC),
              data.table(GFL=GFL)))
saveData('TotalFuelConsumption',
         cffdrs:::.TFCcalc,
         list(data.table(FUELTYPE=FUELTYPE),
              data.table(CFL=CFL),
              data.table(CFB=CFB),
              data.table(SFC=SFC),
              data.table(PC=PC),
              data.table(PDF=PDF),
              data.table(option="TFC")))
