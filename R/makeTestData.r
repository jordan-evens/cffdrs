library(data.table)
PATH <- '../nrcan-cfs-fire/cffdrs.core/tests/data/'
DESIRED_ROWS <- 5000

ACCEL <- list(0, 1)
ASPECT <- seq(-370, 370, by=0.1)
BOOL <- c(TRUE, FALSE)
BUI <- seq(-10, 1000, by=0.1)
BUIEFF <- list(0, 1)
CBH <- seq(-10, 200, by=0.1)
CC <- seq(-10, 110)
CFB <- seq(-1, 2, by=0.01)
CFL <- seq(-10, 4000, by=0.1)
D0 <- seq(-10, 370)
DC <- seq(-10, 1000, by=0.1)
DMC <- seq(-10, 1000, by=0.1)
DJ <- seq(-10, 370)
ELV <- seq(-100, 4000)
FC <- seq(-10, 20000)
FFMC <- seq(-10, 105, by=0.1)
FMC <- seq(-10, 500, by=0.1)
FUELTYPE=c("NF", "WA", "C1", "C2", "C3", "C4", "C5", "C6", "C7",
           "D1", "M1", "M2", "M3", "M4", "S1", "S2",
           "S3", "O1A", "O1B")
GFL <- seq(-10, 200)
GS <- seq(-100, 300)
HR <- seq(-10, 6000)
ISI <- seq(-10, 1000, by=0.1)
LAT <- seq(-370, 370, by=0.1)
LB <- seq(-1, 1.1, by=0.01)
LONG <- seq(-370, 370, by=0.1)
MON <- seq(1, 12)
PC <- seq(-10, 110)
PDF <- seq(-10, 100)
PREC <- seq(-10, 300, by=0.01)
RH <- seq(-10, 110, by=0.01)
ROS <- seq(0, 600, by=0.01)
SAZ <- seq(-370, 370, by=0.1)
SD <- seq(-10, 1e+05)
SFC <- seq(-10, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by=0.1)
WD <- seq(-370, 370, by=0.1)
WS <- seq(0, 500, by=0.1)
THETA <- seq(-360, 360, by=0.01)
WSV <- seq(-10, 500, by=0.1)
WAZ <- seq(-370, 370, by=0.1)

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
saveData('SlopeAdjust',
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
