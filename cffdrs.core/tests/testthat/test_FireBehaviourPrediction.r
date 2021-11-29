test_that("FireBehaviourPrediction", {
  library(cffdrs.core)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  test_fbp$FFMC <- as.numeric(test_fbp$FFMC)
  test_fbp$hr <- as.numeric(test_fbp$hr)
  test_fbp$WS <- as.numeric(test_fbp$WS)
  test_fbp$GFL <- as.numeric(test_fbp$GFL)
  test_fbp$CBH <- as.numeric(test_fbp$CBH)
  test_fbp$CFL <- as.numeric(test_fbp$CFL)
  checkResults('FireBehaviourPrediction_test_fbp',
               FireBehaviourPrediction(test_fbp, "A"))
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
    return(FireBehaviourPrediction(input=input, output="S"))
  }
  checkData('FireBehaviourPrediction',
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
})
