library(cffdrs)
library(data.table)
library(testthat)
# setwd("cffdrs/tests/testthat")
PATH <- "cffdrs/tests/data"
dir.create(PATH, showWarnings = FALSE, recursive = TRUE)
library(raster)

SIG_DIGS <- 6

DESIRED_ROWS <- 5000

DAY <- seq(0, 366)
PERCENT <- seq(0, 100)
RADIANS <- seq(-360, 360, by = 0.1) * pi / 180
ZERO_OR_ONE <- c(0, 1)

ACCEL <- ZERO_OR_ONE
ASPECT <- RADIANS
BOOL <- c(TRUE, FALSE)
BUI <- seq(0, 1000, by = 0.1)
BUIEFF <- ZERO_OR_ONE
CBH <- seq(0, 200, by = 0.1)
CC <- PERCENT
CFB <- seq(-1, 2, by = 0.01)
CFL <- seq(-10, 4000, by = 0.1)
D0 <- DAY
DC <- seq(0, 1000, by = 0.1)
DMC <- seq(0, 1000, by = 0.1)
DJ <- DAY
ELV <- seq(0, 10000)
FC <- seq(-10, 20000)
FFMC <- seq(0, 101, by = 0.1)
FMC <- seq(0, 500, by = 0.1)
FRACTION <- seq(0, 1, by = 0.05)
FUELTYPE <- c(
  "NF", "WA", "C1", "C2", "C3", "C4", "C5", "C6", "C7",
  "D1", "M1", "M2", "M3", "M4", "S1", "S2",
  "S3", "O1A", "O1B"
)
GFL <- seq(0, 100)
GS <- seq(0, 200)
HR <- seq(0, 366 * 24) * 60
HOURS <- seq(0, 23)
ISI <- seq(0, 300, by = 0.1)
LAT <- seq(-90, 90, by = 0.1)
LB <- seq(-1, 1.1, by = 0.01)
# FIX: for some reason the original package just makes negatives positive
LONG <- abs(seq(-180, 360, by = 0.1))
MON <- seq(1, 12)
PC <- PERCENT
PDF <- PERCENT
PREC <- seq(-10, 300, by = 0.01)
RH <- seq(-10, 110, by = 0.01)
ROS <- seq(0, 600, by = 0.01)
SAZ <- RADIANS + pi
SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
SD <- unlist(append(list(-999), seq(0, 100)))
SFC <- seq(0, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by = 0.1)
WD <- RADIANS
WS <- seq(0, 300, by = 0.1)
THETA <- seq(-360, 360, by = 0.01)
WSV <- seq(-10, 500, by = 0.1)
WAZ <- RADIANS + pi
WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)

FBP_ARGS <- list(
  data.table(ID = 1),
  data.table(FUELTYPE = FUELTYPE),
  data.table(FFMC = FFMC),
  data.table(BUI = BUI),
  data.table(WS = WS),
  data.table(WD = WD),
  data.table(FMC = FMC),
  data.table(GS = GS),
  data.table(LAT = LAT),
  data.table(LONG = LONG),
  data.table(ELV = ELV),
  data.table(DJ = DJ),
  data.table(D0 = D0),
  data.table(SD = SD),
  data.table(SH = SH),
  data.table(HR = HR),
  data.table(PC = PC),
  data.table(PDF = PDF),
  data.table(GFL = GFL),
  data.table(CC = CC),
  data.table(THETA = THETA),
  data.table(ACCEL = ACCEL),
  data.table(ASPECT = ASPECT),
  data.table(BUIEFF = BUIEFF),
  data.table(CBH = CBH),
  data.table(CFL = CFL),
  data.table(ISI = ISI)
)

pickRows <- function(d1, num_rows = DESIRED_ROWS) {
  d1 <- data.table(d1)
  # print(d1)
  # print(nrow(d1))
  # print(MAX_ROWS)
  old_names <- colnames(d1)
  while (nrow(d1) > num_rows) {
    # print('loop')
    # print(seq(1, nrow(d1), by=3))
    # print(d1[seq(1, nrow(d1), by=3), ])
    # print('assign')
    d1 <- data.table(d1[seq(1, nrow(d1), by = 3), ])
    # print('end loop')
    # print(nrow(d1))
    stopifnot(!is.null(nrow(d1)))
    colnames(d1) <- old_names
  }
  # print('return')
  return(d1)
}

makeInput <- function(arguments) {
  # print(arguments)
  d1 <- pickRows(arguments[[1]])
  if (1 < length(arguments)) {
    for (n in 2:length(arguments)) {
      # print(n)
      # print(arguments[[n]])
      d2 <- pickRows(arguments[[n]], ceiling(3 * DESIRED_ROWS / nrow(d1)))
      d1 <- pickRows(merge(data.frame(d1), data.frame(d2), by = NULL))
    }
  }
  return(data.table(d1))
}

makeData <- function(name, fct, arguments, split_args, with_input = FALSE) {
  i <- makeInput(arguments)
  if (!split_args) {
    stopifnot(is.data.table(i))
    values <- fct(i)
    i[, c(name)] <- values
    return(i)
  }
  n0 <- nrow(i)
  # i[, c(name)] <- do.call(fct, i)
  r <- list(do.call(fct, i[1, ]))
  isRow <- length(r[[1]]) > 1
  if (isRow) {
    r <- r[[1]]
    for (n in 2:nrow(i)) {
      r2 <- do.call(fct, i[n, ])
      r <- rbind(r, r2)
    }
    stopifnot(nrow(i) == n0)
    if (with_input) {
      r <- cbind(i, r)
    }
    return(r)
  } else {
    for (n in 2:nrow(i)) {
      r <- append(r, do.call(fct, i[n, ]))
    }
    i[, c(name)] <- unlist(r)
    stopifnot(nrow(i) == n0)
    return(i)
  }
}

roundData <- function(data) {
  data <- as.data.table(data)
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- signif(data[[col]], SIG_DIGS)
    }
  }
  return(data)
}

saveResults <- function(name, data) {
  # need to specify eol so OS doesn't affect it
  fwrite(roundData(data),
    get_data_path(sprintf("%s.csv", name)),
    row.names = FALSE,
    na = "NA",
    quote = FALSE,
    eol = "\r\n"
  )
}

saveData <- function(name, fct, arguments, split_args = TRUE, with_input = FALSE) {
  print(paste0("Creating ", name))
  saveResults(name, makeData(name, fct, arguments, split_args, with_input))
  checkData(name, fct, arguments, split_args)
}

checkEqual <- function(name, df1, df2) {
  ignore_names <- is.vector(df1)
  df1 <- as.data.table(df1)
  df2 <- as.data.table(df2)
  expect_equal(length(colnames(df1)), length(colnames(df2)))
  if (ignore_names)
  {
    colnames(df2) <- colnames(df1)
  }
  else
  {
    expect_equal(colnames(df1), colnames(df2))
  }
  for (n in sort(colnames(df1)))
  {
    test_that(paste0(name, '$', n), {
      actual <- unlist(df1[[n]])
      expected <- unlist(df2[[n]])
      # unsure if this will cause problems, but seems to fix when column is all NA
      class(actual) <- typeof(expected)
      expect_equal(actual, expected)
    })
  }
}

get_data_path <- function(name) {
  return(fs::path_abs(sprintf("%s/%s", PATH, name)))
}

read_data <- function(name) {
  return(read.csv(get_data_path(sprintf("%s.csv", name))))
}

read_raster <- function(name) {
  return(rast(get_data_path(sprintf("rasters/%s/%s.tif", name, name))))
}

read_raster <- function(name) {
  return(rast(get_data_path(sprintf("rasters/%s/%s.tif", name, name))))
}

checkResults <- function(name, df1)
{
  checkEqual(name, df1, read_data(name))
}

checkData <- function(name, fct, arguments, split_args = TRUE, with_input = FALSE) {
  df1 <- roundData(makeData(name, fct, arguments, split_args, with_input))
  df2 <- read_data(name)
  if (is.null(nrow(df1))) {
    # it's just an array so don't compare column names
    expect_equal(df1, df2[[1]])
  } else {
    df1 <- data.table(df1)
    df2 <- data.table(df2)
    if (split_args) {
      actual <- df1[[name]]
      expected <- df2[[name]]
      expect_equal(actual, expected)
    } else {
      expect_equal(colnames(df1), colnames(df2))
      for (n in sort(colnames(df1))) {
        test_that(paste0(name, "$", n), {
          actual <- unlist(df1[[n]])
          expected <- unlist(df2[[n]])
          expect_equal(actual, expected)
        })
      }
    }
  }
}

fctOnInput <- function(fct) {
  return(function(ID, FUELTYPE, FFMC, BUI, WS, WD, FMC, GS, LAT, LONG, ELV, DJ, D0,
                  SD, SH, HR, PC, PDF, GFL, CC, THETA, ACCEL, ASPECT, BUIEFF,
                  CBH, CFL, ISI) {
    input <- data.frame(
      ID = ID,
      FUELTYPE = FUELTYPE,
      FFMC = FFMC,
      BUI = BUI,
      WS = WS,
      WD = WD,
      FMC = FMC,
      GS = GS,
      LAT = LAT,
      LONG = LONG,
      ELV = ELV,
      DJ = DJ,
      D0 = D0,
      SD = SD,
      SH = SH,
      HR = HR,
      PC = PC,
      PDF = PDF,
      GFL = GFL,
      CC = CC,
      THETA = THETA,
      ACCEL = ACCEL,
      ASPECT = ASPECT,
      BUIEFF = BUIEFF,
      CBH = CBH,
      CFL = CFL,
      ISI = ISI
    )
    stopifnot(1 == nrow(input))
    result <- fct(input = input, output = "S")
    stopifnot(1 == nrow(result))
    return(result)
  })
}
saveData(
  "BackRateOfSpread",
  cffdrs:::.BROScalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(FFMC = FFMC),
    data.table(BUI = BUI),
    data.table(WSV = WSV),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(PC = PC),
    data.table(PDF = PDF),
    data.table(CC = CC),
    data.table(CBH = CBH)
  )
)
saveData(
  "BuildupEffect",
  cffdrs:::.BEcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(BUI = BUI)
  )
)
saveData(
  "BuildupIndex",
  cffdrs:::.buiCalc,
  list(
    data.table(dmc = DMC),
    data.table(dc = DC)
  )
)
saveData(
  "CriticalSurfaceIntensity",
  cffdrs:::.CFBcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(ROS = ROS),
    data.table(CBH = CBH),
    data.table(option = c("CSI"))
  )
)
saveData(
  "CriticalSurfaceRateOfSpread",
  cffdrs:::.CFBcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(ROS = ROS),
    data.table(CBH = CBH),
    data.table(option = c("RSO"))
  )
)
saveData(
  "CrownFractionBurned",
  cffdrs:::.CFBcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(ROS = ROS),
    data.table(CBH = CBH)
  )
)
saveData(
  "CrownFuelConsumption",
  cffdrs:::.TFCcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(CFL = CFL),
    data.table(CFB = CFB),
    data.table(SFC = SFC),
    data.table(PC = PC),
    data.table(PDF = PDF),
    data.table(option = "CFC")
  )
)
saveData(
  "DistanceAtTime",
  cffdrs:::.DISTtcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(ROSeq = ROS),
    data.table(HR = HR),
    data.table(CFB = CFB)
  )
)
saveData(
  "DroughtCode",
  cffdrs:::.dcCalc,
  list(
    data.table(dc_yda = DC),
    data.table(temp = TEMP),
    data.table(rh = RH),
    data.table(prec = PREC),
    data.table(lat = LAT),
    data.table(mon = MON),
    data.table(lat.adjust = BOOL)
  )
)
saveData(
  "DuffMoistureCode",
  cffdrs:::.dmcCalc,
  list(
    data.table(dmc_yda = DMC),
    data.table(temp = TEMP),
    data.table(rh = RH),
    data.table(prec = PREC),
    data.table(lat = LAT),
    data.table(mon = MON),
    data.table(lat.adjust = BOOL)
  )
)
saveResults(
  "OverwinterDroughtCode_test_wDC_1",
  cffdrs::wDC(DCf = 300, rw = 110)
)
saveResults(
  "OverwinterDroughtCode_test_wDC_2",
  cffdrs::wDC(DCf = 300, rw = 110, a = 1.0, b = 0.9)
)
saveResults(
  "OverwinterDroughtCode_test_wDC_3",
  cffdrs::wDC(
    DCf = c(400, 300, 250), rw = c(99, 110, 200),
    a = c(0.75, 1.0, 0.75), b = c(0.75, 0.9, 0.75)
  )
)
data("test_wDC", package = "cffdrs")
data("test_wDC_fs", package = "cffdrs")
input <- test_wDC
input <- with(input, input[order(id, yr, mon, day), ])
input$date <- as.Date(as.POSIXlt(paste(input$yr, "-", input$mon, "-", input$day, sep = "")))
# select id value 1
input.2 <- input[input$id == 2, ]
test_wDC_fs$date <- as.Date(as.POSIXlt(paste(test_wDC_fs$yr, "-", test_wDC_fs$mon, "-",
  test_wDC_fs$day,
  sep = ""
)))
# match to current id value
input.2.fs <- test_wDC_fs[test_wDC_fs$id == 2, ]
# assign start of winter date (or end of fire season date)
winterStartDate <- input.2.fs[2, "date"]
# assign end of winter date (or start of new fire season date)
winterEndDate <- input.2.fs[3, "date"]
# Accumulate overwinter precip based on chosen dates
curYr.prec <- sum(input.2[(input.2$date > winterStartDate & input.2$date < winterEndDate), ]$prec)
# Assign a fall DC value
fallDC <- 500
saveResults(
  "OverwinterDroughtCode_test_wDc_4",
  cffdrs::wDC(DCf = fallDC, rw = curYr.prec)
)
fallDC <- 250
saveResults(
  "OverwinterDroughtCode_test_wDc_5",
  cffdrs::wDC(DCf = fallDC, rw = curYr.prec)
)
saveData(
  "OverwinterDroughtCode",
  cffdrs::wDC,
  list(
    data.table(DCf = DMC),
    data.table(rw = seq(0, 1000)),
    data.table(a = FRACTION),
    data.table(b = FRACTION)
  )
)
saveData("sdmc",
  cffdrs::sdmc,
  list(
    data.table(dmc = DMC),
    data.table(temp = TEMP),
    data.table(prec = PREC),
    data.table(rh = RH),
    data.table(mon = MON),
    data.table(day = DJ),
    data.table(ws = WS)
  ),
  split_args = FALSE
)
saveData("hffmc",
  cffdrs::hffmc,
  list(
    data.table(hr = HOURS),
    data.table(temp = TEMP),
    data.table(prec = PREC),
    data.table(rh = RH),
    data.table(ws = WS)
  ),
  split_args = FALSE
)
fctGFMC <- function(input) {
  return(cffdrs::gfmc(input, GFMCold = rep(85, length(input$temp)), out = "GFMC", batch = FALSE))
}
saveData("gfmcGFMC",
  fctGFMC,
  list(
    data.table(temp = TEMP),
    data.table(rh = RH[RH >= 0 & RH <= 100]),
    data.table(ws = WS),
    data.table(prec = PREC[PREC >= 0]),
    data.table(isol = seq(0, 10000)),
    data.table(mon = MON)
  ),
  split_args = FALSE
)
fctMC <- function(input) {
  return(cffdrs::gfmc(input, GFMCold = rep(85, length(input$temp)), out = "MC", batch = FALSE))
}
saveData("gfmcMC",
  fctMC,
  list(
    data.table(temp = TEMP),
    data.table(rh = RH[RH >= 0 & RH <= 100]),
    data.table(ws = WS),
    data.table(prec = PREC[PREC >= 0]),
    data.table(isol = seq(0, 10000)),
    data.table(mon = MON)
  ),
  split_args = FALSE
)
fctHFFMC <- function(input) {
  return(cffdrs::hffmc(input, ffmc_old = input$ffmc_old, time.step = input$time.step, calc.step = FALSE, batch = FALSE, hourlyFWI = FALSE))
}
saveData("HourlyFineFuelMoistureCode",
  fctHFFMC,
  list(
    data.table(temp = TEMP),
    data.table(rh = RH),
    data.table(ws = WS),
    data.table(prec = PREC),
    data.table(ffmc_old = FFMC),
    data.table(time.step = HOURS)
  ),
  split_args = FALSE
)

saveData(
  "FireBehaviourPrediction",
  fctOnInput(cffdrs:::.FBPcalc),
  FBP_ARGS
)
saveData(
  "FireIntensity",
  cffdrs:::.FIcalc,
  list(
    data.table(FC = FC),
    data.table(ROS = ROS)
  )
)
input <- with(test_wDC, test_wDC[order(id, yr, mon, day), ])
saveResults("FireSeason_test_wDC_1", cffdrs::fireSeason(input[input$id == 1, ]))
saveResults("FireSeason_test_wDC_2", cffdrs::fireSeason(input[input$id == 1, ], fs.start = 10, fs.end = 3))
saveResults("FireSeason_test_wDC_3", cffdrs::fireSeason(input[input$id == 2, ], method = "WF93"))
saveData(
  "FireWeatherIndex",
  cffdrs:::.fwiCalc,
  list(
    data.table(isi = ISI),
    data.table(bui = BUI)
  )
)
saveData(
  "FineFuelMoistureCode",
  cffdrs:::.ffmcCalc,
  list(
    data.table(ffmc_yda = FFMC),
    data.table(temp = TEMP),
    data.table(rh = RH),
    data.table(ws = WS),
    data.table(prec = PREC)
  )
)
saveData(
  "FlankRateOfSpread",
  cffdrs:::.FROScalc,
  list(
    data.table(ROS = ROS),
    data.table(BROS = ROS),
    data.table(LB = LB)
  )
)
saveData(
  "FoliarMoistureContent",
  cffdrs:::.FMCcalc,
  list(
    data.table(LAT = LAT),
    data.table(LONG = LONG),
    data.table(ELV = ELV),
    data.table(DJ = DJ),
    data.table(D0 = D0)
  )
)
saveData(
  "InitialSpreadIndex",
  cffdrs:::.ISIcalc,
  list(
    data.table(ffmc = FFMC),
    data.table(ws = WS),
    data.table(fbpMod = BOOL)
  )
)
saveData(
  "C6IntermediateSurfaceRateOfSpread",
  cffdrs:::.C6calc,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("RSI"))
  )
)
saveData(
  "C6CrownRateOfSpread",
  cffdrs:::.C6calc,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("RSC"))
  )
)
fctRSSC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option) {
  stopifnot("C6" == FUELTYPE)
  RSI <- cffdrs:::.C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  RSS <- RSI * cffdrs:::.BEcalc(FUELTYPE, BUI)
  return(RSS)
}
saveData(
  "C6SurfaceRateOfSpread",
  fctRSSC6,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("RSI"))
  )
)
saveData(
  "C6CrownFractionBurned",
  cffdrs:::.C6calc,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("CFB"))
  )
)
fctCSIC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option) {
  stopifnot("C6" == FUELTYPE)
  stopifnot("RSC" == option)
  CSI <- cffdrs:::.CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, "CSI")
  return(CSI)
}
saveData(
  "C6CriticalSurfaceIntensity",
  fctCSIC6,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("RSC"))
  )
)
fctRSOC6 <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option) {
  stopifnot("C6" == FUELTYPE)
  stopifnot("RSC" == option)
  RSO <- cffdrs:::.CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, "RSO")
  return(RSO)
}
saveData(
  "C6CriticalSurfaceRateOfSpread",
  fctRSOC6,
  list(
    data.table(FUELTYPE = c("C6")),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(CBH = CBH),
    data.table(ROS = ROS),
    data.table(CFB = CFB),
    data.table(RSC = ROS),
    data.table(option = c("RSC"))
  )
)
saveData(
  "LengthToBreadthRatio",
  cffdrs:::.LBcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(WSV = WSV)
  )
)
saveData(
  "LengthToBreadthRatioAtTime",
  cffdrs:::.LBtcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(LB = LB),
    data.table(HR = HR),
    data.table(CFB = CFB)
  )
)
saveData(
  "RateOfSpread",
  cffdrs:::.ROScalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(ISI = ISI),
    data.table(BUI = BUI),
    data.table(FMC = FMC),
    data.table(SFC = SFC),
    data.table(PC = PC),
    data.table(PDF = PDF),
    data.table(CC = CC),
    data.table(CBH = CBH)
  )
)
saveData(
  "RateOfSpreadAtTheta",
  cffdrs:::.ROSthetacalc,
  list(
    data.table(ROS = ROS),
    data.table(FROS = ROS),
    data.table(BROS = ROS),
    data.table(THETA = THETA)
  )
)
saveData(
  "RateOfSpreadAtTime",
  cffdrs:::.ROStcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(ROSeq = ROS),
    data.table(HR = HR),
    data.table(CFB = CFB)
  )
)
fctSlopeISI <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ") {
  # output options include: RAZ and WSV

  # check for valid output types
  validOutTypes <- c("RAZ", "WAZ", "WSV")
  if (!(output %in% validOutTypes)) {
    stop(paste("In 'slopecalc()', '", output, "' is an invalid 'output' type.",
      sep = ""
    ))
  }

  NoBUI <- rep(-1, length(FFMC))
  # Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  # ISI with 0 wind on level grounds
  ISZ <- cffdrs:::.ISIcalc(FFMC, 0)
  # Surface spread rate with 0 wind on level ground
  RSZ <- cffdrs:::.ROScalc(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  # setup some reference vectors
  d <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
    "S1", "S2", "S3", "O1A", "O1B"
  )
  a <- c(
    90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190,
    250
  )
  b <- c(
    0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0,
    0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350
  )
  c0 <- c(
    4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7,
    3.2, 1.4, 1.7
  )
  names(a) <- names(b) <- names(c0) <- d

  # initialize some local vars
  RSZ <- rep(-99, length(FFMC))
  RSF_C2 <- rep(-99, length(FFMC))
  RSF_D1 <- rep(-99, length(FFMC))
  RSF_M3 <- rep(-99, length(FFMC))
  RSF_M4 <- rep(-99, length(FFMC))
  CF <- rep(-99, length(FFMC))
  ISF <- rep(-99, length(FFMC))
  ISF_C2 <- rep(-99, length(FFMC))
  ISF_D1 <- rep(-99, length(FFMC))
  ISF_M3 <- rep(-99, length(FFMC))
  ISF_M4 <- rep(-99, length(FFMC))

  # Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalend ISI
  ISF <- ifelse(FUELTYPE %in% c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1",
    "S1", "S2", "S3"
  ),
  ifelse((1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) >= 0.01,
    log(1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
    log(0.01) / (-b[FUELTYPE])
  ),
  ISF
  )

  # When calculating the M1/M2 types, we are going to calculate for both C2
  # and D1 types, and combine
  # Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
    cffdrs:::.ROScalc(rep("C2", length(ISZ)), ISZ,
      BUI = NoBUI, FMC, SFC, PC, PDF,
      CC, CBH
    ),
    RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
  RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
    cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ,
      BUI = NoBUI, FMC, SFC, PC,
      PDF, CC, CBH
    ), RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
    log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]),
    ISF_C2
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
    log(0.01) / (-b[["C2"]]),
    ISF_C2
  )
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
    log(0.01) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
  ISF <- ifelse(FUELTYPE %in% c("M1", "M2"), PC / 100 * ISF_C2 +
    (1 - PC / 100) * ISF_D1,
  ISF
  )

  # Set % Dead Balsam Fir to 100%
  PDF100 <- rep(100, length(ISI))
  # Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M3"),
    cffdrs:::.ROScalc(rep("M3", length(FMC)),
      ISI = ISZ, BUI = NoBUI, FMC, SFC,
      PC, PDF100, CC, CBH
    ),
    RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
  # Surface spread rate with 0 wind on level ground, using D1
  RSZ <- ifelse(FUELTYPE %in% c("M3"),
    cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ,
      BUI = NoBUI, FMC, SFC, PC,
      PDF100, CC, CBH
    ),
    RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
    log(1 - (RSF_M3 / a[["M3"]])**(1 / c0[["M3"]])) / (-b[["M3"]]), ISF_M3
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
    log(0.01) / (-b[["M3"]]),
    ISF_M3
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
    log(0.01) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  ISF <- ifelse(FUELTYPE %in% c("M3"),
    PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1,
    ISF
  )
  # Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"),
    cffdrs:::.ROScalc(rep("M4", length(FMC)),
      ISI = ISZ, BUI = NoBUI, FMC, SFC,
      PC, PDF100, CC, CBH
    ),
    RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
  RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
  # Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"),
    cffdrs:::.ROScalc(rep("D1", length(ISZ)), ISZ,
      BUI = NoBUI, FMC, SFC, PC,
      PDF100, CC, CBH
    ),
    RSZ
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_D1)
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
    log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
    ISF_M4
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
    log(0.01) / (-b[["M4"]]),
    ISF_M4
  )
  # Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  # Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
    log(0.01) / (-b[["D1"]]),
    ISF_D1
  )
  # Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
  ISF <- ifelse(FUELTYPE %in% c("M4"), PDF / 100 * ISF_M4 + (1 - PDF / 100.) *
    ISF_D1,
  ISF
  )
  # Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
  CF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
    ifelse(CC < 58.8, 0.005 * (exp(0.061 * CC) - 1),
      0.176 + 0.02 * (CC - 58.8)
    ),
    CF
  )
  # Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
  ISF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
    ifelse((1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
      log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) /
        (-b[FUELTYPE]),
      log(0.01) / (-b[FUELTYPE])
    ),
    ISF
  )
  return(ISF)
}
fctSlopeWSE <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ") {
  ISF <- fctSlopeISI(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC,
    PDF, CC, CBH, ISI, output
  )
  if (is.na(ISF) || -99.0 == ISF) {
    return(NA)
  }
  # Eq. 46 (FCFDG 1992)
  m <- 147.27723 * (101 - FFMC) / (59.5 + FFMC)
  # Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
  fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
  # Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
  WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
  # Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF < (0.999 * 2.496 * fF),
    28 - (1 / 0.0818 * log(1 - ISF / (2.496 * fF))),
    WSE
  )
  # Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
  return(WSE)
}
fctSlopeWSX <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ") {
  WSE <- fctSlopeWSE(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
    CC, CBH, ISI, output
  )
  # Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  return(WSX)
}
fctSlopeWSY <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output) {
  WSE <- fctSlopeWSE(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
    CC, CBH, ISI, output
  )
  # Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  return(WSY)
}
fctSlopeWSV <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output) {
  WSE <- fctSlopeWSE(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
    CC, CBH, ISI, output
  )
  # Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  # Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  # Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  return(WSV)
}
fctSlopeRAZ <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output) {
  WSE <- fctSlopeWSE(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
    CC, CBH, ISI, output
  )
  # Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  # Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  # Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  # Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
  RAZ <- acos(WSY / WSV)
  # Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
  # directions
  RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
  return(RAZ)
}
makeSlopeData <- function(name, fct = cffdrs:::.Slopecalc, output = "RAZ") {
  return(makeData(
    name,
    fct,
    list(
      data.table(FUELTYPE = FUELTYPE),
      data.table(FFMC = FFMC),
      data.table(BUI = BUI),
      data.table(WS = WS),
      data.table(WAZ = WAZ),
      data.table(GS = GS),
      data.table(SAZ = SAZ),
      data.table(FMC = FMC),
      data.table(SFC = SFC),
      data.table(PC = PC),
      data.table(PDF = PDF),
      data.table(CC = CC),
      data.table(CBH = CBH),
      data.table(ISI = ISI),
      data.table(output = output)
    ),
    split_args = TRUE,
    with_input = TRUE
  ))
}
# generate individually, but save as a group since arguments are mostly identical
slope_values <- makeSlopeData("SlopeAdjustRAZ")
addSlopeData <- function(name, fct = cffdrs:::.Slopecalc, output = "RAZ") {
  slope_values <<- merge(
    slope_values,
    makeSlopeData(name, fct, output)[, -c("output")]
  )
}
addSlopeData("SlopeAdjustWSV", output = "WSV")
addSlopeData("SlopeEquivalentInitialSpreadIndex", fctSlopeISI)
addSlopeData("SlopeAdjustWSX", fctSlopeWSX)
addSlopeData("SlopeAdjustWSY", fctSlopeWSY)
addSlopeData("SlopeAdjust_WSV", fctSlopeWSV)
addSlopeData("SlopeAdjust_RAZ", fctSlopeRAZ)
saveResults("Slope", slope_values)
saveData(
  "SurfaceFuelConsumption",
  cffdrs:::.SFCcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(FFMC = FFMC),
    data.table(BUI = BUI),
    data.table(PC = PC),
    data.table(GFL = GFL)
  )
)
saveData(
  "TotalFuelConsumption",
  cffdrs:::.TFCcalc,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(CFL = CFL),
    data.table(CFB = CFB),
    data.table(SFC = SFC),
    data.table(PC = PC),
    data.table(PDF = PDF),
    data.table(option = "TFC")
  )
)
fctWSV0 <- function(input = NULL, output = "Primary") {
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  output <- toupper(output)
  # if input does not exist, then set defaults
  if (is.null(input)) {
    input <- data.frame(
      FUELTYPE = "C2", ACCEL = 0, DJ = 180, D0 = 0, ELV = 0, BUIEFF = 1, HR = 1,
      FFMC = 90, ISI = 0, BUI = 60, WS = 10, WD = 0, GS = 0, ASPECT = 0, PC = 50,
      PDF = 35, CC = 80, GFL = 0.35, CBH = 3, CFL = 1, LAT = 55, LONG = -120,
      FMC = 0, THETA = 0
    )
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  # set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- toupper(input$FUELTYPE)
  FFMC <- input$FFMC
  BUI <- input$BUI
  WS <- input$WS
  WD <- input$WD
  FMC <- input$FMC
  GS <- input$GS
  LAT <- input$LAT
  LONG <- input$LONG
  ELV <- input$ELV
  DJ <- input$DJ
  D0 <- input$D0
  SD <- input$SD
  SH <- input$SH
  HR <- input$HR
  PC <- input$PC
  PDF <- input$PDF
  GFL <- input$GFL
  CC <- input$CC
  THETA <- input$THETA
  ACCEL <- input$ACCEL
  ASPECT <- input$ASPECT
  BUIEFF <- input$BUIEFF
  CBH <- input$CBH
  CFL <- input$CFL
  ISI <- input$ISI
  n0 <- nrow(input)
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!exists("FUELTYPE") | is.null(FUELTYPE)) {
    warning("FuelType is a required input, default FuelType = C2 is used in the
            calculation")
    FUELTYPE <- "C2"
  }
  if (!exists("FFMC") | is.null(FFMC)) {
    warning("FFMC is a required input, default FFMC = 90 is used in the
            calculation")
    FFMC <- 90
  }
  if (!exists("BUI") | is.null(BUI)) {
    warning("BUI is a required input, default BUI = 60 is used in the
            calculation")
    BUI <- 60
  }
  if (!exists("WS") | is.null(WS)) {
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- 10
  }
  if (!exists("GS") | is.null(GS)) {
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- 0
  }
  if (!exists("LAT") | is.null(LAT)) {
    warning("LAT is a required input, default LAT=55 is used in the
            calculation")
    LAT <- 55
  }
  if (!exists("LONG") | is.null(LONG)) {
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- -120
  }
  if (!exists("DJ") | is.null(DJ)) {
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- 180
  }
  if (!exists("ASPECT") | is.null(ASPECT)) {
    warning("Aspect is a required input, Aspect = 0 is used in the calculation")
    ASPECT <- 0
  }
  if (!exists("WD") | is.null(WD)) {
    WD <- 0
  }
  if (!exists("FMC") | is.null(FMC)) {
    FMC <- 0
  }
  if (!exists("ELV") | is.null(ELV)) {
    ELV <- 0
  }
  if (!exists("SD") | is.null(SD)) {
    SD <- 0
  }
  if (!exists("SH") | is.null(SH)) {
    SH <- 0
  }
  if (!exists("D0") | is.null(D0)) {
    D0 <- 0
  }
  if (!exists("HR") | is.null(HR)) {
    HR <- 1
  }
  if (!exists("PC") | is.null(PC)) {
    PC <- 50
  }
  if (!exists("PDF") | is.null(PDF)) {
    PDF <- 35
  }
  if (!exists("GFL") | is.null(GFL)) {
    GFL <- 0.35
  }
  if (!exists("CC") | is.null(CC)) {
    CC <- 80
  }
  if (!exists("THETA") | is.null(THETA)) {
    THETA <- 0
  }
  if (!exists("ACCEL") | is.null(ACCEL)) {
    ACCEL <- 0
  }
  if (!exists("BUIEFF") | is.null(BUIEFF)) {
    BUIEFF <- 1
  }
  if (!exists("CBH") | is.null(CBH)) {
    CBH <- 0
  }
  if (!exists("CFL") | is.null(CFL)) {
    CFL <- 0
  }
  if (!exists("ISI") | is.null(ISI)) {
    ISI <- 0
  }
  # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) {
    warning("Input variable Accel is out of range, will be assigned to 1")
  }
  ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  DJ <- ifelse(is.na(DJ), 180, DJ)
  D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  ELV <- ifelse(is.na(ELV), 0, ELV)
  BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  HR <- ifelse(HR < 0, -HR, HR)
  HR <- ifelse(HR > 366 * 24, 24, HR)
  HR <- ifelse(is.na(HR), 0, HR)
  FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  BUI <- ifelse(is.na(BUI), 60, BUI)
  WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  WS <- ifelse(is.na(WS), 10, WS)
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi,
    0, WD
  )
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35,
    GFL
  )
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA >
    2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  SD <- ifelse(is.na(SD), 0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)

  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if (length(FUELTYPE[is.na(FUELTYPE)]) > 0) {
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  }
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  # Convert hours to minutes
  HR <- HR * 60
  # Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  # Any negative longitudes (western hemisphere) are translated to positive
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  ############################################################################
  #                         END
  ############################################################################
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
  CBHs <- c(
    2, 3, 8, 4, 18, 7, 10, 0, 6, 6, 6, 6, 0, 0, 0,
    0, 0
  )
  names(CBHs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
    "O1B"
  )
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH), ifelse(FUELTYPE %in%
    c("C6") & SD > 0 & SH > 0, -11.2 + 1.06 * SH + 0.0017 *
    SD, CBHs[FUELTYPE]), CBH)
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  CFLs <- c(
    0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5, 0, 0.8, 0.8,
    0.8, 0.8, 0, 0, 0, 0, 0
  )
  names(CFLs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
    "O1B"
  )
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE],
    CFL
  )
  FMC <- ifelse(FMC <= 0 | FMC > 120 | is.na(FMC), cffdrs:::.FMCcalc(
    LAT,
    LONG, ELV, DJ, D0
  ), FMC)
  FMC <- ifelse(FUELTYPE %in% c(
    "D1", "S1", "S2", "S3", "O1A",
    "O1B"
  ), 0, FMC)
  ############################################################################
  #                         END
  ############################################################################

  # Calculate Surface fuel consumption (SFC)
  SFC <- cffdrs:::.SFCcalc(FUELTYPE, FFMC, BUI, PC, GFL)
  # Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  # Calculate the net effective windspeed (WSV)
  WSV0 <- cffdrs:::.Slopecalc(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
    FMC, SFC, PC, PDF, CC, CBH, ISI, "WSV"
  )
  return(WSV0)
}
saveData(
  "FireBehaviourPrediction_WSV0",
  fctOnInput(fctWSV0),
  FBP_ARGS
)
fctRAZ0 <- function(input = NULL, output = "Primary") {
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  output <- toupper(output)
  # if input does not exist, then set defaults
  if (is.null(input)) {
    input <- data.frame(
      FUELTYPE = "C2", ACCEL = 0, DJ = 180, D0 = 0, ELV = 0, BUIEFF = 1, HR = 1,
      FFMC = 90, ISI = 0, BUI = 60, WS = 10, WD = 0, GS = 0, ASPECT = 0, PC = 50,
      PDF = 35, CC = 80, GFL = 0.35, CBH = 3, CFL = 1, LAT = 55, LONG = -120,
      FMC = 0, THETA = 0
    )
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  # set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- toupper(input$FUELTYPE)
  FFMC <- input$FFMC
  BUI <- input$BUI
  WS <- input$WS
  WD <- input$WD
  FMC <- input$FMC
  GS <- input$GS
  LAT <- input$LAT
  LONG <- input$LONG
  ELV <- input$ELV
  DJ <- input$DJ
  D0 <- input$D0
  SD <- input$SD
  SH <- input$SH
  HR <- input$HR
  PC <- input$PC
  PDF <- input$PDF
  GFL <- input$GFL
  CC <- input$CC
  THETA <- input$THETA
  ACCEL <- input$ACCEL
  ASPECT <- input$ASPECT
  BUIEFF <- input$BUIEFF
  CBH <- input$CBH
  CFL <- input$CFL
  ISI <- input$ISI
  n0 <- nrow(input)
  stopifnot(1 == n0)
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!exists("FUELTYPE") | is.null(FUELTYPE)) {
    warning("FuelType is a required input, default FuelType = C2 is used in the
            calculation")
    FUELTYPE <- "C2"
  }
  if (!exists("FFMC") | is.null(FFMC)) {
    warning("FFMC is a required input, default FFMC = 90 is used in the
            calculation")
    FFMC <- 90
  }
  if (!exists("BUI") | is.null(BUI)) {
    warning("BUI is a required input, default BUI = 60 is used in the
            calculation")
    BUI <- 60
  }
  if (!exists("WS") | is.null(WS)) {
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- 10
  }
  if (!exists("GS") | is.null(GS)) {
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- 0
  }
  if (!exists("LAT") | is.null(LAT)) {
    warning("LAT is a required input, default LAT=55 is used in the
            calculation")
    LAT <- 55
  }
  if (!exists("LONG") | is.null(LONG)) {
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- -120
  }
  if (!exists("DJ") | is.null(DJ)) {
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- 180
  }
  if (!exists("ASPECT") | is.null(ASPECT)) {
    warning("Aspect is a required input, Aspect = 0 is used in the calculation")
    ASPECT <- 0
  }
  if (!exists("WD") | is.null(WD)) {
    WD <- 0
  }
  if (!exists("FMC") | is.null(FMC)) {
    FMC <- 0
  }
  if (!exists("ELV") | is.null(ELV)) {
    ELV <- 0
  }
  if (!exists("SD") | is.null(SD)) {
    SD <- 0
  }
  if (!exists("SH") | is.null(SH)) {
    SH <- 0
  }
  if (!exists("D0") | is.null(D0)) {
    D0 <- 0
  }
  if (!exists("HR") | is.null(HR)) {
    HR <- 1
  }
  if (!exists("PC") | is.null(PC)) {
    PC <- 50
  }
  if (!exists("PDF") | is.null(PDF)) {
    PDF <- 35
  }
  if (!exists("GFL") | is.null(GFL)) {
    GFL <- 0.35
  }
  if (!exists("CC") | is.null(CC)) {
    CC <- 80
  }
  if (!exists("THETA") | is.null(THETA)) {
    THETA <- 0
  }
  if (!exists("ACCEL") | is.null(ACCEL)) {
    ACCEL <- 0
  }
  if (!exists("BUIEFF") | is.null(BUIEFF)) {
    BUIEFF <- 1
  }
  if (!exists("CBH") | is.null(CBH)) {
    CBH <- 0
  }
  if (!exists("CFL") | is.null(CFL)) {
    CFL <- 0
  }
  if (!exists("ISI") | is.null(ISI)) {
    ISI <- 0
  }
  # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) {
    warning("Input variable Accel is out of range, will be assigned to 1")
  }
  ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  DJ <- ifelse(is.na(DJ), 180, DJ)
  D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  ELV <- ifelse(is.na(ELV), 0, ELV)
  BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  HR <- ifelse(HR < 0, -HR, HR)
  HR <- ifelse(HR > 366 * 24, 24, HR)
  HR <- ifelse(is.na(HR), 0, HR)
  FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  BUI <- ifelse(is.na(BUI), 60, BUI)
  WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  WS <- ifelse(is.na(WS), 10, WS)
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi,
    0, WD
  )
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35,
    GFL
  )
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA >
    2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999.0, as.numeric(SD))
  SD <- ifelse(is.na(SD), 0.0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)
  stopifnot(1 == length(SD))
  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if (length(FUELTYPE[is.na(FUELTYPE)]) > 0) {
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  }
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  # Convert hours to minutes
  HR <- HR * 60
  # Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  # Any negative longitudes (western hemisphere) are translated to positive
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  ############################################################################
  #                         END
  ############################################################################
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
  CBHs <- c(
    2, 3, 8, 4, 18, 7, 10, 0, 6, 6, 6, 6, 0, 0, 0,
    0, 0
  )
  names(CBHs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
    "O1B"
  )
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH), ifelse(FUELTYPE %in%
    c("C6") & SD > 0 & SH > 0, -11.2 + 1.06 * SH + 0.0017 *
    SD, CBHs[FUELTYPE]), CBH)
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  CFLs <- c(
    0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5, 0, 0.8, 0.8,
    0.8, 0.8, 0, 0, 0, 0, 0
  )
  names(CFLs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
    "O1B"
  )
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE],
    CFL
  )
  FMC <- ifelse(FMC <= 0 | FMC > 120 | is.na(FMC), cffdrs:::.FMCcalc(
    LAT,
    LONG, ELV, DJ, D0
  ), FMC)
  FMC <- ifelse(FUELTYPE %in% c(
    "D1", "S1", "S2", "S3", "O1A",
    "O1B"
  ), 0, FMC)
  ############################################################################
  #                         END
  ############################################################################

  # Calculate Surface fuel consumption (SFC)
  SFC <- cffdrs:::.SFCcalc(FUELTYPE, FFMC, BUI, PC, GFL)
  # Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  # Calculate the net effective windspeed (WSV)
  RAZ0 <- cffdrs:::.Slopecalc(
    FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
    FMC, SFC, PC, PDF, CC, CBH, ISI, "RAZ"
  )
  return(RAZ0)
}
saveData(
  "FireBehaviourPrediction_RAZ0",
  fctOnInput(fctRAZ0),
  FBP_ARGS
)
fctCBH <- function(FUELTYPE, CBH, SD, SH) {
  CBHs <- c(
    2, 3, 8, 4, 18, 7, 10, 0, 6, 6, 6, 6, 0, 0, 0,
    0, 0
  )
  names(CBHs) <- c(
    "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A",
    "O1B"
  )
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH), ifelse(FUELTYPE %in%
    c("C6") & SD > 0 & SH > 0, -11.2 + 1.06 * SH + 0.0017 *
    SD, CBHs[FUELTYPE]), CBH)
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  return(CBH)
}
saveData(
  "CrownBaseHeight",
  fctCBH,
  list(
    data.table(FUELTYPE = FUELTYPE),
    data.table(CBH = CBH),
    data.table(SD = SD),
    data.table(SH = SH)
  )
)
data("test_lros", package = "cffdrs")
saveResults(
  "SimardRateOfSpreadLine",
  cffdrs:::lros(test_lros)
)
data("test_pros", package = "cffdrs")
saveResults(
  "SimardRateOfSpreadPoint",
  cffdrs::pros(test_pros)
)
###########################################################################
# Raster tests
###########################################################################
saveRasters <- function(name, rasters)
{
  out_dir <- get_data_path(sprintf("rasters/%s/", name))
  dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
  print(paste0("Creating ", name))
  terra::writeRaster(rasters,
                     sprintf("%s/%s.tif", out_dir, name),
                     overwrite=T)
}

test_fbpRaster <- stack(system.file("extdata", "test_fbpRaster.tif", package="cffdrs"))
input<-test_fbpRaster
# Stack doesn't hold the raster layer names, we have to assign
# them:
names(input)<-c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
                "PDF","GFL","cc","theta","Accel","Aspect","BUIEff","CBH","CFL","ISI")
# Primary outputs:
system.time(foo1<-fbpRaster(input = input))
saveRasters("fbpRaster_test1", foo1)
# Using the "select" option:
system.time(foo2<-fbpRaster(input = input,select=c("HFI","TFC", "ROS")))
saveRasters("fbpRaster_test2", foo2)
# Secondary outputs:
system.time(foo3<-fbpRaster(input = input,output="S"))
saveRasters("fbpRaster_test3", foo3)
# All outputs:
system.time(foo4<-fbpRaster(input = input,output="A"))
saveRasters("fbpRaster_test4", foo4)
### Additional, longer running examples  ###
# Keep only the required input layers, the other layers would be
# assigned with default values:
# keep only the required inputs:
dat0<-input[[c("FuelType","LAT","LONG","FFMC","BUI","WS","GS", "Dj","Aspect")]]
system.time(foo5<-fbpRaster(input = dat0,output="A"))
saveRasters("fbpRaster_test5", foo5)

test_fwiRaster <- stack(system.file("extdata", "test_rast_day01.tif", package="cffdrs"))
names(test_fwiRaster) <- c("temp", "rh", "ws", "prec")

system.time(foo1 <- fwiRaster(input = test_fwiRaster))
saveRasters("fwiRaster_test1", foo1)

system.time(foo2 <- fwiRaster(input = test_fwiRaster,out = "all"))
saveRasters("fwiRaster_test2", foo2)

system.time(foo3 <- fwiRaster(input = test_fwiRaster,out = "fwi"))
saveRasters("fwiRaster_test3", foo3)


data("test_fbp", package = "cffdrs")
test_fbp$FFMC <- as.numeric(test_fbp$FFMC)
test_fbp$hr <- as.numeric(test_fbp$hr)
test_fbp$WS <- as.numeric(test_fbp$WS)
test_fbp$GFL <- as.numeric(test_fbp$GFL)
test_fbp$CBH <- as.numeric(test_fbp$CBH)
test_fbp$CFL <- as.numeric(test_fbp$CFL)
saveResults(
  "FireBehaviourPrediction_test_fbp",
  cffdrs:::.FBPcalc(test_fbp, "A")
)
saveResults("fbp_01", cffdrs::fbp(test_fbp))
saveResults("fbp_02", cffdrs::fbp(test_fbp, output = "Primary"))
saveResults("fbp_03", cffdrs::fbp(test_fbp, "P"))
saveResults("fbp_04", cffdrs::fbp(test_fbp, "Secondary"))
saveResults("fbp_05", cffdrs::fbp(test_fbp, "S"))
saveResults("fbp_06", cffdrs::fbp(test_fbp, "All"))
saveResults("fbp_07", cffdrs::fbp(test_fbp, "A"))
saveResults("fbp_08", cffdrs::fbp(test_fbp[7, ]))
saveResults("fbp_09", cffdrs::fbp(test_fbp[8:13, ]))
saveResults("fbp_10", cffdrs::fbp())
non_fuel <- copy(test_fbp)
non_fuel$FuelType <- "NF"
saveResults("fbp_11", fbp(non_fuel, "All"))
water <- copy(test_fbp)
water$FuelType <- "WA"
saveResults("fbp_12", fbp(water, "All"))

data("test_fwi", package = "cffdrs")
test_fwi$long <- as.numeric(test_fwi$long)
test_fwi$lat <- as.numeric(test_fwi$lat)
test_fwi$yr <- as.numeric(test_fwi$yr)
test_fwi$mon <- as.numeric(test_fwi$mon)
test_fwi$day <- as.numeric(test_fwi$day)
test_fwi$temp <- as.numeric(test_fwi$temp)
test_fwi$rh <- as.numeric(test_fwi$rh)
test_fwi$ws <- as.numeric(test_fwi$ws)
test_fwi$prec <- as.numeric(test_fwi$prec)
saveResults("fwi_01", fwi(test_fwi))
saveResults("fwi_02", fwi(test_fwi, out = "all"))
saveResults("fwi_03", fwi(test_fwi, out = "fwi"))
saveResults("fwi_04", fwi(test_fwi, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55)))
saveResults("fwi_05", fwi(test_fwi, init = data.frame(ffmc = 0, dmc = 0, dc = 0, lat = 55)))
saveResults("fwi_06", fwi(test_fwi, init = data.frame(ffmc = 200, dmc = 1000, dc = 10000, lat = 55)))
saveResults("fwi_07", fwi(test_fwi, lat.adjust = FALSE))
saveResults("fwi_08", fwi(test_fwi, uppercase = FALSE))
saveResults("fwi_09", fwi(test_fwi[7, ]))
saveResults("fwi_10", fwi(test_fwi[8:13, ]))
saveResults("fwi_11", fwi(test_fwi, batch = FALSE))



cmp_text <- function(data, name) {
  data <- roundData(data)
  # comparing NA doesn't work
  df2 <- read.csv(get_data_path(name), na.strings = NULL)
  # HACK: the date range gets converted to '20:25' if as.character() is called directly on the data.frame
  stopifnot(all(as.character(lapply(data, as.character)) == as.character(lapply(df2, as.character))))
}

# make sure things match right now, or test data isn't useful
cmp_text(fbp(test_fbp), "fbp_01.csv")
cmp_text(fbp(test_fbp, output = "Primary"), "fbp_02.csv")
cmp_text(fbp(test_fbp, "P"), "fbp_03.csv")
cmp_text(fbp(test_fbp, "Secondary"), "fbp_04.csv")
cmp_text(fbp(test_fbp, "S"), "fbp_05.csv")
cmp_text(fbp(test_fbp, "All"), "fbp_06.csv")
cmp_text(fbp(test_fbp, "A"), "fbp_07.csv")
cmp_text(fbp(test_fbp[7, ]), "fbp_08.csv")
cmp_text(fbp(test_fbp[8:13, ]), "fbp_09.csv")
cmp_text(fbp(), "fbp_10.csv")
cmp_text(fbp(non_fuel, "All"), "fbp_11.csv")
cmp_text(fbp(water, "All"), "fbp_12.csv")


cmp_text(fwi(test_fwi), "fwi_01.csv")
cmp_text(fwi(test_fwi, out = "all"), "fwi_02.csv")
cmp_text(fwi(test_fwi, out = "fwi"), "fwi_03.csv")
cmp_text(fwi(test_fwi, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55)), "fwi_04.csv")
cmp_text(fwi(test_fwi, init = data.frame(ffmc = 0, dmc = 0, dc = 0, lat = 55)), "fwi_05.csv")
cmp_text(fwi(test_fwi, init = data.frame(ffmc = 200, dmc = 1000, dc = 10000, lat = 55)), "fwi_06.csv")
cmp_text(fwi(test_fwi, lat.adjust = FALSE), "fwi_07.csv")
cmp_text(fwi(test_fwi, uppercase = FALSE), "fwi_08.csv")
cmp_text(fwi(test_fwi[7, ]), "fwi_09.csv")
cmp_text(fwi(test_fwi[8:13, ]), "fwi_10.csv")

print("Done")
