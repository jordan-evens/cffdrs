library(data.table)
PATH <- '../data/'
MAX_ROWS <- 1000

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
ROS <- seq(-10, 600, by=0.01)
SAZ <- seq(-370, 370, by=0.1)
SD <- seq(-10, 1e+05)
SFC <- seq(-10, 20000)
SH <- seq(-10, 110)
TEMP <- seq(-30, 60, by=0.1)
WD <- seq(-370, 370, by=0.1)
WS <- seq(-10, 500, by=0.1)
THETA <- seq(-360, 360, by=0.01)
WSV <- seq(-10, 500, by=0.1)
WAZ <- seq(-370, 370, by=0.1)

pickRows <- function(d1, num_rows=MAX_ROWS)
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
      d2 <- pickRows(arguments[[n]])
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
