test_that("DuffMoistureCode", {
  checkData('DuffMoistureCode',
            cffdrs:::.dmcCalc,
            list(data.table(dmc_yda=DMC),
                 data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(prec=PREC),
                 data.table(lat=LAT),
                 data.table(mon=MON),
                 data.table(lat.adjust=BOOL)))
})
