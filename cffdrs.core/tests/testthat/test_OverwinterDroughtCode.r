test_that("OverwinterDroughtCode", {
  checkData('OverwinterDroughtCode',
            OverwinterDroughtCode,
            list(data.table(DCf=DMC),
                 data.table(rw=seq(0, 1000)),
                 data.table(a=FRACTION),
                 data.table(b=FRACTION)))
})
