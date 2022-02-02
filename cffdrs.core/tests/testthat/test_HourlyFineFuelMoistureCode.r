# test_that("ShelteredDuffMoistureCode", {
#   checkData('ShelteredDuffMoistureCode',
#             ShelteredDuffMoistureCode,
#             list(data.table(temp=TEMP),
#                  data.table(prec=PREC),
#                  data.table(rh=RH),
#                  data.table(mon=MON),
#                  data.table(dmc=DMC)))
# })
test_that("hffmc", {
  checkData('hffmc',
            hffmc,
            list(data.table(hr=HOURS),
                 data.table(temp=TEMP),
                 data.table(prec=PREC),
                 data.table(rh=RH),
                 data.table(ws=WS)),
            split_args=FALSE)
})
