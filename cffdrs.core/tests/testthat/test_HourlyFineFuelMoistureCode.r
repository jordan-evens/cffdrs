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
