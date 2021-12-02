test_that("FireBehaviourPrediction_test_fbp_NoC6", {
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  test_fbp$FFMC <- as.numeric(test_fbp$FFMC)
  test_fbp$hr <- as.numeric(test_fbp$hr)
  test_fbp$WS <- as.numeric(test_fbp$WS)
  test_fbp$GFL <- as.numeric(test_fbp$GFL)
  test_fbp$CBH <- as.numeric(test_fbp$CBH)
  test_fbp$CFL <- as.numeric(test_fbp$CFL)
  checkResults('FireBehaviourPrediction_test_fbp_NoC6',
               FireBehaviourPrediction(test_fbp[!(test_fbp$FuelType %in% c("C6", "c6", "C-6", "c-6")),], "A"))
})
test_that("FireBehaviourPredictionNoC6", {
  checkData('FireBehaviourPredictionNoC6',
            fctOnInput(FireBehaviourPrediction),
            FBP_ARGS)
})
