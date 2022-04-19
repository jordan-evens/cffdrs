test_that("SimardRateOfSpreadLine", {
  test_lros <- read.csv('../../data/test_lros.txt', sep='\t')
  checkResults('SimardRateOfSpreadLine',
               SimardRateOfSpreadLine(test_lros))
})
test_that("SimardRateOfSpreadPoint", {
  test_pros <- read.csv('../../data/test_pros.txt', sep='\t')
  checkResults('SimardRateOfSpreadPoint',
               SimardRateOfSpreadPoint(test_pros))
})
