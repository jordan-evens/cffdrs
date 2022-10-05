test_that("fbp_01", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp)
  expected <- read.csv("../data/fbp_01.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_02", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,output="Primary")
  expected <- read.csv("../data/fbp_02.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_03", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,"P")
  expected <- read.csv("../data/fbp_03.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_04", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,"Secondary")
  expected <- read.csv("../data/fbp_04.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_05", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,"S")
  expected <- read.csv("../data/fbp_05.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_06", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,"All")
  expected <- read.csv("../data/fbp_06.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_07", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp,"A")
  expected <- read.csv("../data/fbp_07.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_08", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp[7,])
  expected <- read.csv("../data/fbp_08.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_09", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp(test_fbp[8:13,])
  expected <- read.csv("../data/fbp_09.csv")
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_10", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  actual <- fbp()
  expected <- read.csv("../data/fbp_10.csv")
  expected$ID <- as.character(expected$ID)
  test_columns(actual, expected)
  expect_equal(actual, expected)
})
test_that("fbp_11", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  non_fuel <- copy(test_fbp)
  non_fuel$FuelType <- "NF"
  checkResults("fbp_11", fbp(non_fuel,"All"))
})
test_that("fbp_12", {
  library(data.table)
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  water <- copy(test_fbp)
  water$FuelType <- "WA"
  checkResults("fbp_12", fbp(water,"All"))
})
