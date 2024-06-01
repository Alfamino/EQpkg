library(testthat)
context("R test")

add_numbers <- function(a,b){
  a+b
}

test_that("General test add_numbers adds two numbers correctly", {
  result <- add_numbers(2, 3)
  expect_equal(result, 5)
})


context("EQ library small test")

library(EQpkg)
test_check("EQpkg")

