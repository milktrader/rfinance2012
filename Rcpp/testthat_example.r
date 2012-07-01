suppressMessages(require(testthat))
context('that we test')

a = 1

test_that("one a is equal to 1", {
  expect_that(1, equals(a))
})

colourise("Red", "red")
