context("Utils")

DF=mtcars
newDF=listifyDF(DF)

test_that("listifyDF is a yields correct object type", {
  expect_is(listifyDF(DF = DF),'data.frame')
  expect_is(listifyDF(DF = DF)[[1]],'list')
})

test_that("listifyDF does not change the dimensions",{
  expect_equal(dim(listifyDF(DF)), dim(DF))
})