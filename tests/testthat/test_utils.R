context("utils")
# get data ----------------------------------------------------------------

branch = "dev"

DF = mtcars

p = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
)


# tests -------------------------------------------------------------------

test_that("listifyDF returns correct object type and throws errors", {
  expect_is(.listifyDF(DF = DF), 'data.frame')
  expect_is(.listifyDF(DF = DF)[[1]], 'list')
  expect_error(.listifyDF(DF = 1))
})

test_that("listifyDF does not change the dimensions", {
  expect_equal(dim(.listifyDF(DF)), dim(DF))
})

test_that(".expandPath returns correct object type and throws errors", {
  expect_is(.expandPath(path = "~/UVA/"), 'character')
  expect_error(.expandPath(1))
  expect_error(.expandPath("~/$HOME/test/$NonExistentVar"))
})

test_that("strformat returns correct object type and throws errors", {
  expect_is(.strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi", VAR2 = "hello")), "character")
  expect_error(.strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi")))
  expect_error(.strformat(1))
})

test_that("makeMetadataSectionAbsolute returns correct object type and throws errors",
          {
            expect_is(.makeMetadataSectionAbsolute(p@config, dirname(p@file)), 'list')
            expect_error(.makeMetadataSectionAbsolute(p@file, 1))
          })

test_that("makeMetadataSectionAbsolute does not change the length(s) of the list",
          {
            expect_equal(as.numeric(lapply(p@config$metadata, length)), as.numeric(lapply(
              .makeMetadataSectionAbsolute(p@config, dirname(p@file)), length
            )))
          })

test_that(".isAbsolute returns correct object type and throws errors", {
  expect_is(.isAbsolute("/home/mjs5kd"), 'logical')
  expect_error(.isAbsolute(1))
})

test_that(".isAbsolute works properly", {
  expect_true(.isAbsolute("/home/mjs5kd"))
  expect_false(.isAbsolute("UVA/data"))
})

test_that("printNestedList throws errors", {
  expect_error(.printNestedList(1))
})

test_that("fetchSamples throws errors", {
  expect_error(fetchSamples(samples = samples(p),attr="test",func = function(x){},action = "include"))
  expect_error(fetchSamples(samples = samples(p),attr="file",func = function(x){stop("test")},action = "include"))
})

test_that("fetchSamples returns correct rows count", {
  expect_equal(NROW(fetchSamples(samples = samples(p),attr="file",func = function(x){grep("frog1", x)},action = "include")),1)
  expect_equal(NROW(fetchSamples(samples = samples(p),attr="sample_name",func = function(x){which(x=="frog_1")},action = "include")),1)
})
