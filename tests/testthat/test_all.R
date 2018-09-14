context("TEST ALL")

# Prep data ---------------------------------------------------------------


DF = mtcars
newDF = listifyDF(DF)
p = Project(
  file = system.file(
    "extdata",
    "example_peps-master",
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
)
p_file_missing = p
p_file_missing@config$metadata$sample_annotation = "missing"
p_subproj1 = Project(
  file = system.file(
    "extdata",
    "example_peps-master",
    "example_subprojects1",
    "project_config.yaml",
    package = "pepr"
  )
)
p_subproj2 = Project(
  file = system.file(
    "extdata",
    "example_peps-master",
    "example_subprojects2",
    "project_config.yaml",
    package = "pepr"
  )
)
p_sub = Project(
  file = system.file(
    "extdata",
    "example_peps-master",
    "example_subannotation1",
    "project_config.yaml",
    package = "pepr"
  )
)
p_implied = Project(
  file = system.file(
    "extdata",
    "example_peps-master",
    "example_implied",
    "project_config.yaml",
    package = "pepr"
  )
)
# Test --------------------------------------------------------------------


test_that("listifyDF returns correct object type and throws errors", {
  expect_is(listifyDF(DF = DF), 'data.frame')
  expect_is(listifyDF(DF = DF)[[1]], 'list')
  expect_error(listifyDF(DF = 1))
})

test_that("listifyDF does not change the dimensions", {
  expect_equal(dim(listifyDF(DF)), dim(DF))
})

test_that("expandPath returns correct object type and throws errors", {
  expect_is(expandPath(path = "~/UVA/"), 'character')
  expect_error(expandPath(1))
})

test_that("listSubprojects returns correct object type, length and throws errors",
          {
            expect_equal(length(listSubprojects(p_subproj1@config)), 2)
            expect_is(listSubprojects(p_subproj2@config), 'character')
            expect_equal(listSubprojects(p@config), NULL)
            expect_error(listSubprojects(1))
          })

test_that("loadConfig returns correct object type", {
  expect_is(loadConfig(
    system.file(
      "extdata",
      "example_peps-master",
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'Config')
  expect_is(loadConfig(
    system.file(
      "extdata",
      "example_peps-master",
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'list')
})

test_that("loadConfig throws errors", {
  expect_error(loadConfig(filename = "a"))
  expect_error(loadConfig(filename = p@config$metadata$sample_annotation))
})

test_that("strformat returns correct object type and throws errors", {
  expect_is(strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi", VAR2 = "hello")), "character")
  expect_error(strformat("{VAR1}{VAR2}_file", list(VAR1 = "hi")))
  expect_error(strformat(1))
})

test_that("makeMetadataSectionAbsolute returns correct object type and throws errors",
          {
            expect_is(makeMetadataSectionAbsolute(p@config, dirname(p@file)), 'list')
            expect_error(makeMetadataSectionAbsolute(p@file, 1))
          })

test_that("makeMetadataSectionAbsolute does not change the length(s) of the list",
          {
            expect_equal(as.numeric(lapply(p@config$metadata, length)), as.numeric(lapply(
              makeMetadataSectionAbsolute(p@config, dirname(p@file)), length
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
  expect_error(printNestedList(1))
})

test_that("Project throws errors", {
  expect_error(Project(file = p@config$metadata$sample_annotation))
})

test_that("Project creates an object of class Project", {
  expect_is(p, 'Project')
})

test_that("getSubsample method throws errors", {
  expect_error(getSubsample(mtcars))
  expect_error(getSubsample(p, "frog_1", "test"))
})

test_that("getSubsample method returns a correct size DF", {
  expect_equal(dim(getSubsample(p_sub, "frog_1", "sub_a")), c(1, 4))
})

test_that(".loadSampleAnnotation returns a data.frame", {
  expect_is(.loadSampleAnnotation(p), 'data.frame')
})

test_that(".loadSampleAnnotation thorws an error when file not fond", {
  expect_error(.loadSampleAnnotation(p_file_missing))
})

test_that(".loadSamplesubnnotation always returns a data.frame", {
  expect_is(.loadSampleSubannotation(p), 'data.frame')
  expect_is(.loadSampleSubannotation(p_subproj2), 'data.frame')
  expect_is(.loadSampleSubannotation(p_sub), 'data.frame')
})

test_that(".implyColumns returns Project object", {
  expect_is(.implyColumns(p_implied), 'Project')
})

test_that(".implyColumns returns Project object", {
  expect_is(.deriveColumns(p), 'Project')
})
