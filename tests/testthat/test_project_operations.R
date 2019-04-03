context("Project operations")

# get data ----------------------------------------------------------------

DF = mtcars

p = Project(
  file = system.file(
    "extdata",
    "example_peps-dev",
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
)
p_file_missing = p
p_file_missing@config$metadata$sample_table = "missing"
p_subproj1 = Project(
  file = system.file(
    "extdata",
    "example_peps-dev",
    "example_subprojects1",
    "project_config.yaml",
    package = "pepr"
  )
)
p_subproj2 = Project(
  file = system.file(
    "extdata",
    "example_peps-dev",
    "example_subprojects2",
    "project_config.yaml",
    package = "pepr"
  )
)
p_sub = Project(
  file = system.file(
    "extdata",
    "example_peps-dev",
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
  )
)
p_implied = Project(
  file = system.file(
    "extdata",
    "example_peps-dev",
    "example_implied",
    "project_config.yaml",
    package = "pepr"
  )
)

yaml = yaml.load_file(system.file(
  "extdata",
  "example_peps-dev",
  "example_subprojects2",
  "project_config.yaml",
  package = "pepr"
))
p_yaml=Project(system.file(
  "extdata",
  "example_peps-dev",
  "example_subprojects2",
  "project_config.yaml",
  package = "pepr"
))

# tests -------------------------------------------------------------------


test_that("getSubsample method throws errors", {
  expect_error(getSubsample(mtcars))
  expect_error(getSubsample(p, "frog_1", "test"))
})

test_that("getSubsample method returns a correct size DF", {
  expect_equal(dim(getSubsample(p_sub, "frog_1", "sub_a")), c(1, 4))
})

test_that(".loadSampleAnnotation returns a Project object", {
  expect_is(.loadSampleAnnotation(p), 'Project')
})

test_that(".loadSampleAnnotation thorws an error when file not fond", {
  expect_error(.loadSampleAnnotation(p_file_missing))
})

test_that(".loadSamplesubnnotation always returns a Project", {
  expect_is(.loadSampleSubannotation(p), 'Project')
  expect_is(.loadSampleSubannotation(p_subproj2), 'Project')
  expect_is(.loadSampleSubannotation(p_sub), 'Project')
})

test_that(".implyColumns returns Project object", {
  expect_is(.implyAttributes(p_implied), 'Project')
})

test_that(".implyColumns returns Project object", {
  expect_is(.deriveAttributes(p), 'Project')
})

test_that(".listSubprojects internal function returns correct object type, length and throws errors",
          {
            expect_equal(length(.listSubprojects(p_subproj1@config)), 2)
            expect_is(.listSubprojects(p_subproj2@config), 'character')
            expect_null(.listSubprojects(p@config))
            expect_error(.listSubprojects(1))
          })

test_that("listSubprojects exported method returns correct object type, length and throws errors",
          {
            expect_equal(length(listSubprojects(p_subproj1)), 2)
            expect_is(listSubprojects(p_subproj1), 'character')
            expect_null(listSubprojects(p))
            expect_error(listSubprojects(1))
            expect_equal(length(listSubprojects(p_subproj1)), 2)
          })

test_that("checkSection returns a correct type", {
  expect_is(checkSection(config(p),"metadata"),"logical")
  expect_is(checkSection(config(p),"test"),"logical")
})

test_that("checkSection returns correct value", {
  expect_equal(checkSection(config(p),c("metadata","sample_table")), T)
  expect_equal(checkSection(config(p),c("test")), F)
})
