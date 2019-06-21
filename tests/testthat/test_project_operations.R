context("Project operations")

# get data ----------------------------------------------------------------

branch = "master"

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
pFileMissing = p
pFileMissing@config$metadata$sample_table = "missing"
pSubproj1 = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subprojects1",
    "project_config.yaml",
    package = "pepr"
  )
)
pSubproj2 = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subprojects2",
    "project_config.yaml",
    package = "pepr"
  )
)
pSub = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
  )
)
pImplied = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_implied",
    "project_config.yaml",
    package = "pepr"
  )
)

pDerived = Project(
    file = system.file(
        "extdata",
        paste0("example_peps-",branch),
        "example_derived",
        "project_config.yaml",
        package = "pepr"
    )
)

# tests -------------------------------------------------------------------

test_that("getSubsample method throws errors", {
  expect_error(getSubsample(mtcars))
  expect_error(getSubsample(p, "frog_1", "test"))
})

test_that("getSubsample method returns a correct size DF", {
  expect_equal(dim(getSubsample(pSub, "frog_1", "sub_a")), c(1, 4))
})

test_that(".loadSampleAnnotation returns a Project object", {
  expect_is(.loadSampleAnnotation(p), 'Project')
})

test_that(".loadSampleAnnotation thorws an error when file not fond", {
  expect_error(.loadSampleAnnotation(pFileMissing))
})

test_that(".loadSamplesubnnotation always returns a Project", {
  expect_is(.loadSampleSubannotation(p), 'Project')
  expect_is(.loadSampleSubannotation(pSubproj2), 'Project')
  expect_is(.loadSampleSubannotation(pSub), 'Project')
})

test_that(".implyAttributes returns Project object", {
  expect_is(.implyAttributes(pImplied), 'Project')
})

test_that(".deriveAttributes returns Project object", {
  expect_is(.deriveAttributes(pDerived), 'Project')
})

test_that(".listSubprojects internal function returns correct object type, length and throws errors",
          {
            expect_equal(length(.listSubprojects(pSubproj1@config)), 2)
            expect_is(.listSubprojects(pSubproj2@config), 'character')
            expect_null(.listSubprojects(p@config))
            expect_error(.listSubprojects(1))
          })

test_that("listSubprojects exported method returns correct object type, length and throws errors",
          {
            expect_equal(length(listSubprojects(pSubproj1)), 2)
            expect_is(listSubprojects(pSubproj1), 'character')
            expect_null(listSubprojects(p))
            expect_error(listSubprojects(1))
            expect_equal(length(listSubprojects(pSubproj1)), 2)
          })

test_that("checkSection returns a correct type", {
  expect_is(checkSection(config(p),"metadata"),"logical")
  expect_is(checkSection(config(p),"test"),"logical")
})

test_that("checkSection returns correct value", {
  expect_equal(checkSection(config(p),c("metadata","sample_table")), T)
  expect_equal(checkSection(config(p),c("test")), F)
})

test_that("activateSubproject does not fail and throws a warning when called 
          with invalid subproject name", {
              expect_warning(activateSubproject(pSubproj1, "test"))
})

test_that("activateSubproject returns a correct object type", {
    expect_is(activateSubproject(pSubproj1, "newLib2"), "Project")
})

test_that(".listSubprojects works with different styles of printing", {
    expect_message(.listSubprojects(config(pSubproj1)))
    expect_output(.listSubprojects(config(pSubproj1),style="cat"))
})

test_that("show methods work", {
    expect_output(show(p))
    expect_output(show(config(p)))
})

test_that("getSample errors", {
    expect_error(getSample(p, "test"))
})

test_that("getSample returns a correct object type", {
    expect_is(getSample(p,"frog_1"), "data.table")
})

test_that("getSample errors", {
    expect_error(getSubsample(p, "test","test"))
    expect_error(getSubsample(pSub,"frog_1", "test"))
})

test_that("getSample returns a correct object type", {
    expect_is(getSubsample(pSub,"frog_1", "sub_a"), "data.table")
})