context("Project object creation/loadConfig")

# get data ----------------------------------------------------------------


branch = "dev"

p = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
)
yaml = yaml.load_file(system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subprojects2",
  "project_config.yaml",
  package = "pepr"
))
p_yaml=Project(system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subprojects2",
  "project_config.yaml",
  package = "pepr"
))

# tests -------------------------------------------------------------------


test_that("loadConfig returns correct object type", {
  expect_is(.loadConfig(
    system.file(
      "extdata",
      "example_peps-dev",
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'Config')
  expect_is(.loadConfig(
    system.file(
      "extdata",
      "example_peps-dev",
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'list')
})

test_that("loadConfig throws errors", {
  expect_error(.loadConfig(filename = "a"))
  expect_error(.loadConfig(filename = p@config$metadata$sample_table))
})

test_that("Project throws errors", {
  expect_error(Project(file = p@config$metadata$sample_table))
})

test_that("Project creates an object of class Project", {
  expect_is(p, 'Project')
})

test_that("Project (loadConfig) produces a proper config file.
          YAML read config has to consist of list elements of the same length
          as the config processed with the Project constructor", {
            expect_equal(unlist(lapply(config(p_yaml)$metadata,length)),
                         unlist(lapply(yaml$metadata,length)))
            })
