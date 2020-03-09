# get data ----------------------------------------------------------------


branch = "cfg2"

cfg = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )

yaml = yaml::yaml.load_file(system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_amendments2",
  "project_config.yaml",
  package = "pepr"
))

cfgAmend = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_amendments2",
  "project_config.yaml",
  package = "pepr"
)

cfgSubtable = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
)

cfgBioc = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_BiocProject",
    "project_config.yaml",
    package = "pepr"
)

configAppend = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_append",
    "project_config.yaml",
    package = "pepr"
)

configDerive = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_derive",
  "project_config.yaml",
  package = "pepr"
)

configImply = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_imply",
  "project_config.yaml",
  package = "pepr"
)

configDuplicate = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_duplicate",
  "project_config.yaml",
  package = "pepr"
)

configImports = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_imports",
  "project_config.yaml",
  package = "pepr"
)

# tests -------------------------------------------------------------------

context("Project object creation")

test_that("Project throws errors", {
  expect_error(Project(file = Project(cfg)@config$sample_table))
})

test_that("Project creates an object of class Project", {
  expect_is(Project(cfg), 'Project')
})

test_that("Project successfully loads a config with bioconductor section", {
    expect_true(is(Project(cfgBioc), "Project"))
})

context("Amendments")

test_that("Project succesfully activates amendments at initialization", {
    expect_equal(NROW(sampleTable(Project(cfgAmend, amendments = "noFrog"))), 4)
})

context("Modifiers: append")

test_that("append modifier works", {
    expect_equal(length(sampleTable(Project(configAppend))[["read_type"]]),4)
})

context("Modifiers: derive")

test_that("derive modifier works", {
  expect_true(all(lapply(sampleTable(Project(configDerive))[["file_path"]], function(x){x=="source1"}))==FALSE)
})

context("Modifiers: imply")

test_that("imply modifier works", {
  s = sampleTable(Project(configImply))
  expect_true(all(s[which(s[,"organism"]=="human") ,"genome"] == "hg38"))
})

context("Modifiers: duplicate")


test_that("duplicate modifier works", {
  s = sampleTable(Project(configDuplicate))
  expect_true(all(s[,"organism"]==s[,"animal"]))
})

context("Import external configs")


test_that("importing external configs works", {
  expect_true(all(sampleTable(Project(configImports))[,"imported_attr"] == "imported_val"))
})
