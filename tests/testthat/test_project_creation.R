context("Project object creation/loadConfig")

# get data ----------------------------------------------------------------


branch = "master"

cfg = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )

yaml = yaml.load_file(system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subprojects2",
  "project_config.yaml",
  package = "pepr"
))

cfgSubproj = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subprojects2",
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

configConst = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_constants",
    "project_config.yaml",
    package = "pepr"
)

# tests -------------------------------------------------------------------


test_that("loadConfig returns correct object type", {
  expect_is(.loadConfig(
    system.file(
      "extdata",
      paste0("example_peps-",branch),
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'Config')
  expect_is(.loadConfig(
    system.file(
      "extdata",
      paste0("example_peps-",branch),
      "example_basic",
      "project_config.yaml",
      package = "pepr"
    )
  ), 'list')
})

test_that("loadConfig throws errors", {
  expect_error(.loadConfig(filename = "a"))
  expect_error(.loadConfig(filename = Project(cfg)@config$metadata$sample_table))
})

test_that("Project throws errors", {
  expect_error(Project(file = Project(cfg)@config$metadata$sample_table))
})

test_that("Project creates an object of class Project", {
  expect_is(p, 'Project')
})

test_that("Project (loadConfig) produces a proper config file.
          YAML read config has to consist of list elements of the same length
          as the config processed with the Project constructor", {
            expect_equal(unlist(lapply(config(Project(cfgSubproj))$metadata, 
                                       length)), unlist(lapply(yaml$metadata,
                                                               length)))
            })

test_that("Project successfully loads a config with bioconductor section", {
    expect_true(is(Project(cfgBioc), "Project"))
})

test_that("Project succesfully activates subproject at initialization", {
    expect_equal(NROW(samples(Project(cfgSubproj, subproject = "noFrog"))), 4)
})

test_that("constant attributes work", {
    expect_equal(length(samples(Project(configConst))[["read_type"]]),4)
})

test_that(".loadSampleAannotation retains backwards compatibility and warns", {
    p=Project(cfg)
    names(p@config$metadata)[which(
        names(p@config$metadata) == "sample_table")] = "sample_annotation"
    expect_warning(.loadSampleAnnotation(p))
})

test_that(".loadSampleAannotation does not allow for missing sample_table 
          if no subprojects defined", {
    p=Project(cfg)
    p@config$metadata = 
        p@config$metadata[-which(names(p@config$metadata) == "sample_table")]
    expect_error(.loadSampleAnnotation(p))
})

test_that(".loadSampleSubannotation allows for missing sample_table if subprojects
          are defined", {
              p=Project(cfgSubtable)
              names(p@config$metadata)[
                  which(names(p@config$metadata) == 
                            "subsample_table")] = "sample_subannotation"
              expect_warning(.loadSampleSubannotation(p))
})

