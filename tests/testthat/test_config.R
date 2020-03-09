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

cfgSubproj = system.file(
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

configConst = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_append",
    "project_config.yaml",
    package = "pepr"
)

# tests -------------------------------------------------------------------


test_that("loadConfig returns correct object type", {
    expect_is(Config(
        system.file(
            "extdata",
            paste0("example_peps-",branch),
            "example_basic",
            "project_config.yaml",
            package = "pepr"
        )
    ), 'Config')
    expect_is(Config(
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
    expect_error(Config("a"))
    expect_error(Config(filename = Project(cfg)@config$sample_table))
})
