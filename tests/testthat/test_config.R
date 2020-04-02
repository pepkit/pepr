# get data ----------------------------------------------------------------


branch = "cfg2"

cfg = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
)

f = yaml::yaml.load_file(system.file(
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

.isAbsolute = function(path) {
    if (!is.character(path)) stop("The path must be character")
    firstChar = substr(path, 1, 1)
    return(identical("/", firstChar) | identical("~", firstChar))
}

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

context("Path expansion")
test_that("env var in looper section are populated",{
    f$looper = list(path="$HOME/test")
    path = paste(tempdir(), "test.yaml", sep="/")
    yaml::write_yaml(file = path, x = f)
    expect_true(.isAbsolute(config(Project(file = path))$looper$path))
})

test_that("paths in looper section are made absolute",{
    f$looper = list(path="~/test")
    path = paste(tempdir(), "test.yaml", sep="/")
    yaml::write_yaml(file = path, x = f)
    x = config(Project(file = path))$looper$path
    expect_true(identical("/", substr(x, 1, 1)))
})
