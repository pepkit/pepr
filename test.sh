packageName="pepr"
packageFolder=paste0("~/code/", packageName)
devtools::document(packageFolder)

Then, check and test it with these commands:

devtools::test(packageFolder)
devtools::run_examples(packageFolder)
devtools::check(packageFolder)
devtools::build_vignettes(packageFolder)