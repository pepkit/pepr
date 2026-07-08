# Live integration tests against the PEPhub API.
#
# These are OPT-IN and never run on CRAN. Enable them with:
#   PEPR_INTEGRATION_TESTS=true
# They exercise the real network path and assert the actual response contract,
# so a change to the PEPhub API (e.g. renamed response keys) fails loudly here
# instead of silently in a mocked unit test or on CRAN's example check.

context("Live PEPhub integration")

test_that("pullProject fetches a project from the live PEPhub API", {
  skip_if_no_integration()

  p = pullProject(registryPath = "databio/example:default")

  expect_is(p, "Project")
  # Assert the response actually populated samples. If the API changes its
  # response schema (as happened with sample_list -> samples), pullProject
  # would return zero samples or error, and these expectations would fail.
  expect_gt(nrow(sampleTable(p)), 0)
  expect_true("sample_name" %in% colnames(sampleTable(p)))
})
