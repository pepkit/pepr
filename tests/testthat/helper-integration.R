# Helper for opt-in integration tests that hit the live PEPhub API.
#
# Integration tests are skipped unless PEPR_INTEGRATION_TESTS=true. They are
# also always skipped on CRAN and when offline, so they never run during
# `R CMD check` on CRAN. Their purpose is to verify the real PEPhub API
# contract (e.g. response key names) that mocked unit tests cannot catch.
skip_if_no_integration <- function() {
  skip_on_cran()
  if (!identical(tolower(Sys.getenv("PEPR_INTEGRATION_TESTS")), "true")) {
    skip("Integration tests disabled; set PEPR_INTEGRATION_TESTS=true to run.")
  }
  skip_if_offline()
}
