skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# FIA -----------------------------------------------------------------------------------------

test_that("show_plots_from works for FIA", {

  # we skip this test on CRAN as we don't have access to the data there
  skip_on_cran()

  # test data
  inventory <- "FIA"
  folder <- Sys.getenv("fia_path")
  states <- c("OR", "CA", "WA")

  # tests
  expect_no_error(
    test_plots <- show_plots_from(inventory, folder, states = states)
  )
  expect_s3_class(test_plots, "sf")
  expect_named(test_plots, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  expect_true(all(unique(test_plots[["STATECD"]]) %in% c(6L, 41L, 53L)))

  # test data
  inventory <- "FFI"
  folder <- Sys.getenv("ffi_path")
  departments <- c("01", "10", "11")

  # tests
  expect_no_error(
    test_plots <- show_plots_from(inventory, folder, departments = departments)
  )
  expect_s3_class(test_plots, "sf")
  expect_named(test_plots, c("CAMPAGNE", "IDP", "DEP", "geometry"))
  expect_true(all(unique(test_plots[["DEP"]]) %in% c("01", "10", "11")))
})
