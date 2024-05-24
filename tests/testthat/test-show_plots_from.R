skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# FIA -----------------------------------------------------------------------------------------

test_that("show_plots_from works for FIA", {

  # we skip this test on CRAN as we don't have access to the data there
  skip_on_cran()

  # test data
  test_inventory <- "FIA"
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("OR", "CA", "WA")

  # tests
  expect_no_error(
    test_plots <- show_plots_from(test_inventory, test_folder, states = test_states)
  )
  expect_s3_class(test_plots, "sf")
  expect_named(test_plots, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  expect_true(all(unique(test_plots[["STATECD"]]) %in% c(6L, 41L, 53L)))

  # Now warnings and errors
  expect_error(
    suppressWarnings(show_plots_from(test_inventory, ".", states = test_states)),
    "No data found at"
  )
  expect_error(
    suppressWarnings(show_plots_from(test_inventory, test_folder, states = "UZ")),
    "No data found at"
  )
  expect_warning(
    show_plots_from(test_inventory, test_folder, states = c(test_states, "UZ")),
    "UZ"
  )
})

test_that("show_plots_from works for FFI", {

  # we skip this test on CRAN as we don't have access to the data there
  skip_on_cran()

  # test data
  test_inventory <- "FFI"
  test_folder <- Sys.getenv("ffi_path")
  test_departments <- c("01", "10", "11")

  # tests
  expect_no_error(
    test_plots <- show_plots_from(test_inventory, test_folder, departments = test_departments)
  )
  expect_s3_class(test_plots, "sf")
  expect_named(test_plots, c("CAMPAGNE", "IDP", "DEP", "geometry"))
  expect_true(all(unique(test_plots[["DEP"]]) %in% test_departments))

  # Now warnings and errors
  expect_error(
    suppressWarnings(show_plots_from(test_inventory, ".", departments = test_departments)),
    "contain any file named"
  )
  expect_error(
    suppressWarnings(show_plots_from(test_inventory, test_folder, departments = "99")),
    "contain any plot"
  )
  expect_warning(
    show_plots_from(test_inventory, test_folder, departments = c(test_departments, "99")),
    "99"
  )
})

test_that("show_plots_from works for IFN", {

  # we skip this test on CRAN as we don't have access to the data there
  skip_on_cran()

  # test data
  test_inventory <- "IFN"
  test_folder <- Sys.getenv("ifn_path")
  test_provinces <- c("08", "24", "01")

  # tests
  expect_no_error(
    test_plots <-
      show_plots_from(test_inventory, test_folder, provinces = test_provinces, version = "ifn3")
  )
  expect_s3_class(test_plots, "sf")
  expect_named(
    test_plots,
    c("crs", "id_unique_code", "version", "province_code", "province_name_original", "plot", "geometry")
  )
  expect_true(all(unique(test_plots[["province_code"]]) %in% test_provinces))

  # Now warnings and errors
  expect_error(
    suppressWarnings(
      show_plots_from(test_inventory, ".", provinces = test_provinces, version = "ifn3")
    ),
    "No data found at"
  )
  expect_error(
    suppressWarnings(
      show_plots_from(test_inventory, test_folder, provinces = "99", version = "ifn3")
    ),
    "No data found at"
  )
  # there is a double warning so I can't test
  # expect_warning(
  #   show_plots_from(
  #     test_inventory, test_folder, provinces = c(test_provinces, "99"), version = "ifn3"
  #   ),
  #   "99"
  # )
})
