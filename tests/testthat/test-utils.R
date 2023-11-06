# Verbose messaging ---------------------------------------------------------------------------

test_that("verbose_msg works as intended", {

  # .verbose with message works
  expect_message(
    verbose_msg(message("foo"), .verbose = TRUE), "foo"
  )
  expect_no_message(
    verbose_msg(message("foo"), .verbose = FALSE)
  )

  # cli::* works
  expect_message(
    verbose_msg(cli::cli_inform(".verbose is TRUE"), .verbose = TRUE), ".verbose is TRUE"
  )
  expect_no_message(
    verbose_msg(cli::cli_inform(".verbose is FALSE"), .verbose = FALSE)
  )

  # expect errors if something is wrong
  expect_error(
    verbose_msg(message("foo"), .verbose = 4)
  )
  expect_error(
    verbose_msg(message("foo"), .verbose = "4")
  )
})

test_that(".read_inventory_data returns lazy_dt for fia", {
  test_file <- fs::path("/data/creaf/projects/emf/international_inventories/data/fia/FIA_DATAMART_MARCH_2023/OR_PLOT.csv")
  test_cmd <- glue::glue("grep -E ',INVYR,|,25,(84167|84167.0),' {test_file}")

  expect_s3_class(.read_inventory_data(test_file), "dtplyr_step_first")
  expect_s3_class(test_res <- .read_inventory_data(test_cmd), "dtplyr_step_first")
  expect_true(nrow(test_res) > 0)

  # wrong one
  test_cmd <- glue::glue("grep -E ',INVYR,|,25,(tururu|tururu.0),' {test_file}")
  expect_s3_class(test_res <- .read_inventory_data(test_cmd), "dtplyr_step_first")
  expect_false(nrow(test_res) > 0)
})

test_that(".read_inventory_data returns lazy_dt for ffi", {
  # placette table
  test_file <- fs::path(Sys.getenv("ffi_path"), "PLACETTE.csv")
  test_cmd <- glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{2}})900863;((?:[^;]+;){{2}})10" {test_file}')

  expect_s3_class(.read_inventory_data(test_file, header = TRUE), "dtplyr_step_first")
  expect_s3_class(test_res <- .read_inventory_data(test_cmd, header = TRUE), "dtplyr_step_first")
  expect_true(nrow(test_res) > 0)

  # ARBRE, BOIS_MORT, COUVERT, ECOLOGIE, FLORE and HABITAT tables
  test_file <- fs::path(Sys.getenv("ffi_path"), "ECOLOGIE.csv")
  test_cmd <- glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{1}})900863;" {test_file}')

  expect_s3_class(.read_inventory_data(test_file, header = TRUE), "dtplyr_step_first")
  expect_s3_class(test_res <- .read_inventory_data(test_cmd, header = TRUE), "dtplyr_step_first")
  expect_true(nrow(test_res) > 0)

  # wrong plot or department
  test_cmd <- glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{1}})tururu;" {test_file}')
  expect_s3_class(test_res <- .read_inventory_data(test_cmd, header = TRUE), "dtplyr_step_first")
  expect_false(nrow(test_res) > 0)
})

test_that(".read_inventory_data returns lazy_dt for ifn", {
  # IFN2
  test_file_ifn2 <- fs::path(Sys.getenv("ifn_path"), "PIESMA24.DBF")

  expect_s3_class(test_res_ifn2 <- .read_inventory_data(test_file_ifn2, .ifn = TRUE), "dtplyr_step_first")
  expect_true(nrow(test_res_ifn2) > 0)

  # IFN3
  test_file_ifn3 <- fs::path(Sys.getenv("ifn_path"), "Ifn3p24.accdb")

  expect_s3_class(
    test_res_ifn3 <- .read_inventory_data(test_file_ifn3, tables = "PCMayores", .ifn = TRUE),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn3) > 0)

  # IFN4
  test_file_ifn4 <- fs::path(Sys.getenv("ifn_path"), "Ifn4_Leвn.accdb")

  expect_s3_class(
    test_res_ifn4 <- .read_inventory_data(test_file_ifn4, tables = "PCMayores", .ifn = TRUE),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn4) > 0)
})
