skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)

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


# .read_inventory_data ------------------------------------------------------------------------
test_that(".read_inventory_data returns lazy_dt for fia", {
  test_file <- fs::path(Sys.getenv("fia_path"), "OR_PLOT.csv")
  test_cmd <- glue::glue('grep -E ",INVYR,|,25,(84167|84167.0)," {test_file}')

  expect_s3_class(.read_inventory_data(test_file), "dtplyr_step_first")
  expect_s3_class(test_res <- .read_inventory_data(test_cmd, .ifn = FALSE), "dtplyr_step_first")
  expect_true(nrow(test_res) > 0)

  # wrong one
  test_cmd <- glue::glue('grep -E ",INVYR,|,25,(tururu|tururu.0)," {test_file}')
  expect_s3_class(test_res <- .read_inventory_data(test_cmd, .ifn = FALSE), "dtplyr_step_first")
  expect_false(nrow(test_res) > 0)
})

test_that(".read_inventory_data returns lazy_dt for ffi", {
  # placette table
  test_file <- fs::path(Sys.getenv("ffi_path"), "PLACETTE.csv")
  test_cmd <-
    glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{2}})900863;((?:[^;]+;){{2}})10" {test_file}')

  expect_s3_class(.read_inventory_data(test_file, .ifn = FALSE, header = TRUE), "dtplyr_step_first")
  expect_s3_class(
    test_res <- .read_inventory_data(test_cmd, .ifn = FALSE, header = TRUE), "dtplyr_step_first"
  )
  expect_true(nrow(test_res) > 0)

  # ARBRE, BOIS_MORT, COUVERT, ECOLOGIE, FLORE and HABITAT tables
  test_file <- fs::path(Sys.getenv("ffi_path"), "ECOLOGIE.csv")
  test_cmd <- glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{1}})900863;" {test_file}')

  expect_s3_class(.read_inventory_data(test_file, .ifn = FALSE, header = TRUE), "dtplyr_step_first")
  expect_s3_class(
    test_res <- .read_inventory_data(test_cmd, .ifn = FALSE, header = TRUE), "dtplyr_step_first"
  )
  expect_true(nrow(test_res) > 0)

  # wrong plot or department
  test_cmd <- glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{1}})tururu;" {test_file}')
  expect_s3_class(
    test_res <- .read_inventory_data(test_cmd, .ifn = FALSE, header = TRUE), "dtplyr_step_first"
  )
  expect_false(nrow(test_res) > 0)
})

test_that(".read_inventory_data returns lazy_dt for ifn", {
  # IFN2
  test_file_ifn2 <- fs::path(Sys.getenv("ifn_path"), "PIESMA24.DBF")
  test_colnames_ifn2 <- c(
    "PROVINCIA",
    "ESTADILLO",
    "ESPECIE",
    "NUMORDEN",
    "ARBOL",
    "DIAMETRO1",
    "DIAMETRO2",
    "ALTURA"
  )
  expect_s3_class(
    test_res_ifn2 <- .read_inventory_data(
      test_file_ifn2, test_colnames_ifn2,
      version = "ifn2", province = "24",
      .ifn = TRUE
    ),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn2) > 0)

  # IFN3
  test_file_ifn3 <- fs::path(Sys.getenv("ifn_path"), "Ifn3p24.accdb")
  test_input_ifn3 <- glue::glue("{test_file_ifn3}|PCMayores")
  test_colnames_ifn3 <- c(
    "Estadillo",
    "Cla",
    "Subclase",
    "Especie",
    "nArbol",
    "OrdenIf3",
    "OrdenIf2",
    "Dn1",
    "Dn2",
    "Ht",
    "Calidad",
    "Forma"
  )
  expect_s3_class(
    test_res_ifn3 <- .read_inventory_data(
      test_input_ifn3, test_colnames_ifn3,
      version = "ifn3", province = "24",
      .ifn = TRUE
    ),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn3) > 0)

  # IFN4
  test_file_ifn4 <- fs::path(Sys.getenv("ifn_path"), "Ifn4_Lugo.accdb")
  test_input_ifn4 <- glue::glue("{test_file_ifn4}|PCMayores")
  test_colnames_ifn4 <- c(
    "Provincia",
    "Estadillo",
    "Cla",
    "Subclase",
    "Especie",
    "nArbol",
    "OrdenIf3",
    "OrdenIf4",
    "Dn1",
    "Dn2",
    "Ht",
    "Calidad",
    "Forma"
  )

  expect_s3_class(
    test_res_ifn4 <- .read_inventory_data(
      test_input_ifn4, test_colnames_ifn4,
      version = "ifn4", province = "27",
      .ifn = TRUE
    ),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn4) > 0)
})

# show_plots_from -----------------------------------------------------------------------------
test_that("show_plots_from works as intended", {
  test_departments <- c("01", "10")
  test_provinces <- c("08", "24")
  test_states <- c("OR", "CA")
  test_versions <- c("ifn2", "ifn3", "ifn4")

  expect_s3_class(
    test_ffi <- show_plots_from("FFI", Sys.getenv("ffi_path"), test_departments), "sf"
  )
  expect_s3_class(
    test_fia <- show_plots_from("FIA", Sys.getenv("fia_path"), test_states), "sf"
  )
  expect_s3_class(
    test_ifn <- show_plots_from("IFN", Sys.getenv("ifn_path"), test_provinces, test_versions), "sf"
  )

  # rows
  expect_true(nrow(test_ffi) > 0)
  expect_true(nrow(test_fia) > 0)
  expect_true(nrow(test_ifn) > 0)

  # crs
  expect_identical(sf::st_crs(test_ffi), sf::st_crs(4326))
  expect_identical(sf::st_crs(test_fia), sf::st_crs(4326))
  expect_identical(sf::st_crs(test_ifn), sf::st_crs(4326))

  # names
  expect_named(
    test_ffi, c("CAMPAGNE", "IDP", "DEP", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    test_fia, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    test_ifn,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )

  # versions
  expect_true(all(test_versions %in% unique(test_ifn$version)))

  # admin
  expect_identical(test_ffi$DEP |> unique() |> sort(), test_departments |> sort())
  expect_identical(test_fia$STATEAB |> unique() |> sort(), test_states |> sort())
  expect_identical(test_ifn$province_code |> unique() |> sort(), test_provinces |> sort())
})


# create_filter_list --------------------------------------------------------------------------
test_that("show_plots_from works as intended", {
  test_departments <- c("01", "10")
  test_provinces <- c("08", "24")
  test_states <- c("OR", "CA")
  test_versions <- c("ifn2", "ifn3", "ifn4")

  test_ffi <- show_plots_from("FFI", Sys.getenv("ffi_path"), test_departments)
  test_fia <- show_plots_from("FIA", Sys.getenv("fia_path"), test_states)
  test_ifn <- show_plots_from("IFN", Sys.getenv("ifn_path"), test_provinces, test_versions)

  # list
  expect_type(test_ffi_res <- create_filter_list(test_ffi), "list")
  expect_type(test_fia_res <- create_filter_list(test_fia), "list")
  expect_type(test_ifn_res <- create_filter_list(test_ifn), "list")

  # elements
  expect_named(test_ffi_res, test_departments, ignore.order = TRUE)
  expect_named(test_fia_res, test_states, ignore.order = TRUE)
  expect_named(test_ifn_res, test_provinces, ignore.order = TRUE)

  # length of elments
  expect_true(length(test_ffi_res[[1]]) > 0)
  expect_true(length(test_fia_res[[1]]) > 0)
  expect_true(length(test_ifn_res[[1]]) > 0)
  expect_true(length(test_ffi_res[[2]]) > 0)
  expect_true(length(test_fia_res[[2]]) > 0)
  expect_true(length(test_ifn_res[[2]]) > 0)
})

test_that("clean_empty works as expected", {
  # clean empty method per se is tested in each inventory tests.
  # Here we only test the assertions
  inventory_data_test <- tibble::tibble(
    tree = list(), regen = list(), understory = list()
  )
  inventory_data_no_tree <- inventory_data_test |>
    dplyr::select(-tree)
  inventory_data_no_regen <- inventory_data_test |>
    dplyr::select(-regen)
  inventory_data_no_understory <- inventory_data_test |>
    dplyr::select(-understory)

  # inventory_data assertions
  expect_error(
    clean_empty(inventory_data_no_tree, c("tree", "regen", "understory")),
    "must have columns"
  )
  expect_error(
    clean_empty(inventory_data_no_regen, c("tree", "regen", "understory")),
    "must have columns"
  )
  expect_error(
    clean_empty(inventory_data_no_understory, c("tree", "regen", "understory")),
    "must have columns"
  )

  # cols assertions
  expect_error(
    clean_empty(inventory_data_test, cols = 25),
    "must be one or more"
  )
  expect_error(
    clean_empty(inventory_data_test, cols = c("tree", "regen", "tururu")),
    "must be one or more"
  )
})

test_that("inventory_as_sf works as intended", {
  # conversion to sf method per se is tested in each inventory tests.
  # Here we only test the assertions
  inventory_data_test <- tibble::tibble(
    COORD1 = numeric(), COORD2 = numeric(), crs = numeric()
  )
  inventory_data_no_coord1 <- inventory_data_test |>
    dplyr::select(-COORD1)
  inventory_data_no_coord2 <- inventory_data_test |>
    dplyr::select(-COORD2)
  inventory_data_no_crs <- inventory_data_test |>
    dplyr::select(-crs)
  
  expect_error(
    inventory_as_sf(inventory_data_no_coord1),
    "must have columns for coordinates and crs"
  )
  expect_error(
    inventory_as_sf(inventory_data_no_coord2),
    "must have columns for coordinates and crs"
  )
  expect_error(
    inventory_as_sf(inventory_data_no_crs),
    "must have columns for coordinates and crs"
  )
})