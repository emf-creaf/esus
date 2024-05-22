skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
skip_if(
  Sys.which("grep") == "",
  "grep not found in system commands"
)
# test data -----------------------------------------------------------------------------------

test_plots <- list(
  "01" = 1404119,
  "10" = 900863,
  "11" = c(1436508, 1410492),
  "17" = 1416895,
  "19" = 1407238,
  "27" = c(960830, 923517),
  "2A" = 973917,
  "32" = 1425386,
  "34" = 977193,
  "35" = 1404830,
  "36" = 1438616,
  "39" = 1424577,
  "42" = c(1422347, 939340),
  "44" = 920801,
  "51" = 938482,
  "59" = 960829,
  "64" = 912307,
  "76" = 951430,
  "80" = c(1417044, 1452529),
  "81" = c(1428398, 973950),
  "86" = c(957495, 921133),
  "87" = c(975666, 979897),
  "89" = 1433956,
  "91" = c(1406115, 0),
  "tururu" = 3555
)

test_year <- 2019
test_departments <- names(test_plots)
test_folder <- Sys.getenv("ffi_path")
test_input <- .build_ffi_input_with(
  test_departments, test_year, test_plots, test_folder,
  .verbose = FALSE
)

test_metadonnees <- suppressWarnings(
  readr::read_delim(file = fs::path(test_folder, "metadonnees.csv"), skip = 331) |>
    dplyr::rename(UNITE = "// Unité") |>
    dplyr::as_tibble()
)

test_espar_cdref <- .read_inventory_data(
  fs::path(test_folder, "espar-cdref13.csv"),
  colClasses = list(character = c("// espar")),
  header = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::rename(
    ESPAR = "// espar",
    Libelle  = lib_espar
  ) |>
  #i need to change this because in the file csv it is recorded as "2" and in tree table as "02"
  dplyr::mutate(ESPAR = dplyr::case_when(
    ESPAR == "2" ~ "02",
    ESPAR == "3" ~ "03",
    ESPAR == "4" ~ "04",
    ESPAR == "5" ~ "05",
    ESPAR == "6" ~ "06",
    ESPAR == "7" ~ "07",
    ESPAR == "9" ~ "09",
    TRUE ~ ESPAR
  )) |>
  dplyr::arrange(ESPAR)


test_idp_def_ref <- .read_inventory_data(
  fs::path(test_folder, "PLACETTE.csv"),
  select = c(
    "IDP",
    "DEP"
  ),
  colClasses = list(character = c("IDP", "DEP"))
) |>
  tibble::as_tibble() |>
  unique()

test_cdref <- test_metadonnees |>
  dplyr::filter(
    UNITE == "CDREF13"
  ) |>
  dplyr::rename(CD_REF = Code, Libelle = "Libell\u00e9") |>
  dplyr::mutate(
    lib_cdref = stringr::str_remove_all(Libelle, "\\s*\\(.*?\\)")
  )

# growth_form_lignified_france comes from internal data
test_growth_form_lignified_france <- growth_form_lignified_france

#
# table functions -----------------------------------------------------------------------------
test_that("ffi_plot_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "country", "dep", "dep_name", "plot",
    "year", "visite", "coord_sys", "coordx", "coordy",
    "aspect", "slope", "lign2_pct", "lign1_pct", "herb_pct"
  )

  # object
  expect_s3_class(
    test_res <- ffi_plot_table_process(
      test_input$plot_table[1],
      test_input$soils_table[1],
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$dep), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[1])
  expect_identical(unique(test_res$dep) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_plot_table_process(
      NA_character_,
      test_input$soils_table[1],
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  expect_warning(
    test_error <- ffi_plot_table_process(
      test_input$plot_table[1],
      NA_character_,
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_plot_table_process(
      test_input$plot_table[33],
      test_input$soils_table[33],
      test_input$plots[33],
      test_year,
      test_metadonnees
    )),
    "tbl"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  # error in plot name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_plot_table_process(
      test_input$plot_table[32],
      test_input$soils_table[32],
      test_input$plots[32],
      test_year,
      test_metadonnees
    )),
    "tbl"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("ffi_tree_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "plot", "dep", "year", "tree", "espar",
    "sp_code", "sp_name", "status", "status5", "dia", "height",
    "height_last_recorded", "density_factor"
  )

  # object
  expect_s3_class(
    test_res <- ffi_tree_table_process(
      test_input$tree_table[1],
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$dep), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[1] |> as.character())
  expect_identical(unique(test_res$dep) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_tree_table_process(
      NA_character_,
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_tree_table_process(
      test_input$tree_table[33],
      test_input$plots[33],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_tree_table_process(
      test_input$tree_table[32],
      test_input$plots[32],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

test_that("ffi_shrub_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "plot", "dep", "year", "sp_code",
    "sp_name", "cover", "height", "growth_form"
  )

  # object
  expect_s3_class(
    test_res <- ffi_shrub_table_process(
      test_input$shrub_table[1],
      test_input$plots[1],
      test_year,
      test_cdref,
      test_growth_form_lignified_france,
      test_idp_def_ref
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$dep), 1)

  expect_identical(unique(test_res$year), test_year |> as.integer())
  expect_identical(unique(test_res$plot), test_input$plots[1] |> as.character())
  expect_identical(unique(test_res$dep) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_shrub_table_process(
      NA_character_,
      test_input$plots[1],
      test_year,
      test_cdref,
      test_growth_form_lignified_france,
      test_idp_def_ref
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_shrub_table_process(
      test_input$shrub_table[33],
      test_input$plots[33],
      test_year,
      test_cdref,
      test_growth_form_lignified_france,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1L)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_shrub_table_process(
      test_input$shrub_table[32],
      test_input$plots[32],
      test_year,
      test_cdref,
      test_growth_form_lignified_france,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

test_that("ffi_regen_table_process works as intended", {
  test_plots <- list(
    "38" = 900306,
    "59" = 900684,
    "91" = 0,
    "tururu" = 3555
  )
  test_year <- 2014
  test_departments <- names(test_plots)
  test_input <- .build_ffi_input_with(
    test_departments, test_year, test_plots, test_folder,
    .verbose = FALSE
  )
  expected_names <- c(
    "id_unique_code", "plot", "dep", "year", "sp_code",
    "sp_name", "cover", "dbh", "height", "growth_form"
  )

  # object
  expect_s3_class(
    test_res <- ffi_regen_table_process(
      test_input$regen_table[1],
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$dep), 1)

  expect_identical(unique(test_res$year), test_year |> as.integer())
  expect_identical(unique(test_res$plot), test_input$plots[1] |> as.character())
  expect_identical(unique(test_res$dep) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_regen_table_process(
      NA_character_,
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_regen_table_process(
      test_input$regen_table[4],
      test_input$plots[4],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1L)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ffi_regen_table_process(
      test_input$regen_table[3],
      test_input$plots[3],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)


})

# table process -------------------------------------------------------------------------------

test_that("ffi_tables_process works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # tests data
  expected_names <- c(
    "id_unique_code", "plot", "dep", "dep_name", "country",
    "visite", "year", "coordx", "coordy", "crs", "aspect",
    "slope", "coord_sys", "tree", "understory", "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_res <- suppressWarnings(ffi_tables_process(
      test_departments, test_year, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(all(unique(test_res$dep) %in% names(test_plots)))

  ### missing tables/plots
  # tururu state shouldn't appear
  # inexistent plots (91-0) shouldn't
  # be present, so 31 of 33 elements in filter list
  expect_false("tururu" %in% unique(test_res$dep))
  expect_identical(nrow(test_res), 31L)

  ### missing random files
  # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  test_folder <- fs::path(Sys.getenv("ffi_path"), "missing_files_test")
  # FLORE, without flore, the understory should be empty
  fs::file_move(fs::path(test_folder, "FLORE.csv"), fs::path(test_folder, "_FLORE.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_FLORE.csv"))) {
      fs::file_move(fs::path(test_folder, "_FLORE.csv"), fs::path(test_folder, "FLORE.csv"))
    }
  })
  expect_true(
    suppressWarnings(ffi_tables_process(
      test_departments, test_year, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ) |>
      dplyr::pull(understory) |>
      purrr::list_rbind() |>
      dplyr::pull(shrub) |>
      purrr::list_rbind() |>
      nrow()) < 1
  )
  fs::file_move(fs::path(test_folder, "_FLORE.csv"), fs::path(test_folder, "FLORE.csv"))

  # ARBRE, without ARBRE, the tree should be empty
  fs::file_move(fs::path(test_folder, "ARBRE.csv"), fs::path(test_folder, "_ARBRE.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_ARBRE.csv"))) {
      fs::file_move(fs::path(test_folder, "_ARBRE.csv"), fs::path(test_folder, "ARBRE.csv"))
    }
  })
  expect_true(
    suppressWarnings(ffi_tables_process(
      test_departments, test_year, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ) |>
      dplyr::pull(tree) |>
      purrr::list_rbind() |>
      nrow()) < 1
  )
  fs::file_move(fs::path(test_folder, "_ARBRE.csv"), fs::path(test_folder, "ARBRE.csv"))

  # ECOLOGIE, without ECOLOGIE, the soils should be empty, but also the plot info.
  fs::file_move(fs::path(test_folder, "ECOLOGIE.csv"), fs::path(test_folder, "_ECOLOGIE.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_ECOLOGIE.csv"))) {
      fs::file_move(fs::path(test_folder, "_ECOLOGIE.csv"), fs::path(test_folder, "ECOLOGIE.csv"))
    }
  })
  expect_error(
    suppressWarnings(ffi_tables_process(
      test_departments, test_year, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "Ooops!"
  )
  fs::file_move(fs::path(test_folder, "_ECOLOGIE.csv"), fs::path(test_folder, "ECOLOGIE.csv"))


  # COUVERT , without COUVERT, the REGEN should be empty if year <2015

  if (test_year < 15) {
    fs::file_move(fs::path(test_folder, "COUVERT.csv"), fs::path(test_folder, "_COUVERT.csv"))
    withr::defer({
      if (fs::file_exists(fs::path(test_folder, "_COUVERT.csv"))) {
        fs::file_move(fs::path(test_folder, "_COUVERT.csv"), fs::path(test_folder, "COUVERT.csv"))
      }
    })
    expect_true(
      suppressWarnings(ffi_tables_process(
        test_departments, test_year, test_plots, test_folder,
        .parallel_options = test_parallel_conf,
        .verbose = FALSE
      ) |>
        dplyr::pull(regen) |>
        purrr::list_rbind() |>
        nrow()) < 1
    )

    fs::file_move(fs::path(test_folder, "_COUVERT.csv"), fs::path(test_folder, "COUVERT.csv"))
  }
})

# ffi_to_tibble -------------------------------------------------------------------------------


test_that("ffi_to_tibble works as intended", {

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # tests data
  expected_names <- c(
    "id_unique_code", "plot", "dep", "dep_name", "country",
    "visite", "year", "coordx", "coordy", "crs", "aspect",
    "slope", "coord_sys", "tree", "understory", "regen"
    # "soils"
  )
  test_years <- c(2015, 2019)

  # object
  expect_s3_class(
    test_res <- suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_false("tururu" %in% unique(test_res$dep))
  expect_identical(nrow(test_res), 62L) # two plots dont exist, so 2x2=4 rows less
  expect_true(all(unique(test_res$dep) %in% names(test_plots)))
  expect_true(all(unique(test_res$year) %in% test_years))

  # for plot revisited after 5 year check that species name appear
  expect_identical(!is.na(test_res[["tree"]][[60]]$sp_name), rep(TRUE, 24))

  # tests for clean_empty and as_sf arguments
  sf_expected_names <- c(
    "id_uniquie_code", "plot", "dep", "dep_name", "country",
    "visite", "year", "crs", "aspect", "slope", "coord_sys",
    "tree", "understory", "regen", "geometry", "crs_orig"
    # "soils"
  )
  expect_s3_class(
    sf_res <- suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, test_folder,
      as_sf = TRUE, clean_empty = c("tree", "understory", "regen"),
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "sf"
  )

  expect_named(sf_res, sf_expected_names, ignore.order = TRUE)
  expect_true(nrow(sf_res) < nrow(test_res))
  expect_false(any(purrr::map_lgl(sf_res$tree, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$understory, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$regen, rlang::is_empty)))

  ### test all assertions done in fia_to_tibble
  # departments
  expect_error(
    ffi_to_tibble(
      1:7, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "departments must be a character vector with at least one"
  )
  expect_error(
    ffi_to_tibble(
      character(), test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "departments must be a character vector with at least one"
  )
  expect_error(
    ffi_to_tibble(
      c("tururu"), test_years, list("tururu" = c(2345)), test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "Any of the provided"
  )
  # expect_warning(
  #   ffi_to_tibble(
  #     c("tururu", "01"), test_years, list("tururu" = c(2345), "01" = c(2345)), test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   ),
  #   "Any of the provided"
  # )
  # years
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, as.character(test_years), test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "years must be a numeric vector with at least one"
  )
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, numeric(), test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "years must be a numeric vector with at least one"
  )
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, 2000:2004, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "provided are from before 2005"
  )
  # expect_warning(
  #   ffi_to_tibble(
  #     "33", 2000:2005, list("33" = "83"), test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   ),
  #   "2005 will be ignored"
  # )

  # folder
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, "nonexistantfolder",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "Folder especified"
  )
  # filter list (TODO as testng interactive commands is tricky)
  # parallel options
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, test_folder,
      .parallel_options = list(scheduling = 1L, stdout = TRUE),
      .verbose = FALSE
    )),
    ".parallel_options"
  )
  # verbose
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = "FALSE"
    )),
    ".verbose"
  )
  # ancillary data (tested just by providing an existing wrong folder)
  expect_error(
    suppressWarnings(ffi_to_tibble(
      test_departments, test_years, test_plots, ".",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "must be present"
  )

  # what to expect if departments or filter list are all wrong
  # expect_true(
  #   suppressWarnings(ffi_to_tibble(
  #     "tururu", test_years, list("tururu" = 0), test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   ) |> nrow()) < 1
  # )
})
