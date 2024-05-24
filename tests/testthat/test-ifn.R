skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# test data -----------------------------------------------------------------------------------
general_plots <- ifn_plots_thesaurus |>
  dplyr::filter(
    PROVINCIA %in% c(
      "01", "02", "03", "04", "05", "06", "07", "10", "12", "13", "17",
      "23", "27", "30", "33", "31", "38", "40", "50", "49"
    ),
    ESTADILLO %in% (c(
      0, 5, 11, 14, 19, 23, 51, 61,
      78, 80, 90, 99, 105, 114, 135, 156,
      172, 190, 233, 261, 269, 283, 325, 328,
      377, 412, 419, 426, 444, 445, 479, 499,
      532, 537, 626, 629, 672, 679, 744, 761,
      783, 818, 1021, 1057, 1120, 1138, 1216, 1223,
      1460, 1463, 1483, 1518, 1557, 1728, 1839, 2003,
      2064, 2944, 3374, 1406115
    ) |> stringr::str_pad(width = 4, side = "left", pad = "0"))
  )

test_ifn2_plots <- general_plots |>
  dplyr::filter(
    class_ifn2 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn2_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn2_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn2_provinces <- names(test_ifn2_plots)
test_ifn2_folder <- Sys.getenv("ifn_path")
test_ifn2_especies <- species_ifn_internal
test_ifn2_provinces_dictionary <- ifn_provinces_dictionary
test_ifn2_version <- "ifn2"
test_ifn2_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn2_version,
    test_ifn2_provinces,
    test_ifn2_plots,
    test_ifn2_folder,
    .verbose = TRUE
  )
)

test_ifn3_plots <- general_plots |>
  dplyr::filter(
    class_ifn3 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn3_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn3_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn3_provinces <- names(test_ifn3_plots)
test_ifn3_folder <- Sys.getenv("ifn_path")
test_ifn3_especies <- species_ifn_internal
test_ifn3_provinces_dictionary <- ifn_provinces_dictionary
test_ifn3_version <- "ifn3"
test_ifn3_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn3_version,
    test_ifn3_provinces,
    test_ifn3_plots,
    test_ifn3_folder,
    .verbose = TRUE
  )
)

test_ifn4_plots <- general_plots |>
  dplyr::filter(
    class_ifn4 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn4_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn4_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn4_provinces <- names(test_ifn4_plots)
test_ifn4_folder <- Sys.getenv("ifn_path")
test_ifn4_especies <- species_ifn_internal
test_ifn4_provinces_dictionary <- ifn_provinces_dictionary
test_ifn4_version <- "ifn4"
test_ifn4_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn4_version,
    test_ifn4_provinces,
    test_ifn4_plots,
    test_ifn4_folder,
    .verbose = TRUE
  )
)

test_ifn234_plots <- ifn_plots_thesaurus |>
  dplyr::filter(dplyr::if_all(dplyr::starts_with("class_"), ~ . != "xx")) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn234_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn234_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn234_provinces <- names(test_ifn234_plots)
test_ifn234_folder <- Sys.getenv("ifn_path")
test_ifn234_especies <- species_ifn_internal
test_ifn234_provinces_dictionary <- ifn_provinces_dictionary
test_ifn234_versions <- c("ifn2", "ifn3", "ifn4")

# individual tables ---------------------------------------------------------------------------

test_that("ifn_tree_table_process for ifn2 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "plot",
    "sp_code",
    "sp_name",
    "tree",
    "dia",
    "height",
    "density_factor",
    "cubing_form",
    "quality_wood"
  )

  expect_s3_class(
    test_ifn2_res <- ifn_tree_table_process(
      test_ifn2_input$tree_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$id_unique_code), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$id_unique_code) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn2_input$tree_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn2_input$tree_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})


test_that("ifn_tree_table_process for ifn3 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "Clase",
    "Subclase",
    "plot",
    "sp_code",
    "sp_name",
    #tree number id in ifn4
    "tree",
    #CUALIDAD 6 = dead but providing functions
    "quality_wood",
    "cubing_form",
    #check codes to understand origin and trace of individuals
    "tree_ifn2",
    "tree_ifn3",
    #diameter in cm
    "dia",
    #height in m
    "height",
    "density_factor"
  )


  # object
  expect_s3_class(
    test_ifn3_res <- ifn_tree_table_process(
      test_ifn3_input$tree_table[3],
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$id_unique_code), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$id_unique_code) |> as.character(),
    test_ifn3_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in province name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn3_input$tree_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn3_input$tree_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)


})

test_that("ifn_tree_table_process for ifn4 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "Clase",
    "Subclase",
    "plot",
    "sp_code",
    "sp_name",
    # #tree number id in ifn4
    "tree",
    #CUALIDAD 6 = dead but providing functions
    "quality_wood",
    "cubing_form",
    #check codes to understand origin and trace of individuals
    "tree_ifn3",
    "tree_ifn4",
    #diameter in cm
    "dia",
    #height in m
    "height",
    "density_factor"
  )


  # object
  expect_s3_class(
    test_ifn4_res <- ifn_tree_table_process(
      test_ifn4_input$tree_table[1],
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$id_unique_code), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$id_unique_code) |> as.character(),
    test_ifn4_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn4_input$tree_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn4_input$tree_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)


})

test_that("ifn_shrub_table_process for ifn2 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "plot",
    "sp_code",
    "sp_name",
    "height",
    "cover"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_shrub_table_process(
      test_ifn2_input$shrub_table[1],
      test_ifn2_version,
      test_ifn2_input$plots[1],
      test_ifn2_input$province[1],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$plot), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$id_unique_code) |> as.character(),
    test_ifn2_input$plots[1]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[1],
      test_ifn2_input$province[1],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn2_input$shrub_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn2_input$shrub_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})
test_that("ifn_shrub_table_process for ifn3 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "Clase",
    "Subclase",
    "plot",
    "sp_name",
    "sp_code",
    "height",
    "cover"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_shrub_table_process(
      test_ifn3_input$shrub_table[1],
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$plot), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$id_unique_code) |> as.character(),
    test_ifn3_input$plots[1]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn3_input$shrub_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn3_input$shrub_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})
test_that("ifn_shrub_table_process for ifn4 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "Clase",
    "Subclase",
    "plot",
    "sp_name",
    "sp_code",
    "height",
    "cover"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_shrub_table_process(
      test_ifn4_input$shrub_table[1],
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$plot), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$id_unique_code) |> as.character(),
    test_ifn4_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn4_input$shrub_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn4_input$shrub_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})



test_that("ifn_regen_table_process for ifn2 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "plot",
    "sp_code",
    "sp_name",
    "dbh",
    "height",
    "n",
    "density_factor"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_regen_table_process(
      test_ifn2_input$regen_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$plot), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$id_unique_code) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn2_input$regen_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(
      ifn_regen_table_process(
        test_ifn2_input$regen_table[nrow(test_ifn2_input)],
        test_ifn2_version,
        test_ifn2_input$plots[nrow(test_ifn2_input)],
        test_ifn2_input$province[nrow(test_ifn2_input)],
        test_ifn2_especies
      )
    ),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})
test_that("ifn_regen_table_process for ifn3 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "plot",
    "Clase",
    "Subclase",
    "sp_code",
    "sp_name",
    "dbh",
    "height",
    "n",
    "density_factor"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_regen_table_process(
      test_ifn3_input$regen_table[1],
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$plot), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$id_unique_code) |> as.character(),
    test_ifn3_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn3_input$regen_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn3_input$regen_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})
test_that("ifn_regen_table_process for ifn4 works as intended", {

  expected_names <- c(
    "id_unique_code",
    "province_code",
    "plot",
    "Clase",
    "Subclase",
    "sp_code",
    "sp_name",
    "dbh",
    "height",
    "n",
    "density_factor"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_regen_table_process(
      test_ifn4_input$regen_table[3],
      test_ifn4_version,
      test_ifn4_input$plots[3],
      test_ifn4_input$province[3],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$plot), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$id_unique_code) |> as.character(),
    test_ifn4_input$plots[3]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn4_input$regen_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn4_input$regen_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})


test_that("ifn_plot_table_process for ifn2  works as intended", {

  expected_names <- c(
    "id_unique_code",
    "country",
    "province_code",
    "province_name_original",
    "ca_name_original",
    "plot",
    "year",
    "version",
    "sheet_ntm",
    "huso",
    "COORDEX",
    "COORDEY",
    "coord_sys",
    "crs",
    "slope_mean",
    "slope",
    "elev",
    "aspect"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_plot_table_process(
      test_ifn2_input$plot_table[3],
      test_ifn2_input$coord_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)

  expect_length(unique(test_ifn2_res$plot), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)

  expect_identical(
    unique(test_ifn2_res$id_unique_code) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn2_input$coord_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn2_input$plot_table[nrow(test_ifn2_input) - 2],
      test_ifn2_input$coord_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn2_input$plot_table[nrow(test_ifn2_input)],
      test_ifn2_input$coord_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})

test_that("ifn_plot_table_process for ifn3  works as intended", {

  expected_names <- c(
    "id_unique_code",
    "country",
    "year",
    "ca_name_original",
    "province_code",
    "province_name_original",
    "plot",
    "Clase",
    "Subclase",
    "version",
    "type",
    "aspect",
    "slope",
    "crs",
    "coord_sys",
    "COORDEX",
    "COORDEY",
    "sheet_ntm",
    "huso"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_plot_table_process(
      test_ifn3_input$plot_table[6],
      test_ifn3_input$coord_table[6],
      test_ifn3_version,
      test_ifn3_input$plots[6],
      test_ifn3_input$province[6],
      test_ifn3_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)

  expect_length(unique(test_ifn3_res$plot), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)

  expect_identical(
    unique(test_ifn3_res$id_unique_code) |> as.character(),
    test_ifn3_input$plots[6]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[6] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn3_input$coord_table[6],
      test_ifn3_version,
      test_ifn3_input$plots[6],
      test_ifn3_input$province[6],
      test_ifn3_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn3_input$plot_table[nrow(test_ifn3_input) - 2],
      test_ifn3_input$coord_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn3_input$plot_table[nrow(test_ifn3_input)],
      test_ifn3_input$coord_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})

test_that("ifn_plot_table_process for ifn4  works as intended", {
  expected_names <- c(
    "id_unique_code",
    "country",
    "year",
    "ca_name_original",
    "province_code",
    "province_name_original",
    "plot",
    "Clase",
    "Subclase",
    "version",
    "type",
    "aspect",
    "slope",
    "crs",
    "coord_sys",
    "COORDEX",
    "COORDEY",
    "sheet_ntm",
    "huso"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_plot_table_process(
      test_ifn4_input$plot_table[3],
      test_ifn4_input$coord_table[3],
      test_ifn4_version,
      test_ifn4_input$plots[3],
      test_ifn4_input$province[3],
      test_ifn4_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)

  expect_length(unique(test_ifn4_res$plot), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)

  expect_identical(
    unique(test_ifn4_res$id_unique_code) |> as.character(),
    test_ifn4_input$plots[3]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn4_input$coord_table[6],
      test_ifn4_version,
      test_ifn4_input$plots[6],
      test_ifn4_input$province[6],
      test_ifn4_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn4_input$plot_table[nrow(test_ifn4_input) - 2],
      test_ifn4_input$coord_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn4_input$plot_table[nrow(test_ifn4_input)],
      test_ifn4_input$coord_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})

# tables process -----------------------------------------------------------------------------------

test_that("ifn_tables_process ifn2 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn2_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn2_plots <- test_ifn2_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })
  test_ifn2_plots$tururu <- c("tururu_0355_NN_A1_A1")
  test_ifn2_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")

  # tests data
  expected_names <- c(
    "id_unique_code",
    "country",
    "year",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "plot",
    "version",
    "sheet_ntm",
    "huso",
    "coord_sys",
    "coordx",
    "coordy",
    "crs",
    "slope_mean",
    "slope",
    "elev",
    "aspect",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- suppressWarnings(ifn_tables_process(
      test_ifn2_provinces, test_ifn2_version, test_ifn2_plots, test_ifn2_folder,
      .parallel_options = test_ifn2_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names)
  expect_true(all(unique(test_ifn2_res$province_code) %in% names(test_ifn2_plots)))

  ### missing tables/plots
  # tururu and "91" province shouldn't appear
  # inexistent plots shouldn't also appear
  expect_false("tururu" %in% unique(test_ifn2_res$province_code))
  expect_identical(
    nrow(test_ifn2_res),
    ((test_ifn2_plots |> purrr::flatten() |> length()) - 3) |> as.integer()
  )

  ### missing random files
  # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

test_that("ifn_tables_process ifn3 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn3_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn3_plots <- test_ifn3_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })
  test_ifn3_plots$tururu <- c("tururu_0355_NN_A1_A1")
  test_ifn3_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")

  # tests data
  expected_names <- c(
    "id_unique_code",
    "country",
    "year",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "plot",
    "class",
    "subclass",
    "version",
    "type",
    "sheet_ntm",
    "huso",
    "coord_sys",
    "coordx",
    "coordy",
    "crs",
    "slope",
    "aspect",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- suppressWarnings(ifn_tables_process(
      test_ifn3_provinces, test_ifn3_version, test_ifn3_plots, test_ifn3_folder,
      .parallel_options = test_ifn3_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(all(unique(test_ifn3_res$province_code) %in% names(test_ifn3_plots)))

  # ### missing tables/plots
  # # tururu state shouldn't appear
  # # inexistent plots (91-0) shouldn't
  # # be present, so 12 of 14 elements in filter list
  expect_false("tururu" %in% unique(test_ifn3_res$province_code))
  expect_identical(
    nrow(test_ifn3_res), as.integer((test_ifn3_plots |> purrr::flatten() |> length()) - 3)
  )

  # ### missing random files
  # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

test_that("ifn_tables_process ifn4 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn4_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn4_plots <- test_ifn4_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })
  test_ifn4_plots$tururu <- c("tururu_0355_NN_A1_A1")
  test_ifn4_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")

  # tests data
  expected_names <- c(
    "id_unique_code",
    "country",
    "year",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "plot",
    "class",
    "subclass",
    "version",
    "type",
    "sheet_ntm",
    "huso",
    "coord_sys",
    "coordx",
    "coordy",
    "crs",
    "slope",
    "aspect",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- suppressWarnings(ifn_tables_process(
      test_ifn4_provinces, test_ifn4_version, test_ifn4_plots, test_ifn4_folder,
      .parallel_options = test_ifn4_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(all(unique(test_ifn4_res$province_code) %in% names(test_ifn4_plots)))

  ### missing tables/plots
  # tururu state shouldn't appear, inexistent plots (91-0) shouldn't be present,
  # Also, "06" = c(2064,325), "07" = c(499) and "10" = c(3374) don't exist in IFN4
  expect_false("tururu" %in% unique(test_ifn4_res$province_code))
  expect_identical(
    nrow(test_ifn4_res),
    ((test_ifn4_plots |> purrr::flatten() |> length()) - 3) |> as.integer()
  )

  # ### missing random files
  # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

# ifn_to_tibble -------------------------------------------------------------------------------

test_that("ifn_to_tibble  ifn 2-3-4 works as intended", {

  # tests config
  test_ifn234_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 16000 plots in test_ifn234_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn234_plots <- test_ifn234_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })
  test_ifn234_plots$tururu <- c("tururu_0355_NN_A1_A1")
  test_ifn234_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")

  # tests data
  expected_names <- c(
    "id_unique_code", "country", "year", "ca_name_original", "province_name_original",
    "province_code", "plot", "version", "sheet_ntm", "huso", "coord_sys", "coordx", "coordy",
    "crs", "slope_mean", "slope", "elev", "aspect", "tree", "understory", "regen",
    "class", "subclass", "type"
  )

  # object
  expect_s3_class(
    test_ifn234_res <- suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn234_res, expected_names)
  expect_false("tururu" %in% unique(test_ifn234_res$province_code))
  expect_identical(
    nrow(test_ifn234_res),
    (((test_ifn234_plots |> purrr::flatten() |> length()) - 3) |> as.integer()) * 3L
  )
  expect_true(all(unique(test_ifn234_res$province_code) %in% names(test_ifn234_plots)))
  expect_true(all(unique(test_ifn234_res$version) %in% test_ifn234_versions))

  # tests for clean_empty and as_sf arguments
  sf_expected_names <- c(
    "id_unique_code", "country", "year", "ca_name_original", "province_name_original",
    "province_code","plot", "version", "sheet_ntm", "huso", "coord_sys", "crs",
    "slope_mean", "slope", "elev", "aspect", "tree", "understory",
    "regen", "class", "subclass", "type", "geometry", "crs_orig"
    # "soils"
  )
  expect_s3_class(
    sf_res <- suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      as_sf = TRUE, clean_empty = c("tree", "understory", "regen"),
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "sf"
  )

  expect_named(sf_res, sf_expected_names, ignore.order = TRUE)
  expect_true(nrow(sf_res) < nrow(test_ifn234_res))
  expect_false(any(purrr::map_lgl(sf_res$tree, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$understory, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$regen, rlang::is_empty)))

  ## test assertions in ifn_to_tibble
  # provinces
  expect_error(
    ifn_to_tibble(
      1:7, test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    ),
    "character vector"
  )
  expect_error(
    ifn_to_tibble(
      character(), test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    ),
    "at least one"
  )
  expect_error(
    ifn_to_tibble(
      c("tururu"), test_ifn234_versions, list("tururu" = c(2345)), test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    ),
    "Any of the provided"
  )
  # versions
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, 1:3, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "character vector"
  )
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, character(), test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "at least one"
  )
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, c("ifn4", "ifn5"), test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "Only valid"
  )

  # folder
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, test_ifn234_versions, test_ifn234_plots, "nonexistantfolder",
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = FALSE
    )),
    "Folder especified"
  )

  # parallel optios
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = list(scheduling = 1L, stdout = TRUE),
      .verbose = FALSE
    )),
    ".parallel_options"
  )

  # verbose
  expect_error(
    suppressWarnings(ifn_to_tibble(
      test_ifn234_provinces, test_ifn234_versions, test_ifn234_plots, test_ifn234_folder,
      .parallel_options = test_ifn234_parallel_conf,
      .verbose = "FALSE"
    )),
    ".verbose"
  )

})
