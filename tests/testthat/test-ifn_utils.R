skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)

# build path and input ------------------------------------------------------------------------

test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn2 ", {

  test_plots <- ifn_plots_thesaurus |>
    dplyr::filter(
      PROVINCIA %in% c("06", "07", "10", "30", "31", "33", "40", "49"),
      ESTADILLO %in% c(
        "2064", "1138", "0325", "0679", "0114", "0499", "3374", "0261", "0078", "1223",
        "0135", "0761", "1518", "0283", "0412", "1216", "1728", "0105", "0099", "0532", "0005"
      ),
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
  test_plots$tururu <- c("tururu_0005_NN_A1_A1")

  test_provinces <- names(test_plots)
  test_version <- "ifn2"
  test_folder <- Sys.getenv("ifn_path")

  expected_names <- c(
    "province",
    "plots",
    "version",
    "plot_table",
    "tree_table",
    "shrub_table",
    "regen_table",
    "coord_table"
  )


  # warnings and messages
  expect_warning(
    .build_ifn_input_with(test_version, test_provinces, test_plots, ".", .verbose = TRUE),
    "Files for provinces"
  )
  expect_warning(
    .build_ifn_input_with(test_version, test_provinces, test_plots, test_folder, .verbose = TRUE),
    "Files for provinces"
  )
  expect_message(
    suppressWarnings(
      .build_ifn_input_with(test_version, test_provinces, test_plots, test_folder, .verbose = TRUE)
    ),
    "Getting ready to retrieve"
  )
  expect_no_message(
    test_res <- suppressWarnings(
      .build_ifn_input_with(test_version, test_provinces, test_plots, test_folder, .verbose = FALSE)
    )
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == length(test_plots |> purrr::flatten()))
  # and for the correct counties
  expect_identical(
    unique(test_res[["province"]]) |> sort(),
    names(test_plots) |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |>
      unlist() |>
      sort(),
    unique(test_plots) |>
      unlist() |>
      as.character() |>
      sort()

  )

  # we can test here also if .build_ifn_file_path works
  # .build_ifn_file_path
  # a correct custom one


  expect_identical(
    as.character(test_res[["plot_table"]][1]),
    glue::glue("{test_folder}DATEST06.DBF")
  )
  # incorrect ones, that will be tested later when loading the data
  expect_identical(
    test_res[["plot_table"]][length(test_res[["plot_table"]])],
    NA_character_
  )


  # ## Test filter_list = NULL - this needs to be implemenetd
  # expect_s3_class(
  #   test_res_filter_list <- suppressWarnings(
  #     .build_ifn_input_with(test_version, test_provinces, NULL, test_folder, .verbose = FALSE)
  #   ),
  #   "tbl"
  # )
  #  expect_false("tururu" %in% (test_res_filter_list$province_name |> unique()))
  # expect_true(all((test_res_filter_list$province_name |> unique()) %in% test_provinces))
  #   expect_length(test_res_filter_list$province |> unique(), length(test_provinces) - 1)
})

test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn3 ", {

  test_plots <- ifn_plots_thesaurus |>
    dplyr::filter(
      PROVINCIA %in% c("06", "07", "10", "30", "31", "33", "40", "49"),
      ESTADILLO %in% c(
        "2064", "1138", "0325", "0679", "0114", "0499", "3374", "0261", "0078", "1223",
        "0135", "0761", "1518", "0283", "0412", "1216", "1728", "0105", "0099", "0532", "0005"
      ),
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
  test_plots$tururu <- c("tururu_0005_NN_A1_A1")

  test_provinces <- names(test_plots)
  test_version <- "ifn3"
  test_folder <- Sys.getenv("ifn_path")

  expected_names <- c(
    "province",
    "plots",
    "version",
    "plot_table",
    "tree_table",
    "shrub_table",
    "regen_table",
    "coord_table"
  )


  # warnings and messages
  expect_warning(
    .build_ifn_input_with(test_version, test_provinces, test_plots, ".", .verbose = TRUE),
    "Files for provinces"
  )
  expect_warning(
    .build_ifn_input_with(test_version, test_provinces, test_plots, test_folder, .verbose = TRUE),
    "Files for provinces"
  )
  expect_message(
    suppressWarnings(
      .build_ifn_input_with(test_version, test_provinces, test_plots, test_folder, .verbose = TRUE)
    ),
    "Getting ready to retrieve"
  )
  expect_no_message(
    test_res <- suppressWarnings(.build_ifn_input_with(
      test_version, test_provinces, test_plots, test_folder, .verbose = FALSE
    ))
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == length(test_plots |> purrr::flatten()))
  # and for the correct counties
  expect_identical(
    unique(test_res[["province"]]) |> sort(),
    names(test_plots) |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |>
      unlist() |>
      sort(),
    unique(test_plots) |>
      unlist() |>
      as.character() |>
      sort()

  )

  # we can test here also if .build_ifn_file_path works
  # .build_ifn_file_path
  # a correct custom one


  expect_identical(
    as.character(test_res[["plot_table"]][1]),
    glue::glue("{test_folder}Ifn3p06.accdb|PCParcelas")
  )
  # incorrect ones, that will be tested later when loading the data
  expect_identical(
    test_res[["plot_table"]][length(test_res[["plot_table"]])],
    NA_character_
  )
  # expect_identical(
  #   test_res[["plot_table"]][33],
  #   glue::glue('{test_folder}PLACETTE.csv')
  # )

  # ## Test filter_list = NULL - this needs to be implemenetd
  # expect_s3_class(
  #   test_res_filter_list <- suppressWarnings(
  #     .build_ifn_input_with(test_version, test_provinces, NULL, test_folder, .verbose = FALSE)
  #   ),
  #   "tbl"
  # )
  # expect_false("tururu" %in% (test_res_filter_list$province |> unique()))
  # expect_true(all((test_res_filter_list$province_code|> unique()) %in% test_provinces))
  # expect_length(test_res_filter_list$province_code |> unique(), length(test_provinces) - 1)
})

test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn4 ", {

  test_plots <- ifn_plots_thesaurus |>
    dplyr::filter(
      PROVINCIA %in% c("06", "07", "10", "30", "31", "33", "40", "49"),
      ESTADILLO %in% c(
        "2064", "1138", "0325", "0679", "0114", "0499", "3374", "0261", "0078", "1223",
        "0135", "0761", "1518", "0283", "0412", "1216", "1728", "0105", "0099", "0532", "0005"
      ),
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
  test_plots$tururu <- c("tururu_0005_NN_A1_A1")

  test_provinces <- names(test_plots)
  test_version <- "ifn4"
  test_folder <- Sys.getenv("ifn_path")

  expected_names <- c(
    "province",
    "plots",
    "version",
    "plot_table",
    "tree_table",
    "shrub_table",
    "regen_table",
    "coord_table"
  )

  # warnings and messages
  expect_warning(
    .build_ifn_input_with(test_version, test_provinces, test_plots, ".", .verbose = TRUE),
    "Files for provinces"
  )
  expect_message(
    .build_ifn_input_with(
      test_version, test_provinces[-9], test_plots[-9], test_folder, .verbose = TRUE
    ),
    "Getting ready to retrieve"
  )
  expect_no_message(
    suppressWarnings(test_res <- .build_ifn_input_with(
      test_version, test_provinces, test_plots, test_folder, .verbose = FALSE
    ))
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == length(test_plots |> purrr::flatten()))
  # and for the correct counties
  expect_identical(
    unique(test_res[["province"]]) |> sort(),
    names(test_plots) |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |>
      unlist() |>
      sort(),
    unique(test_plots) |>
      unlist() |>
      as.character() |>
      sort()

  )

  # we can test here also if .build_ifn_file_path works
  # .build_ifn_file_path
  # a correct custom one


  expect_identical(
    as.character(test_res[["plot_table"]][1]),
    glue::glue("{test_folder}Ifn4_Extremadura.accdb|PCParcelas")
  )
  # incorrect ones, that will be tested later when loading the data
  expect_identical(
    test_res[["plot_table"]][length(test_res[["plot_table"]])],
    NA_character_
  )
  # expect_identical(
  #   test_res[["plot_table"]][33],
  #   glue::glue('{test_folder}PLACETTE.csv')
  # )

  # ## Test filter_list = NULL - this needs to be implemenetd
  # expect_s3_class(
  #   test_res_filter_list <- suppressWarnings(
  #     .build_ifn_input_with(test_version, test_provinces, NULL, test_folder, .verbose = FALSE)
  #   ),
  #   "tbl"
  # )
  # expect_false("tururu" %in% (test_res_filter_list$province |> unique()))
  # expect_true(all((test_res_filter_list$province |> unique()) %in% test_provinces))
  # expect_length(test_res_filter_list$province |> unique(), length(test_provinces) - 1)
})

# get plots and transform summary -------------------------------------------------------------

test_provinces <- c("06", "07", "10", "30", "31", "33", "40", "49", "tururu")
test_folder <- Sys.getenv("ifn_path")
test_version <- "ifn2"

test_that(".get_plots_from_province works as intended for ifn2", {

  # error
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[1], ".", test_version)),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(
    test_res_ok <- .get_plots_from_province(test_provinces[1], test_folder, test_version),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(nrow(test_res_ok) > 0)
  # expect values
  expect_identical(unique(test_res_ok$province_code), "06")
  expect_identical(unique(test_res_ok$province_name_original), "Badajoz")
  expect_identical(
    unique(.get_plots_from_province(test_provinces[3], test_folder, test_version)$province_code),
    "10"
  )
  expect_identical(
    unique(
      .get_plots_from_province(test_provinces[3], test_folder, test_version)$province_name_original
    ),
    "Cáceres"
  )

  ## wrong state
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[9], test_folder, test_version)),
    "aborting"
  )
})

test_that("show_plots_from_ifn works as intended for ifn2", {

  # error
  expect_error(
    suppressWarnings(show_plots_from_ifn(".", test_provinces[1], test_version)),
    "No data found at"
  )

  ## results are ok
  # class
  expect_s3_class(
    suppressWarnings(test_res_ok <- show_plots_from_ifn(test_folder, test_provinces, test_version)),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$province_code |> unique(), test_provinces[1:(length(test_provinces) - 1)]
  )
})

test_that(".transform_plot_summary_ifn works as intended for ifn2", {

  test_summary <- suppressWarnings(show_plots_from_ifn(test_folder, test_provinces, test_version))

  # One state, one year
  # correct object
  expect_type(
    test_res_06 <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces[1]),
    "list"
  )
  # correct names
  expect_named(test_res_06, "06")
  # expect results
  expect_length(test_res_06, 1)
  expect_true(length(test_res_06[[1]]) > 1)


  ## all states all years
  expect_type(
    test_res <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces),
    "list"
  )
  # correct names
  expect_named(test_res, test_provinces[1:(length(test_provinces) - 1)], ignore.order = TRUE)
  # expect results
  expect_length(test_res, 8)
  for (prov in seq_along(test_res)) {
    expect_true(length(test_res[[prov]]) > 1)
  }
})

test_version <- "ifn3"

test_that(".get_plots_from_province works as intended for ifn3", {

  # error
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[1], ".", test_version)),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(
    test_res_ok <- .get_plots_from_province(test_provinces[1], test_folder, test_version),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(nrow(test_res_ok) > 0)
  # expect values
  expect_identical(unique(test_res_ok$province_code), "06")
  expect_identical(unique(test_res_ok$province_name_original), "Badajoz")
  expect_identical(
    unique(.get_plots_from_province(test_provinces[3], test_folder, test_version)$province_code),
    "10"
  )
  expect_identical(
    unique(
      .get_plots_from_province(test_provinces[3], test_folder, test_version)$province_name_original
    ),
    "Cáceres"
  )

  ## wrong state
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[9], test_folder, test_version)),
    "aborting"
  )
})

test_that("show_plots_from_ifn works as intended for ifn3", {

  # error
  expect_error(
    suppressWarnings(show_plots_from_ifn(".", test_provinces[1], test_version)),
    "No data found at"
  )

  ## results are ok
  # class
  expect_s3_class(
    suppressWarnings(test_res_ok <- show_plots_from_ifn(test_folder, test_provinces, test_version)),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$province_code |> unique(), test_provinces[1:(length(test_provinces) - 1)]
  )
})

test_that(".transform_plot_summary_ifn works as intended for ifn3", {

  test_summary <- suppressWarnings(show_plots_from_ifn(test_folder, test_provinces, test_version))

  # One state, one year
  # correct object
  expect_type(
    test_res_06 <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces[1]),
    "list"
  )
  # correct names
  expect_named(test_res_06, "06")
  # expect results
  expect_length(test_res_06, 1)
  expect_true(length(test_res_06[[1]]) > 1)


  ## all states all years
  expect_type(
    test_res <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces),
    "list"
  )
  # correct names
  expect_named(test_res, test_provinces[-length(test_provinces)], ignore.order = TRUE)
  # expect results
  expect_length(test_res, length(test_provinces[-length(test_provinces)]))
  for (prov in seq_along(test_res)) {
    expect_true(length(test_res[[prov]]) > 1)
  }
})

test_version <- "ifn4"

test_that(".get_plots_from_province works as intended for ifn4", {

  # error
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[1], ".", test_version)),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(
    test_res_ok <- .get_plots_from_province(test_provinces[1], test_folder, test_version),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(nrow(test_res_ok) > 0)
  # expect values
  expect_identical(unique(test_res_ok$province_code), "06")
  expect_identical(unique(test_res_ok$province_name_original), "Badajoz")
  expect_identical(
    unique(.get_plots_from_province(test_provinces[3], test_folder, test_version)$province_code),
    "10"
  )
  expect_identical(
    unique(
      .get_plots_from_province(test_provinces[3], test_folder, test_version)$province_name_original
    ),
    "Cáceres"
  )

  ## wrong province
  expect_error(
    suppressWarnings(.get_plots_from_province(test_provinces[9], test_folder, test_version)),
    "aborting"
  )
})

test_that("show_plots_from_ifn works as intended for ifn4", {

  # error
  expect_error(
    suppressWarnings(show_plots_from_ifn(".", test_provinces[1], test_version)),
    "No data found at"
  )

  ## results are ok
  # class
  expect_s3_class(
    suppressWarnings(test_res_ok <- show_plots_from_ifn(test_folder, test_provinces, test_version)),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(
    test_res_ok,
    c(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "geometry"
    ),
    ignore.order = TRUE
  )
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 7 states
  expect_identical(
    test_res_ok$province_code |> unique(), test_provinces[1:(length(test_provinces) - 1)]
  )
})

test_that(".transform_plot_summary_ifn works as intended for ifn4", {

  test_summary <- suppressWarnings(show_plots_from_ifn(test_folder, test_provinces, test_version))

  # One state, one year
  # correct object
  expect_type(
    test_res_06 <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces[1]),
    "list"
  )
  # correct names
  expect_named(test_res_06, "06")
  # expect results
  expect_length(test_res_06, 1)
  expect_true(length(test_res_06[[1]]) > 1)


  ## all states all years
  expect_type(
    test_res <- .transform_plot_summary_ifn(test_summary, test_version, test_provinces),
    "list"
  )
  # correct names
  expect_named(test_res, test_provinces[1:(length(test_provinces) - 1)], ignore.order = TRUE)
  # expect results
  expect_length(test_res, 8)
  for (prov in seq_along(test_res)) {
    expect_true(length(test_res[[prov]]) > 1)
  }
})

test_that("create_filter_list_ifn works as inteded", {
  # test data
  test_version <- c("ifn2", "ifn3", "ifn4")
  test_summary <- suppressWarnings(show_plots_from_ifn(test_folder, test_provinces, test_version))

  # just check we have the three inventories here
  expect_true(all(test_version %in% unique(test_summary$version)))

  # errors
  # object error
  expect_error(create_filter_list_ifn("test_summary"), "data.frame")
  expect_error(create_filter_list_ifn(as.list(test_summary)), "data.frame")
  # names error
  expect_error(create_filter_list_ifn(iris), "expected names")
  # rows error
  expect_error(create_filter_list_ifn(test_summary |> dplyr::filter(version == "ifn5")), "one row")

  # correct object
  expect_type(
    test_res <- create_filter_list_ifn(test_summary),
    "list"
  )
  # correct names
  expect_named(test_res, test_provinces[1:(length(test_provinces) - 1)], ignore.order = TRUE)
  # expect results
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)

})
