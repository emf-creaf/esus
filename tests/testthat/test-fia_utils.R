skip()
# build path and input ------------------------------------------------------------------------

test_that(".build_fia_input_with and .build_fia_file_path work as intended", {
  test_plots <- list(
    "MN" = list(
      "137" = c(29396, 25064),
      "17" = 20005,
      "31" = 20421,
      "71" = 20210
    ),
    "CA" = list(
      "15" = c(53519, 63676),
      "105" = c(70128, 83043),
      "61" = 69600
    ),
    "AL" = list(
      "121" = 33,
      "73" = 20,
      "131" = 73,
      "1" = 27,
      "81" = 13
    ),
    "MO" = list(
      "113" = 20144,
      "119" = 20129,
      "225" = 20168,
      "221" = 20084,
      "88" = 20012
    ),
    "OH" = list(
      "41" = 3878,
      "167" = 2121,
      "25" = 5374,
      "103" = 4704,
      "53" = 3579
    ),
    "OR" = list(
      "59" = c(76413, 76413),
      "17" = 63905,
      "31" = 95724,
      "71" = 99371
    ),
    "tururu" = list(
      "1" = 2500
    )
  )
  test_year <- 2010
  test_states <- names(test_plots)
  test_folder <- Sys.getenv("fia_path")
  expected_names <- c(
    "state", "county", "plots",
    "tree_table",
    "plot_table",
    "survey_table",
    "cond_table",
    "subplot_table",
    "p3_understory_table",
    "seedling_table",
    "soils_loc_table",
    "soils_lab_table",
    "veg_subplot_table",
    "p2_veg_subplot_table"
  )

  # warnings and messages
  expect_warning(
    test_res <- .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = TRUE),
    "file doesn't exists"
  )
  expect_message(
    .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = FALSE)
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == 31L)
  # and for the correct counties
  expect_identical(
    unique(test_res[["county"]]) |> sort(),
    purrr::map_depth(test_plots, 1, names) |> purrr::flatten_chr() |> unique() |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |> sort(),
    purrr::map_depth(test_plots, 1, purrr::flatten_dbl) |> purrr::flatten_dbl() |> unique() |> sort()
  )

  # we can test here also if .build_fia_file_path works
  # .build_fia_file_path
  # a correct one
  expect_identical(
    test_res[["survey_table"]][1],
    paste0(test_folder, names(test_plots)[1], "_SURVEY.csv")
  )
  # a correct custom one
  expect_identical(
    test_res[["plot_table"]][1],
    paste0("grep -P \",INVYR,|,137,(29396|29396.0),\" ", test_folder, names(test_plots)[1], "_PLOT.csv")
  )
  # an incorrect one
  expect_identical(
    test_res[["plot_table"]][31], NA_character_
  )

  ## Test filter_list = NULL
  expect_s3_class(
    test_res_filter_list <- suppressWarnings(
      .build_fia_input_with(test_year, test_states, NULL, test_folder, .verbose = FALSE)
    ),
    "tbl"
  )
  expect_false("tururu" %in% (test_res_filter_list$state |> unique()))
  expect_true(all((test_res_filter_list$state |> unique()) %in% test_states))
  expect_length(test_res_filter_list$state |> unique(), length(test_states) - 1)
})

test_that(".get_plots_from_state works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("OR", "WA", "CA", "tururu")

  # error
  expect_error(
    suppressWarnings(.get_plots_from_state(test_states[1], ".")),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(test_res_ok <- .get_plots_from_state(test_states[1], test_folder), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  # expect rows
  expect_true(nrow(test_res_ok) > 0)
  # expect values
  expect_identical(unique(test_res_ok$STATECD), 41L)
  expect_identical(unique(test_res_ok$STATEAB), "OR")
  expect_identical(unique(.get_plots_from_state(test_states[3], test_folder)$STATECD), 6L)
  expect_identical(unique(.get_plots_from_state(test_states[3], test_folder)$STATEAB), "CA")

  ## wrong state
  expect_error(
    suppressWarnings(.get_plots_from_state(test_states[4], test_folder)),
    "aborting"
  )
})


test_that("show_plots_from_fia works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("OR", "WA", "CA")

  # error
  expect_error(
    suppressWarnings(show_plots_from_fia( ".", test_states[1])),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(test_res_ok <- show_plots_from_fia(test_folder, test_states), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$STATEAB |> unique(), test_states
  )
})

test_that(".transform_plot_summary works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("OR", "WA", "CA")
  test_summary <- show_plots_from_fia(test_folder, test_states)
  test_years <- c(2005, 2010, 2015)

  # One state, one year
  # correct object
  expect_type(
    test_res_2005_OR <- .transform_plot_summary(test_summary, test_years[1], test_states[1]),
    "list"
  )
  # correct names
  expect_named(test_res_2005_OR, "OR")
  # expect results
  expect_length(test_res_2005_OR, 1)
  expect_true(length(test_res_2005_OR[[1]]) > 1)
  # correct counties
  expect_named(
    test_res_2005_OR[["OR"]],
    test_summary |>
      dplyr::filter(STATEAB == "OR", INVYR == test_years[1]) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  ## all states all years
  expect_type(
    test_res <- .transform_plot_summary(test_summary, test_years, test_states),
    "list"
  )
  # correct names
  expect_named(test_res, c("OR", "WA", "CA"), ignore.order = TRUE)
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)
  # correct counties
  expect_named(
    test_res[["OR"]],
    test_summary |>
      dplyr::filter(STATEAB %in% "OR", INVYR %in% test_years) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  expect_named(
    test_res[["CA"]],
    test_summary |>
      dplyr::filter(STATEAB %in% "CA", INVYR %in% test_years) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  ## error
  expect_error(
    .transform_plot_summary(
      tibble::tibble(
        "INVYR" = vector(),
        "STATECD" = vector(),
        "COUNTYCD" = vector(),
        "PLOT" = vector(),
        "STATEAB" = vector(),
        "geometry" = vector()
      ),
      test_years[1], test_states[1]
    )
  )
})

test_that("create_filter_list_fia works as inteded", {
  # test data
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("OR", "WA", "CA")
  test_summary <- show_plots_from_fia(test_folder, test_states) |>
    dplyr::filter(INVYR %in% c(2005, 2010, 2015))

  # errors
  # object error
  expect_error(create_filter_list_fia("test_summary"), "data.frame")
  expect_error(create_filter_list_fia(as.list(test_summary)), "data.frame")
  # names error
  expect_error(create_filter_list_fia(iris), "expected names")
  # rows error
  expect_error(create_filter_list_fia(test_summary |> dplyr::filter(INVYR == 1800)), "one row")

  # correct object
  expect_type(
    test_res <- create_filter_list_fia(test_summary),
    "list"
  )
  # correct names
  expect_named(test_res, c("CA", "OR", "WA"))
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)

})
