skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)

# build path and input ------------------------------------------------------------------------

test_that(".build_ffi_input_with and .build_ffi_file_path work as intended", {
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
    "80" = c(1417044,1452529),
    "81" = c(1428398, 973950),
    "86" = c(957495,921133),
    "87" = c(975666,979897),
    "89" = 1433956,
    "91" = c(1406115, 0),
    "tururu" = 3555
  )

  test_year <- 2019
  test_departments <- names(test_plots)
  test_folder <- Sys.getenv("ffi_path")
  expected_names <- c(
    "department",
    "plots",
    "plot_table",
    "tree_table",
    "shrub_table",
    "soils_table"
  )

  # warnings and messages
   expect_warning(
    .build_ffi_input_with(test_departments, test_year, test_plots, ".", .verbose = TRUE),
    "file doesn't exist"
   )
  expect_message(
     .build_ffi_input_with(test_departments, test_year,  test_plots, test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    test_res <-
      .build_ffi_input_with(test_departments, test_year , test_plots, test_folder, .verbose = FALSE)
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
    unique(test_res[["department"]]) |> sort(),
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

  # we can test here also if .build_fia_file_path works
  # .build_fia_file_path
  # a correct custom one
  expect_identical(
    as.character(test_res[["plot_table"]][1]),
    glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{2}})1404119;((?:[^;]+;){{2}})01" {test_folder}PLACETTE.csv')
  )
  # incorrect ones, that will be tested later when loading the data
  expect_identical(
    test_res[["plot_table"]][32],
    glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{2}})0;((?:[^;]+;){{2}})91" {test_folder}PLACETTE.csv')
  )
  expect_identical(
    test_res[["plot_table"]][33],
    glue::glue('grep -P "CAMPAGNE|(^(?:[^;]+;){{2}})3555;((?:[^;]+;){{2}})tururu" {test_folder}PLACETTE.csv')
  )

  ## Test filter_list = NULL
  expect_s3_class(
    test_res_filter_list <- suppressWarnings(
      .build_ffi_input_with(test_departments, test_year, NULL, test_folder, .verbose = FALSE)
    ),
    "tbl"
  )
  expect_false("tururu" %in% (test_res_filter_list$department |> unique()))
  expect_true(all((test_res_filter_list$department |> unique()) %in% test_departments))
  expect_length(test_res_filter_list$department |> unique(), length(test_departments) - 1)
})

test_that(".get_plots_from_department works as intended", {
  test_folder <- Sys.getenv("ffi_path")
  test_departments <- c("01", "10", "11", "tururu")

  # error
  expect_error(
    suppressWarnings(.get_plots_from_department(test_departments[1], ".")),
    "folder doesn't contain"
  )
  ## results are ok
  # class
  expect_s3_class(test_res_ok <- .get_plots_from_department(test_departments[1], test_folder), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok,c("CAMPAGNE", "IDP", "DEP", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # expect values
  expect_identical(unique(test_res_ok$DEP), "01")
  expect_identical(unique(.get_plots_from_department(test_departments[3], test_folder)$DEP), "11")

  ## wrong department
  expect_error(
    suppressWarnings(.get_plots_from_department(test_departments[4], test_folder)),
    "aborting"
  )
  # multiple departments
  expect_s3_class(
    test_res_multiple <- .get_plots_from_department(test_departments, test_folder),
    "sf"
  )
  # crs
  expect_identical(sf::st_crs(test_res_multiple), sf::st_crs(4326))
  # names
  expect_named(test_res_multiple,c("CAMPAGNE", "IDP", "DEP", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_multiple) > 0
  )
  # expect values
  expect_false("tururu" %in% (test_res_multiple$DEP |> unique()))
  expect_identical(unique(test_res_multiple$DEP) |> sort(), test_departments[-4] |> sort())
})

#
test_that("show_plots_from_ffi works as intended", {
  test_folder <- Sys.getenv("ffi_path")
  test_departments <- c("01", "10", "11", "tururu")

  # error
  expect_error(
    suppressWarnings(show_plots_from_ffi(".", test_departments[1])),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(test_res_ok <- show_plots_from_ffi(test_folder, test_departments), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("CAMPAGNE", "IDP", "DEP", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$DEP |> unique() |> sort(), test_departments[-4] |> sort()
  )
})

test_that(".transform_plot_summary_ffi works as intended", {
  test_folder <- Sys.getenv("ffi_path")
  test_departments <- c("01", "10", "11")
  test_summary <-  show_plots_from_ffi(test_folder, test_departments)
  test_years <- c(2005, 2010, 2015)

  # One department, one year
  # correct object
  expect_type(
    test_res_01 <- .transform_plot_summary_ffi(test_summary, test_years[1], test_departments[1]),
    "list"
  )
  # correct names
  expect_named(test_res_01, "01")
  # expect results
  expect_length(test_res_01, 1)
  expect_true(length(test_res_01[[1]]) > 1)
  # correct counties
  expect_equal(
    test_res_01[["01"]],
    test_summary |>
      dplyr::filter(DEP == "01", CAMPAGNE == test_years[1]) |>
      dplyr::pull(IDP) |>
      unique() |>
      as.character()
  )

  # All departments all years
  # correct object
  expect_type(
    test_res <- .transform_plot_summary_ffi(test_summary, test_years, test_departments),
    "list"
  )
  # correct names
  expect_named(test_res, c("01", "10", "11"), ignore.order = TRUE)
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)
  # correct counties
  expect_equal(
    test_res[["01"]],
    test_summary |>
      dplyr::filter(DEP %in% "01", CAMPAGNE %in% test_years) |>
      dplyr::pull(IDP) |>
      unique() |>
      as.character()
  )
  expect_equal(
    test_res[["11"]],
    test_summary |>
      dplyr::filter(DEP %in% "11", CAMPAGNE %in% test_years) |>
      dplyr::pull(IDP) |>
      unique() |>
      as.character()
  )
})

test_that("create_filter_list_ffi works as inteded", {
  # test data
  test_folder <- Sys.getenv("ffi_path")
  test_departments <- c("01", "10", "11", "tururu")
  test_summary <- show_plots_from_ffi(test_folder, test_departments) |>
    dplyr::filter(CAMPAGNE %in% c(2005, 2010, 2015))

  # errors
  # object error
  expect_error(create_filter_list_ffi("test_summary"), "data.frame")
  expect_error(create_filter_list_ffi(as.list(test_summary)), "data.frame")
  # names error
  expect_error(create_filter_list_ffi(iris), "expected names")
  # rows error
  expect_error(create_filter_list_ffi(test_summary |> dplyr::filter(CAMPAGNE == 1800)), "one row")

  # correct object
  expect_type(
    test_res <- create_filter_list_ffi(test_summary),
    "list"
  )
  # correct names
  expect_named(test_res, c("01", "10", "11"))
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)

})
