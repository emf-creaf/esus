skip()
# test data -----------------------------------------------------------------------------------

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
    "88" = 20012 # this doesn't exist, so NAs in lat lon
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
    "17" = 63905, # this doesn't exist, so NAs in lat lon
    "31" = 95724, # this doesn't exist, so NAs in lat lon
    "71" = 99371 # this doesn't exist, so NAs in lat lon
  ),
  "tururu" = list(
    "1" = 2500
  )
)
test_year <- 2010L
test_states <- names(test_plots)
test_folder <- Sys.getenv("fia_path")
test_ref_species <- .read_inventory_data(fs::path(test_folder, "REF_SPECIES.csv")) |>
  dplyr::as_tibble()
test_ref_plant_dictionary <- .read_inventory_data(fs::path(test_folder, "REF_PLANT_DICTIONARY.csv")) |>
  dplyr::as_tibble()
test_input <-
  .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = FALSE)

# table functions -----------------------------------------------------------------------------
test_that("fia_plot_table_process works as intended", {

  expected_names <- c(
    "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM", "COUNTYCD", "PLOT",
    "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
    "RSCD", "DESIGNCD", "LAT", "LAT_ORIGINAL", "LON", "LON_ORIGINAL", "COORD_SYS",
    "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL"
  )

  # object
  expect_s3_class(
    test_res <- fia_plot_table_process(
      test_input$plot_table[1],
      test_input$survey_table[1],
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  # No tables
  expect_warning(
    test_error <- fia_plot_table_process(
      NA_character_,
      test_input$survey_table[1],
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  expect_warning(
    test_error <- fia_plot_table_process(
      test_input$plot_table[1],
      NA_character_,
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  expect_warning(
    test_error <- fia_plot_table_process(
      test_input$plot_table[1],
      test_input$survey_table[1],
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_plot_table_process(
      test_input$plot_table[30],
      test_input$survey_table[30],
      test_input$cond_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_year
    )),
    "tbl"
  )
  # expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) == 1L)
  expect_true(all(
    is.na(test_no_plot_error[["LON"]]),
    is.na(test_no_plot_error[["LON_ORIGINAL"]]),
    is.na(test_no_plot_error[["LAT"]]),
    is.na(test_no_plot_error[["LAT_ORIGINAL"]])
  ))
})

test_that("fia_tree_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "TREE",
    "STATUS", "DIA", "HT", "SP_NAME", "SP_CODE", "DENSITY"
  )

  # object
  expect_s3_class(
    test_res <- fia_tree_table_process(
      test_input$tree_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_tree_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_tree_table_process(
      test_input$tree_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_year,
      test_ref_species
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_p3_understory_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "HT", "COVER", "GROWTH_HABIT"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p3_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p3_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # herb object
  expect_s3_class(
    test_res <- fia_p3_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("Forb/herb", "Graminoids"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p3_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("Forb/herb", "Graminoids"),
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_p3_understory_table_process(
      test_input$p3_understory_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)

  ### TODO
  # tests for the combination warning
  # find plots with p2 data to test
})

test_that("fia_p2_understory_table_process works as intended", {

  # skip()
  # test data
  test_plots <- list(
    "MT" = list(
      "39" = 9358, # in 2018
      "99" = 90332, # in 2015
      "89" = 81566 # in 2013
    ),
    "CA" = list(
      "36" = 73711, # in 2019
      "107" = 70234, # in 2019
      "23" = 61746 # in 2018
    ),
    "AK" = list(
      "261" = 29480, # in 2019
      "100" = 16966, # in 2013
      "110" = 48368 # in 2010
    ),
    "CO" = list(
      "19" = 80254, # in 2015
      "69" = 86019, # in 2010
      "83" = 84264 # in 2013
    ),
    "OR" = list(
      "9" = 60747, # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
      "57" = 91924, # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
      "71" = 87064 # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
    ),
    "MN" = list("137" = 29396) # this for 2010 should have only p3
  )
  test_year <- 2019L
  # test_years <- c(2010, 2013, 2015, 2018, 2019)
  test_states <- names(test_plots)
  test_folder <- Sys.getenv("fia_path")
  test_ref_species <- .read_inventory_data(fs::path(test_folder, "REF_SPECIES.csv")) |>
    dplyr::as_tibble()
  test_ref_plant_dictionary <- .read_inventory_data(fs::path(test_folder, "REF_PLANT_DICTIONARY.csv")) |>
    dplyr::as_tibble()
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = FALSE)

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "GROWTH_HABIT_CD", "HT", "COVER", "GROWTH_HABIT"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[7],
      test_input$plots[7],
      test_input$county[7],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[7])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[7])

  # errors
  expect_warning(
    test_error <- fia_p2_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # herb object
  expect_s3_class(
    test_res <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[7],
      test_input$plots[7],
      test_input$county[7],
      test_year,
      c("FB", "GR"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[7])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[7])

  # errors
  expect_warning(
    test_error <- fia_p2_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("FB", "GR"),
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)


  ### TODO
  # tests for the combination warning
})

test_that("fia_understory_table_process works as intended", {
  # test data
  test_plots <- list(
    "MT" = list(
      "39" = 9358, # in 2018
      "99" = 90332, # in 2015
      "89" = 81566 # in 2013
    ),
    "CA" = list(
      "36" = 73711, # in 2019
      "107" = 70234, # in 2019
      "23" = 61746 # in 2018
    ),
    "AK" = list(
      "261" = 29480, # in 2019
      "100" = 16966, # in 2013
      "110" = 48368 # in 2010
    ),
    "CO" = list(
      "19" = 80254, # in 2015
      "69" = 86019, # in 2010
      "83" = 84264 # in 2013
    ),
    "OR" = list(
      "9" = 60747, # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
      "57" = 91924, # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
      "71" = 87064 # in 2019 -> these, as per plot table should have both p3 and p2 in 2019
    ),
    "MN" = list("137" = 29396) # this for 2010 should have only p3
  )
  test_year <- 2019L
  # test_years <- c(2010, 2013, 2015, 2018, 2019)
  test_states <- names(test_plots)
  test_folder <- Sys.getenv("fia_path")
  test_ref_species <- .read_inventory_data(fs::path(test_folder, "REF_SPECIES.csv")) |>
    dplyr::as_tibble()
  test_ref_plant_dictionary <- .read_inventory_data(fs::path(test_folder, "REF_PLANT_DICTIONARY.csv")) |>
    dplyr::as_tibble()
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = FALSE)
  expected_names_p2 <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "GROWTH_HABIT_CD", "HT", "COVER", "GROWTH_HABIT"
  )
  expected_names_p3 <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "HT", "COVER", "GROWTH_HABIT"
  )
  # p2 shrubs
  expect_s3_class(
    test_res <- suppressWarnings(fia_understory_table_process(
      test_input$p3_understory_table[7],
      test_input$p2_veg_subplot_table[7],
      test_input$plots[7],
      test_input$county[7],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res) > 1)
  expect_named(test_res, expected_names_p2)
  expect_identical(test_res$YEAR |> unique(), test_year)
  expect_identical(test_res$STATECD |> unique(), 2L)
  expect_identical(test_res$COUNTYCD |> unique() |> as.character(), test_input$county[7])
  expect_identical(test_res$GROWTH_HABIT_CD |> unique(), "SH")
  # We expect the same results in the general understory function than in the individual
  # ones
  expect_identical(
    test_res,
    fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[7],
      test_input$plots[7],
      test_input$county[7],
      test_year,
      "SH",
      test_ref_plant_dictionary
    )
  )

  # no p2 no p3 warnings
  expect_warning(
    test_empty <- fia_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$p2_veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    ),
    "Skipping understory data for plot"
  )
  expect_true(nrow(test_empty) < 1)

  # p3 shrubs
  test_year <- 2010L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = FALSE)
  expect_s3_class(
    test_res_p3 <- suppressWarnings(fia_understory_table_process(
      test_input$p3_understory_table[16],
      test_input$p2_veg_subplot_table[16],
      test_input$plots[16],
      test_input$county[16],
      2010,
      "Shrub", "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res_p3) > 1)
  expect_named(test_res_p3, expected_names_p3)
  expect_identical(test_res_p3$YEAR |> unique(), test_year)
  expect_identical(test_res_p3$STATECD |> unique(), 27L)
  expect_identical(test_res_p3$COUNTYCD |> unique() |> as.character(), test_input$county[16])
  expect_true(stringr::str_detect(test_res_p3$GROWTH_HABIT |> unique(), "Shrub"))
  # We expect the same results in the general understory function than in the individual
  # ones
  expect_identical(
    test_res_p3,
    fia_p3_understory_table_process(
      test_input$p3_understory_table[16],
      test_input$plots[16],
      test_input$county[16],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    )
  )

  # p3 and p2
  test_year <- 2009L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = FALSE)
  expect_s3_class(
    test_res_p3_p2 <- fia_understory_table_process(
      test_input$p3_understory_table[13],
      test_input$p2_veg_subplot_table[13],
      test_input$plots[13],
      test_input$county[13],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    ),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res_p3_p2) > 1)
  # we expect the p2 names, as some of the data comes from p2
  expect_named(test_res_p3_p2, expected_names_p2)

  expect_identical(test_res_p3_p2$YEAR |> unique(), test_year)
  expect_identical(test_res_p3_p2$STATECD |> unique(), 41L)
  expect_identical(test_res_p3_p2$COUNTYCD |> unique() |> as.character(), test_input$county[13])
})

test_that("fia_seedling_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "N", "TPA_UNADJ", "DENSITY"
  )

  # object
  expect_s3_class(
    test_res <- fia_seedling_table_process(
      test_input$seedling_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_seedling_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_seedling_table_process(
      test_input$seedling_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_year,
      test_ref_species
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_subplot_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SLOPE", "ASPECT", "MACRCOND", "SUBPCOND", "MICRCOND"
  )

  # object
  expect_s3_class(
    test_res <- fia_subplot_table_process(
      test_input$subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_subplot_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_subplot_table_process(
      test_input$subplot_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_year
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_soils_lab_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "PLOT", "COUNTYCD", "STATECD", "YEAR", "LAYER_TYPE",
    "FIELD_MOIST_WATER_CONTENT_PCT_MEAN", "TOTAL_WATER_CONTENT_PCT_MEAN",
    "RESIDUAL_WATER_CONTENT_PCT_MEAN", "BULK_DENSITY_MEAN", "COARSE_FRACTION_PCT_MEAN",
    "C_ORG_PCT_MEAN"
  )

  # object
  expect_s3_class(
    test_res <- fia_soils_lab_table_process(
      test_input$soils_lab_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_soils_lab_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_soils_lab_table_process(
      test_input$soils_lab_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_input$state[30],
      test_year
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_soils_loc_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "FORFLTHK_MEAN",
    "FORFLTHK_MEAN_ORIGINAL", "LTRLRTHK_MEAN", "LTRLRTHK_MEAN_ORIGINAL", "TXTRLYR1",
    "TXTRLYR1_ORIGINAL", "TXTRLYR2", "TXTRLYR2_ORIGINAL", "DPTHSBSL_MEAN",
    "DPTHSBSL_MEAN_ORIGINAL", "ROCK_COVER_PCT_MEAN", "ROCK_COVER_PCT_MEAN_ORIGINAL"
  )

  # object
  expect_s3_class(
    test_res <- fia_soils_loc_table_process(
      test_input$soils_loc_table[1],
      test_input$veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_soils_loc_table_process(
      NA_character_,
      test_input$veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_warning(
    test_no_plot_error <- fia_soils_loc_table_process(
      test_input$soils_loc_table[30],
      test_input$veg_subplot_table[30],
      test_input$plots[30],
      test_input$county[30],
      test_input$state[30],
      test_year
    ),
    "combination of plot, county and year"
  )
  expect_s3_class(test_no_plot_error, "tbl")
  expect_true(nrow(test_no_plot_error) < 1L)
})

# table process -------------------------------------------------------------------------------

test_that("fia_table_process works as intended", {

  ### TODO
  # - test new understory logic

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # tests data
  expected_names <- c(
    "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM", "COUNTYCD", "PLOT",
    "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
    "RSCD", "DESIGNCD", "LAT", "LAT_ORIGINAL", "LON", "LON_ORIGINAL", "COORD_SYS",
    "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL",
    "tree", "understory", "regen", "subplot", "soils"
  )

  # object
  expect_s3_class(
    test_res <- suppressWarnings(fia_tables_process(
      test_year, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names)
  expect_true(all(unique(test_res$STATEAB) %in% names(test_plots)))
  # this test is wrong:
  # expect_true(all(
  #   unique(test_res$COUNTYCD) %in%
  #     (purrr::map_depth(test_plots, .depth = 1, names) |> purrr::flatten_chr() |> unique())
  # ))

  ### missing tables/plots
  # tururu state shouldn't appear
  # inexistent plots (MO-88-20012, OR-17-63905, OR-31-95724, OR-71-99371) shouldn't
  # be present, so 26 of 31 elements in filter list
  expect_false("tururu" %in% unique(test_res$STATEAB))
  expect_identical(nrow(test_res), 26L)

  ### missing random files
  # This is done with files in a folder for testing that lacks some files:
  # MN -> No TREE, we expect results, but all the tree tibbles for MN are empty tibbles
  # CA -> No COND, we expect no plots from CA as all PLOT info is missing
  # OR -> No SEEDLING, we expect results, but all the seedling tibbles for MN are empty tibbles
  # AL -> No SOIL (both), we expect results, but all the soil tibbles for AL are empty tibbles
  # MO -> No VEG_SUBPLOT, we expect_results, but understory should be empty tibbles for MO
  # OH -> No SUBPLOT, we expect_results, but subplot should be empty tibbles for OH
  test_folder <- fs::path(Sys.getenv("fia_path"), "missing_files_test")
  expect_s3_class(
    test_res_missing_files <- suppressWarnings(fia_tables_process(
      test_year, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  expect_true(
    (test_res_missing_files |>
      dplyr::filter(STATEAB == "MN") |>
      dplyr::pull(tree) |>
      purrr::list_rbind() |>
      nrow()) < 1
  )
  expect_false(all(c("tururu", "CA") %in% unique(test_res_missing_files$STATEAB)))
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "OR") |>
       dplyr::pull(regen) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "AL") |>
       dplyr::pull(soils) |>
       purrr::list_rbind() |>
       dplyr::pull(soils_lab) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )

  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "AL") |>
       dplyr::pull(soils) |>
       purrr::list_rbind() |>
       dplyr::pull(soils_loc) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "MO") |>
       dplyr::pull(understory) |>
       purrr::list_rbind() |>
       dplyr::pull(shrub) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "MO") |>
       dplyr::pull(understory) |>
       purrr::list_rbind() |>
       dplyr::pull(herbs) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(STATEAB == "OH") |>
       dplyr::pull(subplot) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )

})

# fia_to_tibble -------------------------------------------------------------------------------

test_that("fia_to_tibble works as intended", {

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # tests data
  expected_names <- c(
    "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM", "COUNTYCD", "PLOT",
    "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
    "RSCD", "DESIGNCD", "LAT", "LAT_ORIGINAL", "LON", "LON_ORIGINAL", "COORD_SYS",
    "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL",
    "tree", "understory", "regen", "subplot", "soils"
  )
  test_years <- c(2005, 2010)

  # object
  expect_s3_class(
    test_res <- suppressWarnings(fia_to_tibble(
      test_years, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names)
  expect_false("tururu" %in% unique(test_res$STATEAB))
  expect_identical(nrow(test_res), 52L) # four plots dont exist, so 4x2=8 rows less
  expect_true(all(unique(test_res$STATEAB) %in% names(test_plots)))
  expect_true(all(
    unique(test_res$COUNTYCD) %in%
      (purrr::map_depth(test_plots, .depth = 1, names) |> purrr::flatten_chr() |> unique())
  ))
  expect_true(all(unique(test_res$YEAR) %in% test_years))

  ### test all assertions done in fia_to_tibble
  # states
  expect_error(
    fia_to_tibble(
      test_years, 1:7, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "states must be a character vector with at least one"
  )
  expect_error(
    fia_to_tibble(
      test_years, character(), test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "states must be a character vector with at least one"
  )
  # years
  expect_error(
    fia_to_tibble(
      as.character(test_years), test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "years must be a numeric vector with at least one"
  )
  expect_error(
    fia_to_tibble(
      numeric(), test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "years must be a numeric vector with at least one"
  )
  # folder
  expect_error(
    fia_to_tibble(
      test_years, test_states, test_plots, "nonexistentfolder",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "Folder especified"
  )
  # filter list (TODO as testng interactive commands is tricky)
  # parallel options
  expect_error(
    fia_to_tibble(
      test_years, test_states, test_plots, test_folder,
      .parallel_options = list(scheduling = 2L, stdout = TRUE),
      .verbose = FALSE
    ),
    ".parallel_options"
  )
  # verbose
  expect_error(
    fia_to_tibble(
      test_years, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = "FALSE"
    ),
    ".verbose"
  )
  # ancillary data (tested just by providing an existing wrong folder)
  expect_error(
    fia_to_tibble(
      test_years, test_states, test_plots, ".",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "must be present"
  )

  # what to expect if states or filter list are all wrong
  expect_error(
    suppressWarnings(fia_to_tibble(
      test_years, test_states[7], test_plots[7], test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "Ooops!"
  )

})
