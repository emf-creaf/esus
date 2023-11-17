skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# test data -----------------------------------------------------------------------------------

test_plots <- list(
  "05" = c(61, 14, 328),
  "10" = c(2101,3374,261),
  "13" = c(51, 419,783),
   "27" = c(90, 190,537)
  #  "91" = c(1406115, 0),
    # "tururu" = 3555
)

test_provinces <- names(test_plots)
test_folder <- Sys.getenv("ifn_path")
test_input <- .build_ifn_input_with (
  version = "ifn2",
  test_provinces,
  test_plots, 
  test_folder,
  .verbose = TRUE, 
)

 test_especies <- ESPECIES
 test_provinces_dictionary <- ifn_provinces_dictionary
 

test_that("ifn_tree_table_process works as intended", {
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "DIA",
    "HT",
    "DENSITY"
  )
  
  # object


    expect_s3_class(
    test_res <- ifn_tree_table_process(
      test_input$tree_table[3],
      test_input$plots[3],
      test_input$province[3],
      test_especies
    ),
    "tbl"
  )
  
  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)
  

  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$province_code), 1)
  

  expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
  
  # errors
  expect_warning(
    test_error <- ifn_tree_table_process(
      NA_character_,
      test_input$plots[3],
      test_input$province[3],
      test_especies
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

   # error in department name, gives an empty tibble
   expect_s3_class(
     test_error <- suppressWarnings(ifn_tree_table_process(
       test_input$tree_table[15],
       test_input$plots[15],
       test_input$province[15],
       test_especies
     )),
     "tbl"
   )
   expect_true(nrow(test_error) < 1)
   # error in plot name, should return an empty tibble
   expect_s3_class(
     test_error <- suppressWarnings(ifn_tree_table_process(
       test_input$tree_table[13],
       test_input$plots[13],
       test_input$province[13],
       test_especies
     )),
     "tbl"
   )
   expect_true(nrow(test_error) < 1)
})

test_that("ifn_shrub_table_process works as intended", {
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "Hm",
    "COVER"
  )

  # object
  
  
  expect_s3_class(
    test_res <- ifn_shrub_table_process(
      test_input$shrub_table[1],
      test_input$plots[1],
      test_input$province[1],
      test_especies
    ),
    "tbl"
  )
  
  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)
  
  
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$province_code), 1)
  
  
  expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[1] |> as.character())
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[1]|> as.numeric())
  
  # errors
  expect_warning(
    test_error <- ifn_shrub_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$province[1],
      test_especies
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_shrub_table_process(
      test_input$shrub_table[15],
      test_input$plots[15],
      test_input$province[15],
      test_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_shrub_table_process(
      test_input$shrub_table[13],
      test_input$plots[13],
      test_input$province[13],
      test_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})


test_that("ifn_regen_table_process works as intended", {
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "NUMERO",
    "Hm",
    "REGENA"
  )
  
  # object
  
  
  expect_s3_class(
    test_res <- ifn_regen_table_process(
      test_input$regen_table[3],
      test_input$plots[3],
      test_input$province[3],
      test_especies
    ),
    "tbl"
  )
  
  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)
  
  
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$province_code), 1)
  
  
  expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
  
  # errors
  expect_warning(
    test_error <- ifn_regen_table_process(
      NA_character_,
      test_input$plots[3],
      test_input$province[3],
      test_especies
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_regen_table_process(
      test_input$regen_table[15],
      test_input$plots[15],
      test_input$province[15],
      test_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_regen_table_process(
      test_input$regen_table[13],
      test_input$plots[13],
      test_input$province[13],
      test_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

test_that("ifn_plot_table_process works as intended", {
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "province_code",
    "province_name_original",
    "ca_name_original",
    "PLOT",
    "YEAR",
    "version",
    "HOJA",
    "Huso",
    "COORDEX",
    "COORDEY",
    "COORD_SYS",
    "crs",
    "PENDIEN2",
    "SLOPE",
    "ELEV",
    "ASPECT",
    "soils"
  )
  
  # object
  
  
  expect_s3_class(
    test_res <- ifn_plot_table_process(
      test_input$plot_table[7],
      test_input$plots[7],
      test_input$province[7],
      test_provinces_dictionary
      
    ),
    "tbl"
  )
  
  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)
  
  
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$province_code), 1)
  
  
  expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[7] |> as.character())
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[7]|> as.numeric())
  
  # errors
  expect_warning(
    test_error <- ifn_plot_table_process(
      NA_character_,
      test_input$plots[3],
      test_input$province[3],
      test_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  
  # error in department name, gives an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_plot_table_process(
      test_input$regen_table[15],
      test_input$plots[15],
      test_input$province[15],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_plot_table_process(
      test_input$regen_table[13],
      test_input$plots[13],
      test_input$province[13],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

# tables process -----------------------------------------------------------------------------------

test_that("ffi_tables_process works as intended", {
  
  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #
  
  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))
  
  # tests data
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "PLOT",
    "YEAR",
    "version",
    "HOJA",
    "COORD_SYS",
    "COORD1",
    "COORD2",
    "crs",
    "PENDIEN2",
    "SLOPE", 
    "ELEV",
    "ASPECT",
    "tree",
    "understory",
    "regen",
    "soils"
  )
  
  # object
  expect_s3_class(
    test_res <- suppressWarnings(ifn_tables_process(
      test_provinces, test_input$version, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )
  
  # data integrity
  expect_named(test_res, expected_names)
  expect_true(all(unique(test_res$provinces) %in% names(test_plots)))
  
  # ### missing tables/plots
  # # tururu state shouldn't appear
  # # inexistent plots (91-0) shouldn't
  # # be present, so 31 of 33 elements in filter list
  # expect_false("tururu" %in% unique(test_res$province))
  # expect_identical(nrow(test_res), 31L)
  # 
  # ### missing random files
  # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # test_folder <- fs::path(Sys.getenv("ffi_path"), "missing_files_test")
  # # FLORE, without flore, the understory should be empty
  # fs::file_move(fs::path(test_folder, "FLORE.csv"), fs::path(test_folder, "_FLORE.csv"))
  # withr::defer({
  #   if (fs::file_exists(fs::path(test_folder, "_FLORE.csv"))) {
  #     fs::file_move(fs::path(test_folder, "_FLORE.csv"), fs::path(test_folder, "FLORE.csv"))
  #   }
  # })
  # expect_true(
  #   suppressWarnings(ffi_tables_process(
  #     test_departments, test_year, test_plots, test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   ) |>
  #     dplyr::pull(understory) |>
  #     purrr::list_rbind() |>
  #     dplyr::pull(shrub) |>
  #     purrr::list_rbind() |>
  #     nrow()) < 1
  # )
  # fs::file_move(fs::path(test_folder, "_FLORE.csv"), fs::path(test_folder, "FLORE.csv"))
  # 
  # # ARBRE, without ARBRE, the tree should be empty
  # fs::file_move(fs::path(test_folder, "ARBRE.csv"), fs::path(test_folder, "_ARBRE.csv"))
  # withr::defer({
  #   if (fs::file_exists(fs::path(test_folder, "_ARBRE.csv"))) {
  #     fs::file_move(fs::path(test_folder, "_ARBRE.csv"), fs::path(test_folder, "ARBRE.csv"))
  #   }
  # })
  # expect_true(
  #   suppressWarnings(ffi_tables_process(
  #     test_departments, test_year, test_plots, test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   ) |>
  #     dplyr::pull(tree) |>
  #     purrr::list_rbind() |>
  #     nrow()) < 1
  # )
  # fs::file_move(fs::path(test_folder, "_ARBRE.csv"), fs::path(test_folder, "ARBRE.csv"))
  # 
  # # ECOLOGIE, without ECOLOGIE, the soils should be empty, but also the plot info.
  # fs::file_move(fs::path(test_folder, "ECOLOGIE.csv"), fs::path(test_folder, "_ECOLOGIE.csv"))
  # withr::defer({
  #   if (fs::file_exists(fs::path(test_folder, "_ECOLOGIE.csv"))) {
  #     fs::file_move(fs::path(test_folder, "_ECOLOGIE.csv"), fs::path(test_folder, "ECOLOGIE.csv"))
  #   }
  # })
  # expect_error(
  #   suppressWarnings(ffi_tables_process(
  #     test_departments, test_year, test_plots, test_folder,
  #     .parallel_options = test_parallel_conf,
  #     .verbose = FALSE
  #   )),
  #   "Ooops!"
  # )
  # fs::file_move(fs::path(test_folder, "_ECOLOGIE.csv"), fs::path(test_folder, "ECOLOGIE.csv"))
  # 
})
