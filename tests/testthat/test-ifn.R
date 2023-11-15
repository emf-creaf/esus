skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# test data -----------------------------------------------------------------------------------

test_plots <- list(
  "05" = c(61, 140, 328),
  "10" = c(2101,3374,261),
  "13" = c(51, 419,783),
  "27" = c(90, 190,537)
   # "91" = c(1406115, 0),
   #  "tururu" = 3555
)

test_provinces <- names(test_plots)
test_folder <- Sys.getenv("ifn_path")
test_input <- .build_ifn_input_with (
  version="ifn2",
  test_provinces,
  test_plots, 
  test_folder,
  .verbose =TRUE, 
)



test_especies <- 
  .read_excel_sheet(test_folder, "MaximaActualidad_ATOMaDic2022_dd.xlsx", "ESPECIES") |>
    dplyr::as_tibble()


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

  # # error in department name, gives an empty tibble
  # expect_s3_class(
  #   test_error <- suppressWarnings(ifn_tree_table_process(
  #     test_input$tree_table[15],
  #     test_input$plots[15],
  #     test_input$province[15],
  #     test_especies
  #   )),
  #   "tbl"
  # )
  # expect_true(nrow(test_error) < 1)
  # # error in plot name, should return an empty tibble
  # expect_s3_class(
  #   test_error <- suppressWarnings(ifn_tree_table_process(
  #     test_input$tree_table[13],
  #     test_input$plots[13],
  #     test_input$province[13],
  #     test_especies
  #   )),
  #   "tbl"
  # )
  # expect_true(nrow(test_error) < 1)
})


test_that("ifn_shrub_table_process works as intended", {
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "HT",
    "COVER"
  )

  # object
  
  
  expect_s3_class(
    test_res <- ifn_shrub_table_process(
      test_input$shrub_table[3],
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
    test_error <- ifn_shrub_table_process(
      NA_character_,
      test_input$plots[3],
      test_input$province[3],
      test_especies
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  
  # # error in department name, gives an empty tibble
  # expect_s3_class(
  #   test_error <- suppressWarnings(ifn_tree_table_process(
  #     test_input$tree_table[15],
  #     test_input$plots[15],
  #     test_input$province[15],
  #     test_especies
  #   )),
  #   "tbl"
  # )
  # expect_true(nrow(test_error) < 1)
  # # error in plot name, should return an empty tibble
  # expect_s3_class(
  #   test_error <- suppressWarnings(ifn_tree_table_process(
  #     test_input$tree_table[13],
  #     test_input$plots[13],
  #     test_input$province[13],
  #     test_especies
  #   )),
  #   "tbl"
  # )
  # expect_true(nrow(test_error) < 1)
})