skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)

# build path and input ------------------------------------------------------------------------

test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn2 ", {
 
   test_plots <- list(
    
    "06" = c(2064,1138,325),
    "07" = c(679,114,499),
    "10" = c(3374,261),
    "26" = c(960,495,172),
    "30" = c(78, 1223),
    "31" = c(135,761,1518),
    "33" = c(283),
    "40" = c(412,1216,1728),
    "49" = c(105,99,532),
      "tururu" = c(5)
  )
  
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
    .build_ifn_input_with( test_version, test_provinces, test_plots, ".", .verbose = TRUE),
    "file doesn't exist"
  )
  expect_message(
    .build_ifn_input_with( test_version, test_provinces, test_plots, test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    test_res <-
      .build_ifn_input_with( test_version,test_provinces, test_plots, test_folder, .verbose = FALSE)
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
    test_res[["plot_table"]][24],
    NA_character_
  )


  # ## Test filter_list = NULL - this needs to be implemenetd
  # expect_s3_class(
  #   test_res_filter_list <- suppressWarnings(
  #     .build_ifn_input_with(test_version, test_provinces, NULL, test_folder, .verbose = FALSE)
  #   ),
  #   "tbl"
  # )
  #  expect_false("tururu" %in% (test_res_filter_list$province |> unique()))
  # expect_true(all((test_res_filter_list$province |> unique()) %in% test_provinces))
  #  # expect_length(test_res_filter_list$province |> unique(), length(test_provinces) - 1)
})
test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn3 ", {
  
  test_plots <- list(
    
    "06" = c(2064,1138,325),
    "07" = c(679,114,499),
    "10" = c(3374,261),
    "26" = c(960,495,172),
    "30" = c(78, 1223),
    "31" = c(135,761,1518),
    "33" = c(283),
    "40" = c(412,1216,1728),
    "49" = c(105,99,532),
    "tururu" = c(5)
  )
  
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
    .build_ifn_input_with( test_version, test_provinces, test_plots, ".", .verbose = TRUE),
    "file doesn't exist"
  )
  expect_message(
    .build_ifn_input_with( test_version, test_provinces, test_plots, test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    test_res <-
      .build_ifn_input_with( test_version,test_provinces, test_plots, test_folder, .verbose = FALSE)
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
    test_res[["plot_table"]][24],
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

test_that(".build_ifn_input_with and .build_ifn_file_path work as intended for ifn4 ", {
  
  test_plots <- list(
    
    "06" = c(2064,1138,325),
    "07" = c(679,114,499),
    "10" = c(3374,261),
    "26" = c(960,495,172),
    "30" = c(78, 1223),
    "31" = c(135,761,1518),
    "33" = c(283),
    "40" = c(412,1216,1728),
    "49" = c(105,99,532),
     "tururu" = c(5)
  )
  
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
  
  
  # # warnings and messages
  # expect_warning(
  #   .build_ifn_input_with( test_version, test_provinces, test_plots, ".", .verbose = TRUE),
  #   "file doesn't exist"
  # )
  # expect_message(
  #   .build_ifn_input_with( test_version, test_provinces, test_plots, test_folder, .verbose = TRUE),
  #   "Getting ready to retrieve"
  # )
  expect_no_message(
    test_res <-
      .build_ifn_input_with( test_version,test_provinces, test_plots, test_folder, .verbose = FALSE)
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
    test_res[["plot_table"]][24],
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