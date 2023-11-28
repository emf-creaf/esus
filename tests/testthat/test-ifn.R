skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# test data -----------------------------------------------------------------------------------

test_plots <- list(
  "01" = c(19,80,1120),
  "02"= c(11,444,1839),
  "03"= c(626,1021,23),
  "04"= c(233,5,445),
  "05" = c(61, 14, 328),
  "06" = c(2064,1138,325),
  "07" = c(679,114,499),
  "10" = c(3374,261),
  "12" = c(156,1463,377),
  "13" = c(51, 419,783),
  "17" = c(2003,629,2944),
  "23" = c(269,1460,444),
  "26" = c(960,495,172),
  "27" = c(90, 190,537),
  "30" = c(78,1223,1057),
  "33" = c(818,283,1483),
  "31" = c(135,761,1518),
  "38" = c(672,426,1557),
  "40" = c(412,1216,1728),
  "50" = c(172, 479,744),
  "49" = c(105,99,532)
   # "91" = c(1406115, 0),
   # "tururu" = 3555
)

test_provinces <- names(test_plots)
test_folder <- Sys.getenv("ifn_path")
   test_version = "ifn2"
# 
 test_input <- .build_ifn_input_with (
  test_version,
  test_provinces,
  test_plots,
  test_folder,
  .verbose = TRUE,
)

 test_especies <- ESPECIES
 test_provinces_dictionary <- ifn_provinces_dictionary

# 
# test_that("ifn_tree_table_process for ifn2 works as intended", {
# 
#   test_version = "ifn2"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
# 
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "PLOT",
#     "SP_CODE",
#     "SP_NAME",
#     "DIA",
#     "HT",
#     "DENSITY"
#   )
# 
#   # object
# 
# 
#     expect_s3_class(
#     test_res <- ifn_tree_table_process(
#       test_input$tree_table[3],
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "tbl"
#   )
# 
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
# 
# 
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
# 
# 
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
# 
#   # errors
#   expect_warning(
#     test_error <- ifn_tree_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
# 
#    # error in department name, gives an empty tibble
#    expect_s3_class(
#      test_error <- suppressWarnings(ifn_tree_table_process(
#        test_input$tree_table[66],
#        test_version,
#        test_input$plots[66],
#        test_input$province[66],
#        test_especies
#      )),
#      "tbl"
#    )
#    expect_true(nrow(test_error) < 1)
#    # error in plot name, should return an empty tibble
#    expect_s3_class(
#      test_error <- suppressWarnings(ifn_tree_table_process(
#        test_input$tree_table[64],
#        test_version,
#        test_input$plots[64],
#        test_input$province[64],
#        test_especies
#      )),
#      "tbl"
#    )
#    expect_true(nrow(test_error) < 1)
# })
# 
# 
# test_that("ifn_tree_table_process for ifn3 works as intended", {
#   
#   test_version<- "ifn3"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT", 
#     "province_code",
#     "Clase",
#     "Subclase",
#     "PLOT",
#     "SP_CODE",
#     "SP_NAME",
#     #tree number id in ifn4
#      "nArbol",
#     #CUALIDAD 6 = dead but providing functions
#     "Calidad",
#     "Forma",
#     #check codes to understand origin and trace of individuals
#     "OrdenIf2",
#     "OrdenIf3",
#     #diameter in cm
#     "DIA",
#     #height in m
#     "HT", 
#     "DENSITY"
#     
#   )
#   
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_tree_table_process(
#       test_input$tree_table[3],
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_tree_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_tree_table_process(
#       test_input$tree_table[66],
#       test_version,
#       test_input$plots[66],
#       test_input$province[66],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_tree_table_process(
#       test_input$tree_table[64],
#       test_version,
#       test_input$plots[64],
#       test_input$province[64],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   
#   
# })
# # 
# test_that("ifn_tree_table_process for ifn4 works as intended", {
#   
#   test_plots <- list(
#     # "01" = c(19,80,1120),
#     # # "02"= c(11,444,1839),
#     # "03"= c(626,1021,23),
#     # "04"= c(233,5,445),
#     # "05" = c(61, 14, 328),
#     "06" = c(2064,1138,325),
#     "07" = c(679,114,499),
#     "10" = c(3374,261)
#     # "12" = c(156,1463,377),
#     # "13" = c(51, 419,783),
#     # "17" = c(2003,629,2944),
#     # "23" = c(269,1460,444),
#     # "26" = c(960,495,172),
#     # "27" = c(90, 190,537),
#     # "30" = c(78,1223,1057),
#     # "33" = c(818,283,1483),
#     # "31" = c(135,761,1518),
#     # "38" = c(672,426,1557),
#     # "40" = c(412,1216,1728),
#     # # "50" = c(172, 479,744),
#     # "49" = c(105,99,532),
#      # "91" = c(1406115, 0),
#      # "tururu" = 3555
#   )
#   
#   test_provinces <- names(test_plots)
#   test_version<- "ifn4"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT", 
#     "province_code",
#     "Clase",
#     "Subclase",
#     "PLOT",
#     "SP_CODE",
#     "SP_NAME",
#     # #tree number id in ifn4
#      "nArbol",
#     #CUALIDAD 6 = dead but providing functions
#     "Calidad",
#     "Forma",
#     #check codes to understand origin and trace of individuals
#     "OrdenIf3",
#     "OrdenIf4",
#     #diameter in cm
#     "DIA",
#     #height in m
#     "HT", 
#     "DENSITY"
#   )
#   
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_tree_table_process(
#       test_input$tree_table[1],
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[1] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[1]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_tree_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_tree_table_process(
#       test_input$tree_table[9],
#       test_version,
#       test_input$plots[9],
#       test_input$province[9],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_tree_table_process(
#       test_input$tree_table[11],
#       test_version,
#       test_input$plots[11],
#       test_input$province[11],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   
#   
# })
# 
# test_that("ifn_shrub_table_process for ifn2 works as intended", {
# 
#   
#   test_version = "ifn2"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "PLOT",
#     "SP_CODE",
#     "SP_NAME",
#     "Hm",
#     "COVER"
#   )
# 
#   # object
# 
# 
#   expect_s3_class(
#     test_res <- ifn_shrub_table_process(
#       test_input$shrub_table[1],
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "tbl"
#   )
# 
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
# 
# 
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
# 
# 
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[1] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[1]|> as.numeric())
# 
#   # errors
#   expect_warning(
#     test_error <- ifn_shrub_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
# 
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[66],
#       test_version,
#       test_input$plots[66],
#       test_input$province[66],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[64],
#       test_version,
#       test_input$plots[64],
#       test_input$province[64],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })
# test_that("ifn_shrub_table_process for ifn3 works as intended", {
#   
#   
#   test_version = "ifn3"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "Clase", 
#     "Subclase",
#     "PLOT",
#     "SP_NAME",
#     "SP_CODE",
#     "HT",
#     "COVER"
#   )
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_shrub_table_process(
#       test_input$shrub_table[1],
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[1] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[1]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_shrub_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[66],
#       test_version,
#       test_input$plots[66],
#       test_input$province[66],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[64],
#       test_version,
#       test_input$plots[64],
#       test_input$province[64],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })
# test_that("ifn_shrub_table_process for ifn4 works as intended", {
#   
#   
#   
#   test_plots <- list(
#     # "01" = c(19,80,1120),
#     # # "02"= c(11,444,1839),
#     # "03"= c(626,1021,23),
#     # "04"= c(233,5,445),
#     # "05" = c(61, 14, 328),
#     "06" = c(2064,1138,325),
#     "07" = c(679,114,499),
#     "10" = c(3374,261)
#     # "12" = c(156,1463,377),
#     # "13" = c(51, 419,783),
#     # "17" = c(2003,629,2944),
#     # "23" = c(269,1460,444),
#     # "26" = c(960,495,172),
#     # "27" = c(90, 190,537),
#     # "30" = c(78,1223,1057),
#     # "33" = c(818,283,1483),
#     # "31" = c(135,761,1518),
#     # "38" = c(672,426,1557),
#     # "40" = c(412,1216,1728),
#     # # "50" = c(172, 479,744),
#     # "49" = c(105,99,532),
#     #this gies an error
#       # "91" = c(1406115, 0),
#       # "tururu" = 3555
#   )
#   
#   test_provinces <- names(test_plots)
#   test_version<- "ifn4"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "Clase", 
#     "Subclase",
#     "PLOT",
#     "SP_NAME",
#     "SP_CODE",
#     "HT",
#     "COVER"
#   )
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_shrub_table_process(
#       test_input$shrub_table[1],
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[1] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[1]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_shrub_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[9],
#       test_version,
#       test_input$plots[9],
#       test_input$province[9],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_shrub_table_process(
#       test_input$shrub_table[11],
#       test_version,
#       test_input$plots[11],
#       test_input$province[11],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })
# 
# 
# 
# test_that("ifn_regen_table_process for ifn2 works as intended", {
# 
#   
#   test_version = "ifn2"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "PLOT",
#     "SP_CODE",
#     "SP_NAME",
#     "NUMERO",
#     "Hm",
#     "REGENA"
#   )
# 
#   # object
# 
# 
#   expect_s3_class(
#     test_res <- ifn_regen_table_process(
#       test_input$regen_table[3],
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "tbl"
#   )
# 
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
# 
# 
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
# 
# 
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
# 
#   # errors
#   expect_warning(
#     test_error <- ifn_regen_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
# 
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[66],
#       test_version,
#       test_input$plots[66],
#       test_input$province[66],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[64],
#       test_version,
#       test_input$plots[64],
#       test_input$province[64],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })
# test_that("ifn_regen_table_process for ifn3 works as intended", {
#   
#   test_version = "ifn3"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "province_code",
#     "PLOT",
#     "Clase",
#     "Subclase",
#     "SP_CODE",
#     "SP_NAME",
#     "CatDes", 
#     "Tipo", 
#     "NumPies",
#     "Hm"
#   )
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_regen_table_process(
#       test_input$regen_table[3],
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_regen_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[66],
#       test_version,
#       test_input$plots[66],
#       test_input$province[66],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[64],
#       test_version,
#       test_input$plots[64],
#       test_input$province[64],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })
# test_that("ifn_regen_table_process for ifn4 works as intended", {
#   
#   
#   test_plots <- list(
# 
#      "06" = c(2064,1138,325),
#     "07" = c(679,114,499),
#     "10" = c(3374,261),
#     # "26" = c(960,495,172),
#     # "30" = c(78, 1223),
#     # "31" = c(135,761,1518),
#     # "33" = c(283),
#     # "40" = c(412,1216,1728),
#     # "49" = c(105,99,532)
#     
#     # "91" = c(1406115, 0),
#     # "tururu" = 3555
#   )
#   
#   test_provinces <- names(test_plots)
#   test_version <- "ifn4"
#   test_input <- .build_ifn_input_with (
#     test_version,
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE,
#   )
#   
#   expected_names <- c(
#       "ID_UNIQUE_PLOT",
#       "province_code",
#       "PLOT",
#       "Clase",
#       "Subclase",
#       "SP_CODE",
#       "SP_NAME",
#       "CatDes", 
#       "Tipo", 
#       "NumPies",
#       "Hm"
#   )
#   
#   # object
#   
#   
#   expect_s3_class(
#     test_res <- ifn_regen_table_process(
#       test_input$regen_table[3],
#       test_version,
#       test_input$plots[3],
#       test_input$province[3],
#       test_especies
#     ),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names, ignore.order = TRUE)
#   expect_true(nrow(test_res) > 0)
#   
#   
#   expect_length(unique(test_res$PLOT), 1)
#   expect_length(unique(test_res$province_code), 1)
#   
#   
#   expect_identical(unique(test_res$PLOT)|> as.character(), test_input$plots[3] |> as.character())
#   #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
#   expect_identical(unique(test_res$province_code) |> as.numeric(), test_input$province[3]|> as.numeric())
#   
#   # errors
#   expect_warning(
#     test_error <- ifn_regen_table_process(
#       NA_character_,
#       test_version,
#       test_input$plots[1],
#       test_input$province[1],
#       test_especies
#     ),
#     "Some files"
#   )
#   expect_s3_class(test_error, "tbl")
#   expect_true(nrow(test_error) < 1)
#   
#   # error in department name, gives an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[9],
#       test_version,
#       test_input$plots[9],
#       test_input$province[9],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
#   # error in plot name, should return an empty tibble
#   expect_s3_class(
#     test_error <- suppressWarnings(ifn_regen_table_process(
#       test_input$regen_table[11],
#       test_version,
#       test_input$plots[11],
#       test_input$province[11],
#       test_especies
#     )),
#     "tbl"
#   )
#   expect_true(nrow(test_error) < 1)
# })


test_that("ifn_plot_table_process for ifn2  works as intended", {

 
  

  test_version = "ifn2"
  
  test_input <- .build_ifn_input_with (
    test_version,
    test_provinces,
    test_plots,
    test_folder,
    .verbose = TRUE,
  )
  

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
      test_input$plot_table[1],
      test_version,
      test_input$plots[1],
      test_input$province[1],
      test_provinces_dictionary

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
    test_error <- ifn_plot_table_process(
      NA_character_,
      test_version,
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
      test_input$regen_table[66],
      test_version,
      test_input$plots[66],
      test_input$province[66],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_plot_table_process(
      test_input$regen_table[64],
      test_version,
      test_input$plots[64],
      test_input$province[64],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

test_that("ifn_plot_table_process for ifn3  works as intended", {
  
  
  test_plots <- list(
    
    "06" = c(2064,1138,325),
    "07" = c(679,114,499),
    "10" = c(3374,261),
    "26" = c(960,495,172),
    "30" = c(78, 1223),
    "31" = c(135,761,1518),
    "33" = c(283),
    "40" = c(412,1216,1728),
    "49" = c(105,99,532)
    
     # "91" = c(1406115, 0),
     # "tururu" = 3555
  )
  
  test_provinces <- names(test_plots)
  test_version <- "ifn3"
  test_input <- .build_ifn_input_with (
    test_version,
    test_provinces,
    test_plots,
    test_folder,
    .verbose = TRUE,
  )
  
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_code",
    "province_name_original",
    "PLOT",
    "Clase", 
    "Subclase",
    "YEAR",
    "version",
    "Tipo",
    "ASPECT",
    "SLOPE",
    "crs",
    "COORD_SYS",
    "COORDEX",
    "COORDEY",
    "HOJA",
    "Huso",
    "soils"
  )
  
  # object
  
  
  expect_s3_class(
    test_res <- ifn_plot_table_process(
      test_input$plot_table[1],
      test_version,
      test_input$plots[1],
      test_input$province[1],
      test_provinces_dictionary
      
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
    test_error <- ifn_plot_table_process(
      NA_character_,
      test_version,
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
      test_input$regen_table[26],
      test_version,
      test_input$plots[26],
      test_input$province[26],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_error <- suppressWarnings(ifn_plot_table_process(
      test_input$regen_table[24],
      test_version,
      test_input$plots[24],
      test_input$province[24],
      test_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_error) < 1)
})

# # tables process -----------------------------------------------------------------------------------
# 
# test_that("ifn_tables_process works as intended", {
# 
#   ### TODO
#   # - test what happens when some tables are NAs (not found when building the input)
#   # -
#   #
# 
#   # tests config
#   test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
#   future::plan(future::multisession, workers = 3)
#   withr::defer(future::plan(future::sequential))
# 
#   # tests data
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "COUNTRY",
#     "YEAR",
#     "ca_name_original",
#     "province_name_original",
#     "province_code",
#     "PLOT",
#     "version",
#     "HOJA",
#     "COORD_SYS",
#     "COORD1",
#     "COORD2",
#     "crs",
#     "PENDIEN2",
#     "SLOPE",
#     "ELEV",
#     "ASPECT",
#     "tree",
#     "understory",
#     "regen",
#     "soils"
#   )
# 
#   # object
#   expect_s3_class(
#     test_res <- suppressWarnings(ifn_tables_process(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     )),
#     "tbl"
#   )
# 
#   # data integrity
#   expect_named(test_res, expected_names)
#   expect_true(all(unique(test_res$province_code) %in% names(test_plots)))
# 
#   # ### missing tables/plots
#   # # tururu state shouldn't appear
#   # # inexistent plots (91-0) shouldn't
#   # # be present, so 12 of 14 elements in filter list
#   # expect_false("tururu" %in% unique(test_res$province))
#   # expect_identical(nrow(test_res), 12L)
# 
#   # ### missing random files
#   # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
#   # to do:
# 
# 
# 
# })
# 
# 
# # ifn_to_tibble -------------------------------------------------------------------------------
# 
# 
# test_that("ifn_to_tibble works as intended", {
# 
#   # tests config
#   test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
#   future::plan(future::multisession, workers = 3)
#   withr::defer(future::plan(future::sequential))
# 
#   # tests data
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "COUNTRY",
#     "YEAR",
#     "ca_name_original",
#     "province_name_original",
#     "province_code",
#     "PLOT",
#     "version",
#     "HOJA",
#     "COORD_SYS",
#     "COORD1",
#     "COORD2",
#     "crs",
#     "PENDIEN2",
#     "SLOPE",
#     "ELEV",
#     "ASPECT",
#     "tree",
#     "understory",
#     "regen",
#     "soils")
#   test_version <- c("ifn2")
# 
# 
#   # object
#   expect_s3_class(
#     test_res <- suppressWarnings(ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     )),
#     "tbl"
#   )
# 
#   # data integrity
#   expect_named(test_res, expected_names)
#   expect_false("tururu" %in% unique(test_res$province_code))
#   expect_identical(nrow(test_res), 62L) # two plots dont exist, so 2x2=4 rows less
#   expect_true(all(unique(test_res$province_code) %in% names(test_plots)))
#   expect_true(all(unique(test_res$version) %in% test_version))
# 
#   ### test all assertions done in ifn_to_tibble
#   # provinces
#   expect_error(
#     ifn_to_tibble(
#       1:7, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "provinces must be a character vector with at least one"
#   )
#   expect_error(
#     ifn_to_tibble(
#       character(), test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "departments must be a character vector with at least one"
#   )
#   # years
#   expect_error(
#     ifn_to_tibble(
#       test_departments, as.character(test_years), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "years must be a numeric vector with at least one"
#   )
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, charater(), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "years must be a character vector with at least one"
#   )
#   # folder
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, "nonexistantfolder",
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "Folder especified"
#   )
#   # filter list (TODO as testng interactive commands is tricky)
#   # parallel options
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = list(scheduling = 2L, stdout = TRUE),
#       .verbose = FALSE
#     ),
#     ".parallel_options"
#   )
#   # verbose
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = "FALSE"
#     ),
#     ".verbose"
#   )
#   # ancillary data (tested just by providing an existing wrong folder)
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, ".",
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "must be present"
#   )
# 
#   # what to expect if departments or filter list are all wrong
#   expect_true(
#     suppressWarnings(ifn_to_tibble(
#       "tururu", test_version, list("tururu" = 0), test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ) |> nrow()) < 1
#   )
# })
# 
# # ifn_to_tibble -------------------------------------------------------------------------------
# 
# 
# test_that("ifn_to_tibble  ifn 2-3-4 works as intended", {
#   
#   test_plots <- list(
#     "01" = c(19,80,1120),
#     "02"= c(11,444,1839),
#     "03"= c(626,1021,23),
#     "04"= c(233,5,445),
#     "05" = c(61, 14, 328),
#     "06" = c(2064,1138,325),
#     "07" = c(679,114,499),
#     "10" = c(3374,261),
#     "12" = c(156,1463,377),
#     "13" = c(51, 419,783),
#     "17" = c(2003,629,2944),
#     "23" = c(269,1460,444),
#     "26" = c(960,495,172),
#     "27" = c(90, 190,537),
#     "30" = c(78,1223,1057),
#     "33" = c(818,283,1483),
#     "31" = c(135,761,1518),
#     "38" = c(672,426,1557),
#     "40" = c(412,1216,1728),
#     "50" = c(172, 479,744),
#     "49" = c(105,99,532),
#     # "91" = c(1406115, 0),
#     # "tururu" = 3555
# 
#   
#   "06" = c(2064,1138,325),
#   "07" = c(679,114,499),
#   "10" = c(3374,261),
#   "26" = c(960,495,172),
#   "30" = c(78, 1223),
#   "31" = c(135,761,1518),
#   "33" = c(283),
#   "40" = c(412,1216,1728),
#   "49" = c(105,99,532)
#   )
#   
#   
#   # tests config
#   test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
#   future::plan(future::multisession, workers = 3)
#   withr::defer(future::plan(future::sequential))
#   
#   # tests data
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "COUNTRY",
#     "YEAR",
#     "ca_name_original",
#     "province_name_original",
#     "province_code",
#     "PLOT",
#     "version",
#     "HOJA",
#     "COORD_SYS",
#     "COORD1",
#     "COORD2",
#     "crs",
#     "PENDIEN2",
#     "SLOPE",
#     "ELEV",
#     "ASPECT",
#     "tree",
#     "understory",
#     "regen",
#     "soils")
#   test_version <- c("ifn2")
#   
#   
#   # object
#   expect_s3_class(
#     test_res <- suppressWarnings(ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     )),
#     "tbl"
#   )
#   
#   # data integrity
#   expect_named(test_res, expected_names)
#   expect_false("tururu" %in% unique(test_res$province_code))
#   expect_identical(nrow(test_res), 62L) # two plots dont exist, so 2x2=4 rows less
#   expect_true(all(unique(test_res$province_code) %in% names(test_plots)))
#   expect_true(all(unique(test_res$version) %in% test_version))
#   
#   ### test all assertions done in ifn_to_tibble
#   # provinces
#   expect_error(
#     ifn_to_tibble(
#       1:7, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "provinces must be a character vector with at least one"
#   )
#   expect_error(
#     ifn_to_tibble(
#       character(), test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "departments must be a character vector with at least one"
#   )
#   # years
#   expect_error(
#     ifn_to_tibble(
#       test_departments, as.character(test_years), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "years must be a numeric vector with at least one"
#   )
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, charater(), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "years must be a character vector with at least one"
#   )
#   # folder
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, "nonexistantfolder",
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "Folder especified"
#   )
#   # filter list (TODO as testng interactive commands is tricky)
#   # parallel options
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = list(scheduling = 2L, stdout = TRUE),
#       .verbose = FALSE
#     ),
#     ".parallel_options"
#   )
#   # verbose
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = "FALSE"
#     ),
#     ".verbose"
#   )
#   # ancillary data (tested just by providing an existing wrong folder)
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, ".",
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "must be present"
#   )
#   
#   # what to expect if departments or filter list are all wrong
#   expect_true(
#     suppressWarnings(ifn_to_tibble(
#       "tururu", test_version, list("tururu" = 0), test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ) |> nrow()) < 1
#   )
# })
# 
