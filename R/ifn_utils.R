.build_ifn_input_with <- function(
    version, provinces, filter_list, folder, .verbose, .call = rlang::caller_env()
) {

   # browser()
  # first, if is null filter list, create it
  if (is.null(filter_list)) {
    filter_list <- list("24" = c(6))
    ## TODO
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve {.strong {filter_list |> purrr::flatten() |> purrr::flatten_dbl() |> length()}} plots for {.val {version}}"
    )), .verbose
  )

  filter_list <- filter_list |>
    tibble::enframe() |>
    tidyr::unnest(cols = value) |>
    purrr::set_names(c("province", "plots")) |>
    dplyr::mutate(
      plots = as.character(plots),
      version = as.character(version)
    ) |>
    dplyr::select(province, plots, version) |>
    dplyr::mutate(
      plot_table = .build_ifn_file_path(
        province,
        type = "plot",
        version,
        folder,
        .call = .call
      ),
      tree_table = .build_ifn_file_path(
        province,
        type = "tree",
        version,
        folder,
        .call = .call
      ),
      shrub_table = .build_ifn_file_path(
        province,
        type = "shrub",
        version,
        folder,
        .call = .call
      ),
      regen_table = .build_ifn_file_path(
        province,
        type = "regen",
        version,
        folder,
        .call = .call
      ),
      coord_table = ifelse(
        test = version %in% c("ifn3", "ifn4"),
        yes = .build_ifn_file_path(
          province,
          type = "coord",
          version,
          folder,
          .call = .call
        ),
        no = NA_character_
      )
  )

  # if (version %in% c("ifn3", "ifn4")){
  #
  #   filter_list<- filter_list|>
  #     dplyr::mutate(
  #       coord_table = .build_ifn_file_path(
  #         province,
  #         type = "coord",
  #         version,
  #         folder,
  #         .call = .call
  #       )
  #     )
  #
  #
  # }

  return(filter_list)
}


#' Read IFN data
#'
#' Read the different IFN db formats
#'
#' This functions checks the IFN file format (by checking the file extension) and
#' dispatch the corresponding function: \code{\link[foreign]{read.dbf}} for IFN2 and
#' \code{\link[Hmisc]{mdb.get}} for IFN3 and 4
#'
#' @param input file to read as it appears in the input data frame
#' @param ... extra arguments for the reading function
#'
#' @return A data.frame with the inventory table
#'
#' @noRd
.read_ifn_data <- function(input, colnames, ...) {


  # first, if ifn3 or ifn4 we have file name and table name in a character separated by "|" but if
  # we have ifn2 we have the name of the corresponding file.
  table_name <- NULL
  if (stringr::str_detect(input, "\\|")) {
    file_and_table <- stringr::str_split(input, "\\|", n = 2, simplify = TRUE)
    input <- file_and_table[1]
    table_name <- file_and_table[2]
  }

  # we need to check if dbf or accdb, and use the correct function to
  # simply read and return, the main function .read_inventory_data will take care of the rest
  file_ext <- fs::path_ext(input)

  res <- switch(
    file_ext,
    "DBF" = foreign::read.dbf(input, as.is = FALSE) |>
      dplyr::select(colnames) |>
      dplyr::mutate(
        PROVINCIA = as.character(PROVINCIA),
        PROVINCIA = stringr::str_pad(PROVINCIA, width = 2, side = "left", pad = "0"),
        ESTADILLO = as.character(ESTADILLO)
    ),
    "accdb" = .read_accdb_data(input, table_name) |>
      dplyr::select(colnames) |>
      dplyr::mutate(
      Estadillo = as.character(Estadillo)
      )
  )

  return(res)
}

.read_accdb_data <- function(input, table_name) {

  # first thing check the SO
  so <- .Platform$OS.type

  # unix or windows?
  if (so == "unix") {
    if (isFALSE(
      .sys_cmd_warning("mdb-tables", c(
        "x" = "{.emph mdbtools} system utility not found.",
        "i" = "{.emph mdbtools} is needed to read Spanish inventory (IFN) data versions 3 and 4.",
        "i" = "Please check your package manager (apt, brew, port...) to install it.",
        "i" = "More info at https://github.com/mdbtools/mdbtools"
      ))
    )) {
      cli::cli_abort("Aborting")
    }

    res <- Hmisc::mdb.get(input, tables = table_name)
  } else {
    res <- RODBC::odbcConnectAccess2007(input) |>
      RODBC::sqlFetch(table_name)
  }

  return(res)

}

.ifn4_prov_code_translator <- function(province) {
  res <- ifn_provinces_dictionary |>
    dplyr::filter(province_code %in% province) |>
    dplyr::pull(ifn4_files_labels)

  # In case province provided is not in the dictionary (because error, tururu tests)
  # then res is going to be character(0), so we convert to empty string
  if (length(res) < 1) {
    res <- ""
  }

  return(res)
}

.build_ifn_file_path <- function(province, type, version, folder = ".", .call = rlang::caller_env()) {

   # browser()
  #
  # Ok, so here we need to do some things. Depending on .version (the IFN version) we need to
  # provide different things.
  purrr::pmap_chr(
    .l = list(province, type, version),
    .f = \(province, type, version) {

      if (version == "ifn2") {
        file_name <- switch(
          type,
          "plot" = fs::path(folder, glue::glue("DATEST{province}.DBF")),
          "tree" = fs::path(folder, glue::glue("PIESMA{province}.DBF")),
          "shrub" = fs::path(folder, glue::glue("MATORR{province}.DBF")),
          "regen" = fs::path(folder, glue::glue("PIESME{province}.DBF"))
        )

        # return path
        table_path <- file_name
      }

      if (version == "ifn3") {
        file_name <- fs::path(folder, glue::glue("Ifn3p{province}.accdb"))
        table_name <- switch(
          type,
          "tree" = "PCMayores",
          "shrub" = "PCMatorral",
          "regen" = "PCRegenera",
          "plot" = "PCParcelas",
          "coord" = "PCDatosMap"
          # "coord" = "Listado definitivo"
          # others needed
        )

        table_path <- glue::glue("{file_name}|{table_name}")
      }

      if (version == "ifn4") {

        # this is a little trickier. We don't always have provinces so, we need to translate the
        # province code to the correct ifn4 label
        province <- .ifn4_prov_code_translator(province)
        file_name <- fs::path(folder, glue::glue("Ifn4_{province}.accdb"))
        table_name <- switch(
          type,
          "tree" = "PCMayores",
          "shrub" = "PCMatorral",
          "regen" = "PCRegenera",
          "plot" = "PCParcelas",
          "coord" = "PCDatosMap"
          # others needed
        )

        table_path <- glue::glue("{file_name}|{table_name}")
      }

      # check file exists
      if (!fs::file_exists(file_name)) {
        cli::cli_warn(c(
          "{.path {file_name}} file doesn't exists",
          "!" = "Please check if {.path {folder}} is the correct path",
          "i" = "Skipping {.path {file_name}}"
        ), call = .call)
        return(NA_character_)
      }
      return(table_path)
    }
  )
}

#' obtaining crs for different coordinate systems IFN
#'
#' This function  get_crs reads var huso and coordinate system of plot table process the tree table for one plot and one IFN
#'
#' @param tree_data file that contains the tree table for that plot
#' @param plot plot_id code
#' @param province province code
#' @param ref_tree_ifn data frame containing the species code reference table
#'
#' @noRd
#
#
# get_crs <- function(Huso,COORD_SYS){
#
#   if (Huso == 30 & COORD_SYS == "ED50"){
#     crs = 23030
#   }
#
#   if (Huso == 31 & COORD_SYS == "ED50"){
#     crs =  4326
#   }
#
#   if (Huso == 29 & COORD_SYS == "ED50" ){
#     crs = 23029
#   }
#
#   if (Huso == 30 & COORD_SYS == "ETRS89"){
#     crs = 25830
#   }
#
#   if (Huso == 31 & COORD_SYS == "ETRS89" ){
#     crs = 25831
#   }
#
#   if (Huso == 29 & COORD_SYS == "ETRS89"){
#     crs = 25829
#   }
#
#   if (Huso == 28 & COORD_SYS == "ED50"){
#     crs = 23028
#   }
#
#   if (Huso == 28 & COORD_SYS == "WGS84"){
#     crs =32628
#   }
#
#   return(crs)
#
# }
