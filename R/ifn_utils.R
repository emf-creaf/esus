#' Build the IFN input data frame to iterate by plots for the specified version
#'
#' IFN input table creator
#'
#' This function takes the user input (version, provinces, plots and folder) and build the input to
#' be able to iterate by plots in a year. If no plots filter list is provided, this function uses
#' \code{\link{.get_plots_from_province}} and \code{\link{.transform_plot_summary_ifn}} to create a
#' \code{filter_list} with all plots for each province for that year.
#'
#' @inheritParams ifn_tables_process
#'
#' @return A data frame with state, county, plot and table file names
#'
#' @noRd
.build_ifn_input_with <- function(
  version, provinces, filter_list, folder, .verbose, .call = rlang::caller_env()
) {

  # first, if is null filter list, create it
  if (is.null(filter_list)) {
    get_plots_safe <- purrr::safely(
      .get_plots_from_province,
      otherwise = tibble::tibble(
        "version" = vector(),
        "province_name_original" = vector(),
        "id_unique_code" = vector(),
        "crs" = vector(),
        "COORDEX" = vector(),
        "COORDEY" = vector(),
        "geometry" = vector()
      )
    )
    transform_safe <- purrr::safely(
      .transform_plot_summary_ifn,
      otherwise = list()
    )

    filter_list <- purrr::map(
      provinces,
      .f = \(province) {
        res <- get_plots_safe(province, folder, version, .call = .call)[["result"]] |>
          transform_safe(version, province)
        res[["result"]]
      }
    ) |>
      purrr::flatten()
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve
      {.strong {filter_list |> purrr::flatten() |> as.character() |> length()}}
      plots for {.val {version}}"
    ), call = .call), .verbose
  )

  input_df <- filter_list |>
    tibble::enframe() |>
    tidyr::unnest(cols = "value") |>
    purrr::set_names(c("province", "plots")) |>
    dplyr::mutate(
      plots = as.character(.data$plots),
      version = as.character(version)
    ) |>
    dplyr::select("province", "plots", "version") |>
    dplyr::mutate(
      plot_table = .build_ifn_file_path(
        .data$province,
        type = "plot",
        version,
        folder,
        .call = .call
      ),
      tree_table = .build_ifn_file_path(
        .data$province,
        type = "tree",
        version,
        folder,
        .call = .call
      ),
      shrub_table = .build_ifn_file_path(
        .data$province,
        type = "shrub",
        version,
        folder,
        .call = .call
      ),
      regen_table = .build_ifn_file_path(
        .data$province,
        type = "regen",
        version,
        folder,
        .call = .call
      ),
      coord_table = ifelse(
        test = version %in% c("ifn3", "ifn4"),
        yes = .build_ifn_file_path(
          .data$province,
          type = "coord",
          version,
          folder,
          .call = .call
        ),
        no = NA_character_
      )
    )

  return(input_df)
}

#' Get plots from one province
#'
#' Get plots from one province
#'
#' This function takes one province and return an \code{\link[sf]{sf}} objects with all plots on it
#' for the version provided. Only works for one province as in the IFN each province have its own
#' file.
#'
#' @param province province two numbers code
#' @param folder The path to the folder containing the IFN db files, as character.
#'
#' @return An \code{\link[sf]{sf}} object with the province plots
#'
#' @noRd
.get_plots_from_province <- function(province, folder, version, .call = rlang::caller_env()) {

  plot_path <- .build_ifn_file_path(province, "plot", version, folder, .call = .call)

  coord_path <- ifelse(
    test = version %in% c("ifn3", "ifn4"),
    yes = .build_ifn_file_path(province, type = "coord", version, folder, .call = .call),
    no = NA_character_
  )

  if (is.na(plot_path)) {
    cli::cli_abort(c(
      "{.path {folder}} folder doesn't contain the file corresponding to {.val {version}} version,
      aborting."
    ), call = .call)
  }

  plots_arg_value <- rlang::quo(.data$id_unique_code)
  # If file exists, business as usual. We use the general function (ifn_plot_table_process), because
  # it takes care of the version logic for us, DRY!!!
  # The only thing we need to take care of is the dancing coord ref systems. But for that is the crs
  # variable, so we group and transform to common crs
  # browser()
  res <- ifn_plot_table_process(
    plot_path, coord_path, version, plots_arg_value, province, ifn_provinces_dictionary
  ) |>
    dplyr::filter(!is.na(.data$id_unique_code)) |>
    dplyr::select(
      "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "crs", "COORDEX", "COORDEY"
    ) |>
    dplyr::group_by(.data$crs) |>
    dplyr::group_modify(
      .f = \(crs_group, crs_code) {
        crs_group |>
          sf::st_as_sf(
            coords = c("COORDEX", "COORDEY"),
            crs = sf::st_crs(unique(crs_code[["crs"]]))
          ) |>
          sf::st_transform(crs = 4326)
      }
    ) |>
    sf::st_as_sf()

  return(res)
}

#' Transform plots sf objects into a filter list
#'
#' Plots into filter list
#'
#' This function gets plots returned from \code{\link{.get_plots_from_province}} and transform
#' them in a valid \code{filter_list} for using in \code{\link{ifn_to_tibble}}.
#'
#' @param plot_summary \code{\link[sf]{sf}} object from \code{\link{show_plots_from_ifn}}
#' @param versions IFN versions to filter by
#' @param provinces Character vector with the province two-number codes to filter by
#'
#' @return A valid \code{filter_list} with the needed structure for \code{ifn_to_tibble}
#'
#' @noRd
.transform_plot_summary_ifn <- function(plot_summary, versions, provinces) {

  filter_list <- plot_summary |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$version %in% versions, .data$province_code %in% provinces) |>
    dplyr::select("province_code", "id_unique_code") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$province_code) |>
    dplyr::summarise(plots = list(.data$id_unique_code), .groups = "keep") |>
    dplyr::group_map(.f = \(province_plots, province_code) {
      tibble::deframe(province_plots) |>
        # list() |>
        purrr::set_names(province_code[[1]])
    }) |>
    purrr::flatten()

  return(filter_list)
}

#' Get available plots from provinces
#'
#' Obtain an sf of available plots in the specified provinces
#'
#' This function retrieves all plots for the provinces in \code{provinces} argument and return an sf
#' object.
#'
#' @param folder Path to IFN db files
#' @param provinces Character vector with two-number province codes
#' @param versions IFN versions vector
#'
#' @return An \code{\link[sf]{sf}} object with all plots available in \code{provinces} for the
#'   provided IFN version
#'
#' @noRd
show_plots_from_ifn <- function(folder, provinces, versions, .call = rlang::caller_env()) {
  # safe version
  get_plots_safe <- purrr::safely(
    .get_plots_from_province,
    otherwise = NULL
  )

  res <- purrr::map(
    provinces,
    .f = \(prov) {
      purrr::map(
        versions,
        .f = \(version) {
          get_plots_safe(prov, folder, version, .call = .call)$result
        }
      ) |>
        purrr::list_rbind()
    }
  ) |>
    purrr::keep(.p = \(i) {
      nrow(i) > 0
    }) |>
    purrr::list_rbind()

  if (nrow(res) < 1) {
    cli::cli_abort(c(
      "No data found at {.path {folder}} for the provided province codes",
      "i" = "Please check if {.path {folder}} is the correct folder for IFN data"
    ), call = .call)
  }

  res <- res |>
    sf::st_as_sf()

  return(res)
}

#' Read IFN data
#'
#' Read the different IFN db formats
#'
#' This functions checks the IFN file format (by checking the file extension) and
#' dispatch the corresponding function: \code{\link[foreign]{read.dbf}} for IFN2 and
#' \code{\link{.read_accdb_data}} for IFN3 and 4
#'
#' @param input file to read as it appears in the input data frame
#' @param colnames character vector with column names to select when reading
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
    "DBF" = foreign::read.dbf(input, as.is = TRUE) |>
      dplyr::select(dplyr::any_of(colnames)) |>
      dplyr::mutate(
        PROVINCIA = stringr::str_pad(
          as.character(.data$PROVINCIA), width = 2, side = "left", pad = "0"
        ),
        ESTADILLO = as.character(.data$ESTADILLO)
      ) |>
      .ifn_unique_id_creator(...),
    "accdb" = .read_accdb_data(input, table_name) |>
      dplyr::select(dplyr::any_of(colnames)) |>
      dplyr::mutate(
        Estadillo = as.character(.data$Estadillo),
        Subclase = .ifn_subclass_fixer(.data$Subclase)
      ) |>
      .ifn_unique_id_creator(...)
  )

  return(res)
}

#' Read IFN accdb data
#'
#' Read the IFN db data from IFN 3 and 4
#'
#' This functions calls the corresponding reading function based on the OS of the user. In Windows
#' \code{\link[RODBC]{odbcConnectAccess2007}} and \code{\link[RODBC]{sqlFetch}} are used, while
#' in unix \code{\link[Hmisc]{mdb.get}} is used.
#'
#' @param input file to read as it appears in the input data frame.
#' @param table_name character with the table name to read from the accdb file.
#'
#' @return A data.frame with the inventory table
#'
#' @noRd
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

    # in linux, space characters must be escaped
    if (stringr::str_detect(input, "(Ifn4_.* .*\\.accdb)$")) {
      input <- stringr::str_replace(input, "(Ifn4_.* .*\\.accdb)$", "'\\1'")
    }

    # read the table
    res <- Hmisc::mdb.get(input, tables = table_name)
  } else {
    res <- RODBC::odbcConnectAccess2007(input) |>
      RODBC::sqlFetch(table_name)
  }

  return(res)

}

#' Helper to translate province code to IFN 4 province/autnomous community name
#'
#' Province code to IFN 4 name
#'
#' IFN4 data doesn't follow the one province one file convention. Some ACs have all provinces in one
#' file. This function translates the province code to the corresponding IFN4 name when creating
#' inputs or doing other operations.
#'
#' @param provinces Character vector with two-number province codes
#'
#' @return a vector of the same length as \code{provinces} with the corresponding IFN4 names
#'
#' @noRd
.ifn4_prov_code_translator <- function(provinces) {
  res <- ifn_provinces_dictionary |>
    dplyr::filter(.data$province_code %in% provinces) |>
    dplyr::pull(.data$ifn4_files_labels)

  # In case province provided is not in the dictionary (because error, tururu tests)
  # then res is going to be character(0), so we convert to empty string
  if (length(res) < 1) {
    res <- ""
  }

  return(res)
}

#' Create the path and system call for reading IFN db files
#'
#' Create IFN db file path with extra sugar
#'
#' This functions builds the path to IFN db files based on the province and the version of the IFN.
#' Also, usign the \code{type} argument, we add the corresponding table name to read from the db
#' files to avoid reading all tables.
#'
#' @param province Character vector with two-number code for provinces.
#' @param type Character, table type. One of "tree", "plot", "shrub", "regen", "coord".
#' @param folder Character, path to the folder with the FIA csv files.
#'
#' @return Character vector with the paths (and table name when necessary) to use with
#'   \code{\link{.read_inventory_data}}.
#'
#' @noRd
.build_ifn_file_path <- function(
  province, type, version, folder = ".", .call = rlang::caller_env()
) {

  # Ok, so here we need to do some things. Depending on .version (the IFN version) we need to
  # provide different things.
  res <- purrr::pmap_chr(
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
        return(NA_character_)
      }
      return(table_path)
    }
  )

  if (sum(is.na(res)) > 0) {
    cli::cli_warn(c(
      "!" = "Files for provinces {.values {province[which(is.na(res))] |> unique()}} not found.",
      "i" = "Skipping plots {type} data from those provinces"
    ), call = .call)
  }

  return(res)
}

#' Fixing subclasses format on the fly
#'
#' Fix common errors in subclasses codes on the fly
#'
#' This function takes a vector of subclasses codes and fix the common errors found. (i) trimming
#' white spaces on both sides, (ii) substitute wrong designations in IFN3.
#'
#' @param subclasses Character vector of subclasses codes
#'
#' @return A vector of the same length as \code{subclasses} with the codes fixed
#'
#' @noRd
.ifn_subclass_fixer <- function(subclasses) {
  # This helper fix errors with subclasses format
  subclasses |>
    # remove any ocurring extra whitespaces
    stringr::str_trim("both") |>
    # in IFN3, replace wrong 2C and 2E plots with 3C and 3E
    stringr::str_replace("^2C$", "3C") |>
    stringr::str_replace("^2E$", "3E") |>
    # in IFN3, replace wrong 5 subclass with 4
    stringr::str_replace("^5$", "4")
}

#' Unique ID creator for IFN plots
#'
#' Unique ID creator for IFN plots
#'
#' Codes in IFN versions are not unique and follow a complicated and prone to error subclasses
#' system that makes complicated to follow the plots in time. This function takes all the
#' information needed to identify the plot, checks with the internal dictionary
#' \code{ifn_plots_thesaurus} and returns an unique plot id that can be used to identify the plot
#' in all IFN versions it's present.
#'
#' @param data IFN table as obtained by the individual table processing functions
#' @param version IFN version
#' @param province Two-number province code. Needed in IFN 4 to filter multiprovince db
#' @param .dry Return data as is, no unique id created, but if \code{.padding} is TRUE, Estadillo
#'   var is padded.
#' @param .padding Logical indicating if the estadillo var (name differs in versions) must be
#'   padded to 4 length. Default to TRUE as this is necessary for most cases.
#'
#' @return The same \code{data} object, with a new column, \code{id_unique_code}, in case
#'   \code{.dry} is \code{FALSE}. If \code{.dry = TRUE}, \code{data} as is.
#'
#' @noRd
.ifn_unique_id_creator <- function(data, version, province, .dry = FALSE, .padding = TRUE) {

  if (isTRUE(.dry)) {
    if (isTRUE(.padding)) {
      # here to solve the estadillo padding. As dry only occurs in ifn3 and ifn4 coords tables,
      # due to the lack of Subclase in those, we can safely assume the var is called Estadillo
      data <- data |>
        dplyr::mutate(
          Estadillo = stringr::str_pad(.data$Estadillo, width = 4, side = "left", pad = "0")
        )
    }

    return(data)
  }

  if (version == "ifn2") {
    res <- ifn_plots_thesaurus |>
      dplyr::filter(.data$class_ifn2 == "NN") |>
      dplyr::select("id_code", "PROVINCIA", "ESTADILLO") |>
      dplyr::right_join(
        data |>
          dplyr::mutate(
            ESTADILLO = stringr::str_pad(.data$ESTADILLO, width = 4, side = "left", pad = "0")
          ),
        by = c("PROVINCIA", "ESTADILLO")
      ) |>
      dplyr::rename(id_unique_code = "id_code")
  } else {

    data_temp <- data |>
      dplyr::mutate(
        Estadillo = stringr::str_pad(.data$Estadillo, width = 4, side = "left", pad = "0"),
        class = paste0(.data$Cla, .data$Subclase)
      )

    if (version == "ifn4") {
      data_temp <- data |>
        dplyr::filter(
          province == stringr::str_pad(.data$Provincia, width = 2, side = "left", pad = "0")
        ) |>
        dplyr::mutate(
          Estadillo = stringr::str_pad(.data$Estadillo, width = 4, side = "left", pad = "0"),
          class = paste0(.data$Cla, .data$Subclase)
        )
    }

    res <- ifn_plots_thesaurus |>
      dplyr::filter(.data$PROVINCIA == province) |>
      dplyr::select("id_code", Estadillo = "ESTADILLO", class = paste0("class_", version)) |>
      dplyr::filter(!is.na(.data$class), .data$class != "xx") |>
      dplyr::right_join(
        data_temp,
        by = c("Estadillo", "class")
      ) |>
      dplyr::rename(id_unique_code = "id_code")
  }

  return(res)
}

#' Create a filter list from the plots info
#'
#' User workflow for creating the filter list from the plots info
#'
#' @param plots_info \code{\link[sf]{sf}}, data frame or tibble with the plots info, as obtained
#'   from \code{\link{show_plots_from_ifn}}
#'
#' @return  A valid \code{filter_list} with the needed structure for \code{ifn_to_tibble}
#'
#' @noRd
create_filter_list_ifn <- function(plots_info) {
  ## assertions
  # this process is independent from ifn_to_tibble, and the user can modify plots_info to
  # filter plots and provinces. So we can not assume plots_info is going to have the str we
  # need. So, we assert and inform the user if something is wrong

  # assert class
  assertthat::assert_that(
    inherits(plots_info, c("tbl", "sf", "data.frame")),
    msg = cli::cli_abort(c(
      "{.arg plots_info} must be a data.frame or something coercible to one,
      as the result of {.code show_plots_from()}"
    ))
  )
  # assert col names
  assertthat::assert_that(
    all(names(plots_info) %in% c(
      "crs", "id_unique_code", "version", "province_code",
      "province_name_original", "plot", "geometry"
    )),
    msg = cli::cli_abort(c(
      "{.arg plots_info} provided don't have the expected names",
      "i" = "Expected names are {.value {c('crs', 'id_unique_code', 'version', 'province_code', 'province_name_original', 'plot', 'geometry')}}"
    ))
  )
  # assert there is data
  assertthat::assert_that(
    nrow(plots_info) > 0,
    msg = cli::cli_abort(c(
      "{.arg plots_info} must have at least one row"
    ))
  )

  # loop around states
  plots_versions <- plots_info[["version"]] |>
    unique()
  province_codes <- plots_info[["province_code"]] |>
    unique()

  plots_info |>
    .transform_plot_summary_ifn(plots_versions, province_codes)
}


## #' obtaining crs for different coordinate systems IFN
## #'
## #' This function  get_crs reads var huso and coordinate system of plot table process the tree table
## #' for one plot and one IFN
## #'
## #' @param tree_data file that contains the tree table for that plot
## #' @param plot plot_id code
## #' @param province province code
## #' @param ref_tree_ifn data frame containing the species code reference table
## #'
## #' @noRd
##
##
