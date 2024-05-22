#' Raw FFI data to tibble
#'
#' Transform raw FFI plot data into tidy data for easier use
#'
#' This function will take every year specified and will retrieve and transform the plot data for
#' the departments and plots provided. For that, csv files from FFI must reside in the folder
#' indicated in the \code{folder} argument.
#'
#' @param departments A character vector with the code for the departments, \emph{i.e.} \code{"01"}
#'   or \code{c("01", "10")}. See examples for details.
#' @param years A numeric vector with the years to extract de data from.
#' @param filter_list A list of departments and plots to extract the data from. If \code{NULL} all
#'   plots for the department for all years will be extracted, which can use a big amount of memory.
#'   See details.
#' @param folder The path to the folder containing the FFI csv files, as character.
#' @param clean_empty Vector with column names from where to remove empty
#'   results. Can be one or more of \code{"tree"},
#'   \code{"understory"} and \code{"regen"}. If more than one,
#'   only plots with data in all columns selected will be
#'   retained.
#' @param as_sf Logical indicating if the data must be returned as an spatial object. This always
#'   can be done later, as the data contains coordinates and crs info. Default to \code{FALSE}.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ffi_to_tibble} will attempt to process all
#'   plots for the departments and years provided. This will result in sometimes hundred of
#'   thousands plots to be extracted, processed and returned, which in turn will cause a big use of
#'   memory (specially when running in parallel processes) and long times of calculation.
#'   Is better to provide a list of departments with the counties and plots to look for to narrow
#'   the process. This \code{filter_list} should have the following structure:
#'   \preformatted{
#'    list(
#'    "01" = 1404119,
#'    "10" = 900863,
#'    "11" = c(1436508, 1410492))
#'    )
#'   }
#'   \code{esus} package offers workflows to create this automatically, see
#'   \code{vignette("filtering_plots", pkg = "esus")} for more details.
#'
#' @section Parallel:
#'   Processing the plots from within a year can be done in parallel (\code{esus} uses internally
#'   the \code{\link[furrr]{furrr}} package for this). This means that, if parallelization is
#'   active, several processes are launched to retrieve the plots data for that year. This is
#'   repeated for all years provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellelization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{ffi_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, and herbs
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#' 
#' @examples
#' \donttest{
#' \dontrun{
#' library(esus)
#' ffi_to_tibble(
#'   departments = c("01"), years = c(2019),
#'   filter_list = list("01" = c(1404119)),
#'   folder = "path/to/ffi/data"
#' )
#' }
#' }
#'
#' @export
ffi_to_tibble <- function(
  departments,
  years,
  filter_list,
  folder,
  clean_empty = NULL,
  as_sf = FALSE,
  ...,
  .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
  .verbose = TRUE
) {
  ## Assertions and checks ##
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  # departments
  assertthat::assert_that(
    is.character(departments), length(departments) > 0,
    msg = cli::cli_abort("departments must be a character vector with at least one department code")
  )

  # departments are valid
  valid_departments <- c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22",
    "23", "24", "25", "26", "27", "28", "29", "2A", "2B", "30", "31",
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
    "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53",
    "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64",
    "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "76",
    "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87",
    "88", "89", "90", "91", "92", "93", "94", "95"
  )
  invalid_departments <- which(!departments %in% valid_departments)

  if (identical(length(departments), length(invalid_departments))) {
    cli::cli_abort(c(
      "x" = "Any of the provided {.arg departments} ({.val {departments}}) are valid. Aborting."
    ))
  } else {
    if (length(invalid_departments) > 0) {
      cli::cli_warn(c(
        "!" = "Some {.arg departments} are not valid: {.val {departments[invalid_departments]}}",
        "i" = "These will be skipped in the process"
      ))
    }
  }

  # years
  assertthat::assert_that(
    is.numeric(years), length(years) > 0,
    msg = cli::cli_abort("years must be a numeric vector with at least one year")
  )

  # years are valid, meaning that years before 2005 are removed.
  if (any(years < 2005)) {
    years <- years[years >= 2005]

    if (length(years) < 1) {
      cli::cli_abort(c(
        "x" = "All {.arg years} provided are from before 2005. FFI data starts in 2005. Aborting..."
      ))
    }

    cli::cli_warn(c(
      "!" = "Years before 2005 will be ignored as FFI data starts in 2005."
    ))
  }

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      "Folder especified ({.path {folder}}) doesn't exists.
      Please create the folder first and populate it with the needed FFI csv files"
    )
  )

  # filter_list
  if (is.null(filter_list)) {
    if (interactive()) {
      cli::cli_inform(c(
        "You haven't specified any plots in the {.arg filter_list} argument.",
        "x" = "This will cause to retrieve {.strong ALL} plots for the selected
        departments and years",
        "!" = "This will use a lot of memory and time, as hundred of thousands plots
        will potentially be evaluated",
        "TODO: add info about how to create the filter list",
        ""
      ))

      user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
      if (user_auth == 2L) {
        cli::cli_abort("Aborting per user request")
      }
    }
  }

  # parallel options
  assertthat::assert_that(
    inherits(.parallel_options, "furrr_options"),
    msg = cli::cli_abort(".parallel_options must come from {.code furrr::furrr_options}")
  )

  # verbose
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  # ancillary data needed
  # espar cdref
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "espar-cdref13.csv")),
    msg = cli::cli_abort(
      "{.file espar-cdref13.csv} must be present at {.path {folder}} to be able to continue"
    )
  )
  # def metadonnes --> this file is edited from source!!
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "metadonnees.csv")),
    msg = cli::cli_abort(
      "{.file metadonnees.csv} file must be present at {.path {folder}} to be able to continue"
    )
  )

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(years)} year{?s}")
    ),
    .verbose
  )

  # get the caller environment to propagate errors
  .call <- rlang::caller_env(0)

  # create a safe version of ffi_tables_process to return NULL in errors
  safe_ffi_tables_process <- purrr::safely(
    ffi_tables_process,
    otherwise = NULL
  )

  ## send the years in loop to process table function
  inventory_data <- purrr::map(
    years,
    .f = \(year) {
      safe_ffi_tables_process(
        departments, year, filter_list, folder,
        .parallel_options, .verbose, .call = .call,
        ...
      )$result
    },
    .progress = .verbose
  ) |>
    purrr::list_rbind() |>
    clean_empty(clean_empty)

  if (isTRUE(as_sf)) {
    inventory_data <- inventory_data |>
      inventory_as_sf()
  }

  return(inventory_data)
}


#' Inner function to process all tables for one year
#'
#' Processing all tables for one year
#'
#' This function is intended to be called internally by \code{\link{ffi_to_tibble}} for each
#' year. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @inherit ffi_to_tibble
#'
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @noRd
ffi_tables_process <- function(
  departments, year, filter_list, folder,
  .parallel_options, .verbose, .call = rlang::caller_env(), ...
) {

  # Create input df for year
  input_df <- .build_ffi_input_with(departments, year, filter_list, folder, .verbose, .call = .call)

  # Get needed ancillary data (changed for excel)
  espar_cdref <- .read_inventory_data(
    fs::path(folder, "espar-cdref13.csv"),
    colClasses = list(character = c("// espar", "cd_ref")),
    header = TRUE
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      ESPAR = "// espar",
      Libelle = "lib_espar"
    ) |>
    #i need to change this because in the file csv it is recorded as "2" and in tree table as "02"
    dplyr::mutate(ESPAR = dplyr::case_when(
      ESPAR == "2" ~ "02",
      ESPAR == "3" ~ "03",
      ESPAR == "4" ~ "04",
      ESPAR == "5" ~ "05",
      ESPAR == "6" ~ "06",
      ESPAR == "7" ~ "07",
      ESPAR == "9" ~ "09",
      TRUE ~ ESPAR
    )) |>
    dplyr::arrange(.data$ESPAR)

  metadonnees <- suppressWarnings(
    readr::read_delim(
      file = fs::path(folder, "metadonnees.csv"), skip = 331,
      show_col_types = FALSE
    )
  ) |>
    dplyr::rename(UNITE = "// Unit\u00e9") |>
    dplyr::as_tibble()

  cd_ref <-  metadonnees |>
    dplyr::filter(.data$UNITE == "CDREF13") |>
    dplyr::rename(CD_REF = "Code", Libelle = "Libell\u00e9") |>
    dplyr::mutate(lib_cdref = stringr::str_remove_all(.data$Libelle, "\\s*\\(.*?\\)"))

  idp_dep_ref <- .read_inventory_data(
    fs::path(folder, "PLACETTE.csv"),
    select = c("IDP", "DEP"),
    colClasses = list(character = c("IDP", "DEP"))
  ) |>
    tibble::as_tibble() |>
    unique()

  # loop for each row of the input_df
  # temp_res <- purrr::pmap(
  temp_res <- furrr::future_pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(department, plots, tree_table, plot_table, shrub_table, soils_table, regen_table) {
      plot_info <- ffi_plot_table_process(plot_table, soils_table, plots, year, metadonnees, .call)
      tree <- ffi_tree_table_process(tree_table, plots, year, espar_cdref, idp_dep_ref, .call) |>
        dplyr::select(!dplyr::any_of(c(
          "id_unique_code", "country", "dep", "dep_name", "plot", "year", 
          "visite","coord_sys", "coordx", "coordy", "aspect", "slope",
          "lign1_pct", "lign2_pct", "herb_pct"
        )))
      #this table has information for both regen after 2015 and shrub (origin FLORE)
      #we apply filter to select based on growth form
      shrub_regen <- ffi_shrub_table_process(
        shrub_table, plots, year, cd_ref, growth_form_lignified_france, idp_dep_ref, .call
      ) |>
        dplyr::select(!dplyr::any_of(c(
          "id_unique_code", "country", "dep", "dep_name", "plot", "year", 
          "visite","coord_sys", "coordx", "coordy", "aspect", "slope",
          "lign1_pct", "lign2_pct", "herb_pct"
        )))
      shrub <- tibble::tibble()
      regen <- tibble::tibble()

      #shrub should be the same all time
      if (nrow(shrub_regen) > 0) {
        # IMPORTANT NOTE: there is an unsolved problem, not all species of french inventory
        #are present  in  growth_form_lignified_france
        #thus we systematically loss some records that are NA  when we apply filter.
        #We could fill missing values of growth form file manually from the list of fr_species
        #and have a static copy!!!
        shrub <- shrub_regen
        #quitamos filtro temporalmente
        # dplyr::filter(.data$GrowthForm == "shrub")
      }

      if (nrow(shrub) < 1) {
        shrub <- tibble::tibble()
      }

      if (year < 2015) {
        #before, info was collected in table couvert (see french documentation)
        regen <- ffi_regen_table_process(
          regen_table, plots, year, espar_cdref, idp_dep_ref, .call
        ) |>
          dplyr::select(!dplyr::any_of(c(
            "id_unique_code", "country", "dep", "dep_name", "plot", "year", 
            "visite","coord_sys", "coordx", "coordy", "aspect", "slope",
            "lign1_pct", "lign2_pct", "herb_pct"
          )))
      } else {
        # check if we have data in shrub_regen
        if (nrow(shrub_regen) > 0) {
          regen <- shrub_regen |>
            # IMPORTANT note that there is an unsolved problem, not all species are present
            #in  growth_form_lignified_france thus we systematically loss some records that are NA
            #(no here but in ffi tables process when we apply filter).
            #We could fill missing growth form manually from the list of fr_species
            #and have a static copy!!!
            #quitamos filtro temporalmente
            # dplyr::filter(.data$GrowthForm == "tree") |>
            dplyr::mutate(
              #default dbh, added to be  coherent with table from regen process
              DBH = NA
            )
          # check if both have data
          if (nrow(regen) < 1) {
            regen <- tibble::tibble()
          }
        }
      }

      # if for some reason plot info return an empty tibble (missing files), detect it here to avoid
      # transformation of empty data errors
      if (nrow(plot_info) < 1) {
        return(tibble::tibble())
      }

      # we select herbs
      herbs <- plot_info |>
        dplyr::select("herb_pct")
      # we create understory with herbs and shrub
      understory <- plot_info |>
        dplyr::select("lign1_pct", "lign2_pct") |>
        dplyr::mutate(shrub = list(shrub), herbs = list(herbs))
      # we put together all tables in a data frame
      plot_info <- plot_info |>
        dplyr::mutate(
          crs = 2154,
          tree = list(tree),
          understory = list(understory),
          regen = list(regen)
        ) |>
        dplyr::select(
          "id_unique_code", "country", "dep", "dep_name", "plot", "year", 
          "visite","coord_sys",  "crs", "coordx", "coordy", "aspect", "slope",
          "tree", "understory", "regen"
        )

      return(plot_info)
    }
  ) |>
    purrr::list_rbind()

  # something went wrong (bad counties and plots, wrong filter list...)
  if (nrow(temp_res) < 1) {
    cli::cli_abort("Ooops! Something went wrong, exiting...", call = .call)
  }

  temp_res |>
    # filtering the missing plots. This is done based on the fact plot table functions returns NAs
    # for all vars, including coords, when the plot is not found
    # dplyr::filter(!(
    #   is.na(.data$COORD2) & is.na(.data$COORD2_last_recorded) &
    #     is.na(.data$COORD1) & is.na(.data$COORD1_last_recorded)
    # ))
    dplyr::filter(!(is.na(.data$coordy) & is.na(.data$coordx)))
}

#' FFI data tables process
#'
#' Process to gather needed data from FFI csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param plot_data,tree_data,shrub_data,soils_data Paths to the files with the corresponding data
#' @param plot Numeric, plot code
#' @param year Numeric, year to extract
#' @param espar_cdref,idp_dep_ref,metadonnees,growth_form_lignified_france tables. These tables
#'   are automatically read/created in \code{\link{ffi_tables_process}} based on the folder
#'   provided.
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'   year.
#'
#' @importFrom dplyr desc
#'
#' @name ffi_tables_processing
#' @noRd
NULL

#' FFI data tables process
#' @describeIn ffi_tables_processing Process to gather needed data from plot and soils tables
#' @noRd
ffi_plot_table_process <- function(
  plot_data, soils_data, plot, year, metadonnees, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(plot_data, soils_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}}  for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  plot_raw <- .read_inventory_data(
    plot_data,
    select = c("CAMPAGNE", "VISITE", "IDP", "XL", "YL", "DEP"),
    colClasses = list(character = c("DEP", "IDP")),
    header = TRUE
  )

  # We check before continuing, because maybe we don't have rows
  if (nrow(plot_raw) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty plot info for plot {.var {plot}} in year {.var {year}} "
    ), call = .call)
    return(dplyr::tibble())
  }

  # join with metadonnes
  plot_processed <- plot_raw |>
    dplyr::left_join(
      y = metadonnees |>
        dplyr::filter(.data$UNITE == "DP") |>
        dplyr::rename(DEP = "Code", DEP_NAME = "Libell\u00e9") |>
        dplyr::select("DEP", "DEP_NAME"),
      by = "DEP"
    ) |>
    # transformations
    dplyr::mutate(
      id_unique_code = (paste("FR", .data$DEP, .data$IDP, sep = "_")),
      COORD_SYS = "LAMBERT"
    ) |>
    dplyr::select(
      "id_unique_code", "IDP", "DEP", "DEP_NAME", "VISITE", "CAMPAGNE", "XL", "YL", "COORD_SYS"
    ) |>
    dplyr::rename(plot = "IDP", year = "CAMPAGNE") |>
    data.table::as.data.table() |>
    #arrange by year descending to apply extract ffi metadata: two var last record and original
    dplyr::arrange(desc(.data$year)) |>
    #there might be more than 1 record
    dplyr::distinct() |>
    .extract_ffi_metadata(
      c("id_unique_code", "plot", "DEP", "DEP_NAME", "VISITE", "COORD_SYS", "XL", "YL"),
      plot,
      year,
      .soil_mode = TRUE
    ) |>
    dplyr::mutate(
      plot  = plot, year = year,
      # possible missing vars, fill with original if necessary
      id_unique_code = dplyr::if_else(
        is.na(.data$id_unique_code), .data$id_unique_code_last_recorded, .data$id_unique_code
      ),
      plot = dplyr::if_else(
        is.na(.data$plot), .data$plot_last_recorded, .data$plot
      ),
      dep = dplyr::if_else(
        is.na(.data$DEP), .data$DEP_last_recorded, .data$DEP
      ),
      dep_name = dplyr::if_else(
        is.na(.data$DEP_NAME), .data$DEP_NAME_last_recorded, .data$DEP_NAME
      ),
      visite = dplyr::if_else(
        is.na(.data$VISITE), .data$VISITE_last_recorded, .data$VISITE
      ),
      coord_sys = dplyr::if_else(
        is.na(.data$COORD_SYS), .data$COORD_SYS_last_recorded, .data$COORD_SYS
      ),
      coordx = dplyr::if_else(
        is.na(.data$XL), .data$XL_last_recorded, .data$XL
      ),
      coordy = dplyr::if_else(
        is.na(.data$YL), .data$YL_last_recorded, .data$YL
      )
    ) |>
    dplyr::as_tibble()

  eco_filtered_data <- .read_inventory_data(
    soils_data,
    select = c("CAMPAGNE", "IDP", "EXPO", "PENT2", "LIGN1", "LIGN2", "HERB"),
    colClasses = list(character = c("IDP")),
    header = TRUE
  ) |>
    dplyr::select("IDP", "CAMPAGNE", "EXPO", "PENT2", "LIGN1", "LIGN2", "HERB") |>
    dplyr::rename(plot = "IDP", year = "CAMPAGNE") |>
    # dplyr::arrange(desc(.data$CAMPAGNE)) |>
    # transformations
    dplyr::mutate(
      #CONVERSION TO SEXAGESIMAL
      EXPO = 0.9 * .data$EXPO
    ) |>
    dplyr::arrange(desc(.data$year)) |>
    # there might be more than 1 record
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_ffi_metadata(
      c("plot", "EXPO", "PENT2", "LIGN1", "LIGN2", "HERB"),
      plot,
      year,
      .soil_mode = TRUE
    ) |>
    dplyr::mutate(
      plot = plot, year = year,
      # possible missing vars, fill with original if necessary
      plot = dplyr::if_else(is.na(.data$plot), .data$plot_last_recorded, .data$plot),
      aspect = dplyr::if_else(is.na(.data$EXPO), .data$EXPO_last_recorded, .data$EXPO),
      slope = dplyr::if_else(is.na(.data$PENT2), .data$PENT2_last_recorded, .data$PENT2),
      lign1_pct = dplyr::if_else(is.na(.data$LIGN1), .data$LIGN1_last_recorded, .data$LIGN1),
      lign2_pct = dplyr::if_else(is.na(.data$LIGN2), .data$LIGN2_last_recorded, .data$LIGN2),
      herb_pct = dplyr::if_else(is.na(.data$HERB), .data$HERB_last_recorded, .data$HERB),
    ) |>
   
    tibble::as_tibble()

  plot_info <- dplyr::left_join(
    plot_processed, eco_filtered_data,
    by = c("plot", "plot_last_recorded", "year")
  ) |>
    dplyr::mutate(
      plot = plot, year = year,
      id_unique_code = paste("FR", .data$dep, .data$plot, sep = "_"), 
      country = "FR"
    ) |>
    dplyr::as_tibble() |>
    dplyr::select(
      "id_unique_code", "country", "dep", "dep_name", "plot", "year", 
      "visite","coord_sys", "coordx", "coordy", "aspect", "slope",
      "lign1_pct", "lign2_pct", "herb_pct"
    )

  return(plot_info)
}



#' @describeIn ffi_tables_processing Process to gather needed data from tree tables
#' @noRd
ffi_tree_table_process <- function(
  tree_data, plot, year, espar_cdref, idp_dep_ref, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(tree_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # we need the raw data for later, but the filtered data for the rows check, so we store both
  tree_raw_data <- .read_inventory_data(
    tree_data,
    select = c("CAMPAGNE", "IDP", "A", "W", "ESPAR", "VEGET", "VEGET5", "C13", "HTOT"),
    colClasses = list(character = c("ESPAR", "IDP")),
    header = TRUE
  )

  # we filter the data for plot/year and status (alive)????
  tree_filtered_data <- tree_raw_data |>
    dplyr::filter(
      .data$IDP == plot,
      .data$CAMPAGNE == year
    )


  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(tree_filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty tree info for plot {.var {plot}} in year {.var {year}} "
    ), call = .call)
    return(dplyr::tibble())
  }
  # browser()
  #IMPORTANT do NOT change this!!!!!!!!!!
  # we need to get all data first to fill missing values of known var
  # filter per year is done at the end
  tree <- tree_raw_data |>
    # we filter the data for plot
    dplyr::filter(.data$IDP == plot) |>
    # transformations and filters
    dplyr::mutate(
      density_factor = .data$W,
      C13 = as.numeric(.data$C13),
      DIA = (.data$C13 / pi) * 100, # transformation to diameter
      year = .data$CAMPAGNE,
      # ensure VEGET and VEGET5 are integers
      VEGET = as.integer(.data$VEGET),
      VEGET5 = as.integer(.data$VEGET5)
    ) |>
    # join with espar_cdref
    dplyr::left_join(
      y = espar_cdref |>
        dplyr::select("ESPAR", "cd_ref", "lib_cdref") |>
        dplyr::as_tibble(),
      by = "ESPAR"
    ) |>
    dplyr::left_join(
      y = idp_dep_ref |> tibble::as_tibble(),
      by = "IDP"
    ) |>
    dplyr::mutate(
      id_unique_code = paste("FR", .data$DEP, .data$IDP, sep = "_")
    ) |>
    dplyr::rename(
      dep = "DEP",
      plot = "IDP",
      SP_NAME = "lib_cdref",
      SP_CODE = "cd_ref",
      tree = "A",
      height = "HTOT", # ht in meters
      STATUS = "VEGET",
      STATUS5 = "VEGET5"
    ) |>
    #selection of final variables
    dplyr::select(
      "id_unique_code", "plot", "dep", "year", "tree", "ESPAR",
      "SP_CODE", "SP_NAME", "STATUS", "STATUS5", "DIA", "height", "density_factor"
    ) |>
    # homogeneization
    dplyr::group_by(.data$id_unique_code) |>
    #important DO NOT CHANGE THIS:  WE ARRANGE BY plot, tree AND YEAR,
    #some variables are register only in first visit but are important to have in revisit
    dplyr::arrange(.data$id_unique_code, .data$tree, .data$year) |>
    # espar var will appear empty "" in the revisited plots , we first convert to NA
    dplyr::mutate(ESPAR = dplyr::na_if(.data$ESPAR, "")) |>
    # arrange by year descending to apply extract ffi metadata: two var last record and original
    dplyr::arrange(desc(.data$year)) |>
    # there might be more than 1 record
    dplyr::distinct() |>
    # data.table::as.data.table() |>
    dplyr::as_tibble() |>
    # dplyr::group_by(
    #   # needed for later, no effect on the grouping:
    #   .data$id_unique_code, .data$DEP, .data$density_factor,
    #   # real grouping ones:
    #   .data$plot, .data$tree
    # ) |>
    tidyr::nest(
      .key = ".metadata",
      .by = c(
        # needed for later, no effect on the grouping:
        "id_unique_code", "dep", "density_factor",
        # real grouping ones:
        "plot", "tree"
      )
    ) |>
    dplyr::mutate(
      .extracted_metadata = purrr::map(
        .data$.metadata,
        .f = \(metadata) {
          .extract_ffi_metadata(
            data_processed = metadata,
            vars =  c(
              "ESPAR", "SP_CODE", "SP_NAME",
              "STATUS", "STATUS5", "DIA", "height"
            ),
            plot = .data$plot |> unique(),
            year = year, .soil_mode = TRUE, .year_fun = min
          ) |>
            dplyr::as_tibble()
        }
      )
    ) |>
    dplyr::select(!".metadata") |>
    unnest(cols = c(".extracted_metadata")) |>
    dplyr::mutate(
      year = year,
      espar = dplyr::if_else(is.na(.data$ESPAR), .data$ESPAR_last_recorded, .data$ESPAR),
      sp_code = dplyr::if_else(is.na(.data$SP_CODE), .data$SP_CODE_last_recorded, .data$SP_CODE),
      sp_name = dplyr::if_else(is.na(.data$SP_NAME), .data$SP_NAME_last_recorded, .data$SP_NAME),
      status = dplyr::if_else(is.na(.data$STATUS), .data$STATUS_last_recorded, .data$STATUS),
      status5 = dplyr::if_else(is.na(.data$STATUS5), .data$STATUS5_last_recorded, .data$STATUS5),
      dia = dplyr::if_else(is.na(.data$DIA), .data$DIA_last_recorded, .data$DIA)
    ) |>
    dplyr::select(
      "id_unique_code", "plot", "dep", "year", "tree", "espar",
      "sp_code", "sp_name", "status",  "status5", "dia", "density_factor",
      "height", "height_last_recorded"
    ) |>
    dplyr::ungroup()

  return(tree)
}


#' @describeIn ffi_tables_processing Process to gather needed data from shrub tables
#' @noRd
ffi_shrub_table_process <- function(
  shrub_data, plot, year,
  cd_ref, growth_form_lignified_france, idp_dep_ref, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(shrub_data)))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names

  shrub_filtered_data <- .read_inventory_data(
    shrub_data,
    select = c("CAMPAGNE", "IDP", "CD_REF", "ABOND"),
    header = TRUE,
    colClasses = list(character = c("IDP", "CD_REF"))
  ) |>
    # we  filtering the data for plot/year and status (alive)
    dplyr::filter(
      .data$IDP == plot,
      .data$CAMPAGNE == year
    )

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(shrub_filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty understory info for plot {.var {plot}} in year {.var {year}} "
    ), call = .call)
    return(dplyr::tibble())
  }

  # transformations and filters
  shrub <- shrub_filtered_data |>
    dplyr::mutate(
      YEAR = .data$CAMPAGNE,
      # conversion to percentage
      ABOND = dplyr::case_when(
        # presence faible	Taux de recouvrement de l'espece inferieur a 5 % et presence faible.
        .data$ABOND == 1 ~ 5,
        #presence nette	Taux de recouvrement de l'espece inferieur a 25 % mais presence nette.
        .data$ABOND == 	2	~ 12.5,
        #Taux de recouvrement de l'espece compris entre 25 et 50 %
        .data$ABOND	== 3 ~	37.5,
        #Taux de recouvrement de l'espece compris entre 25 et 50 %
        #Taux de recouvrement de l'espece compris entre 50% et 75 %.
        .data$ABOND	== 4 ~	62.5,
        #	Taux de recouvrement de l'espece superieur a 75%.
        .data$ABOND	== 5 ~	87.5
      )
    ) |>
    dplyr::left_join(
      y = cd_ref |>
        dplyr::select("CD_REF", "lib_cdref"),
      by = "CD_REF"
    ) |>
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"
    ) |>
    dplyr::mutate(id_unique_code = (paste("FR", .data$DEP, .data$IDP, sep = "_")), height = NA) |>
    dplyr::rename(
      plot = "IDP",
      SP_NAME = "lib_cdref",
      SP_CODE = "CD_REF",
      COVER = "ABOND"
    ) |>
    # we join this table to differentiate between tree , shrub and herbs
    # IMPORTANT note that there is an unsolved problem, not all species are present  in
    #growth_form_lignified_france thus we systematically loss some records that are NA
    #(no here but in ffi tables process when we apply filter).
    #We could fill missing growth form manually from the list of fr_species and have a static copy!!
    dplyr::left_join(
      growth_form_lignified_france |>
        dplyr::select("AccSpeciesName", "GrowthForm") |>
        dplyr::mutate(SP_NAME = .data$AccSpeciesName),
      by = "SP_NAME"
    ) |>
    dplyr::rename(
    dep = "DEP",
    year = "YEAR",
    sp_code = "SP_CODE",
    sp_name = "SP_NAME", 
    cover = "COVER",
    height = "height",
    growth_form = "GrowthForm"
    ) |> 
    # selection of final variables
    dplyr::select(
      "id_unique_code", "plot", "dep", "year", "sp_code", "sp_name", "cover", "height", "growth_form"
    ) |>
    dplyr::as_tibble()

  return(shrub)
}

#' @describeIn ffi_tables_processing Process to gather needed data from regen tables
#' @noRd
ffi_regen_table_process <- function(
  regen_data, plot, year, espar_cdref, idp_dep_ref, .call = rlang::caller_env()
) {

  # TABLE FOR REGEN DEPEND ON YEAR , BEFORE 2015 COUVERT SHOULD BE USED, AFTER 2015 FLORE SHOULD
  # (SAME AS SHRUB PROCESS) SHALL WE INTEGRATE PART OF THIS PROCESS IN SHRUB PROCESS O "REPET"
  # READING OF TABLE IN REGEN ?- now we do the second

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(regen_data)))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  regen_filtered_data <- .read_inventory_data(
    regen_data,
    select = c("CAMPAGNE", "IDP", "ESPAR_C", "TCA", "STRATE"),
    header = TRUE,
    colClasses = list(character = c("IDP", "ESPAR_C"))
  ) |>
    # we  filtering the data for plot/year and status (alive)
    dplyr::filter(.data$IDP == plot, .data$CAMPAGNE == year)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(regen_filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty regeneration info for plot {.var {plot}} in year {.var {year}} "
    ), call = .call)
    return(dplyr::tibble())
  }

  # transformations and filters
  regen <- regen_filtered_data |>
    dplyr::mutate(
      year = .data$CAMPAGNE,
      cover = .data$TCA,
      ESPAR = .data$ESPAR_C
    ) |>
    dplyr::filter(
      # here we only select no recensable records
      .data$STRATE == "NR"
    ) |>
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"
    ) |>
    #join with espar_cdref
    dplyr::left_join(
      y = espar_cdref |>
        dplyr::select("cd_ref", "lib_cdref", "ESPAR") |>
        dplyr::rename(CD_REF = "cd_ref") |>
        dplyr::as_tibble(),
      by = "ESPAR"
    ) |>
    dplyr::mutate(
      id_unique_code = (paste("FR", .data$DEP, .data$IDP, sep = "_")),
      #we add this to be coherent with other inventories .
      #DBH default  value could be set to 7 but may be too high
      dbh = NA, # in
      height = NA,
      growth_form = "tree"
    ) |>
    dplyr::rename(plot = "IDP", dep = "DEP" , sp_name = "lib_cdref", 
                  sp_code = "CD_REF") |>
    # selection of final variables
    dplyr::select("id_unique_code", "plot", "dep", "year", "sp_code", "sp_name",
                  "cover", "dbh", "height", "growth_form") |>
    dplyr::as_tibble()

  return(regen)
}
