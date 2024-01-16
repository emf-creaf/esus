#' Raw FFI data to tibble
#'
#' Transform raw FFI plot data into tidy data for use in models
#'
#' This function will take every year indicated and retrieve and transform the plot data for the departments and
#' plots provided. For that, csv files from FFI must reside in the folder indicated in the
#' \code{folder} argument.
#'
#' @param departments A character vector with the code for the departments.
#' @param years A numeric vector with the years to extract de data from.
#' @param filter_list A list of counties and plots to extract the data from. If \code{NULL} all
#'   plots for the department for all years will be extracted, which can use a big amount of memory. See
#'   details.
#' @param folder The path to the folder containing the FFI csv files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ffi_to_tibble} will attempt to process all
#'   plots for the departments and years provided. This will result in sometimes hundred of thousands
#'   plots to be extracted, processed and returned, will in turn will cause a big use of memory and
#'   long times of calculation (specially when parallelising). Is better to provide a list of departments
#'   with the counties and plots to look after to narrow the process. This \code{filter_list} should
#'   have the following structure:
#'   \preformatted{
#'    list(
#'    "01" = 1404119,
#'    "10" = 900863,
#'    "11" = c(1436508, 1410492))
#'
#'   )
#'   }
#'   \code{esus} package offers workflows to create this automatically, see
#'   \code{vignette("filtering_plots", pkg = "esus")} for more details.
#'
#' @section Parallel:
#'   Processing the plots from within a year can be done in parallel (\code{esus} uses internally the
#'   \code{\link[furrr]{furrr}} package for this). This means that, if parallelization is active,
#'   several processes are launched to retrieve the plots data for that year. This is repeated for
#'   all years provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{ffi_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#'
#' @export
ffi_to_tibble <- function(
    departments,
    years,
    filter_list,
    folder,
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
  ## TODO
  # check all departments are valid

  # years
  assertthat::assert_that(
    is.numeric(years), length(years) > 0,
    msg = cli::cli_abort("years must be a numeric vector with at least one year")
  )
  ## TODO
  # check years are valid

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort( "Folder especified ({.path {folder}}) doesn't exists. Please create the folder first and populate it with the needed FFI csv files")
  )

  # filter_list
  if (is.null(filter_list)) {
    if (interactive()) {
      cli::cli_inform(c(
        "You haven't specified any plots in the {.arg filter_list} argument.",
        "x" = "This will cause to retrieve {.strong ALL} plots  for the selected departments and years",
        "!" = "This will use a lot of memory and time, as hundred of thousands plots will potentially be evaluated",
        "TODO: add info about how to create the filter list",
        ""
      ))

      user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
      if (user_auth == 2L) {
        cli::cli_abort("Aborting per user request")
      }
    }
  }
  ## TODO
  # Check counties and plots??

  ## TODO
  # Check ancillary data is present!!

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
    msg = cli::cli_abort("{.file espar-cdref13.csv} must be present at {.path {folder}} to be able to continue")
  )
  # def metadonnes --> this file is edited from source!!
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "metadonnees.csv")),
    msg = cli::cli_abort("{.file metadonnees.csv} file must be present at {.path {folder}} to be able to continue")
  )



  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(years)} year{?s}")
    ),
    .verbose
  )
  ## send the years in loop to process table function
  purrr::map(
    years,
    .f = \(year) {
      ffi_tables_process(departments, year, filter_list, folder, .parallel_options, .verbose, ...)
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}


#' Inner function to process all tables for one year
#'
#' Processing all tables for one year
#'
#' This function is intended to be called internally by \code{\link{ffi_to_tibble}} for each
#' year. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @describeIn ffi_to_tibble Process one year
#'
ffi_tables_process <- function(
    departments, year, filter_list, folder,
    .parallel_options, .verbose, ...
) {

  # debug
   # browser()

  # Create input df for year
  input_df <- .build_ffi_input_with(departments, year,  filter_list, folder, .verbose)

  # Get needed ancillary data (changed for excel)
  espar_cdref <- .read_inventory_data(
    fs::path(folder, "espar-cdref13.csv"),
    colClasses = list(character = c( "// espar", "cd_ref")),
    header = TRUE
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      ESPAR = "// espar",
      Libellé  = lib_espar
    ) |>
    #i need to change this because in the file csv it is recorded as "2" and in tree table as "02"
    dplyr::mutate(ESPAR = dplyr::case_when(
      ESPAR == "2" ~"02",
      ESPAR == "3" ~"03",
      ESPAR == "4" ~"04",
      ESPAR == "5" ~"05",
      ESPAR == "6" ~"06",
      ESPAR == "7" ~"07",
      ESPAR == "9" ~"09",
      TRUE ~ ESPAR
    )) |>
    dplyr::arrange(ESPAR)

  # here i leave this for the moment as it gives an error in the lecture
  # This is due to an error in data.table::fread (https://github.com/Rdatatable/data.table/issues/5378)
  # For the moment, we go with readr until is fixed
  # metadonnees <- .read_inventory_data(
  #   fs::path(folder, "metadonnees.csv"),
  #   skip = 413, fill = 10
  # )
  #   # dplyr::as_tibble() |>
  #   # dplyr::rename(
  #   #   UNITE = "// Unité"
  #   # )
  metadonnees <- suppressWarnings(readr::read_delim(
     file = fs::path(folder, "metadonnees.csv"), skip = 331,
    # file = fs::path(folder, "metadonnees.csv"), skip = 412,
    show_col_types = FALSE
  )) |>
    dplyr::rename(UNITE = "// Unité") |>
    dplyr::as_tibble()

  cd_ref <-  metadonnees |>
    dplyr::filter(
      UNITE == "CDREF13"
          ) |>
    dplyr::mutate(
      lib_cdref =  stringr::str_remove_all(Libellé, "\\s*\\(.*?\\)")
      ) |>
   dplyr::rename(
     CD_REF = Code
    )


  ESPAR <- metadonnees |>
    dplyr::filter(
      UNITE == "ESPAR1"
    ) |>
    dplyr::rename(
      CD_REF = Code
    )

  idp_dep_ref <- .read_inventory_data(
    fs::path(folder, "PLACETTE.csv"),
    select = c(
      "IDP",
      "DEP"
    ),
    colClasses = list(character = c( "IDP","DEP"))
  ) |>
    tibble::as_tibble() |>
    unique()

  # loop for each row of the input_df
  temp_res <- furrr::future_pmap(
  # temp_res <- purrr::pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(department,
           plots,
           tree_table,
           plot_table,
           shrub_table,
           soils_table,
           regen_table) {
        # browser()
      plot_info <- ffi_plot_table_process(plot_table, soils_table, plots, year, metadonnees)
      tree <- ffi_tree_table_process(tree_table, plots, year,espar_cdref, idp_dep_ref)
      shrub_regen <- ffi_shrub_table_process(shrub_table, plots, year, cd_ref, growth_form_lignified_france, idp_dep_ref)
      # soil <- ffi_soil_table_process(soils_table, plots, year, metadonnees, idp_dep_ref)
      shrub <- tibble::tibble()
      regen <- tibble::tibble()
      if (year < 2015) {
        regen <- ffi_regen_table_process(regen_table, plots, year, espar_cdref,idp_dep_ref)
      } else {
        # check if we have data in shrub_regen
        if (nrow(shrub_regen) > 0) {
          shrub <- shrub_regen |>  dplyr::filter(GrowthForm == "shrub")
          regen <- shrub_regen |>  dplyr::filter(GrowthForm == "tree")

          # check if both have data
          if (nrow(shrub) < 1) {
            shrub <- tibble::tibble()
          }
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
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          VISITE,
          YEAR,
          HERB
        )
      # we create understory with herbs and shrub
      understory <- plot_info |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          VISITE,
          LIGN1,
          LIGN2,
          YEAR
        ) |>
        dplyr::mutate(
          shrub = list(shrub),
          herbs = list(herbs)
        )
      # we put together all tables in a data frame
      plot_info <- plot_info |>
        dplyr::mutate(
          crs = 2154,
          tree = list(tree),
          understory = list(understory),
          # soil = list(soil),
          regen = list(regen)
        ) |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          DEP_NAME,
          COUNTRY,
          VISITE,
          YEAR,
          XL,
          XL_ORIGINAL,
          YL,
          YL_ORIGINAL,
          crs,
          EXPO,
          EXPO_ORIGINAL,
          PENT2,
          PENT2_ORIGINAL,
          COORD_SYS,
          tree,
          understory,
          regen
          # soil
        ) |>
        # HARMONIZATION OF NAMES
        dplyr::rename(
          ASPECT = EXPO,
          ASPECT_ORIGINAL = EXPO_ORIGINAL,
          SLOPE = PENT2,
          SLOPE_ORIGINAL = PENT2_ORIGINAL,
          COORD1 = XL ,
          COORD1_ORIGINAL = XL_ORIGINAL,
          COORD2 = YL,
          COORD2_ORIGINAL = YL_ORIGINAL
          # soils = soil
        )
      return(plot_info)
    }
  ) |>
    purrr::list_rbind()

  # something went wrong (bad counties and plots, wrong filter list...)
  if (nrow(temp_res) < 1) {
    cli::cli_abort("Ooops! Something went wrong, exiting...")
  }

  temp_res |>
    # filtering the missing plots. This is done based on the fact plot table functions returns NAs
    # for all vars, including coords, when the plot is not found
    dplyr::filter(!(is.na(COORD2) & is.na(COORD2_ORIGINAL) & is.na(COORD1) & is.na(COORD1_ORIGINAL)))
}



#' Data tables process
#'
#' Process to gather needed data from FFI csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param plot_data,tree_data,shrub_data,soils_data Paths to the files with the corresponding data
#' @param plot Numeric, plot code
#' @param dep department code
#' @param year Numeric, year to extract
#' @param espar_cdref,metadonnees,growth_form_lignified_france tables. These tables
#'   are automatically read in \code{\link{ffi_tables_process}} based on the folder provided.
#' @param growth_habit Character, growth habit value to filter data (to distinguish between herbs
#'   and shrubs)
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'   year.
#'
#' @importFrom dplyr desc
#' @name tables_processing
NULL

#' @describeIn tables_processing Process to gather needed data from plot, survey and cond tables
ffi_plot_table_process <- function(plot_data, soils_data, plot, year, metadonnees) {

  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(plot_data, soils_data)))
    # !any(c(plot_data, survey_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}}  for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # browser()

  plot_processed <- .read_inventory_data(
    plot_data,
    select = c(
    "CAMPAGNE",
    "VISITE",
    "IDP",
    "XL",
    "YL",
    "DEP"
    # #CSA : Couverture du sol
    # "CSA",
    # #TCAT10 : Taux de couvert total des arbres
    #   "TCAT10",
    #   #UTA1 et UTA2 : Première et deuxième utilisation du sol
    #   "UTA1",
    #   "UTA2"
    ),
    colClasses = list(character = c("DEP","IDP")),
    header = TRUE
  ) |>
    #join with metadonnes
    dplyr::left_join(
      y = metadonnees |>
        dplyr::filter(
          UNITE == "DP"
        ) |>
        dplyr::rename(
          DEP = Code,
          DEP_NAME = Libellé) |>
        dplyr::select(
            DEP ,
            DEP_NAME
        ),
      by = "DEP"
    ) |>
    #transformations
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_")),
      COORD_SYS = "LAMBERT"
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      IDP,
      DEP,
      DEP_NAME,
      VISITE,
      CAMPAGNE,
      XL,
      YL,
      COORD_SYS,
    ) |>
    dplyr::arrange(desc(CAMPAGNE)) |>
    dplyr::rename(
      PLOT = IDP,
      YEAR = CAMPAGNE
    ) |>
    data.table::as.data.table() |>
    dplyr::arrange(desc(YEAR)) |>
    #there might be more than 1 record
    dplyr::distinct() |>
    .extract_ffi_metadata(
      c("ID_UNIQUE_PLOT", "PLOT", "DEP", "DEP_NAME", "VISITE", "COORD_SYS", "XL", "YL"),
      plot,
      year,
      .soil_mode = TRUE
    ) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year
    ) |>
    dplyr::as_tibble()

  eco_filtered_data <- .read_inventory_data(
    soils_data,
    select = c(
      "CAMPAGNE",
      "IDP",
      "EXPO",
      "PENT2",
      "LIGN1",
      "LIGN2",
      "HERB"
    ),
    colClasses = list(character = c("IDP")),
    header = TRUE
  ) |>
    dplyr::select(
      IDP,
      CAMPAGNE,
      EXPO,
      PENT2,
      LIGN1,
      LIGN2,
      HERB
    ) |>
    dplyr::arrange(desc(CAMPAGNE)) |>
    #transformations
    dplyr::rename(
      PLOT = IDP,
      YEAR = CAMPAGNE
    ) |>
    dplyr::mutate(
      #CONVERSION TO SEXAGESIMAL
      EXPO = 0.9 * EXPO
    ) |>
    dplyr::arrange(desc(YEAR)) |>
    #there might be more than 1 record
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_ffi_metadata(
      c("PLOT","EXPO", "PENT2", "LIGN1", "LIGN2", "HERB"),
      plot,
      year,
      .soil_mode = TRUE
    ) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year
    ) |>
    tibble::as_tibble()


  plot_info <- dplyr::left_join(
    plot_processed, eco_filtered_data,
    by = c("PLOT", "PLOT_ORIGINAL", "YEAR")
  ) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year
    ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, PLOT, sep = "_")),
      COUNTRY = "FR"
    ) |>
    dplyr::as_tibble() |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      COUNTRY,
      DEP,
      DEP_NAME,
      PLOT,
      YEAR,
      VISITE,
      COORD_SYS,
      XL,
      XL_ORIGINAL,
      YL,
      YL_ORIGINAL,
      EXPO,
      EXPO_ORIGINAL,
      PENT2,
      PENT2_ORIGINAL,
      LIGN1,
      LIGN2,
      HERB
    )

  return(plot_info)
}



#' @describeIn tables_processing Process to gather needed data from tree table

ffi_tree_table_process <- function(tree_data, plot,  year, espar_cdref, idp_dep_ref) {
  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(tree_data)))
    # !any(c(tree_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # we  filter the data for plot/year and status (alive)????
  tree_raw_data <- .read_inventory_data(
    tree_data,
    select = c(
      "CAMPAGNE", "IDP", "A", "W", "ESPAR", "VEGET", "VEGET5", "C13", "HTOT"
      # "IR5", "TIGE", "TETARD", "QUALITE", "CIBLE"
    ),
    colClasses = list(character = c( "ESPAR", "IDP")),
    header = TRUE
  )

  tree_filtered_data <- tree_raw_data |>
    dplyr::filter(
      IDP == plot,
      CAMPAGNE == year
    ) |>
    tibble::as_tibble()

  # tree_filtered_data <- .read_inventory_data(
  #   tree_data,
  #   select = c(
  #     "CAMPAGNE", "IDP", "A", "W", "ESPAR", "VEGET", "VEGET5", "C13", "HTOT"
  #     # "IR5", "TIGE", "TETARD", "QUALITE", "CIBLE"
  #   ),
  #   colClasses = list(character = c( "ESPAR", "IDP")),
  #   header = TRUE
  # ) |>
  #   dplyr::filter(
  #     IDP == plot,
  #     CAMPAGNE == year
  #   ) |>
  #   tibble::as_tibble()


  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(tree_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
    ))
    return(dplyr::tibble())
  }

  tree <- tree_raw_data |>
    # we filter the datqa for plot
    dplyr::filter(IDP == plot) |>
    # transformations and filters
    dplyr::mutate(
      DENSITY = W,
      C13 = as.numeric(C13),
      #transformation to diameter
      DIA = (C13/pi)*100,
      YEAR = CAMPAGNE,
      #ID_UNIQUE_PLOT= (paste("FR", IDP, sep="_"))
    ) |>
    #join with espar_cdref
    dplyr::left_join(
      y = espar_cdref |>
        dplyr::select(
          ESPAR,
          cd_ref,
          lib_cdref
        ) |>
        dplyr::as_tibble(),
      by = "ESPAR"
    ) |>
    dplyr::left_join(
      y = idp_dep_ref |> tibble::as_tibble(),
      by = "IDP"
    ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR",DEP,IDP, sep = "_"))
    ) |>
    dplyr::rename(
      PLOT = IDP,
     # FR_SP_NAME = LIBELLE,
      SP_NAME = lib_cdref,
      SP_CODE = cd_ref,
      TREE = A,
     #ht in meters
      HT = HTOT,
      STATUS = VEGET
    ) |>
    #selection of final variables
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      DEP,
      YEAR,
      TREE,
      ESPAR,
      #FR_SP_NAME,
      SP_CODE,
      SP_NAME,
      STATUS,
      VEGET5,
      #LIB,
      DIA,
      HT,
      DENSITY,
      # IR5,
      # TIGE,
      # TETARD,
      # QUALITE,
      # CIBLE
    ) |>
    #homogeneization
    #añadir condiciones en funcion de si es na o no ??
    dplyr::group_by(ID_UNIQUE_PLOT) |>
    dplyr::arrange(ID_UNIQUE_PLOT,TREE, YEAR) |>
    dplyr::mutate_at(dplyr::vars(ESPAR), ~ dplyr::na_if(., "")) |>
    dplyr::as_tibble() |>
    tidyr::fill(c(ESPAR,SP_CODE,SP_NAME)) |>
    dplyr::filter(YEAR == year)

  return(tree)
}


#' @describeIn tables_processing
#' Process to gather needed data from shrub table

ffi_shrub_table_process <- function(
  shrub_data, plot, year,
  cd_ref, growth_form_lignified_france, idp_dep_ref
) {

  # Debug
    # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(shrub_data))
    # !any(c(understory_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # 2. col names

  shrub_filtered_data <- .read_inventory_data(
    shrub_data,
    select = c(
      "CAMPAGNE",
      "IDP",
      "CD_REF",
      "ABOND"
    ),
    header = TRUE,
    colClasses = list(character = c("IDP", "CD_REF"))
  ) |>
    # we  filtering the data for plot/year and status (alive)
    dplyr::filter(
      IDP == plot,
      CAMPAGNE == year
    ) |>
    dplyr::as_tibble()


  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(shrub_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that combination of plot and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
    ))
    return(dplyr::tibble())
  }

  # transformations and filters
  shrub <- shrub_filtered_data |>
    dplyr::mutate(
      YEAR = CAMPAGNE,
      # ID_UNIQUE_PLOT= (paste("FR", IDP, sep="_")),
      # cd_ref = as.character(CD_REF),
      #conversion to percentage
      ABOND = dplyr::case_when(
        # présence faible	Taux de recouvrement de l'espèce inférieur à 5 % et présence faible.
        ABOND == 1 ~ 5,
        #présence nette	Taux de recouvrement de l'espèce inférieur à 25 % mais présence nette.
        ABOND == 	2	~ 12.5,
        #Taux de recouvrement de l'espèce compris entre 25 et 50 %
        ABOND	== 3 ~	37.5,
        #Taux de recouvrement de l'espèce compris entre 25 et 50 %
        #Taux de recouvrement de l'espèce compris entre 50% et 75 %.
        ABOND	== 4 ~	62.5,
        #	Taux de recouvrement de l'espèce supérieur à 75%.
        ABOND	== 5 ~	87.5
      )
    ) |>
    dplyr::left_join(
      y = cd_ref |>
        dplyr::select(
          CD_REF,
          lib_cdref
        ) ,
      by = "CD_REF"
    ) |>
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"
    ) |>

    # #join with espar_cdref
    # dplyr::left_join(
    #   y = espar_cdref |>
    #     dplyr::select(
    #       cd_ref,
    #       ESPAR
    #     ) |>
    #     dplyr::rename(
    #       CD_REF = cd_ref
    #     ) |>
    #     dplyr::as_tibble(),
    #   by = "CD_REF"
    # ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_")),
       HT = NA
    ) |>
    dplyr::rename(
      PLOT = IDP,
      SP_NAME = lib_cdref,
      SP_CODE = CD_REF,
      COVER = ABOND
    ) |>
    #selection of final variables
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      DEP,
      YEAR,
      SP_CODE,
      # ESPAR,
      SP_NAME,
      COVER,
      HT
    ) |>
    dplyr::as_tibble()




  #to eliminate herbs i do a join with a database from try

  growth_form_lignified_france <- growth_form_lignified_france |>
    dplyr::select(
      AccSpeciesName,
      GrowthForm
    ) |>
    dplyr::mutate(SP_NAME = AccSpeciesName)

  understory_no_herbs <- shrub |>
    dplyr::left_join(
      y = growth_form_lignified_france,
      by = "SP_NAME"
    ) |>

    #here we collect both tree and shrub but it is also possible to only collect shrub

    # dplyr::filter(
    #   (grepl("tree|shrub", GrowthForm))) |>
    dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          YEAR,
          SP_CODE,
          # ESPAR,
          SP_NAME,
          COVER,
          HT,
          GrowthForm
        ) |>
    dplyr::as_tibble()






  return(understory_no_herbs)





  #another way of eliminating is is with spar_cdref species that only have trees



}


#' @describeIn tables_processing Process to gather needed data from soil table

# ffi_soil_table_process <- function(soils_data, plot, year, metadonnees,idp_dep_ref){
#
#   # Assertions  and checks/validations
#   files_validation <- assertthat::validate_that(
#     !any(is.na(c(soils_data)))
#     # !any(c(soils_data) == NA_character_)
#   )
#
#   # If any file is missing abort and return an empty tibble??
#   if (is.character(files_validation)) {
#     cli::cli_warn(c(
#       "Some files can't be found",
#       "i" = "Skipping plot info for plot {.var {plot}}  for {.var {year}}"
#     ))
#
#     return(dplyr::tibble())
#   }
#
#    # browser()
#   soil_filtered_data <- .read_inventory_data(
#     soils_data,
#     select = c(
#       "CAMPAGNE",
#       "IDP",
#       "DATEECO",
#       "TSOL",
#       # in dm
#       #horizon superieur
#       "PROF1",
#       #horizon inferieur
#       "PROF2",
#       "TEXT1",
#       "TEXT2",
#       "ROCHE",
#
#       # dixiemes
#       "AFFROC",
#       "CAI40",
#       "CAILLOUX",
#       "AFPLA"
#     ),
#     header = TRUE,
#     colClasses = list(character = c("IDP", "TEXT1", "TEXT2", "ROCHE", "TSOL"))
#   ) |>
#     # we  filter the data for plot/year
#     dplyr::filter(
#       IDP == plot,
#       CAMPAGNE == year
#     ) |>
#     dplyr::as_tibble()
#
#   ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
#   if (nrow(soil_filtered_data) < 1) {
#     # warn the user
#     cli::cli_warn(c(
#       "Data missing for that combination of plot and year",
#       "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
#     ))
#     return(dplyr::tibble())
#   }
#
#   soil_meta <- metadonnees |>
#     dplyr::filter(
#       UNITE  %in% c("TSOL", "TEXT1", "ROCHED0"))
#
#   soil <- soil_filtered_data |>
#     dplyr::mutate(
#       YEAR = CAMPAGNE,
#       #dm to cm
#       PROF1 = PROF1*10,
#       PROF2 = PROF2*10
#     ) |>
#     dplyr::left_join(
#       soil_meta |>
#         dplyr::filter(UNITE == "TEXT1") |>
#         dplyr::rename(TEXT1 = Code,
#                       TEXT1_DES = Libellé) |>
#         dplyr::select(TEXT1,TEXT1_DES),
#       by = "TEXT1"
#     ) |>
#     dplyr::left_join(
#       soil_meta |>
#         dplyr::filter(UNITE == "TEXT1") |>
#         dplyr::rename(TEXT2 = Code,
#                       TEXT2_DES = Libellé) |>
#         dplyr::select(TEXT2,TEXT2_DES),
#       by = "TEXT2"
#     ) |>
#     dplyr::left_join(
#       soil_meta |>
#         dplyr::filter(UNITE == "ROCHED0") |>
#         dplyr::rename(ROCHE = Code,
#                       ROCHE_DES = Libellé) |>
#         dplyr::select(ROCHE,ROCHE_DES),
#       by = "ROCHE"
#     ) |>
#     dplyr::left_join(
#       soil_meta |>
#         dplyr::filter(UNITE == "TSOL") |>
#         dplyr::rename(TSOL = Code, TSOL_DES = Libellé) |>
#         dplyr::select(TSOL,TSOL_DES),
#       by = "TSOL") |>
#     dplyr::left_join(
#       y = idp_dep_ref,
#       by = "IDP"
#     ) |>
#     dplyr::mutate(
#       ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_"))
#     ) |>
#     dplyr::rename(
#       PLOT = IDP
#     )
#
#   soil_field <- list(
#     soil |> dplyr::select(
#       ID_UNIQUE_PLOT,
#       PLOT,
#       DEP,
#       YEAR,
#       DATEECO,
#       TSOL,
#       TSOL_DES,
#       ROCHE,
#       ROCHE_DES,
#       TEXT1,
#       TEXT1_DES,
#       TEXT2,
#       TEXT2_DES,
#       PROF1,
#       PROF2,
#       CAI40,
#       CAILLOUX,
#       AFFROC,
#       AFPLA
#     ))
#
#
#   soil_info <- soil |>
#     dplyr::mutate(
#       soil_field = soil_field
#     ) |>
#     dplyr::select(
#       ID_UNIQUE_PLOT,
#       PLOT,
#       DEP,
#       YEAR,
#       DATEECO,
#       soil_field
#
#     ) |>
#     dplyr::as_tibble()
#
#   return(soil_info)
# }


#' @describeIn tables_processing Process to gather needed data from soil table


# TABLE FOR REGEN DEPEND ON YEAR , BEFORE 2015 COUVERT SHOULD BE USED, AFTER 2015 FLORE SHOULD (SAME AS SHRUB PROCESS) SHALL WE INTEGRATE PART OF THIS PROCESS IN SHRUB PROCESS O "REPET" READING OF TABLE IN REGEN ?
ffi_regen_table_process <- function(regen_data, plot, year, espar_cdref,idp_dep_ref){
  # Debug
    # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(regen_data))
    # !any(c(understory_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ))

    return(dplyr::tibble())
  }


  # if (year > 2015) {
  # # 2. col names
  #
  # regen_filtered_data <- .read_inventory_data(
  #   shrub_data,
  #   select = c(
  #     "CAMPAGNE",
  #     "IDP",
  #     "CD_REF",
  #     "ABOND"
  #   ),
  #   header = TRUE,
  #   colClasses = list(character = c("IDP", "CD_REF"))
  # ) |>
  #   # we  filtering the data for plot/year and status (alive)
  #   dplyr::filter(
  #     IDP == plot,
  #     CAMPAGNE == year
  #   ) |>
  #   dplyr::as_tibble()
  #
  #
  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  # if (nrow(regen_filtered_data) < 1) {
  #   # warn the user
  #   cli::cli_warn(c(
  #     "Data missing for that combination of plot and year",
  #     "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
  #   ))
  #   return(dplyr::tibble())
  # }
  #
  #
  # # transformations and filters
  # regen <- regen_filtered_data |>
  #   dplyr::mutate(
  #     YEAR = CAMPAGNE,
  #     # ID_UNIQUE_PLOT= (paste("FR", IDP, sep="_")),
  #     # cd_ref = as.character(CD_REF),
  #     #conversion to percentage
  #     ABOND = dplyr::case_when(
  #       # présence faible	Taux de recouvrement de l'espèce inférieur à 5 % et présence faible.
  #       ABOND == 1 ~ 5,
  #       #présence nette	Taux de recouvrement de l'espèce inférieur à 25 % mais présence nette.
  #       ABOND == 	2	~ 12.5,
  #       #Taux de recouvrement de l'espèce compris entre 25 et 50 %
  #       ABOND	== 3 ~	37.5,
  #       #Taux de recouvrement de l'espèce compris entre 25 et 50 %
  #       #Taux de recouvrement de l'espèce compris entre 50% et 75 %.
  #       ABOND	== 4 ~	62.5,
  #       #	Taux de recouvrement de l'espèce supérieur à 75%.
  #       ABOND	== 5 ~	87.5
  #     )
  #   ) |>
  #   dplyr::left_join(
  #     y = idp_dep_ref,
  #     by = "IDP"
  #   ) |>
  #   #join with espar_cdref
  #   dplyr::left_join(
  #     y = espar_cdref |>
  #       dplyr::select(
  #         cd_ref,
  #         lib_cdref
  #       ) |>
  #       dplyr::rename(
  #         CD_REF = cd_ref
  #       ) |>
  #       dplyr::as_tibble(),
  #     by = "CD_REF"
  #   ) |>
  #
  #   dplyr::mutate(
  #     ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_"))
  #   ) |>
  #   dplyr::rename(
  #     PLOT = IDP,
  #     SP_NAME = lib_cdref,
  #     SP_CODE = CD_REF,
  #     COVER = ABOND
  #   ) |>
  #   #selection of final variables
  #   dplyr::select(
  #     ID_UNIQUE_PLOT,
  #     PLOT,
  #     DEP,
  #     YEAR,
  #     SP_CODE,
  #     SP_NAME,
  #     COVER
  #   ) |>
  #   dplyr::as_tibble()
  # }else{
  #

    regen_filtered_data <- .read_inventory_data(
      regen_data,
      select = c(
        "CAMPAGNE",
        "IDP",
        "ESPAR_C",
        "TCA",
        "STRATE"
      ),
      header = TRUE,
      colClasses = list(character = c("IDP", "ESPAR_C"))
    ) |>
      # we  filtering the data for plot/year and status (alive)
      dplyr::filter(
        IDP == plot,
        CAMPAGNE == year
      ) |>
      dplyr::as_tibble()


    ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(regen_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Data missing for that combination of plot and year",
        "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
      ))
      return(dplyr::tibble())
    }

    # transformations and filters
    regen <- regen_filtered_data |>
      dplyr::mutate(
        YEAR = CAMPAGNE,
        COVER = TCA,
        ESPAR = ESPAR_C
      ) |>
      dplyr::filter(
        STRATE == "NR"
      ) |>
      dplyr::left_join(
        y = idp_dep_ref,
        by = "IDP"
      ) |>
      #join with espar_cdref
      dplyr::left_join(
        y = espar_cdref |>
          dplyr::select(
            cd_ref,
            lib_cdref,
            ESPAR
          ) |>
          dplyr::rename(
            CD_REF = cd_ref
          ) |>
          dplyr::as_tibble(),
        by = "ESPAR"
      ) |>

      dplyr::mutate(
        ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_")),

        # in cm
        DBH =  7.5
      ) |>
      dplyr::rename(
        PLOT = IDP,
        SP_NAME = lib_cdref,
        SP_CODE = CD_REF
      ) |>
      #selection of final variables
      dplyr::select(
        ID_UNIQUE_PLOT,
        PLOT,
        DEP,
        YEAR,
        SP_CODE,
        SP_NAME,
        COVER,
        DBH
      ) |>
      dplyr::as_tibble()
  # }


  return(regen)
}

