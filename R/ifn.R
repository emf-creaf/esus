#' Raw IFN data to tibble
#'
#' Transform raw IFN plot data into tidy data for easier use
#'
#' This function will take every year indicated and retrieve and transform the plot data for the
#' provinces, versions and plots provided. For that, IFN db files must reside in the folder
#' indicated in the \code{folder} argument.
#'
#' @param provinces A character vector with the two-number codes for the provinces.
#' @param versions A character vector with the ifn versions. Valid versions are \code{"ifn2"},
#'   \code{"ifn3"} and \code{"ifn4"}.
#' @param filter_list A list of provinces and plots to extract the data from.
#' @param folder The path to the folder containing the IFN db files, as character.
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
#'   If no \code{filter_list} argument is provided, \code{ifn_to_tibble} will attempt to process all
#'   plots for the provinces and ifn versions provided. This will result in sometimes
#'   thousands plots to be extracted, processed and returned, which in turn will cause a big use of
#'   memory (specially when running in parallel processes) and long times of calculation.
#'   Is better to provide a list of departments with the provinces and plots to look for to
#'   narrow the process. This \code{filter_list} should have the following structure:
#'   \preformatted{
#'    list(
#'    "01" = c("01_0644_NN_A1_A1"),
#'    "08" = c("08_1256_NN_A1_xx", "08_0056_xx_A4_xx"),
#'    "24" = c("24_0270_xx_A4_xx")
#'   )
#'   }
#'   \code{esus} package offers workflows to create this automatically, see
#'   \code{vignette("filtering_plots", pkg = "esus")} for more details.
#'
#' @section Parallel:
#'   Processing the plots from within an IFN version can be done in parallel (\code{esus} uses
#'   internally the \code{\link[furrr]{furrr}} package for this). This means that, if
#'   parallelization is active, several processes are launched to retrieve the plots data for that
#'   IFN version. This is repeated for all versions provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{ifn_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#' 
#' @examples
#' \donttest{
#' \dontrun{
#' library(esus)
#' ifn_to_tibble(
#'   provinces = c("24"), versions = c("ifn3"),
#'   filter_list = list("24" = c("24_0270_xx_A4_xx")),
#'   folder = "path/to/ifn/data"
#' )
#' }
#' }
#'
#' @export
ifn_to_tibble <- function(
  provinces,
  versions,
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
  assertthat::assert_that(.sys_cmd_warning())

  # provinces
  assertthat::assert_that(
    is.character(provinces), length(provinces) > 0,
    msg = cli::cli_abort("provinces must be a character vector with at least one province code")
  )
  # provinces are valid
  valid_provinces <- ifn_plots_thesaurus |>
    dplyr::pull(.data$PROVINCIA) |>
    unique() |>
    sort()
  invalid_provinces <- which(!provinces %in% valid_provinces)

  if (identical(length(provinces), length(invalid_provinces))) {
    cli::cli_abort(c(
      "x" = "Any of the provided {.arg provinces} ({.val {provinces}}) are valid. Aborting."
    ))
  } else {
    if (length(invalid_provinces) > 0) {
      cli::cli_warn(c(
        "!" = "Some {.arg provinces} are not valid: {.val {provinces[invalid_provinces]}}",
        "i" = "These will be skipped in the process"
      ))
    }
  }

  # versions
  assertthat::assert_that(
    is.character(versions), length(versions) > 0,
    msg = cli::cli_abort("versions must be a character vector with at least one")
  )
  assertthat::assert_that(
    all(versions %in% c("ifn2", "ifn3", "ifn4")),
    msg = cli::cli_abort('Only valid {.arg versions} are "ifn2", "ifn3" or "ifn4"')
  )

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      "Folder especified ({.path {folder}}) doesn't exists.
      Please create the folder first and populate it with the needed IFN csv files"
    )
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

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(versions)} cicle{?s}")
    ),
    .verbose
  )

  # get the caller environment to propagate errors
  .call <- rlang::caller_env(0)
  # send the versions in loop to process table function
  inventory_data <- purrr::map(
    versions,
    .f = \(version) {
      ifn_tables_process(
        provinces, version, filter_list, folder, .parallel_options, .verbose, .call, ...
      )
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind() |>
    clean_empty(clean_empty)

  if (isTRUE(as_sf)) {
    inventory_data <- inventory_data |>
      inventory_as_sf()
  }

  return(inventory_data)
}

#' Inner function to process all tables for one version
#'
#' Processing all tables for one version
#'
#' This function is intended to be called internally by \code{\link{ifn_to_tibble}} for each
#' version. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @inherit ifn_to_tibble
#'
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @noRd
ifn_tables_process <- function(
  provinces, version, filter_list, folder,
  .parallel_options, .verbose, .call = rlang::caller_env(), ...
) {

  # Create input df for year
  input_df <- .build_ifn_input_with(version, provinces, filter_list, folder, .verbose, .call)

  ## Debugging needs purrr maps, as future maps are in other R processes and we
  ## can't access the objects.
  ## temp_res <- purrr::pmap(
  temp_res <- furrr::future_pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(
      province, plots, version, tree_table, plot_table, shrub_table, regen_table, coord_table
    ) {

      # browser()
      plot_info <- ifn_plot_table_process(
        plot_table, coord_table, version, plots, province, ifn_provinces_dictionary, .call
      )

      redundant_vars <- c(
        "id_unique_code", "country", "year", "ca_name_original", 
        "province_code","province_name_original", "plot", "Clase",
        "Subclase", "version", "type","aspect", "slope","crs", 
        "coord_sys", "COORDEX",  "COORDEY", "sheet_ntm", "huso"
      )

      tree <- ifn_tree_table_process(
        tree_table, version, plots, province, species_ifn_internal, .call
      ) |>
        dplyr::select(!dplyr::any_of(redundant_vars))
      shrub <- ifn_shrub_table_process(
        shrub_table, version, plots, province, species_ifn_internal, .call
      ) |>
        dplyr::select(!dplyr::any_of(redundant_vars))
      regen <- ifn_regen_table_process(
        regen_table, version, plots, province, species_ifn_internal, .call
      ) |>
        dplyr::select(!dplyr::any_of(redundant_vars))

      # check if there are rows
      if (nrow(plot_info) < 1) {
        return(tibble::tibble())
      }

      # we put together all tables in a data frame
      understory <- tibble::tibble(shrub = list(shrub))

      plot_info |>
        dplyr::rename(coordx = "COORDEX", coordy = "COORDEY", 
                      class =  "Clase", subclass = "Subclase") |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen)
        ) |>
        dplyr::select(
          dplyr::any_of(c(
            "id_unique_code", "country", "year", "ca_name_original", "province_name_original",
            "province_code", "plot", "Clase", "Subclase", "version", "type", "sheet_ntp", "huso",
            "coord_sys", "coordx", "coordy", "crs", "slope_mean", "slope", "elev", "aspect", "tree",
            "understory", "regen"
          ))
        )
    }
  ) |>
    purrr::list_rbind()

  # something went wrong (bad counties and plots, wrong filter list...)
  if (nrow(temp_res) < 1) {
    cli::cli_abort("Ooops! Something went wrong, exiting...", call = .call)
  }

  # filtering the missing plots. This is done based on the fact plot table functions returns NAs
  # for all vars, including coords, when the plot is not found
  temp_res |>
    dplyr::filter(!(is.na(.data$coordx) & is.na(.data$coordy)))

}

#' FIA data tables process
#'
#' Process to gather needed data from FIA csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param tree_data,shrub_data,regen_data,plot_data,coord_data Paths to the files with the corresponding data
#' @param version IFN version
#' @param plot plot unique ID code
#' @param province Province two-number code
#' @param species_ifn_internal,ifn_provinces_dictionary species and provinces dictionary tables.
#'   These tables are included as internal data in the package
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'  IFN version.
#'
#' @name ifn_tables_processing
#' @noRd
NULL

#' IFN data tables process
#' @describeIn ifn_tables_processing Process to gather needed data from tree table
#' @noRd
ifn_tree_table_process <- function(
  tree_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(tree_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read

  if (version == "ifn2") {
    tree_filtered_data <- .read_inventory_data(
      tree_data,
      colnames = c(
        "PROVINCIA", "ESTADILLO", "ESPECIE", "NUMORDEN", "ARBOL", "DIAMETRO1", "DIAMETRO2", "ALTURA", "FORMA", "CALIDAD"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == plot
      ) |>
      tibble::as_tibble()



    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Tree data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ), call = .call)
      return(dplyr::tibble())
    }

    tree <- tree_filtered_data |>
      # transformations and filters
      dplyr::rename(
        province_code = "PROVINCIA", plot = "ESTADILLO", SP_CODE = "ESPECIE",
        tree = "ARBOL", Dn1 = "DIAMETRO1", Dn2 = "DIAMETRO2", height = "ALTURA"
      ) |>
      dplyr::mutate(
        plot = as.character(.data$plot),
        province_code = as.character(.data$province_code),
        Dn1 = as.numeric(.data$Dn1),
        Dn2 = as.numeric(.data$Dn2),
        height = as.numeric(stringr::str_replace(.data$height, ",", ".")),
        SP_CODE = as.numeric(.data$SP_CODE),
        dia = ((.data$Dn1 + .data$Dn2) / 2) * 0.1, # From mm to cm
        height = .data$height, # in meters
        density_factor = dplyr::case_when(
          .data$dia < 12.5 ~ 127.3239546,
          .data$dia >= 12.5 & .data$dia < 22.5 ~ 31.83098865,
          .data$dia >= 22.5 & .data$dia < 42.5 ~ 14.14710607,
          .data$dia >= 42.5 ~ 5.092958185
        )
      )  |>
      # add species info ---> WHAT REFERENCE SHOULD I USEE???
      dplyr::left_join(
        y = species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::rename(
        sp_code = "SP_CODE",
        sp_name = "SP_NAME",
        quality_wood = "CALIDAD",
        cubing_form = "FORMA"  
      ) |> 
      dplyr::select(
        "id_unique_code", "province_code", "plot", "sp_code", "sp_name",
        "tree",
        "dia", # diameter in cm
        "height", # height in m
        "density_factor",
        "cubing_form",
        "quality_wood"
      )

    # Return tree
    return(tree)
  }

  if (version %in% c("ifn3", "ifn4")) {
    tree_filtered_data <-  .read_inventory_data(
      tree_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Especie", "nArbol", "OrdenIf3",
        "OrdenIf2", "OrdenIf4", "Dn1", "Dn2", "Ht", "Calidad", "Forma"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(.data$id_unique_code == plot) |>
      dplyr::rename(any_of(c(
        tree = "nArbol",
        tree_ifn2 = "OrdenIf2",
        tree_ifn4 = "OrdenIf4",
        tree_ifn3 = "OrdenIf3"
      ))
      ) |> 
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(tree_filtered_data)) {
      tree_filtered_data <- tree_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Tree data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ), call = .call)
      return(dplyr::tibble())
    }

    tree <- tree_filtered_data |>
      dplyr::rename(plot = "Estadillo", height = "Ht", Clase = "Cla") |>
      # units transformations
      dplyr::mutate(
        province_code = province,
        # Subclass fixes
        Subclase = .ifn_subclass_fixer(.data$Subclase),
        dia = (.data$Dn1 + .data$Dn2) / 2,
        # MM TO CM
        dia = .data$dia * 0.1,
        SP_CODE = as.numeric(.data$Especie),
        # density represented by each tree considering  plot design (variable radious)
        density_factor = dplyr::case_when(
          .data$dia < 12.5 ~ 127.3239546,
          .data$dia >= 12.5 & .data$dia < 22.5 ~ 31.83098865,
          .data$dia >= 22.5 & .data$dia < 42.5 ~ 14.14710607,
          .data$dia >= 42.5 ~ 5.092958185
        )
      ) |>
      # add species info
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::rename(
        sp_code = "SP_CODE",
        sp_name = "SP_NAME"
      ) |> 
      dplyr::rename(
        quality_wood = "Calidad",
        cubing_form = "Forma"
      ) |> 
      dplyr::select(
        dplyr::any_of(c(
          "id_unique_code", "province_code", "Clase", "Subclase", 
          "plot", "sp_code", "sp_name",
          #tree number id in ifn4
          "tree",
          "tree_ifn2",
          "tree_if3",
          "tree_if4",
          #CUALIDAD 6 = dead but providing functions
          "quality_wood", "cubing_form",
          #check codes to understand origin and trace of individuals
          #diameter in cm
          "dia",
          #height in cm
          "height", "density_factor"
        ))
      )

    # Return tree
    return(tree)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from shrub table
#' @noRd
ifn_shrub_table_process <- function(
  shrub_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(shrub_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping shrub data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  if (version == "ifn2") {
    shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = c("PROVINCIA", "ESTADILLO", "ESPECIE", "FRACCAB", "ALTUMED"),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(.data$id_unique_code == plot) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(shrub_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Shrub data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    #we add the id code
    shrub <- shrub_filtered_data |>
      dplyr::rename(plot = "ESTADILLO", cover = "FRACCAB", height = "ALTUMED") |>
      dplyr::mutate(
        plot = as.character(.data$plot),
        province_code = as.character(.data$PROVINCIA),
        height = as.numeric(.data$height) * 10, # DM TO cm
        cover = as.numeric(.data$cover),
        SP_CODE = as.numeric(.data$ESPECIE)
      ) |>
      # 3. ref_plant_dictionary
      # we join data from plant ref dictionary
      # some symbols apply for multiple species
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::rename(
        sp_name  = "SP_NAME",
        sp_code = "SP_CODE"
      ) |> 
      dplyr::select("id_unique_code", "province_code", "plot", "sp_name",
                    "sp_code", "height", "cover")
    # Return shrub
    return(shrub)
  }

  if (version %in% c("ifn3", "ifn4")) {
    shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = c(
        "Provincia",
        "Estadillo",
        "Cla",
        "Subclase",
        "Especie",
        "Fcc",
        "Hm"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(shrub_filtered_data)) {
      shrub_filtered_data <- shrub_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(shrub_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Shrub data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    shrub <- shrub_filtered_data |>
      dplyr::rename(plot = "Estadillo", height = "Hm", Clase = "Cla") |>
      dplyr::mutate(
        province_code = province,
        # Subclass fixes
        Subclase = .ifn_subclass_fixer(.data$Subclase),
        cover = .data$Fcc,
        # DM TO CM
        height = as.numeric(.data$height) * 10,
        cover = as.numeric(.data$cover),
        SP_CODE = as.numeric(.data$Especie)
      ) |>
      # 3. ref_plant_dictionary
      # we join data from plant ref dictionary
      # some symbols apply for multiple species
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::rename(
        sp_code = "SP_CODE",
        sp_name = "SP_NAME"
      ) |> 
      dplyr::select(
        "id_unique_code", "province_code", "Clase", "Subclase",
        "plot", "sp_name", "sp_code", "height", "cover"
      )

    return(shrub)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from regen table
#' @noRd
ifn_regen_table_process <- function(
  regen_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(regen_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  if (version == "ifn2") {
    regen_filtered_data <- .read_inventory_data(
      regen_data,
      colnames = c("PROVINCIA", "ESTADILLO", "ESPECIE", "NUMERO", "ALTUMED", "REGENA"),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == plot
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(regen_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Regeneration data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    # we add the id code
    regeneration <- regen_filtered_data |>
      dplyr::mutate(
        plot = as.character(.data$ESTADILLO),
        province_code = as.character(.data$PROVINCIA),
        NUMERO = as.numeric(.data$NUMERO),
        Hm = as.numeric(.data$ALTUMED) * 10, # DM TO CM
        SP_CODE = as.numeric(.data$ESPECIE)
      ) |>
      dplyr::left_join(
        y = species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::rename(Numero = "NUMERO", Regena = "REGENA") |>
      #information for different individuals of same species is recorded in different variables
      #but in the same row. we will repeat each line twice so that we can keep records separately
      #and apply the different transformations
      dplyr::slice(rep(dplyr::row_number(), each = 2)) |>
      dplyr::mutate(
        #even rows will give information on regena (species with a diameter below 25)
        # odd rows will give information on species with a diameter above  25 (numero)
        #numero is associated with Hm
        Regena = ifelse(dplyr::row_number() %% 2 != 0, NA, .data$Regena),
        Numero  = ifelse(dplyr::row_number() %% 2 == 0, NA, .data$Numero),
        Hm = ifelse(dplyr::row_number() %% 2 == 0, NA, .data$Hm),
        #dbh default value is lower for species with a diameter below 25 ( regena class)
        dbh = dplyr::case_when(.data$Numero > 0 ~ 5, .data$Regena > 0 ~ 1, TRUE ~ NA),
        density_factor = 127.3239546,
        #default values of N = NUMERO IND * density_factor; Num of individuals is given for each regena class
        #density is equal for all
        n = dplyr::case_when(
          .data$Regena == 1 ~ 2.5 * density_factor,
          .data$Regena == 2 ~ 10 * density_factor,
          .data$Regena == 3 ~ 20 * density_factor,
          .data$Numero > 0 ~ .data$Numero * density_factor,
          TRUE ~ NA
        ),
        # we give a default value for individual below 25 cm of diameter
        height = dplyr::case_when(
          .data$Hm > 0 ~ .data$Hm,
          .data$Regena > 0 ~ 100,
          TRUE ~ NA
        )
      ) |>
      dplyr::select(
        "id_unique_code", "province_code", "plot", "SP_CODE",
        "sp_name", "dbh", "height", "density_factor", "n"
      ) |>
      dplyr::filter(stats::complete.cases(.data$dbh, .data$height, .data$N))

    # Return regen
    return(regeneration)
  }

  if (version %in% c("ifn3", "ifn4")) {
    regen_filtered_data <- .read_inventory_data(
      regen_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Especie", "CatDes",
        "Tipo", "Densidad", "NumPies", "Hm"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(regen_filtered_data)) {
      regen_filtered_data <- regen_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }


    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(regen_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Regeneration data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }


    # we add the id code
    regeneration <- regen_filtered_data |>
      dplyr::rename(plot = "Estadillo", Clase = "Cla") |>
      dplyr::mutate(
        province_code = province,
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        Hm = .data$Hm * 10, # DM TO CM
        SP_CODE = as.numeric(.data$Especie)
      ) |>
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::mutate(
        density_factor = 127.3239546,
        #default values for different catdes (development category)
        dbh = dplyr::case_when(
          .data$CatDes == 1 ~ 0.1,
          .data$CatDes == 2 ~ 0.5,
          .data$CatDes == 3 ~ 1.5,
          .data$CatDes == 4 ~ 5,
          TRUE ~ NA
        ),
        #default values of NUmPies for different values of "Densidad"
        n = dplyr::case_when(
          .data$Densidad == 1 ~ 2.5 * density_factor,
          .data$Densidad == 2 ~ 10 * density_factor,
          .data$Densidad == 3 ~ 20 * density_factor,
          .data$CatDes == 4 ~ NumPies * density_factor,
          TRUE ~ NA
        ),
        #default values of height for different values of "CatDes"
        height = dplyr::case_when(
          .data$CatDes == 1 ~ 10,
          .data$CatDes == 2 ~ 80,
          .data$CatDes == 3 ~ 100,
          .data$CatDes == 4 ~ Hm,
          TRUE ~ NA
        )
      ) |>
      dplyr::rename(
        sp_code = "SP_CODE",
        sp_name = "SP_NAME",
      ) |> 
      dplyr::select(
        "id_unique_code", "province_code", "Clase", "Subclase", "plot",
        "sp_code", "sp_name", "dbh", "height", "n", "density_factor"
      )

    # Return regen
    return(regeneration)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from plot and coords tables
#' @noRd
ifn_plot_table_process <- function(
  plot_data, coord_data, version,
  plot, province, ifn_provinces_dictionary, .call = rlang::caller_env()
) {

  # in some cases (get plots from provinces) we pass a quosure to the plot argument.
  # If not, get the estadillo padded. This is for the coord tables in ifn3 and ifn4
  if (!rlang::is_quosure(plot)) {
    plot_estadillo <- stringr::str_split_i(plot, "_", 2)
  } else {
    plot_estadillo <- rlang::quo(.data$Estadillo)
    if (version == "ifn2") {
      plot_estadillo <- rlang::quo(.data$ESTADILLO)
    }
  }

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(plot_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names
  if (version == "ifn2") {

    plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "PROVINCIA", "ESTADILLO", "HOJA", "ANO", "COORDEX", "COORDEY", "ALTITUD1", "ALTITUD2",
        "PENDIEN1", "PENDIEN2", "FRACCION1", "FRACCION2", "CLASUELO", "ESPESOR", "CLACOBER",
        "CUBIERTA", "ORIENTA1", "ORIENTA2", "MAXPEND1", "MAXPEND2"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == !!plot,
        .data$PROVINCIA == !!province
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(plot_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "plot data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    if (any(is.na(plot_filtered_data$COORDEX), is.na(plot_filtered_data$COORDEY))) {
      cli::cli_warn(c(
        "File {.file {plot_data}} has some errors in the coordinates
        (missing coordinates, bad format...).",
        "i" = "These records will be removed from the results"
      ), call = .call)
    }

    # fix bad formatted coords and filter missing coordinates
    # Ok, here is a little weird, but is what it is. Some IFN2 provinces have letters in the
    # coordinates, which mess with the process. The thing is that due to some
    # exporting/formatting/blackbox process numbers were converted to letters. A is the first number
    # of the first ocurrence, B is the first number of the second ocurrence and so on. i.e. in province
    # "03", A is 7, B is 6 and C is 2
    plot_coord_fixed_data <- plot_filtered_data |>
      dplyr::filter(!is.na(.data$COORDEX), !is.na(.data$COORDEY)) |>
      dplyr::mutate(
        # debug vars, will be removed in the output object
        coordx_orig = .data$COORDEX,
        coordy_orig = .data$COORDEY,
        # corrections per province
        COORDEX = dplyr::case_when(
          # 03
          .data$PROVINCIA == "03" & stringr::str_detect(coordx_orig, "[Aa]") ~
            stringr::str_replace_all(.data$COORDEX, "A", "7"),
          .data$PROVINCIA == "03" & stringr::str_detect(coordx_orig, "[Bb]") ~
            stringr::str_replace_all(.data$COORDEX, "B", "6"),
          .data$PROVINCIA == "03" & stringr::str_detect(coordx_orig, "[Cc]") ~
            stringr::str_replace_all(.data$COORDEX, "C", "2"),
          # 11
          .data$PROVINCIA == "11" & stringr::str_detect(coordx_orig, "[Aa]") ~
            stringr::str_replace_all(.data$COORDEX, "A", "2"),
          # 29
          .data$PROVINCIA == "29" & stringr::str_detect(coordx_orig, "[Aa]") ~
            stringr::str_replace_all(.data$COORDEX, "A", "3"),
          .data$PROVINCIA == "29" & stringr::str_detect(coordx_orig, "[Bb]") ~
            stringr::str_replace_all(.data$COORDEX, "B", "3"),
          # 35
          .data$PROVINCIA == "35" & stringr::str_detect(coordx_orig, "[Dd]") ~
            stringr::str_replace_all(.data$COORDEX, "D", "4"),
          .data$PROVINCIA == "35" & stringr::str_detect(coordx_orig, "[Ee]") ~
            stringr::str_replace_all(.data$COORDEX, "E", "5"),
          # 38
          .data$PROVINCIA == "38" & stringr::str_detect(coordx_orig, "[Aa]") ~
            stringr::str_replace_all(.data$COORDEX, "A", "1"),
          .data$PROVINCIA == "38" & stringr::str_detect(coordx_orig, "[Bb]") ~
            stringr::str_replace_all(.data$COORDEX, "B", "2"),
          .data$PROVINCIA == "38" & stringr::str_detect(coordx_orig, "[Cc]") ~
            stringr::str_replace_all(.data$COORDEX, "C", "3"),
          # default
          TRUE ~ .data$COORDEX
        ),
        COORDEY = dplyr::case_when(
          # 35
          .data$PROVINCIA == "35" & stringr::str_detect(coordy_orig, "[Aa]") ~
            stringr::str_replace_all(.data$COORDEY, "A", "3"),
          .data$PROVINCIA == "35" & stringr::str_detect(coordy_orig, "[Dd]") ~
            stringr::str_replace_all(.data$COORDEY, "D", "3"),
          # 11
          .data$PROVINCIA == "11" & stringr::str_detect(coordy_orig, "[Bb]") ~
            stringr::str_replace_all(.data$COORDEY, "B", "4"),
          TRUE ~ .data$COORDEY
        ) 
          


        # COORDEX = stringr::str_replace_all(.data$COORDEX, "A", "1"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "B", "2"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "C", "3"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "D", "4"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "E", "5"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "F", "6"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "G", "7"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "H", "8"),
        # COORDEX = stringr::str_replace_all(.data$COORDEX, "I", "9"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "A", "1"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "B", "2"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "C", "3"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "D", "4"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "E", "5"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "F", "6"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "G", "7"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "H", "8"),
        # COORDEY = stringr::str_replace_all(.data$COORDEY, "I", "9")
      )

    # we add the id code
    # browser()
    info_plot <- plot_coord_fixed_data |>
      dplyr::rename(
        plot = "ESTADILLO", slope = "MAXPEND2", elev = "ALTITUD2",
        year = "ANO", aspect = "ORIENTA2", slope_mean = "PENDIEN2",
        sheet_ntm = "HOJA"
      ) |>
      dplyr::mutate(
        country = "ES",
        province_code = .data$PROVINCIA,
        elev = as.numeric(.data$elev) * 100,
        slope = as.numeric(stringr::str_replace(.data$slope, ",", ".")),
        slope = dplyr::case_when(
          .data$slope == 1 ~ 1.5,
          .data$slope == 2 ~ 7.5,
          .data$slope == 3 ~ 16,
          .data$slope == 4 ~ 27,
          .data$slope == 5 ~ 40
        ),
        aspect = as.numeric(.data$aspect),
        # COORDEX = dplyr::if_else(
        #   province_code %in% c("07"),
        #   as.numeric(.data$COORDEX),
        #   as.numeric(.data$COORDEX) * 1000
        # ),
        # COORDEY = dplyr::if_else(
        #   province_code %in% c("07"),
        #   as.numeric(.data$COORDEY),
        #   as.numeric(.data$COORDEY) * 1000
        # ),
        COORDEX = as.numeric(.data$COORDEX) * 1000,
        COORDEY = as.numeric(.data$COORDEY) * 1000,
        version = version,
        huso = dplyr::case_when(
          # Provinces with Huso 28 ok
          .data$province_code %in% c(
            "35", "38"
          ) ~ 28,
          # Provinces with Huso 29 ok
          .data$province_code %in% c(
            "15", "21", "32", "36"
          ) ~ 29,
          # Provinces with Huso 30 ok
          .data$province_code %in% c(
            "01", "02", "04", "05", "09", "13", "14", "16", "18", "19", "20",
            "23", "26", "28", "29", "30", "31", "34", "40", "42", "45", "46",
            "47", "48"
          ) ~ 30,
          # Provinces with Huso 31 ok
          .data$province_code %in% c(
            "07", "08", "17", "25", "43"
          ) ~ 31,
          # Provinces with mix of Huso 30 and 31
          .data$province_code %in% c(
            "03", "12", "22", "44", "50"
          ) & .data$COORDEX < 5e+05 ~ 31,
          .data$province_code %in% c(
            "03", "12", "22", "44", "50"
          ) & .data$COORDEX > 5e+05 ~ 30,
          # Provinces with mix of Huso 30 and 29
          .data$province_code %in% c(
            "06", "10", "11", "24", "27", "37", "41", "49"
          ) & .data$COORDEX < 5e+05 ~ 30,
          .data$province_code %in% c(
            "06", "10", "11", "24", "27", "37", "41", "49"
          ) & .data$COORDEX > 5e+05 ~ 29,
          .data$province_code %in% c(
            "31", "33", "39"
          ) ~ 30
        ),
        coord_sys = dplyr::case_when(
          .data$province_code %in% c("35", "38") ~ "WGS84",
          .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28",
            "30", "32", "33", "36", "39", "43", "48", "02", "03", "04", "05",
            "06", "09", "10", "11", "12", "13", "14", "16", "18", "19", "21",
            "22", "23", "24", "29", "31", "34", "37", "40", "41", "42", "44",
            "45", "46", "47", "49", "50"
          ) ~ "ED50"
        ),
        crs = dplyr::case_when(
          .data$huso == 30 & .data$coord_sys == "ED50" ~ 23030,
          .data$huso == 31 & .data$coord_sys == "ED50" ~ 23031,
          .data$huso == 29 & .data$coord_sys == "ED50" ~ 23029,
          .data$huso == 30 & .data$coord_sys == "ETRS89" ~ 25830,
          .data$huso == 31 & .data$coord_sys == "ETRS89" ~ 25831,
          .data$huso == 29 & .data$coord_sys == "ETRS89" ~ 25829,
          .data$huso == 28 & .data$coord_sys == "ED50" ~ 23028,
          .data$huso == 28 & .data$coord_sys == "WGS84" ~ 32628,
          TRUE ~ NA_integer_
        )
      ) |>
      dplyr::left_join(
        y = ifn_provinces_dictionary |>
          dplyr::select("province_code", "province_name_original", "ca_name_original"),
        by = "province_code"
      ) |>
      dplyr::select(dplyr::any_of(c(
        "id_unique_code", "country", "ca_name_original", "province_name_original", "province_code",
        "plot", "year", "version", "sheet_ntm", "huso", "COORDEX", "COORDEY", "coord_sys", "crs",
        "slope_mean", "slope", "elev", "aspect" #"coordx_orig", "coordy_orig" # "soils"
      )))

    # return(info_plot)
  }

  if (version %in% c("ifn3", "ifn4")) {
    plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Tipo", "Ano", "Rocosid", "MatOrg",
        "TipSuelo1", "TipSuelo2", "TipSuelo3", "Orienta1", "Orienta2", "MaxPend1", "MaxPend2"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$id_unique_code == !!plot,
        .data$Provincia == as.integer(province)
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(plot_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "plot data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    plot_filtered_data <- plot_filtered_data |>
      dplyr::rename(year = "Ano", plot = "Estadillo", Clase = "Cla", type = "Tipo") |>
      dplyr::mutate(
        year = as.character(.data$year),
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        version = version,
        province_code = as.character(province)
      )

    # we add the id code
    info_plot <- plot_filtered_data |>
      dplyr::rename(aspect = "Orienta1", slope = "MaxPend1") |>
      dplyr::mutate(
        country = "ES",
        # de grados centesimales a sexagesimales??
        aspect = as.numeric(.data$aspect) * 0.9,
        slope = as.numeric(.data$slope),
        slope = dplyr::case_when(
          slope <= 0.6 ~ 1.5,
          slope > 0.6 & slope <= 2.4 ~ 7.5,
          slope > 2.4 & slope <= 4 ~ 16,
          slope > 4 & slope <= 7 ~ 27,
          slope > 7 ~ 40
        ),
        coord_sys = dplyr::case_when(
          .data$version == "ifn4" & .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28",
            "30", "32", "33", "36", "39", "43", "48"
          )  ~ "ED50",
          .data$version == "ifn4" & .data$province_code %in% c("35", "38") ~ "WGS84",
          .data$version == "ifn4" & .data$province_code %in% c(
            "02", "03", "04", "05", "06", "09", "10", "11", "12", "13", "14", "16", "18",
            "19", "21", "22", "23", "24", "29", "31", "34", "37", "40", "41", "42", "44",
            "45", "46", "47", "49", "50"
          ) ~ "ETRS89",
          .data$version == "ifn3" & .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28", "30", "32", "33",
            "36", "39", "43", "48", "02", "03", "04", "05", "06", "09", "10", "11", "12",
            "13", "14", "16", "18", "19", "21", "22", "23", "24", "29", "31", "34", "37",
            "40", "41", "42", "44", "45", "46", "47", "49", "50", "35", "38"
          ) ~ "ED50"
        )
      ) |>
      dplyr::left_join(
        y = ifn_provinces_dictionary |>
          dplyr::select("province_code", "province_name_original", "ca_name_original"),
        by = "province_code"
      ) |>
      # selection of final variables
      dplyr::select(
        "id_unique_code", "country", "ca_name_original", "province_code", "province_name_original",
        "plot", "Clase", "Subclase", "coord_sys", "year", "version", "type", "aspect", "slope"
      )

    files_validation <- assertthat::validate_that(!any(is.na(c(coord_data))))

    coords_filtered_data <- .read_inventory_data(
      coord_data,
      colnames = c(
        "Provincia", "Estadillo", "Clase", "Cla", "Subclase", "Hoja50", "CoorX", "CoorY", "Huso"
      ),
      version = version,
      province = province,
      .dry = TRUE,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$Estadillo == !!plot_estadillo,
        .data$Provincia == as.integer(province)
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(coords_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Coordinates data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    # check for bad formatted or missing coords to inform the user
    if (any(is.na(coords_filtered_data$CoorX), is.na(coords_filtered_data$CoorY))) {
      cli::cli_warn(c(
        "File {.file {plot_data}} has some errors in the coordinates
        (missing coordinates, bad format...).",
        "i" = "These records will be removed from the results"
      ), call = .call)
    }

    # remove bad formatted or missing coordinates
    coords_fixed_data <- coords_filtered_data |>
      dplyr::filter(!is.na(.data$CoorX), !is.na(.data$CoorY))

    coords_data <- coords_fixed_data |>
      dplyr::rename(
        plot = "Estadillo", COORDEX = "CoorX", COORDEY = "CoorY",
        sheet_ntm = "Hoja50", 
        Clase = dplyr::any_of(c("Clase", "Cla"))
      ) |>
      dplyr::mutate(
        sheet_ntm = as.character(.data$sheet_ntm),
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        province_code = province,
        province_code = as.character(.data$province_code),
        version = version
      )

    # Check for existing Huso var and create it if doesn't exists. We do this outside
    # the mutate because the code is wrong when there is more than 1 Huso when coords_data
    # has more than one plot (show_plots_from workflow)
    if (!"HUSO" %in% names(coords_data)) {
      coords_data[["huso"]] <- NA
    } else{coords_data[["huso"]] <- coords_data[["HUSO"]]}

    ## BUG_: coord data now doesn't have unique id, as it has no subclass in table. We need to
    ## join these two tables and we use plot and province code. There is only one
    ## set of coordinates by plot (estadillo), so any plots with the same plot but
    ## different classes have the same coordinates.
    # browser()
    info_plot <- info_plot |>
      dplyr::left_join(
        y = coords_data |>
          dplyr::select(
            dplyr::any_of(c("plot", "province_code", "COORDEX", "COORDEY", "sheet_ntm", "huso"))
          ) |>
          dplyr::distinct(),
        by = c("province_code", "plot")
      ) |>
      # sometimes, plots present in data are not present in coords, so weç
      # remove them
      dplyr::filter(!is.na(.data$COORDEX)) |>
      dplyr::mutate(
        crs = dplyr::case_when(
          # corrections per province
          # provinces ok in 23031
          province_code %in% c(
            "07", "08", "17", "43"
          ) & is.na(.data$huso) & .data$coord_sys == "ED50" ~ 23031,
          # provinces ok in 23029
          province_code %in% c(
            "15", "21", "32", "36"
          ) & is.na(.data$huso) & .data$coord_sys == "ED50" ~ 23029,
          # provinces ok in 23028
          province_code %in% c(
            "35", "38"
          ) & is.na(.data$huso) & .data$coord_sys == "ED50" ~ 23028,
          # Provinces with mix of Huso 30 and 31
          .data$province_code %in% c(
            "03", "12", "22", "25", "44", "50"
          ) & .data$COORDEX < 5e+05 & is.na(.data$huso) &
            .data$coord_sys == "ED50" ~ 23031,
          .data$province_code %in% c(
            "03", "12", "22", "25", "44", "50"
          ) & .data$COORDEX > 5e+05 & is.na(.data$huso) &
            .data$coord_sys == "ED50" ~ 23030,
          # Provinces with mix of Huso 30 and 29
          .data$province_code %in% c(
            "06", "10", "11", "24", "27", "33", "37", "41", "49"
          ) & .data$COORDEX < 5e+05 & is.na(.data$huso) &
            .data$coord_sys == "ED50" ~ 23030,
          .data$province_code %in% c(
            "06", "10", "11", "24", "27", "33", "37", "41", "49"
          ) & .data$COORDEX > 5e+05 & is.na(.data$huso) &
            .data$coord_sys == "ED50" ~ 23029,
          # Province 24, some unidentified plots are in Huso 29 in ifn4
          province_code == "24" & .data$huso == 29 &
            .data$coord_sys == "ETRS89" & .data$COORDEX < 5e+05 ~ 25830,
          is.na(.data$huso) & .data$coord_sys == "ED50" ~ 23030,
          .data$huso == 30 & .data$coord_sys == "ED50" ~ 23030,
          .data$huso == 31 & .data$coord_sys == "ED50" ~ 23031,
          .data$huso == 29 & .data$coord_sys == "ED50" ~ 23029,
          .data$huso == 30 & .data$coord_sys == "ETRS89" ~ 25830,
          .data$huso == 31 & .data$coord_sys == "ETRS89" ~ 25831,
          .data$huso == 29 & .data$coord_sys == "ETRS89" ~ 25829,
          .data$huso == 28 & .data$coord_sys == "ED50" ~ 23028,
          .data$huso == 28 & .data$coord_sys == "WGS84" ~ 32628,
          TRUE ~ NA_integer_
        )
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "id_unique_code", "country", "year", "ca_name_original", 
          "province_code","province_name_original", "plot", "Clase",
          "Subclase", "version", "type","aspect", "slope","crs", 
          "coord_sys", "COORDEX",  "COORDEY", "sheet_ntm", "huso"
        ))
      )

    # return(info_plot)
  }

  # Return plot info
  return(info_plot)
}
