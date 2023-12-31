#' Build the input dataframe to iterate by plots for the year
#'
#' Build the input dataframe
#'
#' This function takes the user input (year, departments, plots and folder) and build the input to be
#' able to iterate by plots in a year. If no plots filter list is provided, this function uses
#' \code{\link{.get_plots_from_department}} and \code{\link{.trasnsform_plot_summary}} to create a
#' \code{filter_list} with all plots for each state for that year.
#'
#' @inheritParams ffi_tables_process
#'
#' @return A data frame with  departments, plot and table file names
#'
#' @noRd
.build_ffi_input_with <- function(
    departments, year, filter_list, folder, .verbose, .call = rlang::caller_env()
) {

  # # first, if is null filter list, create it

   # browser()

  if (is.null(filter_list)) {
      filter_list <-  .get_plots_from_department(departments, folder , .call = .call) |>
          .transform_plot_summary_ffi(year, departments)
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve {.strong {filter_list |>  length()}} plots for {.val {year}}"
    )), .verbose
  )

# browser()
  dep_list <- filter_list


  dep_list |>
        tibble::enframe() |>
        tidyr::unnest(cols = value) |>
        purrr::set_names(c("department", "plots")) |>
      dplyr::mutate(
        plots = as.character(plots)
      ) |>
        dplyr::select(department, plots) |>

        dplyr::mutate(
          plot_table = .build_ffi_file_path(
             department,
             "plot",
             folder,
            .plot = plots,
            .year = year,
            .custom = TRUE,
            .call = .call
          ),
          tree_table = .build_ffi_file_path(
            department,
             "tree", folder,
            .plot = plots,
            .year = year,
            .custom = TRUE,
            .call = .call
          ),
          shrub_table = .build_ffi_file_path(
            department,
            "shrub",
            folder,
            .plot = plots,
            .year = year,
            .custom = TRUE,
            .call = .call
          ),
          soils_table = .build_ffi_file_path(
            department,
            "soils",
            folder,
            .plot = plots,
            .year = year,
            .custom = TRUE,
            .call = .call
          ),
          regen_table = .build_ffi_file_path(
            department,
            "regen",
            folder,
            .plot = plots,
            .year = year,
            .custom = TRUE,
            .call = .call
          )
        )


}




#' Helper to read the PLACETTE.csv file from an state to retrieve the list of plots for that state
#' @noRd

.get_plots_from_department <- function(department, folder, .call = rlang::caller_env()) {

  # browser()
  ## TODO Assertion to ensure PLACETTE.csv file exists, because .build_fia_file_path is fail
  ## resistant, returning always a result (NA_character) to allow its use in loops.
  ## .get_plots_from_department_ is only called from .build_ffi_input_with or show_plots_from_ffia,
  ## that can not check for file existence (this is done in the individual plot functions)

  plot_path <- .build_ffi_file_path(department, "plot", folder)

  if (is.na(plot_path)) {
    cli::cli_abort(c(
      "{.path {folder}} folder doesn't contain any file named {.path PLACETTE.csv}, aborting."
    ), call = .call)
  }

  # If file exists, business as usual:
  plot_data <- plot_path |>
    .read_inventory_data(select = c("CAMPAGNE", "IDP", "XL", "YL", "DEP")) |>
    dplyr::group_by(DEP, IDP) |>
    dplyr::filter(DEP %in% department) |>
    #IN THE CASE THAT THERE ARE NA
    dplyr::filter(!all(is.na(XL))) |>
    dplyr::arrange(CAMPAGNE) |>
    tidyr::fill(
      c(XL, YL), .direction = "updown"
    ) |>
    dplyr::as_tibble()

  if (nrow(plot_data) < 1) {
    cli::cli_abort(c(
      "{.path PLACETTE.csv} file doesn't contain any plot for {.val {department}} department, aborting."
    ), call = .call)
  }

  # crs to build the sf and transform to 4326 to have all in the same coordinate system.
  epgs <- 2154
  res <- plot_data |>
    sf::st_as_sf(
      coords = c("XL", "YL"),
      crs = sf::st_crs(epgs)
    ) |>
    sf::st_transform(crs = 4326)

  return(res)
}

#' show plots from department ffi helper
#'
#' Retrieve all the plots for selected departments
#'
#' Opposite to what happens in \code{\link{show_plots_from_fia}}, here we don't need to
#' iterate by the departments, as all the plots are in one file and
#' \code{\link{.get_plots_from_department}} already works with multiple departments
#'
#' @param folder Character, path to folder containing FFI csv files
#' @param departments Character vector with numeric department code
#' @noRd
show_plots_from_ffi <- function(folder, departments, .call = rlang::caller_env()) {
  # withCallingHandlers(
  #   purrr::map(departments, .f = .get_plots_from_department, folder = folder) |>
  #     purrr::list_rbind() |>
  #     sf::st_as_sf(),
  #   purrr_error_indexed = function(err) {
  #     rlang::cnd_signal(err$parent)
  #   }
  # )
  .get_plots_from_department(departments, folder)
}

#' Helper to transform the plot summary returned by \code{\link{.get_plots_from_department}} in a
#' filter_list object
#' @noRd
.transform_plot_summary_ffi <- function(plot_summary, years, departments) {

  filter_list <- plot_summary |>
    dplyr::as_tibble() |>
    dplyr::filter(
      CAMPAGNE %in% years,
      DEP %in% departments
    ) |>
    dplyr::select(DEP, IDP) |>
    dplyr::distinct() |>
    dplyr::group_by(DEP) |>
    dplyr::mutate(
      IDP = as.character(IDP)
    ) |>
    dplyr::summarise(plots = list(IDP)) |>
    dplyr::group_by(DEP) |>
    dplyr::group_map(.f = \(department_plots, department_name) {
      tibble::deframe(department_plots) |>
        purrr::set_names(department_name[[1]])
    }) |>
    purrr::flatten()

  return(filter_list)
}

#' Create the \code{filter_list} for FFI inventory
#'
create_filter_list_ffi <- function(plots_info) {

  ## assertions
  # this process is independent from ffi, and the user can modify plots_info to
  # filter plots and counties. So we can not assume plots_info is going to have the str we
  # need. So, we assert and inform the user if something is wrong

  ## TODO
  # assert class
  assertthat::assert_that(
    inherits(plots_info, c("tbl", "sf", "data.frame")),
    msg = cli::cli_abort(c(
      "{.arg plots_info} must be a data.frame or something coercible to one, as the result of {.code show_plots_from_ffi()}"
    ))
  )
  # assert col names
  assertthat::assert_that(
    all(names(plots_info) %in% c("CAMPAGNE", "IDP", "DEP", "geometry")),
    msg = cli::cli_abort(c(
      "{.arg plots_info} provided don't have the expected names",
      "i" = "Expected names are {.value {c('CAMPAGNE', 'IDP', 'DEP', 'geometry')}}"
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
  plots_years <- plots_info[["CAMPAGNE"]] |>
    unique()
  departments_names <- plots_info[["DEP"]] |>
    unique() |>
    as.character() |>
    sort()

  res <- plots_info |>
    dplyr::group_by(DEP) |>
    dplyr::group_split() |>
    purrr::set_names(departments_names) |>
    purrr::imap(
      .f = \(department_data, department_name) {
        .transform_plot_summary_ffi(department_data, plots_years, department_name)
      }
    ) |>
    purrr::flatten()

  return(res)
}

#' Create the path and system call for reading FFI csv's
#'
#' Create FFI csv file path with extra sugar
#'
#' This function builds the path to FFI table csv files based on the type of table.
#' Also, using the type, we add the system call to \code{grep} in those tables which it can
#' be used to avoid loading the whole table.
#'
#' @section \code{grep} system call:
#' \code{grep} system library allows to find patterns in text files. This can be used prior
#' to read the file to feed \code{fread} only with the rows we need. For this we build a
#' regular expression that matches the county and plot code, as well as year in the case of
#' some tables. This way we avoid loading the whole table and only the rows we need.
#' In this case, the regular expression used is:
#' \preformatted{
#' ',INVYR,|,{.year},.*,{county},({plot}|{plot}.0),'
#' }
#' \code{",INVYR,"} matches the first row in all tables, because all tables have the Inventory
#' year variable.
#' \code{"|"} means \code{OR} as in R code. This way we match the first row with the part before
#' "|", \emph{OR} the rows with the data as per the part after "|".
#' \code{,{.year},.*,{county},({plot}|{plot}.0)} part matches any row with the values for
#' year, county and plot in an specific order. First year between commas, after that an
#' unspecified number of characters (\code{".*"}), and county and plot together between
#' commas and separated by a comma.
#' \code{({plot}|{plot}.0)} indicates to match both plot code or plot code with a 0 decimal
#' because some states have this variable as a double value.
#'
#' @param type Character, table type. One of "tree", "plot", "soils",  "shrub" or "regen"
#' @param folder Character, path to the folder with the FFI csv files.
#' @param .custom Logical indicating that a custom path, with \code{grep} must be created
#' @param .county,.plot, Vectors of the same length as \code{state}, with county and plot codes
#'   to build the \code{grep} command if \code{.custom} is \code{TRUE}.
#' @param .year Numeric value (length one) with the year to build the \code{grep} command
#'   if \code{.custom} is \code{TRUE}.
#'
#' @return Character vector with the paths (or custom command with path) to use with
#'   \code{\link{.read_inventory_data}}.
#'
#' @noRd
.build_ffi_file_path <- function(
    departments,
    type,
    folder = ".",
    .plot = rep(NA, length(departments)),
    .year = NULL,
    .custom = FALSE,
    .call = rlang::caller_env()
)
{
  #
  # browser()

  ending <- switch(
    type,
    "tree" = "ARBRE.csv",
    "plot" = "PLACETTE.csv",
    "shrub" = "FLORE.csv",
    "soils" = "ECOLOGIE.csv",
    "regen" = "COUVERT.csv"
  )

  # return path
  table_path <- fs::path(folder, glue::glue("{ending}"))

  # check file exists
  if (!fs::file_exists(table_path)) {
    cli::cli_warn(c(
      "{.path {table_path}} file doesn't exist",
      "!" = "Please check if {.path {folder}} is the correct path",
      "i" = "Skipping {.path {table_path}}"
    ), call = .call)
    return(NA_character_)
  }

  # browser()


  if (.custom) {
    # if (type %in% c("plot")) {
    #   customized_path <- glue::glue(
    #     'grep -E "CAMPAGNE|^{.year};.*;{.plot};" {table_path}'
    #     # "grep -E ';{.year};.*;{plots};' {table_path}"
    #   )
    # }

    if (type %in% c("tree", "shrub", "soils", "regen")) {
      customized_path <- glue::glue(
        'grep -P "CAMPAGNE|(^(?:[^;]+;){{1}}){.plot};" {table_path}'
      )
    } else{
      customized_path <- glue::glue(
        'grep -P "CAMPAGNE|(^(?:[^;]+;){{2}}){.plot};((?:[^;]+;){{2}}){departments}" {table_path}'
      )
    }
    return(customized_path)
  }



  return(table_path)
}




#' Helper function to extract plot and soil metadata from from tables
#'
#' Extract year and most recent metadata for plot
#'
#' This function extracts the metadata for a plot in a year, creating two values for
#' each specified variable in \code{vars}. One with the value for the year selected
#' (\code{VAR_ORIG}) and another with the most recent value (\code{VAR}).
#' This function is intended exclusively to be called by the individual table process
#' functions that needs this functionality (plot and soil tables).
#'
#' @param data_processed Table data after reading and processing.
#' @param vars Character, names of variables to be extracted.
#' @param plot Numeric, codes for dep or plot to be processed
#' @param year Numeric with the year to process
#' @param .soil_mode Logical. If \code{TRUE}, \code{.extract_ffi_metadata} is run on soil mode,
#'   which means that no NAs are filtered before returning most recent data, to allow for
#'   different layers to be retrieved. If \code{FALSE}, then years with \code{NA} for \code{VAR}
#'   are removed prior to find the most recent value.
#'
#' @return A data frame with variables in \code{var} values for the desired year and for the
#'   most recent year with value.
#'
#' @importFrom rlang `:=`
#' @noRd
.extract_ffi_metadata <- function(data_processed, vars,  plot, year, .soil_mode = TRUE) {

  # ORIGINAL names
  vars_orig <- paste0(vars, "_ORIGINAL")

  data_processed <- dtplyr::lazy_dt(data_processed, immutable = TRUE)

  # we need the filtering vars in case they are missing (some tables dont have them)
  if (!("PLOT" %in% data_processed$vars)) {
    data_processed <- data_processed |>
      dplyr::mutate(
        PLOT = plot
      )
  }

  # loop among vars
  purrr::map2(
    .x = vars,
    .y = vars_orig,
    .f = \(var, var_orig) {


      # browser()
      filter_nas <- TRUE
      if (!.soil_mode) {
        filter_nas <- rlang::expr(!is.na(!!rlang::sym(var)))
      }

      # value at most recent year
      var_value <- data_processed |>
        dplyr::filter(
          PLOT == plot,
          !!filter_nas
        ) |>
        dplyr::filter(YEAR == max(YEAR, na.rm = TRUE)) |>
        dplyr::pull(var)

      # value at queried year
      var_orig_value <- data_processed |>
        dplyr::filter(
          PLOT == plot,
          YEAR == year
        ) |>
        dplyr::pull(var)

      # NA if data is not found
      if (length(var_orig_value) < 1) {
        var_orig_value <- NA
      }
      if (length(var_value) < 1) {
        var_value <- NA
      }

      # build the tibble
      dplyr::tibble(
        !!var := var_value,
        !!var_orig := var_orig_value
      )
    }
  ) |>
    purrr::list_cbind() |>
    data.table::as.data.table() |>
    dtplyr::lazy_dt(immutable = TRUE)
}

