#' Util to control messaging
#'
#' @param msg Usually a \code{\link[cli]{cli_inform}} call
#'
#' @noRd
verbose_msg <- function(msg, .verbose) {

  # assertions
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  # show message if needed
  if (.verbose) {
    msg
  }
}

#' Show plots for any inventory
#'
#' Show plots with minimal metadata from any inventory
#'
#' This function show the plots available in any inventory. Take into account that this can
#' potentially show all plots in any inventory, so the object returned can be big.
#'
#' @param inventory Character indicating the inventory. Allowed values are \code{"FIA"} for the
#'   USA forest inventory, \code{"FFI"} for the French \emph{Inventaire Forestier} and
#'   \code{"IFN"}, for the Spanish \emph{Inventario Forestal Nacional}.
#'
#' @param folder Character, path to the folder containing the \code{inventory} files.
#' @param ... Other arguments, depending on the \code{inventory}, see details.
#'
#' @section FIA:
#' FIA inventory needs an extra argument, \code{states}, a character vector with the two-letter
#' code for the desired states.
#'
#' @section FFI:
#' TODO
#'
#' @section IFN:
#' TODO
#'
#' @return A \code{\link[sf]{sf}} spatial object in which each row is a plot,. The metadata provided
#'   varies depending on the inventory, but usually includes the state (FIA) / department (FFI)/
#'   provincia (IFN) and year/date/IFN version
#'
#' @examples
#' library(esus)
#'
#' show_plots_from("FIA", folder = ".", states = "OR")
#' # TODO
#'
#'
#' @family Inventory Utils
#' @export
show_plots_from <- function(inventory = c("FIA", "FFI", "IFN"), folder = ".", ...) {

  # General Assertions
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  inventory <- match.arg(inventory)
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      cli::cli_abort(c(
        "Folder especified ({.path {folder}}) doesn't exist.",
        "i" = "Please create the folder first and populate it with the needed {inventory} files"
      ))
    )
  )

  # Switch to each inventory functions
  show_plots_function <- switch(
    inventory,
    "FIA" = show_plots_from_fia,
    "FFI" = show_plots_from_ffi,
    "IFN" = NULL
  )

  show_plots_function(folder, ...)
}

#' Function to read inventory files
#'
#' Read inventory csv file
#'
#' This function uses internally \code{\link[data.table]{fread}} to read the csv files. This way
#' we can leverage the options of \code{fread} to execute \code{grep} system tool to prefilter the
#' rows and others.
#'
#' @param input character vector as provided by \code{\link{.build_fia_file_path}},
#' \code{\link{.build_ffi_file_path}}. See there for details about the \code{grep} usage.
#' @param ... optional arguments for \code{\link[data.table]{fread}}. Most usually fo providing
#'   a list of columns to read with the \code{select} argument.
#' @param .ifn logical value (default \code{FALSE}), indicating if the inventory read is the IFN.
#'   This is needed because the IFN is in DB formats, not csv formats.
#'
#' @return A \code{\link[dtplyr]{lazy_dt}} object, with immutable set to TRUE (to avoid shenanigans
#'   with caching if used)
#' @noRd
.read_inventory_data <- function(input, ..., .ifn = FALSE) {

  # check if we are reading IFN data
  if (isTRUE(.ifn)) {
    res <- .read_ifn_data(input, ...) |>
      dtplyr::lazy_dt(immutable = TRUE)

    return(res)
  }

  # check if special input is provided
  if (stringr::str_detect(input, "^grep")) {
    res <- data.table::fread(cmd = input, ...) |>
      # convert to tibble
      dtplyr::lazy_dt(immutable = TRUE)
    return(res)
  }

  # read the data
  res <- data.table::fread(file = input, ...) |>
    # convert to tibble
    dtplyr::lazy_dt(immutable = TRUE)

  return(res)
}


.sys_cmd_warning <- function(
  cmd = "grep",
  warn_vector = c(
    "x" = "{.emph {cmd}} system utility not found.",
    "i" = "In Windows, {.emph {cmd}} is part of the {.emph RTools} suite, that can be installed from the CRAN page: {.url https://cran.r-project.org/bin/windows/Rtools/}",
    "i" = "Mac and Linux users should have {.emph {cmd}} already installed, but if it is missing, please check your package manager to install it."
  )
) {
  grep_path <- Sys.which(cmd)
  if (stringr::str_length(grep_path) < 4) {
    cli::cli_warn(warn_vector)
    return(invisible(FALSE))
  }

  # cli::cli_inform(c("#####DEBUG#####", "v" = "{.emph {cmd}} found in {.envvar PATH}"))
  return(invisible(TRUE))
}


###

.read_excel_sheet <- function(folder,name_excel_ext, sheet_name) {
  file <- readxl::read_excel(
    fs::path(folder, name_excel_ext), sheet = sheet_name)
  
  # Devuelve los datos
  return(file)
}
