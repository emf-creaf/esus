# on load function, to check for grep dependency
.onLoad <- function(libname, pkgname) {

  # Check if grep is installed
  grep_check <- .sys_cmd_warning()

  # check in unix if mdbtools is installed
  if (.Platform$OS.type == "unix") {
    mdbtools_check <- .sys_cmd_warning(
      "mdb-tables", c(
        "x" = "{.emph mdbtools} system utility not found.",
        "i" = "{.emph mdbtools} is needed to read Spanish inventory (IFN) data.",
        "i" = "Please check your package manager (apt, brew, port...) to install it.",
        "i" = "More info at https://github.com/mdbtools/mdbtools"
      )
    )
  }

  invisible()
}
