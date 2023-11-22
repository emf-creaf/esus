#' Raw ifn data to tibble
#'
#' Transform raw IFN plot data into tidy data for use in models
#'
#' This function will take every year indicated and retrieve and transform the plot data for the departments and
#' plots provided. For that, csv files from IFN must reside in the folder indicated in the
#' \code{folder} argument.
#'
#' @param provinces A character vector with the code for the departments.
#' @param version A character vector with the ifn version.
#' @param filter_list A list of provinces and plots to extract the data from.
#' @param folder The path to the folder containing the IFN csv files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ifn_to_tibble} will attempt to process all
#'   plots for the provinces and ifn version provided. This will result in sometimes hundred of thousands
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
#'   to \code{\link[future.callr]{callr}}, will allow \code{ifn_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#'
#' @export
ifn_to_tibble <- function(
    provinces,
    version,
    filter_list,
    folder,
    ...,
    .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
    .verbose = TRUE
) {

  browser()
  ## Assertions and checks ##
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  # departments
  assertthat::assert_that(
    is.character(provinces), length(provinces) > 0,
    msg = cli::cli_abort("provinces must be a character vector with at least one province code")
  )
  ## TODO
  # check all provinces are valid



  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort( "Folder especified ({.path {folder}}) doesn't exists. Please create the folder first and populate it with the needed IFN csv files")
  )

  # # filter_list
  # if (is.null(filter_list)) {
  #   if (interactive()) {
  #     cli::cli_inform(c(
  #       "You haven't specified any plots in the {.arg filter_list} argument.",
  #       "x" = "This will cause to retrieve {.strong ALL} plots  for the selected departments and years",
  #       "!" = "This will use a lot of memory and time, as hundred of thousands plots will potentially be evaluated",
  #       "TODO: add info about how to create the filter list",
  #       ""
  #     ))
  #
  #     user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
  #     if (user_auth == 2L) {
  #       cli::cli_abort("Aborting per user request")
  #     }
  #   }
  # }
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



  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(version)} cicle{?s}")
    ),
    .verbose
  )

  ## send the version in loop to process table function
  purrr::map(
    version,
    .f = \(version) {
      ifn_tables_process(
        provinces, version, filter_list, folder, .parallel_options, .verbose, ...)
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}
 

#' Inner function to process all tables for one year
#'
#' Processing all tables for one year
#'
#' This function is intended to be called internally by \code{\link{ifn_to_tibble}} for each
#' year. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @describeIn ifn_to_tibble
#'


  ifn_tables_process <- function(
    provinces, version, filter_list, folder,
    .parallel_options, .verbose, ...
  ) {

    # debug
      # browser()

    # Create input df for year

    input_df <- .build_ifn_input_with(version, provinces, filter_list, folder, .verbose)

    temp_res <- furrr::future_pmap(
      # temp_res <- purrr::pmap(
      .progress = .verbose,
      .l = input_df,
      .f = \(province, plots, tree_table, plot_table, shrub_table, regen_table) {

      # browser()
      #
      plot_info <- ifn_plot_table_process(plot_table, version, plots, province, ifn_provinces_dictionary)


      tree <- ifn_tree_table_process(tree_table, version, plots, province, ESPECIES)


      shrub <- ifn_shrub_table_process(shrub_table, version, plots, province, ESPECIES)


      regen <- ifn_regen_table_process(regen_table, version, plots, province,ESPECIES)



      # we put together all tables in a data frame


      understory <- plot_info |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          YEAR,
          province_code,
          PLOT)|>

        dplyr::mutate(
          shrub = list(shrub))


      plot_info |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen)
        ) |>

        dplyr::rename(
          COORD1 = COORDEX,
          COORD2 = COORDEY,

        )|>

        dplyr::select(
          ID_UNIQUE_PLOT,
          COUNTRY,
          YEAR,
          ca_name_original,
          province_name_original,
          province_code,
          PLOT,
          YEAR,
          version,
          HOJA,
          COORD_SYS,
          COORD1,
          COORD2,
          crs,
          PENDIEN2,
          SLOPE,
          ELEV,
          ASPECT,
          tree,
          understory,
          regen,
          soils

        )

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
      dplyr::filter(!(is.na(COORD1)  & is.na(COORD2) ))
  }




#' IFN 2 tree table process
#'
#' Processing tree table for IFN
#'
#' This function  ifn_tree_table_process reads and process the tree table for one plot and one IFN
#'
#' @param tree_data file that contains the tree table for that plot
#' @param plot plot_id code
#' @param province province code
#' @param ref_tree_ifn data frame containing the species code reference table
#'
#' @noRd
#'

ifn_tree_table_process <- function(tree_data, version, plot, province, ESPECIES) {

  # browser()

  # Assertions (things we need) and checks/validations
  #
  # 1. file



  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(tree_data)))
    # !any(c(tree_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read

   # browser()

  if (version == "ifn2"){
    
  tree_filtered_data <-  .read_inventory_data(
    tree_data,
    colnames = dplyr::any_of(c(

      "PROVINCIA",
      "ESTADILLO",
      "ESPECIE",
      "NUMORDEN",
      "ARBOL",
      "DIAMETRO1",
      "DIAMETRO2",
      "ALTURA"
    )),
    .ifn = TRUE
  ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()



  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(tree_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that plot",
      "i" = "Returning empty tibble for plot {.var {plot}} "
    ))
    return(dplyr::tibble())
  }
   # browser()

  tree <- tree_filtered_data |>
    # transformations and filters
    dplyr::rename(
      province_code = PROVINCIA,
      PLOT = ESTADILLO,
      SP_CODE = ESPECIE,
      TREE = ARBOL,
      Dn1 = DIAMETRO1,
      Dn2 = DIAMETRO2,
      HT = ALTURA) |>
    dplyr::mutate(
      PLOT = as.character(PLOT),
      province_code = as.character(province_code),
      Dn1 = as.numeric(Dn1),
      Dn2 = as.numeric(Dn2),
      HT = as.numeric(HT),
      SP_CODE = as.numeric(SP_CODE),
      ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep="_"),
      # From mm to cm
      DIA = ((Dn1 + Dn2)/2)*0.1,
      # From m to cm
      HT = HT*100,
      DENSITY = dplyr::case_when(
        DIA < 12.5 ~ 127.3239546,
        DIA >= 12.5 & DIA < 22.5 ~ 31.83098865,
        DIA >= 22.5 & DIA < 42.5 ~ 14.14710607,
        DIA >= 42.5 ~ 5.092958185
      ))  |>

    # add species info ---> WHAT REFERENCE SHOULD I USEE???
    dplyr::left_join(
      y = ESPECIES |>
        dplyr::select(
          SP_CODE ,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>

    dplyr::select(
        ID_UNIQUE_PLOT,
        province_code,
        PLOT,
        SP_CODE,
        SP_NAME,
        #diameter in cm
        DIA,
        #height in m
        HT,
        DENSITY

    )




  # Return tree
  return(tree)
  }
  
  if (version == "ifn3"){
    
    
    tree_filtered_data <-  .read_inventory_data(
      tree_data,
      colnames = dplyr::any_of(c(
        
        "PROVINCIA",
        "ESTADILLO",
        "ESPECIE",
        "NUMORDEN",
        "ARBOL",
        "DIAMETRO1",
        "DIAMETRO2",
        "ALTURA"
      )),
      .ifn = TRUE
    ) |>
      dplyr::filter(
        ESTADILLO == plot
      ) |>
      tibble::as_tibble()
    
    
    
    # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Data missing for that plot",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ))
      return(dplyr::tibble())
    }
    # browser()
    
    
    
    
    tree <- tree_filtered_data |>
      # units transformations
      dplyr::mutate(
        Provincia_codigo = Provincia,
        # unique inner code
        ID_UNIQUE_PLOT = paste("ES",Provincia_codigo, Estadillo, sep="_"),
        DIA = (Dn1 + Dn2)/2,
        
        # MM TO CM
        DIA = DIA * 0.1,
        SP_CODE = as.numeric(Especie),
        
        # density represented by each tree considering  plot design (variable radious)
        DENSITY = case_when(
          DIA < 12.5 ~ 127.3239546,
          DIA >= 12.5 & DIA < 22.5 ~ 31.83098865,
          DIA >= 22.5 & DIA < 42.5 ~ 14.14710607,
          DIA >= 42.5 ~ 5.092958185
        )) |>
      
      
      #different for ifn3 and 4 ??? 
      
      # add species info
      dplyr::left_join(
        y = ref_tree_ifn|>
          dplyr::select(
            SP_CODE = IFNCODE,
            SP_NAME = IFNNAME), 
        by = "SP_CODE"
      ) |>
      
      
      dplyr::arrange(SP_CODE) |>
      
      dplyr::rename(
        PLOT = Estadillo,
        HT = Ht
        
        
      )|>
      
      
      dplyr::select(
        any_of(c(
          "ID_UNIQUE_PLOT", 
          "Provincia_codigo",
          "Clase",
          "Subclase",
          "IDPARCELA",
          "IDCLASE",
          "PLOT",
          "SP_CODE",
          "SP_NAME",
          #tree number id in ifn4
          "nArbol",
          #CUALIDAD 6 = dead but providing functions
          "Calidad",
          "Forma",
          #check codes to understand origin and trace of individuals
          "OrdenIf2",
          "OrdenIf3",
          "OrdenIf4",
          #diameter in cm
          "DIA",
          #height in m
          "HT", 
          "DENSITY"
        ))
      )
    
    # Return tree
    return(tree)
    
  }
}




ifn_shrub_table_process <- function(shrub_data, version, plot, province, ESPECIES) {


  # browser()

  # Assertions (things we need) and checks/validations
  #
  # 1. file


  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(shrub_data)))
    # !any(c(shrub_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping shrub data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }
  
  if (version =="ifn2"){
    



  # 2. col names
    # browser()

  shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = dplyr::any_of(c(
        "PROVINCIA",
        "ESTADILLO",
        "ESPECIE",
        "FRACCAB",
        "ALTUMED"
      )),
      .ifn = TRUE
    ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()

  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(shrub_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that plot",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }


  #we add the id code
  shrub <- shrub_filtered_data |>

    dplyr::mutate(
      PLOT = as.character(ESTADILLO),
      province_code = as.character(PROVINCIA),
      PLOT = ESTADILLO,
      COVER = FRACCAB,
      Hm = as.numeric(ALTUMED),
      #DM TO M
      Hm = Hm * 0.1 ,
      SP_CODE = as.numeric(ESPECIE),
      ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep = "_")

    ) |>


    # 3. ref_plant_dictionary
    #we join data from plant ref dictionary
    # some symbols apply for multiple species


    dplyr::left_join(
      y =  ESPECIES |>
        dplyr::select(
          SP_CODE ,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      SP_NAME,
      SP_CODE,
      Hm,
      COVER
    )


  #
  # Return shrub
  return(shrub)
  }
}



ifn_regen_table_process <- function(regen_data, version, plot, province, ESPECIES) {


  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(regen_data)))
    # !any(c(regen_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  if (version =="ifn2"){
    
  regen_filtered_data <- .read_inventory_data(
    regen_data,
    colnames = dplyr::any_of(c(
      "PROVINCIA",
      "ESTADILLO",
      "ESPECIE",
      "NUMERO",
      "ALTUMED",
      "REGENA"
    )),
    .ifn = TRUE
  ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()


  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(regen_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that plot",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }



  #we add the id code
  regeneration <- regen_filtered_data |>

    dplyr::mutate(
      PLOT = as.character(ESTADILLO),
      province_code = as.character(PROVINCIA),

      #DM TO M ?
      Hm = as.numeric(ALTUMED) * 0.1,
      SP_CODE = as.numeric(ESPECIE),
      ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep = "_")) |>

    # add species info ---> WHAT REFERENCE SHOULD I USEE???
    dplyr::left_join(
      y = ESPECIES |>

        dplyr::select(
          SP_CODE,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>

    #selection of final variables

    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      #codigo
      SP_CODE,
      #nombre en latin
      SP_NAME,
      NUMERO,
      Hm,
      REGENA
    )

  # Return regen
  return(regeneration)

  }
}



ifn_plot_table_process <- function(plot_data, version, plot, province, ifn_provinces_dictionary){




  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(plot_data)))
    # !any(c(plot_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  # 2. col names
  # browser()
  if (version =="ifn2"){
    

  plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = dplyr::any_of(c(
        "PROVINCIA",
        "ESTADILLO",
        "HOJA",
        "ANO",
        "COORDEX",
        "COORDEY",
        "ALTITUD1",
        "ALTITUD2",
        "PENDIEN1",
        "PENDIEN2",
        "FRACCION1",
        "FRACCION2",
        "CLASUELO",
        "ESPESOR",
        "CLACOBER",
        "CUBIERTA",
        "ORIENTA1",
        "ORIENTA2",
        "MAXPEND1",
        "MAXPEND2"
      )),
      .ifn = TRUE
    ) |>
    dplyr::filter(
      ESTADILLO == plot,
      PROVINCIA == province
    ) |>
    tibble::as_tibble()



  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(plot_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Data missing for that plot",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }


  plot_filtered_data <- plot_filtered_data |>
    dplyr:::mutate(COORDEX = ifelse(grepl("[A-Za-z]", COORDEX), NA, COORDEX),
                   COORDEY = ifelse(grepl("[A-Za-z]", COORDEY), NA, COORDEY))
    {
      if (any(is.na(plot_filtered_data$COORDEY) | is.na(plot_filtered_data$COORDEY))) {
        cli::cli_warn(" File {.file {plot_data}} has   some errors in the coordinates (leters).These records had been substituted by NA")
      }


    }



  #we add the id code
  info_plot <- plot_filtered_data |>

    dplyr::rename(
      PLOT = ESTADILLO,
      SLOPE = MAXPEND2,
      ELEV = ALTITUD2,
      YEAR = ANO,
      ASPECT = ORIENTA2) |>

    dplyr::mutate(

      COUNTRY = "ES",
      province_code = as.character(PROVINCIA),
      ID_UNIQUE_PLOT = paste("ES", province_code,PLOT,sep = "_"),
      # ALTITUD1 = as.numeric(ALTITUD1),
      ELEV = as.numeric(ELEV),
      # ALTITUD1 = ALTITUD1*100,
      ELEV = ELEV*100,
      COORDEX = as.numeric(COORDEX),
      COORDEY = as.numeric(COORDEY),
      COORDEX = 1000 * COORDEX,
      COORDEY = 1000 * COORDEY,
      version = "ifn2",
      Huso = dplyr::case_when(
        province_code %in% c(35, 38) ~ 28,
        province_code %in% c(1, 7, 8, 15,17,20, 25, 26,27,28,30,32,33,36,39,43,48,2,3,4,5,6,9,10,11,12,13,14,16,18,19,21,22,23,24,29,31,
                                34,37,40,41,42,44,45,46,47,49,50) ~ 30
      ),
      COORD_SYS = dplyr::case_when(
        province_code %in% c(35, 38) ~ "WGS84",
        province_code %in% c(1, 7, 8, 15,17,20, 25, 26,27,28,30,32,33,36,39,43,48,2,3,4,5,6,9,10,11,12,13,14,16,18,19,21,22,23,24,29,31,
                                34,37,40,41,42,44,45,46,47,49,50) ~ "ED50")
    ) |>

    dplyr::left_join(
      y = ifn_provinces_dictionary |>
        dplyr::select(
          province_code = province_code,
          province_name_original = province_name_original,
          ca_name_original = ca_name_original
        ),
      by = "province_code")



  soil_info <- info_plot |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      YEAR) |>
    dplyr::mutate(
      soil_field = list(info_plot |>
                          dplyr::select(
                            ID_UNIQUE_PLOT,
                            province_code,
                            PLOT,
                            CLASUELO,
                            ESPESOR,
                            CLACOBER,
                            CUBIERTA))
    ) |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      YEAR,
      soil_field

    ) |>
    tibble::tibble()




  info_plot <- info_plot |>
    dplyr::mutate(
      soils = list(soil_info),

      #linea provisional
      # crs = get_crs(info_plot$Huso, info_plot$COORD_SYS)) |>

      crs = dplyr::case_when(
        Huso == 30 & COORD_SYS == "ED50" ~ 23030,
        Huso == 31 & COORD_SYS == "ED50" ~ 4326,
        Huso == 29 & COORD_SYS == "ED50" ~ 23029,
        Huso == 30 & COORD_SYS == "ETRS89" ~ 25830,
        Huso == 31 & COORD_SYS == "ETRS89" ~ 25831,
        Huso == 29 & COORD_SYS == "ETRS89" ~ 25829,
        Huso == 28 & COORD_SYS == "ED50" ~ 23028,
        Huso == 28 & COORD_SYS == "WGS84" ~ 32628,
        TRUE ~ NA_integer_
      )
      ) |>

    dplyr::select(any_of(c(

      "ID_UNIQUE_PLOT",
      "COUNTRY",
      "ca_name_original",
      "province_name_original",
      "province_code",
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
    ))

    )
  return(info_plot)
  }


  # Return plot with soil
 
}


