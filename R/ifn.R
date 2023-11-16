

ifn_to_tibble <- function(
    provinces,
    ifn,
    filter_list,
    folder,
    ...,
    .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
    .verbose = TRUE
) {

ESPECIES <-.read_excel_sheet(
  folder, 
  "MaximaActualidad_ATOMaDic2022_dd.xlsx", 
  "ESPECIES"
  ) |>
  dplyr::as_tibble()
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

ifn_tree_table_process <- function(tree_data, plot, province, ESPECIES) {
  
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
  
  
  tree_filtered_data <-  .read_inventory_data(
    tree_data,
    select = dplyr::any_of(c(

      "PROVINCIA",
      "ESTADILLO",
      "ESPECIE",
      "NUMORDEN",
      "ARBOL",
      "DIAMETRO1", 
      "DIAMETRO2",
      "ALTURA"
    ), 
    ignore.case = TRUE),
    colClasses = list(character = c("ESTADILLO", "PROVINCIA")),
    header = TRUE,
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
        dplyr::rename(
          name = "Nombre especie"
        ) |> 
        dplyr::select(
          SP_CODE = SPx,
          SP_NAME = name),
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




ifn_shrub_table_process <- function(shrub_data, plot, province, ESPECIES) {
  
  
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
  
 

  # 2. col names
    # browser()
   
  shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      select = dplyr::any_of(c(
        "PROVINCNA", 
        "ESTADILLO", 
        "ESPECIE",
        "FRACCAB", 
        "ALTUMED"
      ), 
      ignore.case = TRUE),
      #this does not seam to work:
      colClasses = list(character = c("ESTADILLO", "PROVINCIA")),
      header = TRUE,
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
        dplyr::rename(
          name = "Nombre especie"
        ) |> 
        dplyr::select(
          SP_CODE = SPx,
          SP_NAME = name),
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



ifn_regen_table_process <- function(regen_data, plot, province, ESPECIES) {
  
  
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
  
  regen_filtered_data <- .read_inventory_data(
    regen_data,
    select = dplyr::any_of(c(
      "PROVINCIA", 
      "ESTADILLO", 
      "ESPECIE",
      "NUMERO", 
      "ALTUMED", 
      "REGENA"
    ),
    ignore.case = TRUE),
    #this does not seam to work:
    colClasses = list(character = c("ESTADILLO", "PROVINCIA")),
    header = TRUE,
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
      "Data missing for thatplot",
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
        dplyr::rename(
          name = "Nombre especie"
        ) |> 
        dplyr::select(
          SP_CODE = SPx,
          SP_NAME = name),
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

