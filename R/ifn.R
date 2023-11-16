

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



ifn_plot_table_process <- function(plot_data,  plot, province){
  
  
  
  
  get_crs <- function(Huso,COORD_SYS){
    
    if (Huso == 30 & COORD_SYS == "ED50"){ 
      crs = 23030
    }
    
    if (Huso == 31 & COORD_SYS == "ED50"){
      crs =  4326
    }
    
    if (Huso == 29 & COORD_SYS == "ED50" ){
      crs = 23029
    }
    
    if (Huso == 30 & COORD_SYS == "ETRS89"){
      crs = 25830
    }
    
    if (Huso == 31 & COORD_SYS == "ETRS89" ){
      crs = 25831
    }
    
    if (Huso == 29 & COORD_SYS == "ETRS89"){
      crs = 25829
    }
    
    if (Huso == 28 & COORD_SYS == "ED50"){
      crs = 23028
    }
    
    if (Huso == 28 & COORD_SYS == "WGS84"){
      crs =32628
    }
    
    return(crs)
    
  }
  
  
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
  
  
  plot_filtered_data <- read_inventory_data(
      plot_data,
      select = dplyr::any_of(c(
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
      ),
      ignore.case = TRUE),
      #this does not seam to work:
      colClasses = list(character = c("ESTADILLO", "PROVINCIA")),
      header = TRUE,
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
  
  ###########
  
  
  plot_filtered_data <- plot_filtered_data |>
    dplyr:::mutate(COORDEX = ifelse(grepl("[A-Za-z]", COORDEX), NA, COORDEX),
                   COORDEY = ifelse(grepl("[A-Za-z]", COORDEY), NA, COORDEY)) %>%
    {
      if (any(is.na(.$COORDEY) | is.na(.$COORDEY))) {
        cli::cli_warn(" File {.file {plot_data}} has   some errors in the coordinates (leters).These records had been substituted by NA")
      }
      .
    }
  
  ############## 
  
  
  #we add the id code   
  info_plot <- plot_filtered_data |>
    
    
    dplyr::mutate(
      PLOT = ESTADILLO,
      YEAR = ANO,
      COUNTRY = "ES",
      province_code = as.numeric(PROVINCIA),
      ID_UNIQUE_PLOT = paste("ES", province_code,PLOT,sep = "_"),
      ALTITUD1 = as.numeric(ALTITUD1),
      ALTITUD2 = as.numeric(ALTITUD2),
      ALTITUD1 = ALTITUD1*100,
      ALTITUD2 = ALTITUD2*100,
      COORDEX = as.numeric(COORDEX),
      COORDEY = as.numeric(COORDEY),
      COORDEX = 1000 * COORDEX,
      COORDEY = 1000 * COORDEY,
      ciclo = "IFN2",
      Huso = case_when(
        province_code %in% c(35, 38) ~ 28,
        province_code %in% c(1, 7, 8, 15,17,20, 25, 26,27,28,30,32,33,36,39,43,48,2,3,4,5,6,9,10,11,12,13,14,16,18,19,21,22,23,24,29,31,
                                34,37,40,41,42,44,45,46,47,49,50) ~ 30
      ),
      COORD_SYS = case_when(
        province_code %in% c(35, 38) ~ "WGS84",
        province_code %in% c(1, 7, 8, 15,17,20, 25, 26,27,28,30,32,33,36,39,43,48,2,3,4,5,6,9,10,11,12,13,14,16,18,19,21,22,23,24,29,31,
                                34,37,40,41,42,44,45,46,47,49,50) ~ "ED50") 
    ) |>
    
    dplyr::left_join(
      y = ProvinciasCCAA |>
        dplyr::select(
          province_code = CODIGO ,
          Provincia_nombre = Provincia,
          Comunidad_nombre = CCAA
        ), 
      by = "province_code") |>
    dplyr::rename(
      PLOT = ESTADILLO,
      SLOPE = MAXPEND2,
      ELEV = ALTITUD2,
      ASPECT = ORIENTA2)
  
  
  
  soil_info <- info_plot |>
    
    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      IDPARCELA,
      YEAR) |>
    dplyr::mutate(
      soil_field = list(info_plot |>
                          dplyr::select( 
                            ID_UNIQUE_PLOT,
                            province_code,
                            PLOT,
                            IDPARCELA,
                            CLASUELO,
                            ESPESOR,
                            CLACOBER,
                            CUBIERTA))
    ) |>
    
    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      IDPARCELA,
      YEAR,
      soil_field
      
    ) |>
    tibble::tibble()
  
  
  
  
  info_plot <- info_plot |>
    dplyr::mutate(
      soils = list(soil_info),
      
      #linea provisional
      crs = get_crs(info_plot$Huso, info_plot$COORD_SYS)) |>
    
    
    dplyr::select(
      
      ID_UNIQUE_PLOT,
      COUNTRY,
      Comunidad_nombre,
      Provincia_nombre,
      province_code,
      PLOT,
      IDPARCELA,
      YEAR,
      ciclo,
      HOJA,
      Huso,
      COORDEX,
      COORDEY,
      COORD_SYS,
      crs,
      PENDIEN2,
      SLOPE,
      ELEV,
      ASPECT,
      soils
      
    )
  
  
  # Return plot with soil
  return(info_plot)
}


