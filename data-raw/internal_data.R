
# FIA states dict -----------------------------------------------------------------------------

fia_states_dictionary <- FIESTA::ref_statecd

# IFN provinces dictionary --------------------------------------------------------------------

ifn_provinces_dictionary <- readxl::read_xls(
  "data-raw/090471228013528a_tcm30-278472.xls", sheet = "NUTS",
  skip = 1
) |>
  dplyr::select(
    province_code = `CÓDIGO PROVINCIA INE`,
    province_name_original = `NOMBRE PROVINCIA`,
    ca_name_original = `NOMBRE COMUNIDAD AUTÓNOMA`
  ) |>
  tidyr::fill(ca_name_original, .direction = "down") |>
  dplyr::mutate(
    # add zeros to province code
    province_code = as.character(province_code) |> stringr::str_pad(2, 'left', "0"),
    # for some reason castilla y león aparece mal en el archivo original
    ca_name_original = dplyr::if_else(
      ca_name_original == "Datos espaciales (IFNXX_MCA)", "Castilla y León", ca_name_original
    ),
    # ifn4 labels, as they mixed provinces and ccaa
    ifn4_files_labels = dplyr::case_when(
      ca_name_original == "Principado de Asturias" ~ "Asturias",
      ca_name_original == "Islas Baleares" ~ "Baleares",
      ca_name_original == "Canarias" ~ "Canarias",
      ca_name_original == "Cataluña" ~ "Cataluña",
      ca_name_original == "Extremadura" ~ "Extremadura",
      ca_name_original == "Región de Murcia" ~ "Murcia",
      ca_name_original == "Comunidad Foral de Navarra" ~ "Navarra",
      ca_name_original == "Pais Vasco" ~ "País Vasco",
      .default = province_name_original
    )# TODO, convert to NA_chr the missing provinces in the IFN4
    # ifn4_files_labels = dplyr::if_else(
    #   ifn4_files_labels %in% c()
    # )
  )

# FFI growth habit data -----------------------------------------------------------------------

# Usaremos GIFT con su paquete de R para acceder a los datos de growth form
cd_ref_edit <- readr::read_delim(
  file = fs::path(Sys.getenv("ffi_path"), "metadonnees.csv"),
  skip = 412
) |>
  dplyr::as_tibble() |>
  dplyr::rename(
    UNITE = "// Unité"
  ) |>
  dplyr::filter(
    UNITE == "CDREF13"
  ) |>
  dplyr::mutate(
    lib_cdref =  stringr::str_remove_all(Libellé, "\\s*\\(.*?\\)")
  ) |>
  dplyr::rename(
    cd_ref = Code
  ) |>
  # dplyr::select(
  #   cd_ref,
  #   lib_cdref
  # ) |>
  dplyr::rename(
    full_name = lib_cdref
  ) |>

  #Add genus and species
  dplyr::mutate(
    genus = stringr::str_extract(full_name, "\\b\\w+\\b"),
    species = stringr::str_extract(full_name, "(?<=\\s)\\w+"),
    species = dplyr::if_else(stringr::str_detect(species, "^\\("), "", species),
    SP_NAME = ifelse(is.na(species) | species == "", genus, paste(genus, species, sep = " "))) |>
  dplyr::arrange(full_name) |>
  dplyr::group_by(genus)

#FINAL VECTOR
fr_species_cdref <- cd_ref_edit$SP_NAME

# growth form from GIFT
trait_meta <- GIFT::GIFT_traits_meta()
gf_trait_id <- trait_meta[which(trait_meta$Trait2 == "Growth_form_1"), ][["Lvl3"]]
growth_form <- GIFT::GIFT_traits(
  trait_IDs = c(gf_trait_id), agreement = 0.66,
  bias_ref = FALSE, bias_deriv = FALSE
)

growth_form_lignified_france <- growth_form |>
  dplyr::filter(work_species %in% fr_species_cdref) |>
  dplyr::select(work_species, work_author, trait_value_1.2.1) |>
  dplyr::distinct() |>
  dplyr::rename(
    GrowthForm = trait_value_1.2.1,
    AccSpeciesName = work_species
  )


#ifn data

# IFN species data and provinces -----------------------------------------------------------------------

ESPECIES <- readxl::read_excel(
  "data-raw/MaximaActualidad_ATOMaDic2022_dd.xlsx", 
  sheet = "ESPECIES") |>
  dplyr::as_tibble() |> 
  dplyr::select(c(3,4,6,7)
  )  


ESPECIES_2 <- data.frame(
  SP_CODE = c(ESPECIES$SPx, ESPECIES$Spx),
  SP_NAME = c(ESPECIES$`Nombre especie`, ESPECIES$`Nombre (especies arbóreas)`)
  ) |> 
  dplyr::arrange(SP_CODE) |> 
  unique() |> 
  dplyr::mutate(
    SP_CODE= as.character(SP_CODE)
  )



MFE50_arboreas <- read_excel("data-raw/MFE50_DD_tcm30-154309.xls", 
                                    sheet = "SP ARBOREAS")|>
  dplyr::as_tibble() |> 
  dplyr::select(c(2,3,5,6)
  ) |> 
  dplyr::rename(
    SP_CODE = `CLAVE IFN`,
    SP_NAME = `NOMBRE IFN3` 
  ) |> 
  dplyr::mutate(
    SP_CODE= as.character(SP_CODE)
  )


MFE50_arbustivas <- read_excel("data-raw/MFE50_DD_tcm30-154309.xls", 
                             sheet = "SP ARBUSTIVAS")|>
  dplyr::as_tibble()|> 
  dplyr::rename(
    SP_NAME = DEFINICIÓN,
    SP_CODE = CÓDIGO
  )


mfe25_especies<- readxl::read_excel(
  "data-raw/mfe25_adic2022_dd_tcm30-528965.xlsx", 
  sheet = "Especies (arbóreas)")|>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(Código)
  ) |> 
  dplyr::rename(
    SP_NAME = Nombre
  ) |>  dplyr::select(
    SP_NAME,
    SP_CODE
  )

shrub_codes_ifn4 <- readr::read_delim(
  "data-raw/shrub_codes_ifn4.csv",delim = ";",
  escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |> 
  dplyr::rename(
    SP_NAME = IFNNAME
  ) |>  dplyr::select(
    SP_NAME,
    SP_CODE
  )

tree_codes_ifn4 <-  readr::read_delim(
  "data-raw/tree_codes_ifn4.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |> 
  dplyr::rename(
    SP_NAME = IFNNAME
  )|>  dplyr::select(
    SP_NAME,
    SP_CODE
  )


SpeciesCodesIFN23 <- readr::read_delim(
  "data-raw/SpeciesCodesIFN23.csv", 
  delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |> 
  dplyr::rename(
    SP_NAME = IFNNAME
  )|>  dplyr::select(
    SP_NAME,
    SP_CODE
  )

ESPECIES_DEF <- ESPECIES_2 |> 
  dplyr::full_join(MFE50_arboreas, by = c("SP_NAME","SP_CODE")) |> 
  dplyr::full_join(mfe25_especies, by = c("SP_NAME","SP_CODE")) |>
  dplyr::full_join(MFE50_arbustivas, by = c("SP_NAME","SP_CODE")) |> 
  dplyr::full_join(shrub_codes_ifn4, by = c("SP_NAME","SP_CODE")) |>  
dplyr::full_join(tree_codes_ifn4, by = c("SP_NAME","SP_CODE")) |> 
dplyr::full_join(SpeciesCodesIFN23, by = c("SP_NAME","SP_CODE")) |> 
  unique()

ESPECIES<-ESPECIES_DEF

# use internal data ---------------------------------------------------------------------------

usethis::use_data(
  fia_states_dictionary,
  growth_form_lignified_france,
  ifn_provinces_dictionary,
  ESPECIES,
  overwrite = TRUE, internal = TRUE
)
