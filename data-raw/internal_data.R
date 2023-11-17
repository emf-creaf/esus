
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
  dplyr::as_tibble()


# use internal data ---------------------------------------------------------------------------

usethis::use_data(
  fia_states_dictionary,
  growth_form_lignified_france,
  ifn_provinces_dictionary,
  ESPECIES,
  overwrite = TRUE, internal = TRUE
)
