test_that(".read_inventory_data returns lazy_dt for ifn", {
  # IFN3
  test_file_ifn3 <- fs::path("Ifn3p35.accdb")
  test_input_ifn3 <- glue::glue("{test_file_ifn3}|PCMayores")
  test_colnames_ifn3 = c(
    "Estadillo",
    "Cla",
    "Subclase",
    "Especie",
    "nArbol",
    "OrdenIf3",
    "OrdenIf2",
    "Dn1",
    "Dn2",
    "Ht",
    "Calidad",
    "Forma"
  )
  expect_s3_class(
    test_res_ifn3 <- .read_inventory_data(test_input_ifn3, test_colnames_ifn3,.ifn = TRUE),
    "dtplyr_step_first"
  )
  expect_true(nrow(test_res_ifn3) > 0)
})
