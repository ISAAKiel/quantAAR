matuskovo <- readr::read_csv("data-raw/Matuskovo.csv")
usethis::use_data(matuskovo, overwrite = TRUE)

matuskovo_material <- as.matrix(matuskovo[,9:34])
rownames(matuskovo_material) <- matuskovo$grave_nr
matuskovo_material <- quantAAR::itremove(matuskovo_material, 1)
usethis::use_data(matuskovo_material, overwrite = TRUE)
