
# Imports Budget de Famille data. This step needs access to confidential data

list_tables <- c("MENAGE")
for (tab in list_tables){
  d <- read_sas(paste0(PATH_COFFRE_BDF, tab, ".sas7bdat"))
  saveRDS(d, file=paste0(data_path, "/tmp/", tab, ".Rds"))
}

rm(d)
gc()

carnets6 <- read_sas(paste0(PATH_COFFRE_BDF_DETAILLE,"carnets6_gestion.sas7bdat"))
saveRDS(carnets6, file=paste0(data_path, "/tmp/carnets6.Rds"))

rm(carnets6)

depense <- read_sas(paste0(PATH_COFFRE_BDF_DETAILLE,"depense.sas7bdat"))
saveRDS(depense, file=paste0(data_path, "/tmp/depense.Rds"))

rm(depense)
gc()