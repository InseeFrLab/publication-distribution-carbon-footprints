# Main script

PATH_COFFRE_EMPREINTE_CARBONE <- Sys.getenv("PATH_COFFRE_EMPREINTE_CARBONE")
PATH_COFFRE_BDF <- Sys.getenv("PATH_COFFRE_BDF")
PATH_COFFRE_BDF_DETAILLE <- Sys.getenv("PATH_COFFRE_BDF_DETAILLE")

mainpath <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/Ventilation empreinte carbone/BDF 2017")
data_path <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/output/data")
graph_path <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/output/graphiques/")

# category used to separate individuals
ind_category <- "vingtieme" # "dixieme" "centieme" "millieme" "vingtieme"  "DNIVIE1"

# 01 : load Budget de Famille data (confidential data)
source("01_import_BdF.R")

# 05 : load imported data
source("05_load_imported_data.R")

# 06 : importe la nomenclature (pour avoir le nom des produits dans la base), et Agribalyse
source("06_Import_Nomenclature_Agribalyse.R", encoding = "UTF-8")

# 11 : creation des categories de menage
source("11_households_categories.R")

# 12 : Nettoyage de la base de données carnets6 pour étudier l’effet prix
source("12_selection_produits.R")

# 14 : construire différents indicateurs de dispersion des prix payés selon les revenus des ménages
source("14_effet_prix.R")

# 15 : construire graphiques de biais lié à l’effet prix pour une sélection de produits
source("15_effet_prix_selection_produits.R")

# 17 : Figures with oils
source("17_figures_oils.R")
