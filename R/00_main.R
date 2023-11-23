# Main script

PATH_COFFRE_EMPREINTE_CARBONE <- Sys.getenv("PATH_COFFRE_EMPREINTE_CARBONE")
PATH_COFFRE_BDF <- Sys.getenv("PATH_COFFRE_BDF")
PATH_COFFRE_BDF_DETAILLE <- Sys.getenv("PATH_COFFRE_BDF_DETAILLE")

mainpath <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/Ventilation empreinte carbone/BDF 2017")
data_path <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/output/data")
graph_path <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/output/graphiques/")

# load packages
pkglist <- c('haven', "dplyr", 'ggplot2', 'xtable', 'reshape2',  'tibble', 'tidyr', 'gridExtra', 'openxlsx', 'readxl', 'readr', 'readODS', 'knitr', 'kableExtra', 'hutils', "data.table", 'stringr')
lapply(pkglist, library, character.only = TRUE)

# category used to separate individuals
ind_category <- "vingtieme" # "dixieme" "centieme" "millieme" "vingtieme"  "DNIVIE1"

# 01 : load Budget de Famille data (confidential data)
source("R/01_import_BdF.R")

# 05 : load imported data
source("R/05_load_imported_data.R")

# 06 : importe la nomenclature (pour avoir le nom des produits dans la base), et Agribalyse
source("R/06_Import_Nomenclature_Agribalyse.R", encoding = "UTF-8")

# 11 : creation des categories de menage
source("R/11_households_categories.R")

# 12 : Nettoyage de la base de données carnets6 pour étudier l’effet prix
source("R/12_selection_produits.R")

# 14 : construire différents indicateurs de dispersion des prix payés selon les revenus des ménages
source("R/14_effet_prix.R")

# 15 : construire graphiques de biais lié à l’effet prix pour une sélection de produits
source("R/15_effet_prix_selection_produits.R")

# 17 : Figures with oils
source("R/17_figures_oils.R")
