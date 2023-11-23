#Import the classification names of the product
#input files:
# - paste0(mainpath, "/Doc BDF 2017/Nomenclature_BDF_2017_VF.xlsx")
# - paste0(mainpath, "/Doc BDF 2017/nomenclature_coicop_EN.xlsx")
# - paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/Ventilation empreinte carbone/AgriBalise/Donnees versions simplifiees/AGRIBALYSE3.1_synthese.csv")
# - paste0(mainpath, "/Donnees/Appariement_BdF_Agribalyse3_1.csv")

#output files:
# - nomenclature
# - nomenclature2
# - agribalyse3_1
# - appariement
################################################################

nomenclature <- openxlsx::read.xlsx(xlsxFile = paste0(mainpath, "/Doc BDF 2017/Nomenclature_BDF_2017_VF.xlsx"),
                                    sheet = "Table 2", 
                                    startRow=2)
nomenclature2 <- openxlsx::read.xlsx(xlsxFile = paste0(mainpath, "/Doc BDF 2017/Nomenclature_BDF_2017_VF.xlsx"),
                                     sheet = "Table 3", 
                                     startRow=1,
                                     colNames=F)
nomenclature_en <- openxlsx::read.xlsx(xlsxFile = paste0(mainpath, "/Doc BDF 2017/nomenclature_coicop_EN.xlsx"),
                                       sheet = "coicop", 
                                       startRow=1,
                                       colNames=F)
colnames(nomenclature_en) <- c("CODE_PRODUIT", "nom_produit_en")
colnames(nomenclature) <- c("Code_general", "CODE_PRODUIT", "NOM_PRODUIT")
colnames(nomenclature2) <- colnames(nomenclature)
nomenclature <- rbind(nomenclature, nomenclature2)
nomenclature2 <- nomenclature[substr(nomenclature$Code_general, 3, 6) == "****" & !is.na(nomenclature$Code_general),]
nomenclature2$NOM_PRODUIT <- tolower(nomenclature2$NOM_PRODUIT)
substr(nomenclature2$NOM_PRODUIT, 1, 1) <- toupper(substr(nomenclature2$NOM_PRODUIT, 1, 1))
nomenclature2$CODE_PRODUIT <- substr(nomenclature2$Code_general, 1, 2)

nomenclature2$code_num <- as.numeric(nomenclature2$CODE_PRODUIT)
nomenclature2 <- merge(nomenclature2, nomenclature_en, by.x="code_num", by.y="CODE_PRODUIT", all.x=TRUE)
nomenclature2$code_num <- NULL


#Import agribalyse :
# version 3.1
file <- paste0(PATH_COFFRE_EMPREINTE_CARBONE, "/Ventilation empreinte carbone/AgriBalise/Donnees versions simplifiees/AGRIBALYSE3.1_synthese.csv")
agribalyse3_1 <- read_csv(file,
                          locale = locale(encoding ="UTF-8"),
                          skip = 2,
                          col_types ="ccccccccccccc")
## note pourquoi DQR3_1 est 100 fois trop grand (et pas Emissions3_1)
agribalyse3_1 <- agribalyse3_1 %>%
  select(`Code\nCIQUAL`,`Nom du Produit en Français`, `LCI Name`, `kg CO2 eq/kg de produit`, `DQR - Note de qualité de la donnée (1 excellente ; 5 très faible)`) %>%
  rename(CIQUAL = `Code\nCIQUAL`,
         Produit = `Nom du Produit en Français`,
         Produit_en = `LCI Name`,
         Emissions = `kg CO2 eq/kg de produit`,
         DQR = `DQR - Note de qualité de la donnée (1 excellente ; 5 très faible)`)
agribalyse3_1$Emissions <- as.numeric(gsub(",", ".", agribalyse3_1$Emissions))
agribalyse3_1$DQR <- as.numeric(gsub(",", ".", agribalyse3_1$DQR))


# on the food products studied, match between BDF and Agribalyse product codes
appariement <- read_csv(paste0(mainpath, "/Donnees/Appariement_BdF_Agribalyse3_1.csv"),
                        locale = locale(encoding ="UTF-8"),
                        col_types="ccccd")
appariement[nchar(appariement$CODE_PRODUIT_BDF) == 5, "CODE_PRODUIT_BDF"] <- paste0("0", appariement$CODE_PRODUIT_BDF)
appariement <- merge(appariement, 
                     agribalyse3_1, 
                     by.x = "CIQUAL_AGRI", 
                     by.y = "CIQUAL",
                     all.x = TRUE)
appariement[c("EMISSIONS", "Produit")] <- NULL # variables en double 

