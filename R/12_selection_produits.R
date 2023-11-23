################################################################################
# input file: depense
# output file: d9
# output figures: Table 1
################################################################################

d <- depense 
### agreger au niveau menage un meme produit
## on enlève les lignes avec montant manquant
d <- d[!is.na(d$MONTANT),]
## on enleve les lignes avec quantite manquante
d <- d[!is.na(d$QUANTITE),]
d <- as.data.table(d)

## on remplace les wp manquants par des 1
d[is.na(d$wp), "wp"] <- 1

## j’agrege au niveau menages x unite, pour identifier les unités dominantes à choisir
d3 <- d[, .(montant_KG = sum(MONTANT * COEFFANNU * wp * (UNITE == "KG")),
            montant_ = sum(MONTANT * COEFFANNU * wp * (UNITE == "")),
            montant_L = sum(MONTANT * COEFFANNU * wp * (UNITE == "L")),
            montant_U = sum(MONTANT * COEFFANNU * wp * (UNITE == "U")),
            montant_total = sum(MONTANT * COEFFANNU * wp),
            nb_KG = sum(UNITE == "KG"),
            nb_ = sum(UNITE == ""),
            nb_L = sum(UNITE == "L"),
            nb_U = sum(UNITE == "U"),
            nb_total = .N),
        by = .(IDENT_MEN, NOMEN6)]

d4 <- d3[, .(montant_KG = sum(montant_KG),
             montant_ = sum(montant_),
             montant_L = sum(montant_L),
             montant_U = sum(montant_U),
             montant_total = sum(montant_total),
             nb_KG = sum(nb_KG),
             nb_ = sum(nb_),
             nb_L = sum(nb_L),
             nb_U = sum(nb_U),
             nb_total = sum(nb_total)),
         by = .(NOMEN6)]

d4 <- d4[, part_montant_KG := montant_KG / montant_total]
d4 <- d4[, part_montant_ := montant_ / montant_total]
d4 <- d4[, part_montant_U := montant_U / montant_total]
d4 <- d4[, part_montant_L := montant_L / montant_total]
d4 <- d4[, part_nb_KG := nb_KG / nb_total]
d4 <- d4[, part_nb_ := nb_ / nb_total]
d4 <- d4[, part_nb_U := nb_U / nb_total]
d4 <- d4[, part_nb_L := nb_L / nb_total]

d4 <- d4[, part_montant_max := pmax(part_montant_KG, part_montant_, part_montant_U, part_montant_L)] 
d4 <- d4[, part_nb_max := pmax(part_nb_KG,part_nb_, part_nb_U, part_nb_L)] 
d4 <- d4[, `:=`(part_montant_max_KG = (part_montant_KG == part_montant_max),
                part_montant_max_ = (part_montant_ == part_montant_max),
                part_montant_max_U = (part_montant_U == part_montant_max),
                part_montant_max_L = (part_montant_L == part_montant_max),
                part_nb_max_KG = (part_nb_KG == part_nb_max),
                part_nb_max_ = (part_nb_ == part_nb_max),
                part_nb_max_U = (part_nb_U == part_nb_max),
                part_nb_max_L = (part_nb_L == part_nb_max))]

d4[d4$part_montant_ == d4$part_montant_max, "unite_choisie"] <- ""
d4[d4$part_montant_U == d4$part_montant_max, "unite_choisie"] <- "U"
d4[d4$part_montant_L == d4$part_montant_max, "unite_choisie"] <- "L"
d4[d4$part_montant_KG == d4$part_montant_max, "unite_choisie"] <- "KG"

### agreger les transactions sur un meme produit pour chaque menage
d7 <- merge(d, d4[, c("unite_choisie", "NOMEN6")], by="NOMEN6")
d7 <- d7[unite_choisie == UNITE]

# on enlève les données hors champ de la consommation finale des ménages
d7bis <- d7[substr(d7$NOMEN6,1, 2) != "13",]

d8 <- d7bis[, .(MONTANT = sum(MONTANT * COEFFANNU * wp),
                QUANTITE = sum(QUANTITE * COEFFANNU * wp),
                pondmen = mean(pondmen)),
            by =.(IDENT_MEN, NOMEN6)]
d8neg <- d8[MONTANT <=0]
d9 <- d8[MONTANT >0]

# pour avoir le nombre de transactions que représentent ces montants <=0 au niveau des ménages
d10 <- merge(d7bis, d8neg[,c("IDENT_MEN", "NOMEN6")], by = c("IDENT_MEN", "NOMEN6"), all.y=T)

### construction de la table décrivant les étapes de sélection
stat_selection <- function(data) {
  return(data.frame(obs=nrow(data), spending = sum(data$MONTANT * data$pondmen * data$wp * data$COEFFANNU, na.rm=T), transactions = sum(data$pondmen)))
}
stat_selection2 <- function(data) {
  return(data.frame(obs=nrow(data), spending = sum(data$MONTANT * data$pondmen, na.rm=T), transactions = sum(data$pondmen)))
}

tab10 <- stat_selection(d10)

### Table 1 Data processing
tab <- rbind(stat_selection(depense), 
             stat_selection(depense[!is.na(depense$MONTANT),]), 
             stat_selection(d),
             stat_selection(d7),
             stat_selection(d7bis),
             stat_selection2(d8),
             stat_selection2(d9))
tab[6, "transactions"] <- tab[5, "transactions"] # en agregeant au niveau menages, on ne perd aucune transaction
tab[7, "transactions"] <- tab[6, "transactions"] - tab10[1 , "transactions"]
tab[, "spending"] <- round(tab[, "spending"] / tab[1, "spending"] * 100, 1)
tab[, "transactions"] <- round(tab[, "transactions"] / tab[1, "transactions"] * 100, 1)
colnames(tab) <- c("# Observations", "Share of spending", "Share of transactions")
rownames(tab) <- c("Raw data", "Without missing spending", "Without missing quantity", "With only most representative unit", "Scope of household final consumption", "At the household level", "Without negative spending")
digits <- cbind(c(0,0, 0, 0, 0, 0, 0), c(0,0, 0, 0, 0, 0, 0), c(1,1,1,1,1,1,1), c(1,1,1,1,1,1,1))
print(xtable(tab, type="latex", digits = digits, align = c("lccc")), floating=FALSE, file= paste0(graph_path, "/tab_descr_selection.tex"))


## descriptive stats in section Data:
length(unique(d9$IDENT_MEN)) # nb of households in final data
# 16140
length(unique(depense$IDENT_MEN)) # nb of households in raw data
# 16977
length(unique(d9$NOMEN6)) # nb of products in final data
# 797
length(unique(depense$NOMEN6)) # nb of products in raw data
# 921
round(sum(unique(d9$pondmen))/1000000, digits=1) # nb of households in final data
# 28.4
round(sum(unique(depense$pondmen))/1000000, digits=1) # nb of households in raw data
# 29.4

