################################################################################
## Graphiques de biais effet prix pour riz, vin et jus
# input data: 
# - table d created in the first lines of 14_effet_prix.R
# - appariement
# output: Figure 3 + its data
################################################################################

# moyenne des contenus carbone d’agribalyse pour ces produits
GHGcontent <- as.data.table(appariement)
GHGcontent <- GHGcontent[, .(mean = mean(Emissions)),
                         by = .(CODE_PRODUIT_BDF)]

fct_biais_effet_prix <- function(code_produit){
  d0 <- d[NOMEN6 == code_produit & QUANTITE >0,]
  
  #Calcul prix moyen Pk :
  num_Pk <- sum(d0$pondmen * d0$MONTANT)
  denom_Pk <- sum(d0$pondmen * d0$QUANTITE)
  Pk <- num_Pk/denom_Pk
  
  #Emissions en kgCo2 par kg de produit dans Agribalyse
  Ek <- GHGcontent$mean[GHGcontent$CODE_PRODUIT_BDF == code_produit] 
  
  #Emissions monétaires en kgCO2 par euro
  ek <- Ek/Pk
  
  #Calcul pour chaque ménage de son empreinte et du biais prix
  d0 <- d0 %>% 
    mutate( Empreinte_reelle = QUANTITE * Ek,
            Biais_prix = ek * MONTANT,
            Empreinte_reelle_tot = sum(Empreinte_reelle * pondmen))
  
  #On somme par catégorie :
  effet_prix_cat <- d0 %>%
    group_by(ind_category) %>% 
    summarise( Empreinte_reelle_dec = sum(Empreinte_reelle * pondmen / Empreinte_reelle_tot * 100)  ,
               Empreinte_imputee_dec = sum(Biais_prix * pondmen / Empreinte_reelle_tot * 100)) %>%
    mutate(Biais_prix_dec = Empreinte_imputee_dec - Empreinte_reelle_dec)

  effet_prix_dixieme <- d0 %>% 
    group_by(dixieme) %>% 
    summarise( Empreinte_reelle_dec = sum(Empreinte_reelle * pondmen / Empreinte_reelle_tot * 100)  ,
               Empreinte_imputee_dec = sum(Biais_prix * pondmen / Empreinte_reelle_tot * 100)) %>%
    mutate(Biais_prix_dec = Empreinte_imputee_dec - Empreinte_reelle_dec)
  
  effet_prix_vingtieme <- d0 %>% 
    group_by(vingtieme) %>% 
    summarise( Empreinte_reelle_dec = sum(Empreinte_reelle * pondmen / Empreinte_reelle_tot * 100)  ,
               Empreinte_imputee_dec = sum(Biais_prix * pondmen / Empreinte_reelle_tot * 100)) %>%
    mutate(Biais_prix_dec = Empreinte_imputee_dec - Empreinte_reelle_dec)
  
  effet_prix_centieme <- d0 %>% 
    group_by(centieme) %>% 
    summarise( Empreinte_reelle_dec = sum(Empreinte_reelle * pondmen / Empreinte_reelle_tot * 100)  ,
               Empreinte_imputee_dec = sum(Biais_prix * pondmen / Empreinte_reelle_tot * 100)) %>%
    mutate(Biais_prix_dec = Empreinte_imputee_dec - Empreinte_reelle_dec)

  return(list("effet_prix_cat" = effet_prix_cat, 
              "effet_prix_dixieme" = effet_prix_dixieme, 
              "effet_prix_vingtieme" = effet_prix_vingtieme, 
              "effet_prix_centieme" = effet_prix_centieme, 
              "nb_obs" = nrow(d0),
              "nb_households" = sum(d0$pondmen)))
}

#Fonction qui trace le barplot du biais lié à l'effet prix
fct_graph_biais_effet_prix <- function(code_produit){
  effet_prix_allcategories <- fct_biais_effet_prix(code_produit)
  
  data_vingtieme <- effet_prix_allcategories$effet_prix_vingtieme
  
  data <- data_vingtieme
  data$categorie <- data$vingtieme
  cat_levels <- data$vingtieme
  data$categorie <- as.factor(data$categorie)
  data$categorie <- factor(data$categorie, levels= cat_levels)
  data$min <- pmin(data$Empreinte_reelle_dec, data$Empreinte_reelle_dec + data$Biais_prix_dec)
  data$max <- pmax(data$Empreinte_reelle_dec, data$Empreinte_reelle_dec + data$Biais_prix_dec)

  #Fait le graphique
  ggplot(data, aes(x=categorie, y=Empreinte_reelle_dec)) +
    geom_bar(stat="identity", fill="skyblue", alpha=0.5)+
    geom_pointrange(aes(x=categorie, y=Empreinte_reelle_dec + Biais_prix_dec, ymin= min, ymax= max), colour="orange", alpha=0.9) +
    scale_fill_brewer(palette="Paired") + 
    theme_minimal() +
    labs(title="", x="", y="")
}

## Figure 3
code_produit <- "011111"
pdf(file=paste0(graph_path, "biais_prix_", code_produit, ".pdf"))
print(fct_graph_biais_effet_prix(code_produit))
dev.off()
# export data
data_tosave <- fct_biais_effet_prix(code_produit)
write.csv2(data_tosave$effet_prix_vingtieme, file = paste0(graph_path, "biais_prix_", code_produit, ".csv"))
# nb of observations in the graph (= nb of surveyed households buying this product)
data_tosave[c("nb_obs", "nb_households")]
# $nb_obs
# [1] 2183
# 
# $nb_households
# [1] 2.7 (rounded)

code_produit <- "021211"
pdf(file=paste0(graph_path, "biais_prix_", code_produit, ".pdf"))
print(fct_graph_biais_effet_prix(code_produit))
dev.off()
# export data
data_tosave <- fct_biais_effet_prix(code_produit)
write.csv2(data_tosave$effet_prix_vingtieme, file = paste0(graph_path, "biais_prix_", code_produit, ".csv"))
# nb of observations in the graph (= nb of surveyed households buying this product)
data_tosave[c("nb_obs", "nb_households")]
# $nb_obs
# [1] 2361
# 
# $nb_households
# [1] 4.8 (rounded)

code_produit <- "012231"
pdf(file=paste0(graph_path, "biais_prix_", code_produit, ".pdf"))
print(fct_graph_biais_effet_prix(code_produit))
dev.off()
# export data
data_tosave <- fct_biais_effet_prix(code_produit)
write.csv2(data_tosave$effet_prix_vingtieme, file = paste0(graph_path, "biais_prix_", code_produit, ".csv"))
# nb of observations in the graph (= nb of surveyed households buying this product)
data_tosave[c("nb_obs", "nb_households")]
# $nb_obs
# [1] 5006
# 
# $nb_households
# [1] 8.0 (rounded)
