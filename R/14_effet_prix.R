################################################################################
## Computes graphs and tables for the description of the price effect bias
# input files:
# - menage
# - d9
# output files:
# - d
# output figures:
# - Figure 1
# - Figure 2
# - Figure 2 data: paste0(graph_path, "/coefficient_log_prix_log_NIVIE_nobs_nbobs100.csv")
# - Figure B.1
# - Table 6
# - statistics directly in the text or figures notes
################################################################################

## 1 data cleaning
d <- merge(select(d9, -pondmen), menage, by="IDENT_MEN")
d <- as.data.table(d)
d$prix <- d$MONTANT / d$QUANTITE
d$log_prix <- log(d$prix)
d$log_NIVIE <- log(d$NIVIE)

## 2 elasticity of prices with standard of living
list_produit <- unique(d$NOMEN6)
loop <-0
for (produit in list_produit){
  data <- d[NOMEN6 == produit]
  if (nrow(d[NOMEN6 == produit]) >=10) {
    reg <- lm(log_prix ~ log_NIVIE, data=data[QUANTITE > 0 & NIVIE >0], weights = pondmen)
    if (loop == 0) coefficients <- data.frame(NOMEN6 = produit, coef = reg$coefficients["log_NIVIE"], pval = summary(reg)$coefficients["log_NIVIE", 4], nobs = nobs(reg), dep_tot = sum(data$MONTANT * data$pondmen, na.rm=T))
    if (loop == 1) coefficients <- rbind(coefficients, data.frame(NOMEN6 = produit, coef = reg$coefficients["log_NIVIE"], pval = summary(reg)$coefficients["log_NIVIE", 4], nobs = nobs(reg), dep_tot = sum(data$MONTANT * data$pondmen, na.rm=T)))
  }
  loop <- 1
}
coefficients$code2 <- substr(coefficients$NOMEN6, 1,2)
coefficients$coef_mean <- mean(coefficients$coef, na.rm=T)
nobs_pc_reg <- quantile(coefficients$nobs, probs=c(0.25, 0.5)) # quantiles sur les seuls produits avec au moins 10 observations


## Figure 2
pdf(file= paste0(graph_path, "/coefficient_log_prix_log_NIVIE_nobs_nbobs100.pdf"))
d_graph <- coefficients[coefficients$nobs >= 100,]
coef_mean <- aggregate(d_graph$coef, by=list(code2 = d_graph$code2), FUN="mean")
d_graph <- merge(d_graph, nomenclature2[c("nom_produit_en", "CODE_PRODUIT")], by.x="code2", by.y="CODE_PRODUIT", all.x=T)
d_graph <- merge(d_graph, coef_mean, by="code2")
d_graph$nom_produit_en <- paste0(d_graph$code2, " ", d_graph$nom_produit_en, " (", round(d_graph$x, 2),")")
g <- ggplot(d_graph, aes(x= nobs, y= coef, colour=nom_produit_en)) + 
  geom_point() + 
  geom_hline(yintercept=mean(d_graph$coef)) + 
  labs(x="", y="") + theme(legend.title = element_blank(), legend.position= c(.55, .75))
print(g)
dev.off()

write.csv2(d_graph, file = paste0(graph_path, "/coefficient_log_prix_log_NIVIE_nobs_nbobs100.csv"))

## nb of products for which we show the elasticity
nrow(d_graph)
## 375

## mean elasticity
round(mean(d_graph$coef), digits=2)
## 0.19


## Figure 1
loop <- 0
for (i in unique(d$vingtieme)){
  prix <- quantile(d[QUANTITE >0 & vingtieme == i, prix], probs = c(.1, .25, .5, .75, .9))
  if (loop == 0) d_graph <- data.frame(vingtieme = i, prix, pc = c(.1, .25, .5, .75, .9))
  if (loop == 1) d_graph <- rbind(d_graph, data.frame(vingtieme = i, prix, pc = c(.1, .25, .5, .75, .9)))
  loop <- 1
}

pdf(file= paste0(graph_path, "/distribution_prix_vingtieme.pdf"))
g <- ggplot(d_graph, aes(x=vingtieme, y = prix, colour=as.factor(pc))) + 
  geom_point() +
  labs(x="", y="") + theme(legend.title = element_blank())
print(g)
dev.off()

# nb of observations used for the graph
length(d[d$QUANTITE > 0, prix]) # without selection
# 300488

## Figure B.1
## = Figure 1 with selection of products bought by at least 1 household in each vingtile
# select products bought by all categories
d_selection <- aggregate(d$MONTANT, by=list(NOMEN6 = d$NOMEN6, vingtieme = d$vingtieme), FUN="sum")
d_selection <- d_selection %>% group_by(NOMEN6) %>%
  summarise(count = n())
d_selection <- d_selection[d_selection$count==20,]

loop <- 0
for (i in unique(d$vingtieme)){
  # with selection
  prix <- quantile(d[QUANTITE >0 & vingtieme == i & NOMEN6 %in% d_selection$NOMEN6, prix], probs = c(.1, .25, .5, .75, .9))
  if (loop == 0) d_graph <- data.frame(vingtieme = i, prix, pc = c(.1, .25, .5, .75, .9))
  if (loop == 1) d_graph <- rbind(d_graph, data.frame(vingtieme = i, prix, pc = c(.1, .25, .5, .75, .9)))
  loop <- 1
}

pdf(file= paste0(graph_path, "/distribution_prix_vingtieme_selection.pdf"))
g <- ggplot(d_graph, aes(x=vingtieme, y = prix, colour=as.factor(pc))) + 
  geom_point() +
  labs(x="", y="") + theme(legend.title = element_blank())
print(g)
dev.off()

length(d[d$QUANTITE > 0 & NOMEN6 %in% d_selection$NOMEN6, prix]) # with selection
# 291816





## 4 Ratios vingtile 20 / vingtile 1
### pour chaque vingtième et chaque produit, on peut calculer des ratios V20/V1 par ex
d1 <- d[QUANTITE >0]
d1 <- d1[, .(prix = mean(prix)), by = .(NOMEN6, vingtieme)]
ratioD20D1 <- merge(d1[vingtieme == 20, c("NOMEN6", "prix")], d1[vingtieme == 1, c("NOMEN6", "prix")], by="NOMEN6")
ratioD20D1$ratio <- ratioD20D1$prix.x / ratioD20D1$prix.y
nrow(ratioD20D1)
## 541 products bought by at least 1 household in vingtile 1 and vingtile 20.
summary(ratioD20D1$ratio)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.032   1.090   1.419   2.023   1.989  22.776 

ratioD20D1 <- ratioD20D1[order(ratioD20D1$ratio),]
ratioD20D1$quant <- 1:length(ratioD20D1$ratio)
ratioD20D1$quant <- ratioD20D1$quant / max(ratioD20D1$quant)
## ratio devient supérieur à 1 pour quantile 0.216


## 5 supplementary statistics for the text :
# nb of households buying those per category
d_tab <- as.data.table(d)
d_tab <- d_tab[NOMEN6 %in% c("011111", "021211", "012231") & QUANTITE > 0,]
d_tab1 <- d_tab[, .(nb_unweighted = .N,
                   nb_weighted = sum(pondmen)),
                 by = .(NOMEN6, vingtieme)]
d_tab2 <- d_tab[, .(nb_unweighted = .N,
                   nb_weighted = sum(pondmen)),
               by = .(NOMEN6)]
min(d_tab1$nb_unweighted) # min nb of surveyed households buying wine, rice or juices in all vingtile
# 66 (66 in carnets6)


## 6 table 6 in annex providing the Agribalyse carbon content for rice, wine and juices

d_tab <- appariement[appariement$CODE_PRODUIT_BDF %in% c("011111", "021211", "012231"),]
d_tab$CIQUAL_AGRI <- NULL
d_tab$CODE_PRODUIT_BDF <- NULL
d_tab$NOM_PRODUIT_AGRI <- NULL
colnames(d_tab) <- c("Product name in Budget des Familles", "Product name in Agribalyse", "GHG content", "Quality index")
# change BDF names in english
d_tab[d_tab$`Product name in Budget des Familles` == "Jus de fruits", "Product name in Budget des Familles"] <- "Fruit juice"
d_tab[d_tab$`Product name in Budget des Familles` == "Riz : nature, précuit, riz SAI...", "Product name in Budget des Familles"] <- "Rice"
d_tab[d_tab$`Product name in Budget des Familles` == "Vins", "Product name in Budget des Familles"] <- "Wine"
digits <- c(4, 4, 4, 2, 2)
d_tab <- d_tab[order(d_tab$`Product name in Budget des Familles`, d_tab$`Product name in Agribalyse`),]
print(xtable(d_tab, type="latex", digits = digits, align = c("ll|l|c|c")), 
              floating=FALSE,
              include.rownames=FALSE,
              hline.after = c(-1,0, 23, 30, nrow(d_tab)), 
              file= paste0(graph_path, "/carbon_content.tex"))


## descriptive stats on oil
d1 <- d[d$NOMEN6 == "011541",]
length(unique(d1$IDENT_MEN))
#  1865
round(sum(d1$pondmen)/1000000, digits=1)
# 2.5
d_tab <- as.data.table(d)
d_tab <- d_tab[NOMEN6 %in% c("011541", "011531") & QUANTITE > 0,]
d_tab1 <- d_tab[, .(nb_unweighted = .N,
                    nb_weighted = sum(pondmen)),
                by = .(NOMEN6, vingtieme)]
d_tab2 <- d_tab[, .(nb_unweighted = .N,
                    nb_weighted = sum(pondmen)),
                by = .(NOMEN6)]
min(d_tab1$nb_unweighted)
# 26 for olive oil (52 for other type of oil)
