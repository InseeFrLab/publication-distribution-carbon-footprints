################################################################################
# input files:
# - paste0(mainpath, "/Donnees/data_CF_decomp_simulations.csv")
# - paste0(mainpath, "/Donnees/data_figures.csv")
# - paste0(mainpath, "/Donnees/emissions_huiles.csv")
# - paste0(mainpath, "/Donnees/data_bdf.csv")
# - paste0(mainpath, "/Donnees/data_simul_quantites_touteshuiles.csv")
# output figures:
# - Table 7 : paste0(mainpath, "/graphiques/CF_decomp_simulations.tex")
# - Figure 5
# - Figure 5 data : paste0(graph_path, "CF_oils_true_imputed.csv")
# - Figure 6
################################################################################

## Table 7 (median of the simulations)

## import the data for the graphs
d_fig <- read.csv(file = paste0(mainpath, "/Donnees/data_CF_decomp_simulations.csv"), encoding = "utf-8")
colnames(d_fig) <- c("var", "cat", "Simul", "value")
d_fig$value <- as.numeric(str_replace(d_fig$value, ",", "."))
mesquantiles <- function(x){
  return(quantile(x, probs=c(.005, .025, .05, .1, .5, .9, .95, .975, .995), na.rm=TRUE))
}
d_fig2 <- as.data.table(d_fig)[, lapply(.SD, mesquantiles),
                               by = c("var", "cat"),
                               .SDcols = "value"]
d_fig2[, 'quantile' := c("P0_5", "P2_5", "P5","P10","P50","P90", "P95", "P97_5", "P99_5"), by = c("var", "cat")]
d_fig2[quantile == "P0_5" & value > 1, pvalue := 1]
d_fig2[quantile == "P99_5" & value < 1, pvalue := 1]
d_fig2[quantile == "P2_5" & value > 1, pvalue := 5]
d_fig2[quantile == "P97_5" & value < 1, pvalue := 5]
d_fig2[quantile == "P5" & value > 1, pvalue := 10]
d_fig2[quantile == "P95" & value < 1, pvalue := 10]
d_fig3 <- d_fig2[, .(pval = min(pvalue, na.rm=TRUE)),
       by = c("var", "cat")]
d_fig3 <- merge(d_fig2, d_fig3, by=c("var", "cat"))
d_fig3 <- d_fig3[quantile == "P50",]
d_fig3[, quantile := NULL]
d_fig3[, pvalue := NULL]

d_fig3$value <- as.character(format(round(d_fig3$value, digits=2)), nsmall = 2)
d_fig3[pval == 1, value2 := paste0(value, "***")]
d_fig3[pval == 5, value2 := paste0(value, "**")]
d_fig3[pval == 10, value2 := paste0(value, "*")]
d_fig3[is.na(value2), value2 := value]
d_fig3 <- d_fig3[, c("var", "cat", "value2")]
d_fig4 <- dcast(d_fig3, var ~ cat, value.var="value2")

d_fig4$order <- c(1,3,2)
d_fig4 <- d_fig4[order(d_fig4$order)]
d_fig4$order <- NULL

print(xtable(d_fig4, type="latex", align = ("lcccccc")), 
      include.rownames=FALSE, 
      file=paste0(graph_path, "/CF_decomp_simulations.tex"))


## import data of the shares of each income category in the CF : true, spending, polar cases 1 and 2
d_fig <- read.csv(file=paste0(mainpath, "/Donnees/data_figures.csv"))
colnames(d_fig) <- c("cat", "CF_true", "CF_dis_spending", "CF_imputed", "case1poor", "case2rich")

# 1 figure 5 with imputed and true carbon footprint
data <- d_fig[, c("cat", "CF_true", "CF_imputed")]
data$CF_true <- 100* as.numeric(str_replace(data$CF_true, ",", "."))
data$CF_imputed <- 100* as.numeric(str_replace(data$CF_imputed, ",", "."))
data$categorie <- data$cat
cat_levels <- data$cat
data$categorie <- as.factor(data$categorie)
data$categorie <- factor(data$categorie, levels= cat_levels)
data$min <- pmin(data$CF_true, data$CF_imputed)
data$max <- pmax(data$CF_true, data$CF_imputed)

write.csv2(data[c("categorie", "CF_true", "CF_imputed")], file=paste0(graph_path, "CF_oils_true_imputed.csv"))

#Fait le graphique
g <- ggplot(data, aes(x=categorie, y=CF_true)) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.5)+
  geom_pointrange(aes(x=categorie, y=CF_imputed, ymin= min, ymax= max), colour="orange", alpha=0.9) +
  scale_fill_brewer(palette="Paired") + 
  theme_minimal() + 
  labs(title="", x="", y="")
g
pdf(file=paste0(graph_path, "CF_oils_true_imputed.pdf"))
print(g)
dev.off()

# 2 Figure 6, with bootstrap estimations and polar cases

# 2.1 import data on all kinds of oils carbon content (agribalyse)
d_emissions_huiles <- read.csv(file = paste0(mainpath, "/Donnees/emissions_huiles.csv"), encoding = "utf-8")
## we multiply the carbon content (kg CO2 / kg of product) by the average density of oils (0.92 kg / L) to get the carbon content per liter
d_emissions_huiles$emissions <- as.numeric(str_replace(d_emissions_huiles$emissions , ",", "."))* 0.92

# 2.2 import data on olive oil
d_bdf <- read.csv(file = paste0(mainpath, "/Donnees/data_bdf.csv"), encoding = "utf-8") %>%
  rename(cat = X)
for (name in c("quantite_olive", "quantite_autres", "depenses_olive", "depenses_autres")) {
  d_bdf[[name]] <- as.numeric(str_replace(d_bdf[[name]], ",", "."))
}
d_bdf$CF_olive <- d_bdf$quantite_olive * d_emissions_huiles[d_emissions_huiles$TypeHuile == "Huile Olive", "emissions"]
d_bdf$CF_olive_total <- sum(d_bdf$CF_olive, na.rm=TRUE)

# 2.3 import data on the simulated quantities for each quintile and each type of oil.
d_quantities <- read.csv(file = paste0(mainpath, "/Donnees/data_simul_quantites_touteshuiles.csv"), encoding = "utf-8")
d_quantities$Quantite <- as.numeric(str_replace(d_quantities$Quantite , ",", "."))
d_quantities <- as.data.table(merge(d_quantities, d_emissions_huiles, by="TypeHuile"))
## sum(is.na(data["emissions"])) # verif that all oils correctly matched
d_quantities2 <- d_quantities[, .(CF_true_sim = sum(Quantite * emissions)), by = .(cat, Simul)]
d_quantities2[, CF_total := sum(CF_true_sim), by=.(Simul)]

# we add the olive oil CF
d_quantities2 <- merge(d_quantities2, d_bdf[c("cat", "CF_olive", "CF_olive_total")], by="cat")
d_quantities2$CF_total <- d_quantities2$CF_total + d_quantities2$CF_olive_total
d_quantities2$CF_true_sim <- d_quantities2$CF_true_sim + d_quantities2$CF_olive

d_quantities3 <- d_quantities2[, lapply(.SD, mesquantiles),
                               by = "cat",
                               .SDcols = c("CF_true_sim", "CF_total")]
d_quantities3[, 'quantile' := c("P0_5", "P2_5", "P5","P10","P50","P90", "P95", "P97_5", "P99_5"), by = "cat"]
d_quantities3[, CF_sim := 100 * CF_true_sim / CF_total]
d_quantities4 <- dcast(d_quantities3[quantile %in% c("P2_5", "P50", "P97_5"), ], cat ~ quantile, value.var = "CF_sim") %>%
  rename(median = P50, CIlow = P2_5, CIhigh = P97_5)


data <- d_fig
for (name in c("CF_true", "CF_dis_spending", "CF_imputed", "case1poor", "case2rich")) {
  data[[name]] <- 100* as.numeric(str_replace(data[[name]], ",", "."))
}
data <- data %>%
  rename(case1 = case1poor, case2 = case2rich)

data <- merge(data, d_quantities4, by="cat")

data$categorie <- data$cat
cat_levels <- data$cat
data$categorie <- as.factor(data$categorie)
data$categorie <- factor(data$categorie, levels= cat_levels)
data$cat <- NULL
data$median_min <- pmin(data$median, data$CF_imputed)
data$median_max <- pmax(data$median, data$CF_imputed)
data$case1_min <- pmin(data$case1, data$CF_imputed)
data$case1_max <- pmax(data$case1, data$CF_imputed)
data$case2_min <- pmin(data$case2, data$CF_imputed)
data$case2_max <- pmax(data$case2, data$CF_imputed)

data1 <- data[c("categorie", "median", "case1", "case2")]
data1 <- as.data.frame(melt(as.data.table(data1), id.vars="categorie"))

data2 <- data[c("categorie", "median_min", "case1_min", "case2_min")]
data2 <- as.data.frame(melt(as.data.table(data2), id.vars="categorie"))
data2 <- data2 %>% rename(min = value) 
data2$variable <- str_replace(data2$variable, "_min", "")
  
data3 <- data[c("categorie", "median_max", "case1_max", "case2_max")]
data3 <- as.data.frame(melt(as.data.table(data3), id.vars="categorie"))
data3 <- data3 %>% rename(max = value) 
data3$variable <- str_replace(data3$variable, "_max", "")

data4 <- merge(data1, data2, by=c("categorie", "variable"))
data4 <- merge(data4, data3, by=c("categorie", "variable"))
data4 <- merge(data4, data[c("categorie", "CF_imputed")], by="categorie")
data4 <- merge(data4, data[c("categorie", "CIlow", "CIhigh")], by="categorie")

data4$variable <- as.character(data4$variable)
data4[data4$variable == "case1", "variable"] <- "polar case 1"
data4[data4$variable == "case2", "variable"] <- "polar case 2"
data4[data4$variable == "median", "variable"] <- "median simulation"
data4$variable <- factor(data4$variable, levels= c("polar case 1", "median simulation", "polar case 2"))


# makes Figure 6
g <- ggplot(data=data4, aes(x=categorie, y=value, fill= variable)) +
  geom_bar(data= data4, stat="identity", alpha=0.5, width=.7, position= position_dodge(width=.8))+
     geom_pointrange(aes(x=categorie, y=CF_imputed, ymin= min, ymax= max), position=position_dodge(width=.8), colour="orange", alpha=0.9, show.legend=FALSE) + 
  geom_errorbar(aes(ymin = CIlow, ymax = CIhigh), width=.2) + # confidence intervals
  scale_fill_brewer(palette="Paired") +
  theme_minimal() + 
  theme(legend.title=element_blank()) + # enleve le titre de legende
  theme(panel.grid.major.x = element_blank()) + # delete y grid lines
  labs(title="", x="", y="")
g

pdf(file=paste0(graph_path, "CF_oils_bootstrap2.pdf"))
print(g)
dev.off()
       