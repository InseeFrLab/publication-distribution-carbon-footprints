################################################################################
# input file: menage
# output file: menage
################################################################################

## construct the income category for individuals

menage$millieme <- hutils::weighted_ntile(menage$NIVIE, weights = menage$pondmen * menage$NPERS, 1000)
menage$centieme <- floor((menage$millieme - 1)/ 10 + 1)
menage$dixieme <- floor((menage$millieme - 1) / 100 + 1)
menage$vingtieme <- floor((menage$millieme - 1) / 50 + 1)
menage$ind_category <- menage[[ind_category]]

# Computation of the weights associated with each category
poids <- menage %>%
  group_by(dixieme) %>%
  summarise(pond_dixieme = sum(pondmen))
menage <- merge(menage, poids, by='dixieme')

poids <- menage %>%
  group_by(vingtieme) %>%
  summarise(pond_vingtieme = sum(pondmen))
menage <- merge(menage, poids, by='vingtieme')

poids <- menage %>%
  group_by(centieme) %>%
  summarise(pond_centieme = sum(pondmen))
menage <- merge(menage, poids, by='centieme')

poids <- menage %>%
  group_by(millieme) %>%
  summarise(pond_millieme = sum(pondmen))
menage <- merge(menage, poids, by='millieme')

poids <- menage %>%
  group_by(ind_category) %>%
  summarise(pond_ind_category = sum(pondmen))
menage <- merge(menage, poids, by='ind_category')
