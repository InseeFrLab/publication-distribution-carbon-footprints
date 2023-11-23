# DistributionCarbonFootprint

The code in this repository generates the figures of the "Challenges in measuring the distribution of carbon footprints: the role of product and price heterogeneity" article by Mathias André, Alexandre Bourgeois, Emmanuel Combet, Matthieu Lequien and Antonin Pottier.

## Data sources

- The source data Budget de Famille from Insee (https://www.insee.fr/fr/metadonnees/source/serie/s1194) are confidential, accessible via a request to the Comité du secret (https://www.comite-du-secret.fr). Metadata are available online (https://data.progedo.fr/studies/doi/10.13144/lil-1416).
- The Footprint database is open-source and is accessible from Ademe (https://base-empreinte.ademe.fr/ or https://agribalyse.ademe.fr/).
- The source data Nielsen from consumer panels are confidential (https://www.nielsen.com/fr/about-us/nielsen-panels/). 
- Concordance tables from NACE Rev.2 to COICOP produced by INSEE are accessible online (annexe 2 page 62 of https://www.insee.fr/fr/statistiques/fichier/2832834/depense-conso-finale-menages-conso-finale-effective-menages-base-2010.pdf).
- Functional COICOP classifications are available online (https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Classification_of_individual_consumption_by_purpose_(COICOP)).

## How to use ?

To reproduce the results of the article :

- open the `DistributionCarbonFootprint.Rproj` file in `RStudio`

- install the required dependencies specified in the `DESCRIPTION.md` file : 

```R
remotes::install_deps(".")
```

- create an `.Renviron` file with the following content and fill the blank to declare the paths of the *coffres* containing input data :

```sh
PATH_COFFRE_EMPREINTE_CARBONE=""
PATH_COFFRE_BDF=""
PATH_COFFRE_BDF_DETAILLE=""
```

- run the `00_main.R` script, which calls all the other ones.

Note : the csv file "Tables 2-5, Figure 4.csv" in the `data/` directory produces tables 2, 3, 4, 5 and data for figure 4.
