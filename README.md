# Replication package for "International factors and inflation risks"
Ignacio Garrón, Vladimir Rodruíguez-Caballero, and Esther Ruiz

# Information about Replication package 
- Date: November 30
- Contact for reproducibility: Ignacio Garrón (igarron@est-econ.uc3m.es)
- Structure: input, output, code, readme file.

# Contents

The code in this replication material generates the 12 figures and 3 tables for 
the paper "Evaluating Probabilistic Classifiers: The Triptych". Each figure and table 
is generated separately by its corresponding script file 
Figure_[xx]_*.R or Table_[xx]_*.R, respectively.

The main contents of the repository are the following:

Code/insample_RR.R: R scripts to create the in-sample forecasts for Tables
and Figures in *Section 2: Data and in-sample analysis*. The output of this
script is saved in *output/Data/*.
Code/outofsample: R scripts to create the out-of-sample forecasts for Tables
and Figures in *Section 3: Out-of-sample forecasts of inflation densities*.
The output of this script is saved in *output/Data/*.
Code/Figure_[xx]_*.R: R scripts to create the respective figures.
Code/Table_[xx]_*.R: R scripts to create the respective tables.
output/Figures/: folder of generated plots as PDF files.
output/Tables/: folder of generated tables as txt files.
output/Data/: folder of processed data files (in-sample and out-of-sample forcasts 
for each model).
input/: folder of final data files that are used in the exercise
input/data_raw: folder of raw data files.

# Data availability and provenance

## Global inflation data base
Monthly Headline CPI from January 1999 to December 2022 (𝑇 = 288) for a set of 𝑁 = 115 
countries around the world from the novel Global Database on Inflation (GDI) 
constructed by Ha et al. (2023). Source: https://www.worldbank.org/en/research/brief/inflation-database.

## World Bank and IMF country classification

World Bank Group country classifications by income level for FY24 (July 1, 203-June
30, 2024). The World Bank Group classifications are updated each year on July 1, based on the GNI per capita
of the previous calendar year. Source: https://blogs.worldbank.org/en/opendata/new-world-bank-groupcountry-
classifications-income-level-fy24.

## Cross country data set

World Bank Group country classifications by income level for FY24 (July 1, 203-June
30, 2024). The World Bank Group classifications are updated each year on July 1, based on the GNI per capita
of the previous calendar year. Source: https://blogs.worldbank.org/en/opendata/new-world-bank-groupcountry-
classifications-income-level-fy24.


## Final dataset to be used in the exercise

- The "input/data_complete_rev_xlsx"" contains the merge of these two data set with the
dataset that is use to compute the monthly inflation series and all the analysis.

-The "X.xlsx"" contains the annualized monthly inflation rates for each country in 
columns based on the headline seasonal adjusted monthly CPI; see main paper.

-The "structure_dfm.xlsx"" contains the zero restrictions applied to the
multilevel-dynamic factor model; see main paper.




