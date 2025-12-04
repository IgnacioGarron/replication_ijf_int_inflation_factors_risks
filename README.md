# Replication Package for *“International Factors and Inflation Risks”*  
**Ignacio Garrón, Vladimir Rodríguez-Caballero, and Esther Ruiz**

# Information about the Replication Package

- **Date:** December 1, 2025  
- **Structure:** *input*, *output*, *code*, *readme* file.

# Contents

This replication package provides the code and data used to generate figures and tables in the paper *“International Factors and Inflation Risks.”*  
Each figure and table is produced by a dedicated script:  
*Figure_[xx]_.R* or *Table_[xx]_.R*, respectively.

The main components of the repository are described below.

---

## *code/*
- **Code/in_sample_01_forecasts.R** – Creates the in-sample forecasts for the tables and figures in *Section 2: Data and In-Sample Analysis*. Output is saved in *output/Data/*.  
- **Code/out_of_sample_01_forecasts.R** – Contains scripts to generate the out-of-sample forecasts for the tables and figures in *Section 3: Out-of-Sample Forecasts of Inflation Densities*. Output is saved in *output/Data/*.  
- **Code/Figure_[xx]_*.R** – Scripts for generating each figure.  
- **Code/Table_[xx]_*.R** – Scripts for generating each table.
  
---

## *output/*
- **output/Figures/** – Generated figures in PDF format.  
- **output/Tables/** – Generated tables in TXT format.  
- **output/Data/** – Processed datasets (both in-sample and out-of-sample forecasts for each model).

---

## *input/*
- **input/** – Finalized datasets used in the empirical analysis.  
- **input/data_raw/** – Raw source data before processing.

---

# Instructions & Computational Requirements

All file paths are relative to the root directory of the replication package.  
Please set your working directory accordingly.

The analysis scripts *Figure_[xx]_*.R and *Table_[xx]_*.R may be run independently and in any order.

All analyses were executed using:

- **R version 4.5.1 (2025-06-13)**  
  Platform: aarch64-apple-darwin20  
  OS: macOS Tahoe 26.0.1

The following R packages are explicitly used in the analysis:

 UComp_5.1.5          Rcpp_1.1.0           pcse_1.9.1.1         pEPA_1.2            
 MCS_0.1.3            murphydiagram_0.12.2 sandwich_3.1-1       np_0.60-18          
 writexl_1.5.4        viridis_0.6.5        viridisLite_0.4.2    sn_2.1.1            
 quantreg_6.1         SparseM_1.84-2       forecast_8.24.0      openxlsx_4.2.8      
 kableExtra_1.4.0     ggpubr_0.6.1         lubridate_1.9.4      forcats_1.0.0       
 stringr_1.5.2        dplyr_1.1.4          purrr_1.1.0          readr_2.1.5         
 tidyr_1.3.1          tibble_3.3.0         ggplot2_4.0.0        tidyverse_2.0.0  

Additional built-in helper functions used across scripts are located in *code/functions/*.

---

# Data Availability and Provenance

## Global Inflation Database

Monthly headline CPI (January 1999–December 2022, 𝑇 = 288) for 𝑁 = 115 countries, obtained from the Global Database on Inflation (GDI)  
developed by **Ha et al. (2023)**.  
Downloaded: September 22, 2023  
Source: https://www.worldbank.org/en/research/brief/inflation-database

---

## World Bank and IMF Country Classification

World Bank Group income classifications for FY24 (July 1, 2023–June 30, 2024).  
These classifications are updated each year on July 1, based on the previous year's GNI per capita.  
Source:  
https://blogs.worldbank.org/en/opendata/new-world-bank-group-country-classifications-income-level-fy24

---

## Datasets for Cross-Country Heterogeneity

We compile an annual dataset using:

### World Development Indicators (WDI)
Variables used:  
1. Unemployment, total (% of total labor force) (national estimate)  
2. Trade (% of GDP)  
3. GDP per capita growth (annual %)  
4. GDP growth (annual %)  
5. Inflation, consumer prices (annual %)  
Source: https://databank.worldbank.org/source/world-development-indicators

### Geopolitical Risk Database  
Country-level GPR index from **Caldara and Iacoviello (2022)**.  
Source: https://www.matteoiacoviello.com/gpr.htm

### Central Bank Independence  
CBIE index from **Romelli (2022)**.  
Source: https://cbidata.org/

---

## Final Merged and Processed Datasets

- **input/data_complete_rev.xlsx** – Merged dataset combining the Global Inflation Database,  
  World Bank/IMF classifications, and computed monthly inflation series.  
- **X.xlsx** – Annualized monthly inflation rates by country based on seasonally adjusted headline CPI (see main paper).  
- **structure_dfm.xlsx** – Zero-restriction structure for the multilevel dynamic factor model (see main paper).
- **data_gpr.xlsx** – Country geopolitical index from Caldara and Iacoviello (2022).
- **CBIData_Romelli_2025.xlsx** – CBI from Romelli (2022).
- **WDD** - Data from World Development Indicators.
---

# References

- **Ha, J., Kose, M., & Ohnsorge, F. (2023).** *One-stop source: A global database of inflation.*  
  *Journal of International Money and Finance*, 137:102896.

- **Romelli, D. (2022).** *The political economy of reforms in central bank design: Evidence from a new data set.*  
  *Economic Policy*, 37(112):641–688.

- **Caldara, D., & Iacoviello, M. (2022).** *Measuring geopolitical risk.*  
  *American Economic Review*, 112(4):1194–1225.
