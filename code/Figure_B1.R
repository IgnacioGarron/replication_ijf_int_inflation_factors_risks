library(tidyverse)
library(ggpubr)
library(kableExtra)
library(openxlsx)
library(forecast)
rm(list = ls()) # Clear environment

source("functions/data_complete.R")
source("functions/replace_outliers.R")

bannerCommenter::banner("Preliminaries")
#################################################################
##                        Preliminaries                        ##
#################################################################

data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

bannerCommenter::banner("PCA: Advanced vs Emerging")
#################################################################
##                  PCA: Advanced vs Emerging                  ##
#################################################################

table <- data %>% 
  filter(date == as.Date("2000-01-01")) %>% 
  group_by(group_income) %>% 
  summarise(N = n())

colnames(table) <- c("group_geo","N")

table <- rbind(
  table,
  data %>% 
    filter(date == as.Date("2000-01-01")) %>% 
    group_by(group_geo) %>% 
    summarise(N = n())
)

kbl(table, caption = "", booktabs = TRUE,
    linesep = c(","), format = "latex") %>%
  kable_classic(full_width = FALSE) %>% 
  kable_styling(latex_options = "scale_down")

# Groups
f_weo1 <- data %>% filter(group_income == "Advanced Economies") %>% group_by(country) %>% summarise(n=n())
f_weo2 <- data %>% filter(group_income == "Emerging and Developing Economies") %>% group_by(country) %>% summarise(n=n())
f_weo3 <- data %>% filter(group_income == "Low income EMDEs") %>% group_by(country) %>% summarise(n=n())

# Main data matrix
X <- data %>% pivot_wider(names_from = "country", values_from = "inf_sa", id_cols = "date")
X <- as.matrix(X[-1,-1])

################################################################################
# Function to compute scree plot and save PNG
################################################################################

save_scree <- function(pca, n_factors, file_name) {
  df <- data.frame(
    IC = (pca$values / length(pca$values))[1:n_factors] * 100,
    N  = 1:n_factors
  )
  
  p <- ggplot(df, aes(x = N, y = IC)) +
    geom_line() +
    geom_point() +
    ylim(0,40)+
    ylab("Variance explained (%)") +
    xlab("Factor") +
    theme_bw()
  
  print(p)
  ggsave(paste0("../output/Figures/Figure_B1_", file_name, ".png"), p, width = 6, height = 4, dpi = 300)
}

################################################################################
# Global
################################################################################

X <- scale(X)
pca <- eigen(t(X) %*% X / 287)

save_scree(pca, 30, "scree_global")

################################################################################
# Advanced Economies
################################################################################

pca <- eigen(t(X[, f_weo1$country]) %*% X[, f_weo1$country] / 287)
save_scree(pca, 30, "scree_advanced")

################################################################################
# Middle/High Income Emerging & Developing Economies
################################################################################

pca <- eigen(t(X[, f_weo2$country]) %*% X[, f_weo2$country] / 287)
save_scree(pca, 30, "scree_emde_high")

################################################################################
# Low Income Emerging & Developing Economies
################################################################################

pca <- eigen(t(X[, f_weo3$country]) %*% X[, f_weo3$country] / 287)
save_scree(pca, 30, "scree_emde_low")

################################################################################
# PCA: Geographical
################################################################################

bannerCommenter::banner("PCA: Geographical")

X <- data %>% pivot_wider(names_from = "country", values_from = "inf_sa", id_cols = "date")
X <- as.matrix(X[-1,-1])
X <- scale(X)
  
# EUROPE
f_europe <- data %>% filter(group_geo == "Europe") %>% group_by(country) %>% summarise(n=n())
pca <- eigen(t(X[, f_europe$country]) %*% X[, f_europe$country] / 287)
save_scree(pca, 30, "scree_europe")

# AFRICA
f_africa <- data %>% filter(group_geo == "Africa") %>% group_by(country) %>% summarise(n=n())
pca <- eigen(t(X[, f_africa$country]) %*% X[, f_africa$country] / 287)
save_scree(pca, 30, "scree_africa")

# AMERICA
f_america <- data %>% filter(group_geo == "America") %>% group_by(country) %>% summarise(n=n())
pca <- eigen(t(X[, f_america$country]) %*% X[, f_america$country] / 287)
save_scree(pca, 20, "scree_america")

# ASIA
f_asia <- data %>% filter(group_geo == "Asia") %>% group_by(country) %>% summarise(n=n())
pca <- eigen(t(X[, f_asia$country]) %*% X[, f_asia$country] / 287)
save_scree(pca, 20, "scree_asia")



