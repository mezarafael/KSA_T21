
# Package names
packages <- c("dplyr","ggplot2","ggplot2","ggpubr", "readxl", "dplyr", "tidyr", "reshape2", "grid", "gridBase", "gridExtra", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE)

# Load country-specific data ----------------------------------------------

# census populations, smoking parameters, mortality, life expectancy by smoking status, birth cohort, calendar year
load(paste0("data/mort_ksa.RData")) # mortality
load(paste0("data/smk_ksa.RData")) # smoking init/cess cast as AC = age-cohort
load(paste0("data/pop_ksa.RData")) # census pop
load(paste0("data/le_ksa.RData")) # life expectancies

# initialize prevalences in age 2013
df_F.prevs2013 <- readRDS(file = "APC_Analysis/AgePrevF_099.rds") # age-specific prevalences in 2013
df_M.prevs2013 <- readRDS(file = "APC_Analysis/AgePrevM_099.rds")

# smoking survey data for model verification
load('data/surveyprevs_ksa.Rda')

# Set model inputs --------------------------------------------------------

v_policy.ages <- c(18:20) # ages affected by the policy
date_variable <- format(Sys.Date(), "%m.%d.%y")
startyear <- 2010   # starting year
endyear <- 2100   # final calendar year
policyyear <- 2025   # policy year
calyears <- endyear-startyear+1     # number of cohort years
v_calyears <- startyear:endyear  # index of calendar years 2000 -2100
# v_stdbirths <- 1000000    #fixed birthrate/population size for prevalence calculations
v_mla.effects <- c(main=0.3391,upper=0.5266,lower=0.1517,baseline=0)  # main T21 policy effect estimate with upper, lower bounds
