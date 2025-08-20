rm(list = ls()) 
setwd("/Users/jt1462/Dropbox/ITC_Proposal/KSA/")

source('R/01_model_inputs.R')
source('R/02_model_functions.R')

# Modify smoking parameters
m_M.cessAP = m_M.cessAP*1.0
# m_F.cessAP = m_M.cessAP*1.0
# m_F.cessAP[14:40,] = 0.1246198 # flatten female cessation to constant level from ages 13+

namethisrun <- "aug2025"

# Run model and get prevalence, mortality -----------------------------------------------------

df_mortality.out <- NULL
df_prev.out <- NULL
l_pop.out <- list()
l_smkparams <- list()

for (e in seq_along(v_mla.effects)) {
      print(v_mla.effects[e])
      df_out=run_model(v_mla.effects[e], names(v_mla.effects)[e])
      df_mortality.out=rbind(df_mortality.out, df_out$df_mort.outputs)
      df_prev.out=rbind(df_prev.out, df_out$df_CSprevs)
      l_pop.out[[names(v_mla.effects)[e]]] <- df_out$l_pop_out 
      l_smkparams[[names(v_mla.effects)[e]]] <- df_out$df_smkparams 
}

# # Save outputs
# save(df_mortality.out, df_prev.out, l_pop.out, file=paste0('output/model_run_', date_variable, '.RData'))

source('R/04_visualization.R',echo=TRUE)


# Economic Benefits Calculation based on Value of a Statistical Life-------------------------------------------------------------------------

# --- Parameters ---
gni_avgincome_ksa_2024 <- 35.57  # Average income based on GNI World Bank estimates in thousands USD
gni_avgincome_us_2017 <- 55.98
vsl_usa_2017 <- 9.631            # US value of statistical life in millions USD
vsl_ksa_2024_usd <- vsl_usa_2017 * gni_avgincome_ksa_2024 / gni_avgincome_us_2017 # KSA value of statistical life in 2024 in millions USD

# --- Function to compute discounted benefits ---
compute_discounted_benefit <- function(effect_type, df, vsl_million, discount_rate = 0.03, start_year = 2025, end_year = 2100) {
  df_subset <- subset(df, sex == "Males" & year >= start_year & mla.effect == effect_type,
                      select = c("year", "SADsAverted", "cSADsAverted", "mla.effect"))
  
  df_subset$benefit <- df_subset$SADsAverted * vsl_million * 1e6
  
  years <- df_subset$year
  discount_weights <- 1 / (1 + discount_rate) ^ (years - min(years))
  df_subset$benefit_discounted <- df_subset$benefit * discount_weights
  
  total_usd_billion <- sum(df_subset$benefit_discounted) / 1e9
  total_sar_billion <- total_usd_billion * 3.75
  
  list(
    effect = effect_type,
    usd_billion = total_usd_billion,
    sar_billion = total_sar_billion
  )
}

# --- Apply function to each scenario ---
effects <- c("main", "upper", "lower")
results <- lapply(effects, compute_discounted_benefit, df = df_mortality.out, vsl_million = vsl_ksa_2024_usd)

# --- View results ---
results

# Create a dataframe to examine the calculation more closely
annual_cost_benefit <- subset(df_mortality.out,sex=="Males" & year>=2025 & mla.effect=="main")[,c("year","SADsAverted","cSADsAverted","mla.effect")]
annual_cost_benefit$benefit <- annual_cost_benefit$SADsAverted * vsl_ksa_2024_usd # in millions

d.c = 0.03 # discount rate of 3%
v.d = c( c(1 / (1 + d.c) ^ (0:(length(2025:2100)-1)))) # vector of discount weights

annual_cost_benefit$discount_weights <- v.d 
annual_cost_benefit$benefit_discounted <- annual_cost_benefit$benefit * annual_cost_benefit$discount_weights # apply discounting vector 

sum(annual_cost_benefit$benefit_discounted) # in millions USD
sum(annual_cost_benefit$benefit_discounted)*3.75 # in millions SAR

write.csv(annual_cost_benefit, 'output/annual_cost_benefit.csv',row.names = FALSE)
