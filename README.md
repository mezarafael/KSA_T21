## The Kingdom of Saudi Arabia Tobacco Control Policy Model

**1. Open 'R/03_main_analysis.R' to perform the analysis.**

**2. Set working directory to folder of your choosing.** 

**3. Source 'R/01_model_inputs.R'**
- Installs and loads necessary packages
- Load country-specific data on mortality, smoking initiation and cessation, Census population estimates, and life expectancies ages 0-99 (rows), years 2010-2100 (columns).
- Load initial never smoking, current smoking, and former smoking prevalences for ages 0-99 by sex in the year 2013 (initial year). 
- Load smoking prevalence data for 2019 World Health Survey (WHS), 2019 Global Adult Tobacco Survey (GATS), and 2013 Saudi Health Interview Survey (SHIS) by age group and sex
- Set model parameters for the effects of Tobacco 21 policies by age (v_mla.effects)

**3. Source 'R/02_model_functions.R'**
- Create model function to generate prevalence estimates, calculate smoking-attributable mortality, calculate life-years lost
- Create model function that uses Tobacco 21 policy effect (mla.effect) to run baseline and policy scenarios for males and females and generate mortality, prevalence outcomes

**4. Provide a name for your model run 'aug2025'**
- Run the for loop to generate outcomes using each of the policy effect parameters in 'v_mla.effects'.

**5. Source 'R/04_visualization.R'**
- Create a PDF of figures for review. This PDF will be saved in the 'output' folder.

**6. Calculate economic benefits based on value of a statistical life**
- Use GNI World Bank estimates for average income
- Convert the value of a statistical life for the USA in USD in 2017 to KSA in USD for 2024.
- Calculate benefits discounted at 3%
- Save this as a .csv file in your 'output' folder.
