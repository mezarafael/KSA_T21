# contains generate_prevs(), calculate_mort(), run_model()function --------
# sex = 'Females'
# m_initAP = m_F.initAP
# m_cessAP = m_F.cessAP
# m_p_mortCS_AP = m_p_F.mortCS_AP
# m_p_mortNS_AP = m_p_F.mortNS_AP
# a_p_mortYSQ_AP = a_p_F.mortYSQ_AP
# df_pop = df_F.pop
# df_prevs2013 = df_F.prevs2013
# name=names(v_mla.effects)[e]
# mla.effect <- v_mla.effects[e]

generate_prevs <- function(startyear, sex, m_initAP, m_cessAP, 
                           m_p_mortNS_AP, m_p_mortCS_AP, a_p_mortYSQ_AP,
                           df_pop,df_prevs2013){
  
  # Matrices to store population by age for current year
  m_NS <- matrix(0,100,calyears)                    # matrix for Never Smokers
  m_CS <- matrix(0,100,calyears)                    # matrix for Current Smokers
  a_FS_YSQ <- array(data = 0,dim = c(100,calyears,40))               # matrix for Former Smokers by YSQ
  
  # Initialize all states using 2013 prevalences
  m_NS[,1] <- df_pop[,1] * df_prevs2013$Neverprev
  m_CS[,1] <- df_pop[,1] * df_prevs2013$Currprev
  # assign former smoking by distribution of years since quit
  a_FS_YSQ[,1,] <- df_pop[,1] * df_prevs2013$Formprev * a_ysq_dist 
  
  # Then fill population with births at age 0
  m_NS[1,2:calyears] <- unlist(df_pop[1,2:calyears])
  
  for (y in 2:length(startyear:endyear)){  # loop over years
    for (a in 2:100){
      m_NS[a,y] <- m_NS[a-1,y-1]*(1-m_initAP[a-1,y-1])*(1-m_p_mortNS_AP[a-1,y-1])
      m_CS[a,y] <- m_NS[a-1,y-1]*m_initAP[a-1,y-1]*(1-m_p_mortNS_AP[a-1,y-1]) + m_CS[a-1,y-1]*(1-m_cessAP[a-1,y-1])*(1-m_p_mortCS_AP[a-1,y-1]) 
      a_FS_YSQ[a,y,1] <- m_CS[a-1,y-1]*(m_cessAP[a-1,y-1])*(1-m_p_mortCS_AP[a-1,y-1]) # Formers in first year after quitting
      for (j in 2:39){ # subsequent years
        a_FS_YSQ[a,y,j] <- a_FS_YSQ[a-1,y-1,j-1]*(1-a_p_mortYSQ_AP[a-1,y-1,j]) 
      }
      a_FS_YSQ[a,y,40] <- a_FS_YSQ[a-1,y-1,39]*(1-a_p_mortYSQ_AP[a-1,y-1,39]) + a_FS_YSQ[a-1,y-1,40]*(1-a_p_mortYSQ_AP[a-1,y-1,40]) 
    }
  }
  # total population of formers
  m.FS_YSQsum <- apply(a_FS_YSQ, 1:2, sum) # sum across YSQ matrices in the array for each cell (row=1=age, col=2=year)
  
  # total simulated population is used as the denominator for simulated prevalence
  m_popAP <- m_NS+m_CS+m.FS_YSQsum
  
  # store prevalence for each age, year, and YSQ category
  m_NSprevAP <- m_NS/m_popAP 
  m_CSprevAP <- m_CS/m_popAP
  m_FSprevAP <- 1 - m_NSprevAP - m_CSprevAP
  a_FSprevAP <- array(0,dim=c(100,calyears,40))
  
  for (j in 1:40){
    a_FSprevAP[,,j] <- a_FS_YSQ[,,j]/m_popAP   
  }
  colnames(m_popAP) <- colnames(m_CSprevAP) <- colnames(m_NSprevAP) <- colnames(a_FSprevAP) <- colnames(m_FSprevAP) <- startyear:endyear
  
  return(list(m_NSprevAP= m_NSprevAP,
              m_CSprevAP= m_CSprevAP, 
              a_FSprevAP= a_FSprevAP, 
              m_FSprevAP= m_FSprevAP)) 
}

#------------------------------------------------------------------------------
# Calculate number of Smoking-Attributable Deaths (SADs), and
# Years of Life Lost (YLL) using output from generate_prev function
#-------------------------------------------------------------------------------

calculate_mort<-function(l_M.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                        a_p_mortYSQ_AP, m_M.NS.LE, df_M.pop){
  
  m_popdist<- as.matrix(df_M.pop) #reformatted census data
  
  # Smoking-attributable deaths associated with current smoking
  m_SAD_AP <- m_popdist[,paste0(v_calyears)]*(l_M.base.prev$m_CSprevAP[,paste0(v_calyears)]*(m_p_M.mortCS_AP[,paste0(v_calyears)]-m_p_M.mortNS_AP[,paste0(v_calyears)]))
  
  # Calculate SADs using former smoking mortality probability by years since quitting (YSQ) with multiple former smoker compartments
  for (j in 1:40){
    m_SAD_AP <- m_SAD_AP+m_popdist[,paste0(v_calyears)]*l_M.base.prev$a_FSprevAP[,paste0(v_calyears),j]*(a_p_M.mortYSQ_AP[,,j]-m_p_M.mortNS_AP[,paste0(v_calyears)])
  }
  df_SAD_AP <- as.data.frame(m_SAD_AP)
  
  v_SADyear=colSums(df_SAD_AP)
  
  ###------------- LIFE YEARS LOST--------------------------
  df_YLL_AP <- df_SAD_AP*m_M.NS.LE #life expectancy of never smokers 
  v_YLLyear <- colSums(df_YLL_AP)
  
  ##---------- output as AC----------------------------------
  m_SAD_AP <- as.matrix(df_SAD_AP)
  m_YLL_AP <- as.matrix(df_YLL_AP)
  
  n <- nrow(m_SAD_AP)
  m <- ncol(m_SAD_AP)
  
  return(list(df_SAD_AP=df_SAD_AP, v_SADyear= v_SADyear, 
              df_YLL_AP= df_YLL_AP,v_YLLyear=v_YLLyear))
}

#-------------------------------------------------------------------------------

run_model <- function(mla.effect, name){

  # RUN STATUS QUO MODEL
  # set initiation and cessation for policy to be the same as the baseline 
  # generate_prevs() determines prevalence and calculate_mort() determines mortality outcomes
  # generate_prevs inputs: (starting cohort, sex, initiation, cessation , policy_initiation, policy_cessation,
  # neversmoker_mortality, currentsmoker_mortality, formersmoker_mortality, mortality_by_YSQ, state_number_of_births)

  sex <- 'Males'
  l_M.base.prev <- generate_prevs(startyear, sex, m_M.initAP, m_M.cessAP, 
                                  m_p_M.mortNS_AP,m_p_M.mortCS_AP, a_p_M.mortYSQ_AP, 
                                  df_M.pop, df_M.prevs2013)
  
  l_M.base.mort <- calculate_mort(l_M.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                               a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.pop)
  
  sex <- 'Females'
  l_F.base.prev <- generate_prevs(startyear, sex, m_F.initAP, m_F.cessAP,
                                  m_p_M.mortNS_AP, m_p_M.mortCS_AP, a_p_M.mortYSQ_AP, 
                                  df_F.pop, df_F.prevs2013)
  
  l_F.base.mort <- calculate_mort(l_F.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP,
                               a_p_M.mortYSQ_AP, m_F.NS.LE, df_F.pop)
  
  
  df_annualSADs <-as.data.frame(rbind(cbind(l_F.base.mort$v_SADyear,"Females",startyear:endyear),
                        cbind(l_M.base.mort$v_SADyear,"Males",startyear:endyear),
                        cbind(l_F.base.mort$v_SADyear+l_M.base.mort$v_SADyear,"Both",startyear:endyear)))
  colnames(df_annualSADs) <- c("SADs","sex","year")
  df_annualSADs$SADs <- as.numeric(df_annualSADs$SADs)
  df_annualSADs$year <- as.numeric(df_annualSADs$year)
  
  # ggplot(data=df_annualSADs)+geom_line(aes(x=year, y=SADs, colour=sex)) +scale_y_continuous(breaks=seq(0,70000,5000))
  
  # ggplot(x=startyear:endyear,y=colSums(df_F.pop))
  # plot(colSums(df_M.pop))
  # lines(colSums(df_F.pop))

  # pop<-as.data.frame(rbind(cbind(colSums(df_F.pop),"F",startyear:endyear),cbind(colSums(df_M.pop),"M",startyear:endyear)))
  # ggplot(pop)+geom_line(aes(x=as.numeric(pop$V3),y=as.numeric(pop$V1),colour=pop$V2))
  
  m_MLAeffectsAP <- matrix(1,100,calyears)
  colnames(m_MLAeffectsAP) <- startyear:endyear

  ## APPLY POLICY EFFECTS TO BASELINE INITIATION Starting in Policy Year 2025-2200
  for (age in v_policy.ages) {
    m_MLAeffectsAP[age, (policyyear-startyear+1):calyears] <- 1 - mla.effect
  }

  m_F.init.policy_AP <- m_MLAeffectsAP*m_F.initAP
  m_M.init.policy_AP <- m_MLAeffectsAP*m_M.initAP
  
  ## APPLY POLICY EFFECTS TO BASELINE INITIATION Starting in Policy Year 2025-2200
  for (age in v_policy.ages) {
    m_MLAeffectsAP[age, (policyyear-startyear+1):calyears] <- 1 - mla.effect
  }
  
  ### RUN POLICY SCENARIOS   
  l_M.base.mort <- calculate_mort(l_M.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                                  a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.pop)
  
  sex <- 'Males'
  l_M.policy.prev <- generate_prevs(startyear, sex, m_M.init.policy_AP, m_M.cessAP,
                                    m_p_M.mortNS_AP,m_p_M.mortCS_AP, a_p_M.mortYSQ_AP, df_M.pop, df_M.prevs2013)
  
  l_M.policy.mort <- calculate_mort(l_M.policy.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                                    a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.pop)
  
  sex <- 'Females'
  l_F.policy.prev <- generate_prevs(startyear, sex, m_F.init.policy_AP, m_F.cessAP,
                                    m_p_M.mortNS_AP,m_p_M.mortCS_AP, a_p_M.mortYSQ_AP, df_F.pop, df_F.prevs2013)
  
  l_F.policy.mort <- calculate_mort(l_F.policy.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                                    a_p_M.mortYSQ_AP, m_F.NS.LE, df_F.pop)
  
  #------------------- format prev for outputting -----------------------------------
  df_CSprevs <- NULL
  
  # age groups to loop through
  v_minage <- c(15, 15, 25, 45, 65)
  v_maxage <- c(99, 24, 44, 64, 99)
  
  m_M.CSprev <- l_M.policy.prev$m_CSprevAP
  m_F.CSprev <- l_F.policy.prev$m_CSprevAP
  
  for (i in c(1:5)){
    minage <- v_minage[i]
    maxage <- v_maxage[i]

    # Create population matrices
    m.M.pop_AP <- as.matrix(df_M.pop[,paste0(startyear:endyear)])
    m.F.pop_AP <- as.matrix(df_F.pop[,paste0(startyear:endyear)])
    
    # Calculate prevalence for men
    v_M.prev.minmax <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) / colSums(m.M.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate prevalence for women
    v_F.prev.minmax <- colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ]) / colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate combined prevalence for both men and women
    v_numerator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ])
    v_denominator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    v_B.prev.minmax <- v_numerator / v_denominator
    
    # Combine data into a single data frame for the current age group
    df_CSprev_temp <- as.data.frame(rbind(
      cbind(v_M.prev.minmax, "Males"),
      cbind(v_F.prev.minmax, "Females"),
      cbind(v_B.prev.minmax, "Both")
    ))
    
    # Set column names
    colnames(df_CSprev_temp) <- c("prev", "sex")
    
    # Add additional columns
    df_CSprev_temp$age <- paste0(minage, ".", maxage)
    df_CSprev_temp$year <- rep(names(v_M.prev.minmax), 3)
    df_CSprev_temp$mla.effect <- name
    
    # Combine with the final data frame
    df_CSprevs <- rbind(df_CSprevs, df_CSprev_temp)
  }
  
  df_CSprevs$prev<- as.numeric(df_CSprevs$prev)
  df_CSprevs$year<- as.numeric(df_CSprevs$year)
  
  #--------------format prev for output ----------------------------------------
  #-----------------------------------------------------------------------------
  
  m_M.smokers <- l_M.policy.prev$m_CS
  m_F.smokers <- l_F.policy.prev$m_CS
  m_M.popAP <- l_M.policy.prev$m_popAP
  m_F.popAP <- l_F.policy.prev$m_popAP
  m_M.CSprevAP <- cbind(l_M.policy.prev$m_CSprevAP, 'Males')
  m_F.CSprevAP <- cbind(l_F.policy.prev$m_CSprevAP, 'Females')
  
  l_pop_out <- list(
    mla.effect=mla.effect, 
    m_M_smokers = l_M.policy.prev$m_CS,
    m_F_smokers = l_F.policy.prev$m_CS,
    m_M_popAP = l_M.policy.prev$m_popAP,
    m_F_popAP = l_F.policy.prev$m_popAP
  )
  
  #---------------format policy YLL and SADs for outputting --------------------
  #annual sum YLL
  v_M.YLLyear <- l_M.policy.mort$v_YLLyear
  v_F.YLLyear <- l_F.policy.mort$v_YLLyear
  v_B.YLLyear <- v_M.YLLyear + v_F.YLLyear
  #cumulative sum YLL
  v_M.cYLL <- cumsum(v_M.YLLyear)
  v_F.cYLL <- cumsum(v_F.YLLyear)
  v_B.cYLL <- cumsum(v_B.YLLyear)
  
  #annual sum SAD
  v_M.SADyear <- l_M.policy.mort$v_SADyear
  v_F.SADyear <- l_F.policy.mort$v_SADyear
  v_B.SADyear <- v_M.SADyear+ v_F.SADyear
  #cumulative sum SAD
  v_M.cSAD <- cumsum(v_M.SADyear)
  v_F.cSAD <- cumsum(v_F.SADyear)
  v_B.cSAD <- cumsum(v_B.SADyear)
  
  #---------------- LYG using YSQ ----------------------------------------------------------------------------
  
  df_M.LYG_AP <- l_M.base.mort$df_YLL_AP - l_M.policy.mort$df_YLL_AP
  df_F.LYG_AP <- l_F.base.mort$df_YLL_AP - l_F.policy.mort$df_YLL_AP
  #annual sum LYG
  v_M.LYGyear <- colSums(df_M.LYG_AP)
  v_F.LYGyear <- colSums(df_F.LYG_AP)
  v_B.LYGyear <- v_M.LYGyear + v_F.LYGyear
  #cumulative sum LYG
  v_M.cLYG <- cumsum(v_M.LYGyear)
  v_F.cLYG <- cumsum(v_F.LYGyear)
  v_B.cLYG <- cumsum(v_B.LYGyear)
  
  #annual sum SADs averted 
  v_M.SADs_averted <- l_M.base.mort$v_SADyear - l_M.policy.mort$v_SADyear
  v_F.SADs_averted <- l_F.base.mort$v_SADyear - l_F.policy.mort$v_SADyear
  v_B.SADs_averted <- v_M.SADs_averted + v_F.SADs_averted
  #cumulative sum SADs averted
  v_M.cSADs_averted <- cumsum(v_M.SADs_averted)
  v_F.cSADs_averted <- cumsum(v_F.SADs_averted)  
  v_B.cSADs_averted <- cumsum(v_B.SADs_averted)
 
  ##---combine mortality outputs------------------------------------------------------
  m_M.mortout <- cbind(v_M.YLLyear, v_M.cYLL, v_M.SADyear, v_M.cSAD, 
                    v_M.LYGyear, v_M.cLYG, v_M.SADs_averted,
                    v_M.cSADs_averted,startyear:endyear, 'Males')
  
  m_F.mortout <- cbind(v_F.YLLyear, v_F.cYLL, v_F.SADyear, v_F.cSAD, 
                      v_F.LYGyear, v_F.cLYG, v_F.SADs_averted,
                      v_F.cSADs_averted, startyear:endyear, 'Females')
  
  m_B.mortout <- cbind(v_B.YLLyear, v_B.cYLL, v_B.SADyear, v_B.cSAD, 
                      v_B.LYGyear, v_B.cLYG, v_B.SADs_averted,
                      v_B.cSADs_averted, startyear:endyear, 'Both')
  
  df_mort.outputs <- as.data.frame(rbind(m_M.mortout,m_F.mortout,m_B.mortout))
  colnames(df_mort.outputs) <- c('YLL','cYLL','SADs', 'cSAD', 'LYG', 'cLYG',
                              'SADsAverted','cSADsAverted','year', 'sex' )
  df_mort.outputs$mla.effect <- name
  df_mort.outputs <- df_mort.outputs %>% mutate(across(c('YLL','cYLL','SADs',
                                                         'cSAD', 'LYG', 'cLYG', 
                                                         'SADsAverted','cSADsAverted',
                                                         'year'), as.numeric))

  # store dataframe with smoking initiation parameters during the policy year
  df_smkparams <- as.data.frame(c(m_F.init.policy_AP[,paste0(policyyear)], m_F.initAP[,paste0(policyyear)], 
                        m_M.init.policy_AP[,paste0(policyyear)], m_M.initAP[,paste0(policyyear)]))
  df_smkparams$sex <- c(rep("females",200),rep("males",200))
  df_smkparams$scenario <- c(rep("policy",100),rep("baseline",100),rep("policy",100),rep("baseline",100))
  df_smkparams$age <- rep(seq(0:99)-1,4)
  colnames(df_smkparams)[1] <- paste0("prob",policyyear)
  
  return(list(df_mort.outputs= df_mort.outputs, l_pop_out=l_pop_out, df_CSprevs=df_CSprevs, df_smkparams=df_smkparams))
}
