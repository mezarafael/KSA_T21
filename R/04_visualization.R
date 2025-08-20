#load('output/model_output_10.09.24.RData')  #load only needed if running file separately

## Smoking parameters
init_fig <- ggplot(data=l_smkparams$main) + 
  geom_line(aes(x=age,y = prob2025,colour = sex,linetype = scenario)) + 
  theme_light()+
  labs(title=paste0("Initiation probabilities in 2025"))+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

cess2025 <- as.data.frame(c(m_M.cessAP[,policyyear-startyear+1], m_F.cessAP[,policyyear-startyear+1]))
cess2025$sex <- c(rep("males",100),rep("females",100))
cess2025$age <- rep(seq(0:99)-1,2)
colnames(cess2025)[1] <-"prob2025"
cess_fig <- ggplot(data=cess2025) + 
  geom_line(aes(x=age,y = prob2025,colour = sex)) + 
  theme_light()+
  labs(title=paste0("Cessation probabilities in 2025"))+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

mort2025 <- as.data.frame(c(m_p_M.mortNS_AP[,policyyear-startyear+1],
                            m_p_M.mortCS_AP[,policyyear-startyear+1],
                            a_p_M.mortYSQ_AP[,policyyear-startyear+1,10],
                            a_p_M.mortYSQ_AP[,policyyear-startyear+1,20],
                            m_p_F.mortNS_AP[,policyyear-startyear+1],
                            m_p_F.mortCS_AP[,policyyear-startyear+1],
                            a_p_F.mortYSQ_AP[,policyyear-startyear+1,10],
                            a_p_F.mortYSQ_AP[,policyyear-startyear+1,20]))
mort2025$age <- rep(seq(0:99)-1,8)
mort2025$sex <- c(rep("males",400),rep("females",400))
mort2025$smkstat <- rep(c(rep("never",100),rep("current",100),rep("former_10ysq",100),rep("former_20ysq",100)),2)
colnames(mort2025)[1] <-"prob2025"
mort_fig <- ggplot(data=mort2025) + 
  geom_line(aes(x=age,y = prob2025,colour = smkstat)) + 
  theme_light()+
  labs(title=paste0("Mortality probabilities in 2025")) +facet_wrap("sex")

mort_fig_log <- ggplot(data=mort2025) + 
  geom_line(aes(x=age,y = log(prob2025),colour = smkstat)) + 
  theme_light()+
  labs(title=paste0("Mortality probabilities in 2025, log scale")) +facet_wrap("sex")

## Prevalence figure
prev_projections<-function(agegrp, whichsex){
  fig1 <- ggplot() + 
    geom_line(data=subset(df_prev.out, age==agegrp & sex==whichsex), aes(x=year, y=prev*100, color=mla.effect), lwd = 0.6) + 
    geom_pointrange(data=subset(df_surveydata, age==agegrp & sex==whichsex),
                    aes(x=year,y=prev*100, shape=survey,ymin=lower*100,ymax=upper*100))+
    scale_y_continuous(name="Prevalence (%)",limits=c(0,35),seq(0,35,5)) +
    scale_x_continuous(name="Year",limits=c(2010,2100),breaks=seq(2010,2100,10)) +
    theme_light()+ 
    labs(title= paste0("Smoking prevalence, ",whichsex," ages ",agegrp))+
    theme(legend.text=element_text(size=9),
          legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",
          legend.title=element_blank(),
          text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)))
  return(fig1)
}

## Smoking Attributable Deaths Averted figure
SADs_averted_fig <- function(whichsex) {
  model <- subset(df_mortality.out, sex == whichsex)
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cSADsAverted, color = mla.effect), lwd = 0.6) +
    scale_y_continuous(name = "SADs averted", 
                       labels = scales::comma, 
                       limits = c(0, 15000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("Cumulative SADs averted, ", whichsex))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title=element_blank(),
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

## Annual Smoking Attributable Deaths  figure
annual_SADs_fig <- ggplot(data = subset(df_mortality.out, mla.effect == "baseline")) + 
    geom_line(aes(x = year, y = SADs, color = sex), lwd = 0.6) +
    scale_y_continuous(name = "Annual SADs", 
                       labels = scales::comma, 
                       limits = c(0, 14000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(startyear, endyear), 
                       breaks = seq(startyear, endyear, 5)) +
    labs(title = paste0("Annual smoking-attributable deaths"))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_blank(),
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))

# Population projections
pop_proj <- as.data.frame(rbind(cbind(colSums(df_F.pop),"Females",startyear:endyear),cbind(colSums(df_M.pop),"Males",startyear:endyear)))
colnames(pop_proj) <- c("popest","sex","year")
pop_proj$popest <- as.numeric(pop_proj$popest)
pop_proj$year <- as.numeric(pop_proj$year)

pop_fig <- ggplot(data=pop_proj)+
  geom_line(aes(x=year,y=popest,colour=sex))+
  scale_y_continuous(name = "Total population", 
                     labels = scales::comma, 
                     limits = c(0, 38000000), 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "Year", 
                     limits = c(startyear, endyear), 
                     breaks = seq(startyear, endyear, 5)) +
  labs(title = "Annual population")+
  theme_light() + 
  guides(color = guide_legend(nrow = 3)) +
  guides(linetype = guide_legend(nrow = 3)) +
  theme(legend.title = element_text(size = 9), 
        text = element_text(size = 10), 
        axis.text.x = element_text(angle = 60, hjust = 1))

## Life Years Gained
LYG_fig <- function(whichsex) {
  model <- subset(df_mortality.out, sex == whichsex)
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cLYG, color = mla.effect), lwd = 0.6) +
    scale_y_continuous(name="Life Years Gained",labels=scales::comma,limits=c(0, 300000),breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("Cumulative life-years gained, ", whichsex))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_blank(), 
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

pdf(file=paste0('output/T21_results_', date_variable, '_',namethisrun,'.pdf'), width=11, height=8.5, onefile=TRUE)
ggarrange(init_fig,cess_fig)
mort_fig
mort_fig_log
ggarrange(prev_projections(15.99,"Males"),prev_projections(15.99,"Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
ggarrange(prev_projections(15.24,"Males"),prev_projections(15.24,"Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
ggarrange(prev_projections(25.44,"Males"),prev_projections(25.44,"Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
ggarrange(prev_projections(45.64,"Males"),prev_projections(45.64,"Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
ggarrange(prev_projections(65.99,"Males"),prev_projections(65.99,"Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
annual_SADs_fig
pop_fig
ggarrange(SADs_averted_fig("Males"), SADs_averted_fig("Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
ggarrange(LYG_fig("Males"),LYG_fig("Females"),ncol=2, nrow=1, common.legend=TRUE, legend='top')
dev.off()


## Smoking parameters
init_figM <- ggplot(data=subset(l_smkparams$main, sex=='males')) + 
  geom_line(aes(x=age,y = prob2025,linetype = scenario)) + 
  theme_light()+
  labs(title=paste0("Initiation probabilities in 2025"))+
  scale_y_continuous(name = "Annual probability")+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

cess_figM <- ggplot(data=subset(cess2025,sex=='males')) + 
  geom_line(aes(x=age,y = prob2025)) + 
  theme_light()+
  labs(title=paste0("Cessation probabilities in 2025"))+
  scale_y_continuous(name = "Annual probability")+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

mort_figM <- ggplot(data=subset(mort2025,sex=='males')) + 
  geom_line(aes(x=age,y = prob2025,colour = smkstat)) + 
  theme_light()+
  scale_y_continuous(name = "Annual probability")+
  labs(title=paste0("Mortality probabilities in 2025"))+
  theme(legend.title = element_blank())

mort_fig_logM <- ggplot(data=subset(mort2025,sex=='males')) + 
  geom_line(aes(x=age,y = prob2025,colour = smkstat)) + 
  theme_light()+
  scale_y_continuous(name = "Annual probability (log scale)")+
  scale_y_continuous(name = "Annual probability (log scale)")+
  scale_y_log10()+
  labs(title=paste0("Mortality probabilities in 2025, log scale")) +
  theme(legend.title = element_blank())

## Annual Smoking Attributable Deaths  figure
annual_SADs_figM <- ggplot(data = subset(df_mortality.out, sex == "Males")) + 
  geom_line(aes(x = year, y = SADs, color = mla.effect), lwd = 0.6) +
  scale_y_continuous(name = "Smoking-attributable deaths", 
                     labels = scales::comma, 
                     limits = c(0, 10000), 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "Year", 
                     limits = c(startyear, endyear), 
                     breaks = seq(startyear, endyear, 5)) +
  labs(title = paste0("Annual smoking-attributable deaths, males"))+
  theme_light() + 
  guides(color = guide_legend(nrow = 3)) +
  guides(linetype = guide_legend(nrow = 3)) +
  theme(legend.title =  element_blank(),  
        text = element_text(size = 10), 
        axis.text.x = element_text(angle = 60, hjust = 1))

pop_figM <- ggplot(data=subset(pop_proj,sex=='Males'))+
  geom_line(aes(x=year,y=popest))+
  scale_y_continuous(name = "Total population", 
                     labels = scales::comma, 
                     limits = c(0, 38000000), 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "Year", 
                     limits = c(startyear, endyear), 
                     breaks = seq(startyear, endyear, 5)) +
  labs(title = "Annual male population, United Nations projections")+
  theme_light() + 
  guides(color = guide_legend(nrow = 3)) +
  guides(linetype = guide_legend(nrow = 3)) +
  theme(legend.title = element_text(size = 9), 
        text = element_text(size = 10), 
        axis.text.x = element_text(angle = 60, hjust = 1))
  
pdf(file=paste0('output/fig1_males_initcess.pdf'), width=6, height=4, onefile=TRUE)
ggarrange(init_figM,cess_figM,common.legend=TRUE, legend='top')
dev.off()

pdf(file=paste0('output/fig2_males_mort.pdf'), width=8, height=4, onefile=TRUE)
ggarrange(mort_figM,mort_fig_logM,common.legend=TRUE, legend='right')
dev.off()

pdf(file=paste0('output/fig3_males_prev.pdf'), width=6, height=4, onefile=TRUE)
ggarrange(prev_projections(15.99,"Males"),ncol=1,nrow=1,legend='right')
dev.off()

pdf(file=paste0('output/fig4_SADsavert_LYG.pdf'), width=8, height=4, onefile=TRUE)
ggarrange(SADs_averted_fig("Males"), LYG_fig("Males"),ncol=2, nrow=1, common.legend=TRUE, legend='right')
dev.off()

pdf(file=paste0('output/fig5_prev_by_age.pdf'), width=8, height=6, onefile=TRUE)
ggarrange(prev_projections(15.24,"Males"),
          prev_projections(25.44,"Males"),
          prev_projections(45.64,"Males"),
          prev_projections(65.99,"Males"),
          ncol=2, nrow=2, common.legend=TRUE, legend='top')
dev.off()

pdf(file=paste0('output/fig6_annualSADs.pdf'), width=6, height=4, onefile=TRUE)
annual_SADs_figM
dev.off()

pdf(file=paste0('output/fig7_popsize.pdf'), width=6, height=4, onefile=TRUE)
pop_figM
dev.off()