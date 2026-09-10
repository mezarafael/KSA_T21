#load('output/model_output_XXXXX.RData')  #load only needed if running file separately

## Smoking parameters ---------------------------------------------------
init_figM <- ggplot(data=subset(l_smkparams$main, sex=='Males')) + 
  geom_line(aes(x=Age,y = prob2026,linetype = scenario)) + 
  theme_light()+
  labs(title=paste0("A. Initiation"))+
  scale_y_continuous(name = "Annual probability", n.breaks=10)+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "center"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

cess2026 <- as.data.frame(m_M.cessAP[,policyyear-startyear+1])
cess2026$sex <- rep("Males",100)
cess2026$Age <- seq(0:99)-1
colnames(cess2026)[1] <-"prob2026"
cess_figM <- ggplot(data=subset(cess2026,sex=='Males')) + 
  geom_line(aes(x=Age,y = prob2026)) + 
  theme_light()+
  labs(title=paste0("B. Cessation"))+
  scale_y_continuous(name = "Annual probability",n.breaks=10)+
  theme(legend.text=element_text(size=9),
        legend.position = c(.95, .95),legend.justification = c("right", "center"), legend.box.just = "right",legend.title=element_blank(),
        text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
        legend.background = element_rect(fill=alpha('white', 0.4)))

pdf(file=paste0('output/fig1_males_initcess.pdf'), width=8, height=4, onefile=TRUE)
ggarrange(init_figM,cess_figM,common.legend=TRUE, legend='right')
dev.off()

# Mortality figures -------------------------------------------------------
mort2026 <- as.data.frame(c(m_p_M.mortNS_AP[,policyyear-startyear+1],
                            m_p_M.mortCS_AP[,policyyear-startyear+1],
                            a_p_M.mortYSQ_AP[,policyyear-startyear+1,10],
                            a_p_M.mortYSQ_AP[,policyyear-startyear+1,20]))
mort2026$Age <- rep(seq(0:99)-1,4)
mort2026$sex <- rep("Males",400)
mort2026$smkstat <- c(rep("Never smoking",100),rep("Current smoking",100),rep("Former smoking (10 ysq)",100),rep("Former smoking (20 ysq)",100))
colnames(mort2026)[1] <-"prob2026"
mort_figM <- ggplot(data=subset(mort2026,sex=='Males')) + 
  geom_line(aes(x=Age,y = prob2026,colour = smkstat)) + 
  theme_light()+
  scale_y_continuous(name = "Annual probability")+
  labs(title=paste0("A. Linear scale"))+
  theme(legend.title = element_blank())

mort_fig_logM <- ggplot(data=subset(mort2026,sex=='Males')) + 
  geom_line(aes(x=Age,y = prob2026,colour = smkstat)) + 
  theme_light()+
  scale_y_log10(name = "Annual probability")+
  labs(title=paste0("B. Logarithmic scale")) +
  theme(legend.title = element_blank())

pdf(file=paste0('output/fig2_males_mort.pdf'), width=8, height=4, onefile=TRUE)
ggarrange(mort_figM,mort_fig_logM,common.legend=TRUE, legend='right')
dev.off()


# Prevalence figures ------------------------------------------------------

df_prev.out_MAIN <- subset(df_prev.out, mla.effect=="main")
df_prev.out_MAIN$lower <- subset(df_prev.out, mla.effect=="lower")$prev
df_prev.out_MAIN$upper <- subset(df_prev.out, mla.effect=="upper")$prev
df_prev.out$mla.effect[df_prev.out$mla.effect=="baseline"] <- "Baseline"
df_prev.out$mla.effect[df_prev.out$mla.effect=="main"] <- "T21"

prev_projections<-function(agegrp, whichsex){
  fig1 <- ggplot() + 
    geom_line(data=subset(df_prev.out, age==agegrp & sex==whichsex & mla.effect!="lower" & mla.effect!="upper" ), 
              aes(x=year, y=prev*100, linetype=mla.effect), lwd = 0.6) + 
    geom_ribbon(data=subset(df_prev.out_MAIN, age==agegrp & sex==whichsex), 
                aes(x=year, ymin=lower*100,ymax=upper*100),fill="lightblue", alpha=0.5)+
    geom_pointrange(data=subset(df_surveydata, age==agegrp & sex==whichsex),
                    aes(x=year,y=prev*100, shape=survey,ymin=lower*100,ymax=upper*100))+
    scale_y_continuous(name="Prevalence (%)",limits=c(0,35),seq(0,35,5)) +
    scale_x_continuous(name="Year",limits=c(2010,2100),breaks=seq(2010,2100,10)) +
    theme_light()+ 
    labs(title= paste0("Smoking prevalence, ",whichsex," ages ",agegrp))+
    theme(legend.text=element_text(size=9),
          legend.position = c(.95, .95),legend.justification = c("right", "center"), legend.box.just = "right",
          legend.title=element_blank(),
          text = element_text(size=9),axis.text.x=element_text(angle=60, hjust=1),
          legend.background = element_rect(fill=alpha('white', 0.4)))
  return(fig1)
}

pdf(file=paste0('output/fig3_males_prev.pdf'), width=6, height=4.5, onefile=TRUE)
ggarrange(prev_projections(15.99,"Males"),ncol=1,nrow=1,legend='right')
dev.off()

pdf(file=paste0('output/fig5_prev_by_age.pdf'), width=8, height=6, onefile=TRUE)
ggarrange(prev_projections(15.24,"Males"),
          prev_projections(25.44,"Males"),
          prev_projections(45.64,"Males"),
          prev_projections(65.99,"Males"),
          ncol=2, nrow=2, common.legend=TRUE, legend='top')
dev.off()


# Smoking Attributable Deaths Averted & Life years gained-------------------------------------

df_mortality.out_cSADsAverted <- subset(df_mortality.out, mla.effect=="main")
df_mortality.out_cSADsAverted$lower <- subset(df_mortality.out, mla.effect=="lower")$cSADsAverted
df_mortality.out_cSADsAverted$upper <- subset(df_mortality.out, mla.effect=="upper")$cSADsAverted

df_mortality.out$mla.effect[df_mortality.out$mla.effect=="baseline"] <- "Baseline"
df_mortality.out$mla.effect[df_mortality.out$mla.effect=="main"] <- "T21"

df_mortality.out_LYG <- subset(df_mortality.out, mla.effect=="T21")
df_mortality.out_LYG$lower <- subset(df_mortality.out, mla.effect=="lower")$cLYG
df_mortality.out_LYG$upper <- subset(df_mortality.out, mla.effect=="upper")$cLYG

SADs_averted_fig <- function(whichsex) {
  model <- subset(df_mortality.out, sex == whichsex & mla.effect!='lower' &  mla.effect!='upper')
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cSADsAverted, linetype = mla.effect), lwd = 0.6) +
    geom_ribbon(data=df_mortality.out_cSADsAverted, aes(x=year, ymin=lower,ymax=upper),fill="lightblue", alpha=0.5)+
    scale_y_continuous(name = "Smoking-attributable deaths averted", 
                       labels = scales::comma, 
                       limits = c(0, 10000), 
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("A. Smoking-attributable deaths averted"))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title=element_blank(),
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

## Life Years Gained
LYG_fig <- function(whichsex) {
  model <- subset(df_mortality.out, sex == whichsex & mla.effect!='lower' &  mla.effect!='upper')
  fig <- ggplot(data = model) + 
    geom_line(aes(x = year, y = cLYG, linetype = mla.effect), lwd = 0.6) +
    geom_ribbon(data=df_mortality.out_LYG, aes(x=year, ymin=lower,ymax=upper),fill="lightblue", alpha=0.5)+
    
    scale_y_continuous(name="Life-years gained",labels=scales::comma,limits=c(0, 250000),breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2025, 2100, 5)) +
    labs(title = paste0("B. Life-years gained"))+
    theme_light() + 
    guides(color = guide_legend(nrow = 3)) +
    guides(linetype = guide_legend(nrow = 3)) +
    theme(legend.title = element_blank(), 
          text = element_text(size = 10), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(fig)
}

pdf(file=paste0('output/fig4_SADsavert_LYG.pdf'), width=8, height=4, onefile=TRUE)
ggarrange(SADs_averted_fig("Males"), LYG_fig("Males"),ncol=2, nrow=1, common.legend=TRUE, legend='right')
dev.off()

# Annual SADs
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

pdf(file=paste0('output/fig6_annualSADs.pdf'), width=6, height=4, onefile=TRUE)
annual_SADs_figM
dev.off()

## Population projections ------------------------------------------------
pop_proj <- as.data.frame(rbind(cbind(colSums(df_M.pop),"Males",startyear:endyear)))
colnames(pop_proj) <- c("popest","sex","year")
pop_proj$popest <- as.numeric(pop_proj$popest)
pop_proj$year <- as.numeric(pop_proj$year)

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
  
pdf(file=paste0('output/fig7_popsize.pdf'), width=6, height=4, onefile=TRUE)
pop_figM
dev.off()