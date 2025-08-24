setwd("C:/Users/tamee/Downloads/new_runs/cpc")
cpc<- read.csv("tp_s6_8.csv")
cpc_avg<- (data.frame(apply(cpc[,1:5],1,mean)))
colnames(cpc_avg)<- "TP"
cpc_avg$Timeline <- "Observed"
cpc_avg$year<- seq(1979,2014,1)
access<- rep("ACCESS",36)
ec<- rep("EC_EARTH",36)
miroc<- rep("MIROC",36)
mpi<- rep("MPI",36)
mri<- rep("MRI",36)
model<- c(access,ec,miroc,mpi,mri)
cpc_all<- rbind(cpc_avg,cpc_avg,cpc_avg,cpc_avg,cpc_avg)
cpc_all$Model<- model
cpc_all$Diff<- rep(0,180)

#Access
setwd("C:/Users/tamee/Downloads/new_runs/access/historical")
access_hist<- read.csv("tp_s6_8.csv")
access_hist_avg<- (data.frame(apply(access_hist[,1:5],1,mean)))
colnames(access_hist_avg)<- "TP"
access_hist_avg$Timeline <- rep("Historical",36)
access_hist_avg$year<- seq(1979,2014,1)
access_hist_avg$Diff<- rep(0,36)
intercept_access<- rep(mean(access_hist_avg$TP),2)
setwd("C:/Users/tamee/Downloads/new_runs/access/ssp370")
access_ssp370<- read.csv("tp_s6_8.csv")
access_ssp370_avg<- (data.frame(apply(access_ssp370[,1:5],1,mean)))
colnames(access_ssp370_avg)<- "TP"
hist_access<- data.frame(mean(access_hist_avg$TP))
colnames(hist_access)<- "TP"
access_ssp370_avg<- rbind(hist_access,access_ssp370_avg)
access_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
access_ssp370_avg$year<- seq(2014,2100,1)
yr<- seq(1,87,1)
access_ssp370_avg$Diff<- (access_ssp370_avg$TP -rep(hist_access$TP,87)) 
acess_fit_370<- lm(access_ssp370_avg$Diff ~ yr-1)
access_slope_370<- acess_fit_370$coefficients[1]
setwd("C:/Users/tamee/Downloads/new_runs/access/ssp585")
access_ssp585<- read.csv("tp_s6_8.csv")
access_ssp585_avg<- (data.frame(apply(access_ssp585[,1:5],1,mean)))
colnames(access_ssp585_avg)<- "TP"
access_ssp585_avg<- rbind(hist_access,access_ssp585_avg)
access_ssp585_avg$Timeline<- rep("SSP5-8.5",87)
access_ssp585_avg$year<- seq(2014,2100,1)
access_ssp585_avg$Diff<- (access_ssp585_avg$TP -rep(hist_access$TP,87)) 
acess_fit_585<- lm(access_ssp585_avg$Diff ~ yr-1)


access_slope_585<- acess_fit_585$coefficients[1]
access_full<- rbind(access_hist_avg,access_ssp370_avg,access_ssp585_avg)
access_full$Model<- rep("ACCESS",210) 

#Ec_earth
setwd("C:/Users/tamee/Downloads/new_runs/ec_earth/historical")
ec_hist<- read.csv("tp_s6_8.csv")
ec_hist_avg<- data.frame(apply(ec_hist[,1:5],1,mean))
colnames(ec_hist_avg)<- "TP"
ec_hist_avg$Timeline<- rep("Historical",36)
ec_hist_avg$year<- seq(1979,2014,1)
ec_hist_avg$Diff<- rep(0,36)
intercept_ec<- rep(mean(ec_hist_avg$TP),2)
setwd("C:/Users/tamee/Downloads/new_runs/ec_earth/ssp370")
ec_ssp370<- read.csv("tp_s6_8.csv")
ec_ssp370_avg<- (data.frame(apply(ec_ssp370[,1:5],1,mean)))
colnames(ec_ssp370_avg)<- "TP"
hist_ec<- data.frame(mean(ec_hist_avg$TP))
colnames(hist_ec)<- "TP"
ec_ssp370_avg<- rbind(hist_ec,ec_ssp370_avg)
ec_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
ec_ssp370_avg$year<- seq(2014,2100,1)
ec_ssp370_avg$Diff<- (ec_ssp370_avg$TP -rep(hist_ec$TP,87))
ec_fit_370<- lm(ec_ssp370_avg$Diff ~ yr-1)
ec_slope_370<- ec_fit_370$coefficients[1]
setwd("C:/Users/tamee/Downloads/new_runs/ec_earth/ssp585")
ec_ssp585<- read.csv("TP_s6_8.csv")
ec_ssp585_avg<- (data.frame(apply(ec_ssp585[,1:5],1,mean)))
colnames(ec_ssp585_avg)<- "TP"
ec_ssp585_avg<- rbind(hist_ec,ec_ssp585_avg)
ec_ssp585_avg$Timeline<- rep("SSP5-8.5",87)
ec_ssp585_avg$year<- seq(2014,2100,1)
ec_ssp585_avg$Diff<- (ec_ssp585_avg$TP -rep(hist_ec$TP,87))
ec_fit_585<- lm(ec_ssp585_avg$Diff ~ yr-1)
ec_slope_585<- ec_fit_585$coefficients[1]
ec_full<- rbind(ec_hist_avg,ec_ssp370_avg,ec_ssp585_avg)
ec_full$Model<- rep("EC_EARTH",210)

#MIROC
setwd("C:/Users/tamee/Downloads/new_runs/miroc/historical")
miroc_hist<- read.csv("tp_s6_8.csv")
miroc_hist_avg<- data.frame(apply(miroc_hist[,1:5],1,mean))
colnames(miroc_hist_avg)<- "TP"
miroc_hist_avg$Timeline<- rep("Historical",36)
miroc_hist_avg$year<- seq(1979,2014,1)
miroc_hist_avg$Diff<- rep(0,36)
intercept_miroc<- rep(mean(miroc_hist_avg$TP),2)
setwd("C:/Users/tamee/Downloads/new_runs/miroc/ssp370")
miroc_ssp370<- read.csv("TP_s6_8.csv")
miroc_ssp370_avg<- data.frame(apply(miroc_ssp370[,1:5],1,mean))
colnames(miroc_ssp370_avg)<- "TP"
hist_miroc<- data.frame(mean(miroc_hist_avg$TP))
colnames(hist_miroc)<- "TP"
miroc_ssp370_avg<- rbind(hist_miroc,miroc_ssp370_avg)
miroc_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
miroc_ssp370_avg$year<- seq(2014,2100,1)
miroc_ssp370_avg$Diff<- (miroc_ssp370_avg$TP -rep(hist_miroc$TP,87))
miroc_fit_ssp370<- lm(miroc_ssp370_avg$Diff ~ yr-1)
miroc_slope_370<- miroc_fit_ssp370$coefficients[1]
setwd("C:/Users/tamee/Downloads/new_runs/miroc/ssp585")
miroc_ssp585<- read.csv("TP_s6_8.csv")
miroc_ssp585_avg<- data.frame(apply(miroc_ssp585[,1:5],1,mean))
colnames(miroc_ssp585_avg)<- "TP"
miroc_ssp585_avg<- rbind(hist_miroc,miroc_ssp585_avg)
miroc_ssp585_avg$Timeline<- rep("SSP5-8.5",87)
miroc_ssp585_avg$year<- seq(2014,2100,1)

miroc_ssp585_avg$Diff<- (miroc_ssp585_avg$TP -rep(hist_miroc$TP,87))
miroc_fit_ssp585<- lm(miroc_ssp585_avg$Diff ~ yr-1)
miroc_slope_585<- miroc_fit_ssp585$coefficients[1]
miroc_full<- rbind(miroc_hist_avg,miroc_ssp370_avg,miroc_ssp585_avg)
miroc_full$Model <- rep("MIROC",210)

#MPI
setwd("C:/Users/tamee/Downloads/new_runs/MPI/historical")
mpi_hist<- read.csv("tp_s6_8.csv")
mpi_hist_avg<- data.frame(apply(mpi_hist[,1:5],1,mean))
colnames(mpi_hist_avg)<- "TP"
mpi_hist_avg$Timeline<- rep("Historical",36)
mpi_hist_avg$year<- seq(1979,2014,1)
mpi_hist_avg$Diff<- rep(0,36)
intercept_mpi<- rep(mean(mpi_hist_avg$TP),2)
setwd("C:/Users/tamee/Downloads/new_runs/MPI/ssp370")
mpi_ssp370<- read.csv("TP_s6_8.csv")
mpi_ssp370_avg<- data.frame(apply(mpi_ssp370[,1:5],1,mean))
colnames(mpi_ssp370_avg)<- "TP"
hist_mpi<- data.frame(mean(mpi_hist_avg$TP))
colnames(hist_mpi)<- "TP"
mpi_ssp370_avg<- rbind(hist_mpi,mpi_ssp370_avg)
mpi_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
mpi_ssp370_avg$year<- seq(2014,2100,1)
mpi_ssp370_avg$Diff<- (mpi_ssp370_avg$TP -rep(hist_mpi$TP,87))
mpi_fit_ssp370<- lm(mpi_ssp370_avg$Diff ~ yr-1)
mpi_slope_370<- mpi_fit_ssp370$coefficients[1]
setwd("C:/Users/tamee/Downloads/new_runs/MPI/ssp585")
mpi_ssp585<- read.csv("TP_s6_8.csv")
mpi_ssp585_avg<- data.frame(apply(mpi_ssp585[,1:5],1,mean))
colnames(mpi_ssp585_avg)<- "TP"
mpi_ssp585_avg<- rbind(hist_mpi,mpi_ssp585_avg)
mpi_ssp585_avg$Timeline<- rep("SSP5-8.5",87)
mpi_ssp585_avg$year<- seq(2014,2100,1)
mpi_ssp585_avg$Diff<- (mpi_ssp585_avg$TP -rep(hist_mpi$TP,87))
mpi_fit_ssp585<- lm(mpi_ssp585_avg$Diff ~ yr-1)
mpi_slope_585<- mpi_fit_ssp585$coefficients[1]
mpi_full<- rbind(mpi_hist_avg,mpi_ssp370_avg,mpi_ssp585_avg)
mpi_full$Model<- rep("MPI",210)


#MRI
setwd("C:/Users/tamee/Downloads/new_runs/MRI/historical")
mri_hist<- read.csv("tp_s6_8.csv")
mri_hist_avg<- data.frame(apply(mri_hist[,1:5],1,mean))
colnames(mri_hist_avg)<- "TP"
mri_hist_avg$year<- seq(1979,2014,1)
mri_hist_avg$Timeline<- rep("Historical",36)
intercept_mri<- rep(mean(mri_hist_avg$TP),2)
mri_hist_avg$Diff<- rep(0,36)
setwd("C:/Users/tamee/Downloads/new_runs/MRI/ssp370")
mri_ssp370<- read.csv("TP_s6_8.csv")
mri_ssp370_avg<- data.frame(apply(mri_ssp370[,1:5],1,mean))
colnames(mri_ssp370_avg)<- "TP"
hist_mri<- data.frame(mean(mri_hist_avg$TP))
colnames(hist_mri)<- "TP"
mri_ssp370_avg<- rbind(hist_mri,mri_ssp370_avg)

mri_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
mri_ssp370_avg$year<- seq(2014,2100)
mri_ssp370_avg$Diff<- (mri_ssp370_avg$TP -rep(hist_mri$TP,87))
mri_ssp370_avg$Timeline<- rep("SSP3-7.0",87)
mri_fit_ssp370<- lm(mri_ssp370_avg$Diff ~ yr-1)
mri_slope_370<- mri_fit_ssp370$coefficients[1]
setwd("C:/Users/tamee/Downloads/new_runs/MRI/ssp585")
mri_ssp585<- read.csv("TP_s6_8.csv")
mri_ssp585_avg<- data.frame(apply(mri_ssp585[,1:5],1,mean))
colnames(mri_ssp585_avg)<- "TP"
mri_ssp585_avg<- rbind(hist_mri,mri_ssp585_avg)
mri_ssp585_avg$Timeline<- rep("SSP5-8.5",87)
mri_ssp585_avg$year<- seq(2014,2100)
mri_ssp585_avg$Diff<- (mri_ssp585_avg$TP -rep(hist_mri$TP,87))
mri_fit_ssp585<- lm(mri_ssp585_avg$Diff ~ yr-1)
mri_slope_585<- mri_fit_ssp585$coefficients[1]
mri_full<- rbind(mri_hist_avg,mri_ssp370_avg,mri_ssp585_avg)
mri_full$Model<- rep("MRI",210)
mdp_all<- rbind(cpc_all,access_full,ec_full,miroc_full,mpi_full,mri_full)

slope_all<-c(ec_slope_370,ec_slope_585,mpi_slope_370,mpi_slope_585,mri_slope_370,mri_slope_585,access_slope_370,access_slope_585,miroc_slope_370,miroc_slope_585)
pvalue_all<- c(summary(ec_fit_370)$coefficients[,4],summary(ec_fit_585)$coefficients[,4],summary(mpi_fit_ssp370)$coefficients[,4],summary(mpi_fit_ssp585)$coefficients[,4],summary(mri_fit_ssp370)$coefficients[,4],summary(mri_fit_ssp585)$coefficients[,4],summary(acess_fit_370)$coefficients[,4],summary(acess_fit_585)$coefficients[,4],summary(miroc_fit_ssp370)$coefficients[,4],summary(miroc_fit_ssp585)$coefficients[,4])
#pvalue_all<- c(summary(ec_fit_370)$coefficients[,4],summary(ec_fit_585)$coefficients[,4],summary(mpi_fit_ssp370)$coefficients[,4],summary(mpi_fit_ssp585)$coefficients[,4],summary(mri_fit_ssp370)$coefficients[,4],summary(mri_fit_ssp585)$coefficients[,4],summary(acess_fit_370)$coefficients[,4],summary(acess_fit_585)$coefficients[,4],summary(miroc_fit_ssp370)$coefficients[,4],summary(miroc_fit_ssp585)$coefficients[,4])
slope_all<- data.frame(slope_all)
pvalue<- data.frame(pvalue_all)
colnames(slope_all)<- c('slope')
colnames(pvalue)<- c('p_value')
slope_all$Model<- c("EC_EARTH","EC_EARTH","MPI","MPI","MRI","MRI","ACCESS","ACCESS","MIROC","MIROC")
slope_all$Timeline<-c("SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5")
slope_all$TP<- c(130,480,130,480,130,480,130,480,130,480)
slope_all$year<- rep(2020,10)
slope_all$intercept<- c(intercept_ec,intercept_mpi,intercept_mri,intercept_access,intercept_miroc)
pvalue$Model<- c("EC_EARTH","EC_EARTH","MPI","MPI","MRI","MRI","ACCESS","ACCESS","MIROC","MIROC")
pvalue$Timeline<-c("SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5","SSP3-7.0","SSP5-8.5")
pvalue$MDP<-c(10,40,10,40,10,40,10,40,10,40)
pvalue$year<- rep(2020,10)
pval<- pvalue$p_value
stars <- ifelse(pval < 0.001, "***",
                ifelse(pval < 0.01, "**",
                       ifelse(pval < 0.05, "*", "")))

slope_all$stars<- stars
library(ggplot2)
ggplot(data = mdp_all, aes(x = year, y = TP)) +
  geom_line(aes(colour = Timeline)) +
  facet_wrap(~factor(Model, levels = c('EC_EARTH', 'MPI', 'MRI', 'ACCESS', 'MIROC'))) +
  geom_segment(data = slope_all,(aes(x=2014,xend=2100,y=intercept,yend=(intercept+slope*86),color=Timeline)),size=1)+
  #geom_smooth(data = subset(mdp_all, year > 2014), aes(colour = Timeline), method = "lm", se = FALSE) +
  geom_vline(xintercept = 2014, linetype = "solid", color = "grey", size = 1) +
  geom_text(
    data = slope_all, 
    aes(
      x = year,  # Fixed position to maintain consistency across facets
      y = TP,  # Adjusted Y-position to avoid overlap
      
      label = paste0("β: ", round(slope,3),"mm/year",stars),
      color= Timeline
    ),
    
    size = 4, 
    hjust = 0, 
    vjust = 0, 
    
    check_overlap = TRUE
    
  ) +
  
  
  #check_overlap = TRUE)+
  scale_x_continuous(limits = c(1979, 2100)) +
  scale_y_continuous(limits = c(0, 550)) +
  ylab("TP(mm/year)")+
  ggtitle("Total Precipitation(TP)")+
  theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        strip.background = element_rect(fill='white'),
        strip.text = element_text(face='bold',size = 16),
        axis.title.y = element_text(face = 'bold',size=16),
        plot.title=element_text(size=16,face='bold'),legend.title = element_text(size = 16,face='bold'),legend.text = element_text(size=16,face='bold'))

setwd("C:/Users/tamee/Downloads/new_runs")
ggsave("tp_detrend_slope_p_updated.jpeg",width = 12, height=8)
