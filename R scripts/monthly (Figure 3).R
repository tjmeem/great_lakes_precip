mean_updated <- read_csv("E:/MW_ClimateChange/Data for frontiers paper/mean_updated.csv",col_types = cols(Dates = col_date(format = "%m/%d/%Y")))

mean_updated <- read_csv("E:/MW_ClimateChange/Data for frontiers paper/mean_updated.csv",col_types = cols(Dates = col_date(format = "%m/%d/%Y")))


library(tidyverse)


month<- mean_updated%>%
  group_by(month)%>%
  summarise(ACCESS= mean(access),
            BCC=mean(bcc),
            CANESM= mean(can_esm),
            CNRM= mean(cnrm_esm),
            EC_EARTH= mean(ec_earth),
            GFDL=mean(gfdl),
            INM= mean(inm),
            IPSL= mean(ipsl),
            MIROC= mean(miroc),
            MPI= mean(mpi),
            MRI= mean(mri),
            UKESM=mean(ukesm),
            CPC= mean(cpc,na.rm = TRUE))



month_sd<- mean_updated%>%
  group_by(month)%>%
  summarise(CANESM= std.error(can_esm),
            CNRM= std.error(cnrm_esm),
            IPSL= std.error(ipsl,na.rm=TRUE),
            MPI= std.error(mpi),
            GFDL= std.error(gfdl),
            UKESM= std.error(ukesm),
            EC_EARTH= std.error(ec_earth),
            MIROC= std.error(miroc),
            INM= std.error(inm),
            MRI= std.error(mri),
            ACCESS= std.error(access),
            BCC=std.error(bcc),
            CPC= std.error(cpc,na.rm = TRUE))

mean_melt<- reshape2::melt(month,id='month')
mean_up<- mean_melt[1:144,]
mean_cpc<- mean_melt[145:156,]
sd_melt<- reshape2::melt(month_sd,id='month')
sd_up<- sd_melt[1:144,]
sd_cpc<- sd_melt[145:156,]
cpc_max<- mean_cpc$value + (2*sd_cpc$value)
cpc_min<- abs(mean_cpc$value - (2*sd_cpc$value))
mean_up$max<- mean_up$value+ (2*sd_up$value)
mean_up$min<- abs(mean_up$value- (2*sd_up$value))
mean_up$cpc<- rep(mean_cpc$value,12)
mean_up$cpc_max<- rep(cpc_max,12)
mean_up$cpc_min<- rep(cpc_min,12)

setwd("E:/MW_ClimateChange/Data for Frontiers Paper")
month_sig <- read_csv("Draft/month_sig.csv")
mean_up$significance <- month_sig$significance

cs<- c("Model"= 'red',"CPC"="blue")
y_break<- seq(0.001,20,0.01)
ggplot(data = mean_up,aes(x= month)) +
  
  geom_ribbon(aes(ymin=cpc_min,ymax=cpc_max,fill="CPC"),alpha=0.5)+
  
  geom_ribbon(aes(ymin=min,ymax=max,fill="Model"),alpha=0.4) +
  geom_line(aes(y=value),color='red',size=0.5)+
  geom_line(aes(y=cpc),color='blue',size=0.5)+
  geom_point(aes(y=value,shape=significance),size=1)+
  facet_wrap(.~variable) +
  scale_fill_manual(values= cs,name='Dataset')+
  scale_x_continuous(breaks = 1:12)+
  xlab("Month")+
  ylab("Monthly Average Precipitation(mm/day)")+
  scale_shape_discrete(labels=c("Similar with CPC","Different from CPC"))+
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        strip.background = element_rect(fill='white'),strip.text = element_text(face="bold",size=12))+
  theme(axis.text.x = element_text(angle =45, hjust = 1,size=18,face='bold'),axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=25,face='bold'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_text(size=20,face="bold"),legend.position = "none",strip.text =element_text(size=12,face="bold"))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=18,face='bold'),axis.title=element_text(size=20,face="bold"),plot.title=element_text(size=25,face='bold'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_text(size=20,face="bold"),legend.position = "none",strip.text =element_text(size=12,face="bold")) 

setwd("E:/MW_ClimateChange/Data for frontiers paper/Draft/Edits")
ggsave("month_sd.jpeg",width = 12, height=8)
        