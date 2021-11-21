install.packages("raster")
install.packages("ncdf4")
install.packages("lubridate")
install.packages('rts')
install.packages("sp")
install.packages("sf")

library(ncdf4)
library(raster)
library(lubridate)
library(rts)
library(sp)
library(sf)
lakes<- c("Dates","Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior")
Date<- c(seq(as.Date("1979/01/01"),as.Date("2014/12/31"),by="days"))
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper")

region<- st_read("greatlakes_subbasins.shp")
#one
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CanESM5")
can<- rotate(brick("pr_day_CanESM5_historical_r25i1p2f1_gn_18500101-20141231.nc"))
can_mean<- extract(can,region,fun=mean)
can_mean<- as.data.frame(t(can_mean))
can_date<- as.data.frame(getZ(can))
can_mean_df<- cbind(can_date,can_mean)
colnames(can_mean_df)<- lakes
can_df_f<- can_mean_df%>%
  filter(year(as.Date(Dates))>1978)

write.csv(can_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CanESM5/Can_ESM5.csv",row.names = FALSE)


#two
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CNRM-CM6")
cnrm_cm<- (brick("pr_day_CNRM-CM6-1_historical_r30i1p1f2_gr_19500101-20141231.nc"))
cnrm_cm<- rotate(cnrm_cm)
cnrm_cm_mean<- extract(cnrm_cm,region,fun=mean)
cnrm_cm_mean<- as.data.frame(t(cnrm_cm_mean))
cnrm_cm_date<- as.data.frame(getZ(cnrm_cm))
cnrm_cm_df<- cbind(cnrm_cm_date,cnrm_cm_mean)
colnames(cnrm_cm_df)<- lakes

cnrm_cm_f_df<- cnrm_cm_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(cnrm_cm_f_df,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CNRM-CM6/cnrm_cm_pr.csv",row.names = FALSE)
#three
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CNRM-ESM2")
cnrm_esm<- brick("pr_day_CNRM-ESM2-1_historical_r10i1p1f2_gr_19500101-20141231.nc")
cnrmcnrm_esm<- rotate(cnrm_esm)
cnrm_esm_mean<- extract(cnrm_esm,region,fun=mean)
cnrm_esm_mean<- as.data.frame(t(cnrm_esm_mean))
cnrm_esm_date<- as.data.frame(getZ(cnrm_esm))
cnrm_esm_df<- cbind(cnrm_esm_date,cnrm_esm_mean)
colnames(cnrm_esm_df)<- lakes
cnrm_esm_df_f<- cnrm_esm_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(cnrm_esm_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/CNRM-ESM2/cnrm_esm_pr.csv",row.names = FALSE)
#four
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Had-GEM3-GC3-LL")
had_ll<- brick("pr_day_HadGEM3-GC31-LL_historical_r5i1p1f3_gn_19500101-20141230.nc")
had_ll<- rotate(had_ll)
had_ll_mean<- extract(had_ll,region,fun=mean)
had_ll_mean<- as.data.frame(t(had_ll_mean))
had_ll_date<- as.data.frame(getZ(had_ll))
had_ll_df<- cbind(had_ll_date,had_ll_mean)
colnames(had_ll_df)<- lakes
had_ll_df_f<- had_ll_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(had_ll_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Had-GEM3-GC3-LL/Had_ll_pr.csv",row.names = FALSE)

  #five
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/IPSL-CM6A-LR")
ipsm<- brick("pr_day_IPSL-CM6A-LR_historical_r5i1p1f1_gr_18500101-20141231.nc")
ipsm<- rotate(ipsm)
ipsm_mean<- extract(ipsm,region,fun=mean)
ipsm_mean<- as.data.frame(t(ipsm_mean))
ipsm_date<- as.data.frame(getZ(ipsm))
ipsm_df<- cbind(ipsm_date,ipsm_mean)
colnames(ipsm_df)<- lakes
ipsm_df_f<- ipsm_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(ipsm_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/IPSL-CM6A-LR/IPSL_pr.csv",row.names = FALSE)
#six
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/NOR-CPM1")
nor_cpm<- brick("pr_day_NorCPM1_historical_r10i1p1f1_gn_19500101-20141231.nc")
nor_cpm<- rotate(nor_cpm)
nor_cpm_mean<- extract(nor_cpm,region,fun=mean)
nor_cpm_mean<- as.data.frame(t(nor_cpm_mean))
nor_cpm_date<- as.data.frame(getZ(nor_cpm))
nor_cpm_df<- cbind(nor_cpm_date,nor_cpm_mean)
colnames(nor_cpm_df)<- lakes
nor_cpm_df_f<- nor_cpm_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(nor_cpm_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/NOR-CPM1/nor_cpm_pr.csv",row.names = FALSE)
#seven
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/UKESM1")
ukesm1<- brick("pr_day_UKESM1-0-LL_historical_r4i1p1f2_gn_19500101-20141230.nc")
ukesm1<- rotate(ukesm1)
ukesm_mean<- extract(ukesm1,region,fun=mean)
ukesm_mean<- as.data.frame(t(ukesm_mean))
ukesm_date<- as.data.frame(getZ(ukesm1))
ukesm_df<- cbind(ukesm_date,ukesm_mean)
colnames(ukesm_df)<- lakes
ukesm_df_f<- ukesm_df%>%
  filter(year(as.Date(Dates))>1978)
write.csv(ukesm_df,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/UKESM1/ukesm1_pr.csv",row.names = FALSE)
#eight
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/MPI-ESM-1-2-HAM")
mpi_f<- list.files(pattern=".nc")

for (i in 2:4){
  pr<- rotate(brick(mpi_f[i]))
  nm<- paste0("mpi_f",i)
  assign(nm,data.frame(extract(pr,region,fun=mean)))
}
mpi<- cbind(mpi_f2,mpi_f3,mpi_f4)
mpi<- as.data.frame(t(mpi))
colnames(mpi)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior")
dt<- c(seq(as.Date("1970/01/01"),as.Date("2014/12/31"),by="days"))
mpi$date<- dt
mpi_esm<- mpi[mpi$date%in%Date,]
write.csv(mpi_esm,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/MPI-ESM-1-2-HAM/MPI_ESM.csv",row.names = FALSE)

#nine
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/GFDL-ESM4")
gfdl<- list.files(pattern=".nc")

for (i in 2:4){
  p<- rotate(brick(gfdl[i]))
  n<- paste0("gfdl_",i)
  assign(n,data.frame(extract(p,region,fun=mean)))
  
}

d1<- getZ(brick(gfdl[2]))
d2<- getZ(brick(gfdl[3]))
d3<- getZ(brick(gfdl[4]))
gfdl_date<- c(d1,d2,d3)
gfdl_df<- cbind(gfdl_2,gfdl_3,gfdl_4)
gfdl_df<- as.data.frame(t(gfdl_df))
gfdl_df<- cbind(gfdl_date,gfdl_df)
colnames(gfdl_df)<- lakes
gfdl_df_f<- gfdl_df%>%
  filter(year(as.Date(Dates))>1978)
  

write.csv(gfdl_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/GFDL-ESM4/GFDL_ESM4.csv")

#ten
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Had-GEM3-GC3-MM")
fl<- list.files(pattern=".nc")
for (i in 6:14){
  pa<- rotate(brick(fl[i]))
  np<- paste0("had_mm_",i)
  assign(np,data.frame(extract(pa,region,fun=mean)))
}


for (i in 6:14){
  pa<- rotate(brick(fl[i]))
  d<- paste0("had_mm_date",i)
  assign(d, getZ(pa))
}
had_date<- c(had_mm_date6,had_mm_date7,had_mm_date8,had_mm_date9,had_mm_date10,had_mm_date11,had_mm_date12,had_mm_date13,had_mm_date14)
had_mm_df<- cbind(had_mm_6,had_mm_7,had_mm_8,had_mm_9,had_mm_10,had_mm_11,had_mm_12,had_mm_13,had_mm_14)
had_mm_df<- as.data.frame(t(had_mm_df))

had_mm_df<- cbind(had_date,had_mm_df)
colnames(had_mm_df)<- lakes
had_mm_df_f<- had_mm_df%>%
  filter(year(as.Date(Dates))>1978)

write.csv(had_mm_df_f,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Had-GEM3-GC3-MM/Had_mm_pr.csv",row.names = FALSE)
#miroc
setwd("E:/MW_ClimateChange/Data for frontiers paper/Miroc_ES2L/Pr")
miroc_fl<- list.files(pattern = ".nc")
for (i in 1:36){
  pa<- rotate(brick(miroc_fl[i]))
  np<- paste0("miroc_es2l_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
miroc_pr<- cbind(miroc_es2l_1,miroc_es2l_2,miroc_es2l_3,miroc_es2l_4,miroc_es2l_5,miroc_es2l_6,miroc_es2l_7,miroc_es2l_8,miroc_es2l_9,miroc_es2l_10,miroc_es2l_11,miroc_es2l_12,miroc_es2l_13,miroc_es2l_14,miroc_es2l_15,miroc_es2l_16,miroc_es2l_17,miroc_es2l_18,miroc_es2l_19,miroc_es2l_20,miroc_es2l_21,miroc_es2l_22,miroc_es2l_23,miroc_es2l_24,miroc_es2l_25,miroc_es2l_26,miroc_es2l_27,miroc_es2l_28,miroc_es2l_29,miroc_es2l_30,miroc_es2l_31,miroc_es2l_32,miroc_es2l_33,miroc_es2l_34,miroc_es2l_35,miroc_es2l_36)
miroc_precip<- data.frame(t(miroc_pr))
colnames(miroc_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior")
miroc_precip$Dates<- c(seq(as.Date("1979/01/01"),as.Date("2014/12/31"),by="days"))
write.csv(miroc_precip,"miroc_ES2L.csv", row.names = FALSE)
#AWI_ESM
setwd("E:/MW_ClimateChange/Data for frontiers paper/AWI_ESM/Pr")
awi<- list.files(pattern = ".nc")
for (i in 1:36){
  pa<- rotate(brick(awi[i]))
  np<- paste0("awi_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
awi_pr<- cbind(awi_1,awi_2,awi_3,awi_4,awi_5,awi_6,awi_7,awi_8,awi_9,awi_10,awi_11,awi_12,awi_13,awi_14,awi_15,awi_16,awi_17,awi_18,awi_19,awi_20,awi_21,awi_22,awi_23,awi_24,awi_25,awi_26,awi_27,awi_28,awi_29,awi_30,awi_31,awi_32,awi_33,awi_34,awi_35,awi_36)
awi_precip<- data.frame(t(awi_pr))
colnames(awi_precip)<-  c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior")
awi_precip$Dates<- c(seq(as.Date("1979/01/01"),as.Date("2014/12/31"),by="days"))
write.csv(awi_precip,"AWI_ES2L.csv", row.names = FALSE)
#CMCC_CM2_HR4
setwd("E:/MW_ClimateChange/Data for frontiers paper/CMCC_CM2_HR4/Pr")
cmcc1<- list.files(pattern = ".nc")
for (i in 1:5){
  pa<- rotate(brick(cmcc1[i]))
  np<- paste0("cmcc_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
cmcc_hr_date1<- data.frame(getZ(brick(cmcc1[1])))
colnames(cmcc_hr_date1)<- c("Dates")
cmcc_hr_date2<- data.frame(getZ(brick(cmcc1[2])))
colnames(cmcc_hr_date2)<- c("Dates")
cmcc_hr_date3<- data.frame(getZ(brick(cmcc1[3])))
colnames(cmcc_hr_date3)<- c("Dates")
cmcc_hr_date4<- data.frame(getZ(brick(cmcc1[4])))
colnames(cmcc_hr_date4)<- c("Dates")
cmcc_hr_date5<- data.frame(getZ(brick(cmcc1[5])))
colnames(cmcc_hr_date5)<- c("Dates")

cmcc_hr_date<- rbind(cmcc_hr_date1,cmcc_hr_date2,cmcc_hr_date3,cmcc_hr_date4,cmcc_hr_date5)
cmcc_hr<- cbind(cmcc_1,cmcc_2,cmcc_3,cmcc_4,cmcc_5)
cmcc_hr_precip<- data.frame(t(cmcc_hr),(cmcc_hr_date))
colnames(cmcc_hr_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
cmcc_hr_precip<- dplyr::filter(cmcc_hr_precip,year(Dates)>1978)
write.csv(cmcc_hr_precip,"CMCC_HR4.csv", row.names = FALSE)
#CMCC_CM2_SR5
setwd("E:/MW_ClimateChange/Data for frontiers paper/CMCC_CM2_SR5/Pr")
cmcc2<- list.files(pattern = ".nc")
for (i in 1:2){
  pa<- rotate(brick(cmcc2[i]))
  np<- paste0("cmcc_sr_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
cmcc_sr_date1<- data.frame(getZ(brick(cmcc2[1])))
colnames(cmcc_sr_date1)<- c("Dates")
cmcc_sr_date2<- data.frame(getZ(brick(cmcc2[2])))
colnames(cmcc_sr_date2)<- c("Dates")
cmcc_sr_date<- rbind(cmcc_sr_date1,cmcc_sr_date2)
cmcc_sr_pr<- cbind(cmcc_sr_1,cmcc_sr_2)
cmcc_sr_precip<- data.frame(t(cmcc_sr_pr),cmcc_sr_date)
colnames(cmcc_sr_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
cmcc_sr_precip<- dplyr::filter(cmcc_sr_precip,year(Dates)>1978)
write.csv(cmcc_sr_precip,"CMCC_SR5.csv", row.names = FALSE)

#CMCC_ESM2
setwd("E:/MW_ClimateChange/Data for frontiers paper/CMCC_ESM2/Pr")
cmcc3<- list.files(pattern=".nc")
for (i in 1:2){
  pa<- rotate(brick(cmcc3[i]))
  np<- paste0("cmcc_esm_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
cmcc_esm2_date1<- data.frame(getZ(brick(cmcc3[1])))
colnames(cmcc_esm2_date1)<- c("Dates")
cmcc_esm2_date2<- data.frame(getZ(brick(cmcc3[2])))
colnames(cmcc_esm2_date2)<- c("Dates")
cmcc_esm2_date<- rbind(cmcc_esm2_date1,cmcc_esm2_date2)
cmcc_esm2_pr<- cbind(cmcc_esm_1,cmcc_esm_2)
cmcc_esm2_precip<- data.frame(t(cmcc_esm2_pr),cmcc_esm2_date)
colnames(cmcc_esm2_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
cmcc_esm2_precip<- dplyr::filter(cmcc_esm2_precip,year(Dates)>1978)
write.csv(cmcc_esm2_precip,"CMCC_ESM2.csv", row.names = FALSE)
#NCAR_CESM2
setwd("E:/MW_ClimateChange/Data for frontiers paper/NCAR_CESM2/pr")
ncar1<- list.files(pattern=".nc")
for (i in 1:5){
  pa<- rotate(brick(ncar1[i]))
  np<- paste0("ncar_cesm2_",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
ncar_cesm2_date1<- data.frame(getZ(brick(ncar1[1])))
colnames(ncar_cesm2_date1)<- c("Dates")
ncar_cesm2_date2<- data.frame(getZ(brick(ncar1[2])))
colnames(ncar_cesm2_date2)<- c("Dates")

ncar_cesm2_date3<- data.frame(getZ(brick(ncar1[3])))
colnames(ncar_cesm2_date3)<- c("Dates")
ncar_cesm2_date4<- data.frame(getZ(brick(ncar1[4])))
colnames(ncar_cesm2_date4)<- c("Dates")
ncar_cesm2_date5<- data.frame(getZ(brick(ncar1[5])))
colnames(ncar_cesm2_date5)<- c("Dates")
ncar_cesm_pr<- cbind(ncar_cesm2_1,ncar_cesm2_2,ncar_cesm2_3,ncar_cesm2_4,ncar_cesm2_5)
ncar_cesm2_dates<- rbind(ncar_cesm2_date1,ncar_cesm2_date2,ncar_cesm2_date3,ncar_cesm2_date4,ncar_cesm2_date5)
ncar_cesm_precip<- data.frame(t(ncar_cesm_pr),ncar_cesm2_dates)
ncar_cesm2_precip<- dplyr::filter(ncar_cesm_precip,year(Dates)>1978)
colnames(ncar_cesm_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
write.csv(ncar_cesm_precip,"NCAR_CESM2.csv",row.names = FALSE)
#NCAR_CESM2_FV
setwd("E:/MW_ClimateChange/Data for frontiers paper/NCAR_CESM_FV/Pr")
ncar2<- list.files(pattern = ".nc")
for (i in 1:5){
  pa<- rotate(brick(ncar2[i]))
  np<- paste0("ncar_cesm2_fv",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
ncar_cesm2_fv_date1<- data.frame(getZ(brick(ncar2[1])))
colnames(ncar_cesm2_fv_date1)<- c("Dates")
ncar_cesm2_fv_date2<- data.frame(getZ(brick(ncar2[2])))
colnames(ncar_cesm2_fv_date2)<- c("Dates")
ncar_cesm2_fv_date3<- data.frame(getZ(brick(ncar2[3])))
colnames(ncar_cesm2_fv_date3)<- c("Dates")
ncar_cesm2_fv_date4<- data.frame(getZ(brick(ncar2[4])))
colnames(ncar_cesm2_fv_date4)<- c("Dates")
ncar_cesm2_fv_date5<- data.frame(getZ(brick(ncar2[5])))
colnames(ncar_cesm2_fv_date5)<- c("Dates")
ncar_cesm2_fv_date<- rbind(ncar_cesm2_fv_date1,ncar_cesm2_fv_date2,ncar_cesm2_fv_date3,ncar_cesm2_fv_date4,ncar_cesm2_fv_date5)
ncar_cesm2_fv_pr<- cbind(ncar_cesm2_fv1,ncar_cesm2_fv2,ncar_cesm2_fv3,ncar_cesm2_fv4,ncar_cesm2_fv5)
ncar_cesm2_fv_precip<- data.frame(t(ncar_cesm2_fv_pr),ncar_cesm2_fv_date)
colnames(ncar_cesm2_fv_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
ncar_cesm2_fv_precip<- dplyr::filter(ncar_cesm2_fv_precip,year(Dates)>1978)
write.csv(ncar_cesm2_fv_precip,"NCAR_FV.csv",row.names = FALSE)
#NCAR_WA
setwd("E:/MW_ClimateChange/Data for frontiers paper/NCAR_CESM2_WACCM/pr")
ncar3<- list.files(pattern = ".nc")
for (i in 1:5){
  pa<- rotate(brick(ncar3[i]))
  np<- paste0("ncar_cesm3_wa",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
ncar_cesm2_wa_date1<- data.frame(getZ(brick(ncar3[1])))
colnames(ncar_cesm2_wa_date1)<- c("Dates")
ncar_cesm2_wa_date2<- data.frame(getZ(brick(ncar3[2])))
colnames(ncar_cesm2_wa_date2)<- c("Dates")
ncar_cesm2_wa_date3<- data.frame(getZ(brick(ncar3[3])))
colnames(ncar_cesm2_wa_date3)<- c("Dates")
ncar_cesm2_wa_date4<- data.frame(getZ(brick(ncar3[4])))
colnames(ncar_cesm2_wa_date4)<- c("Dates")
ncar_cesm2_wa_date5<- data.frame(getZ(brick(ncar3[5])))
colnames(ncar_cesm2_wa_date5)<- c("Dates")
ncar_cesm2_wa_date<- rbind(ncar_cesm2_wa_date1,ncar_cesm2_wa_date2,ncar_cesm2_wa_date3,ncar_cesm2_wa_date4,ncar_cesm2_wa_date5)
ncar_cesm2_wa_pr<- cbind(ncar_cesm2_wa1,ncar_cesm2_wa2,ncar_cesm2_wa3,ncar_cesm2_wa4,ncar_cesm2_wa5)
wa_precip<- data.frame(t(ncar_cesm2_wa_pr),ncar_cesm2_wa_date)
wa_precip<- dplyr::filter(wa_precip,year(Dates)>1978)
colnames(wa_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
write.csv(wa_precip,"NCAR_WA.csv",row.names = FALSE)
#NOR_LM
setwd("E:/MW_ClimateChange/Data for frontiers paper/Nor_ESM2_LM/Pr")
lm<- list.files(pattern=".nc")
for (i in 1:5){
  pa<- rotate(brick(lm[i]))
  np<- paste0("nor_lm",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
dates1<- data.frame(getZ(brick(lm[1])))
dates2<- data.frame(getZ(brick(lm[2])))
dates3<- data.frame(getZ(brick(lm[3])))
dates4<- data.frame(getZ(brick(lm[4])))
dates5<- data.frame(getZ(brick(lm[5])))
colnames(dates1)<- c("Dates")
colnames(dates2)<- c("Dates")
colnames(dates3)<- c("Dates")
colnames(dates4)<- c("Dates")
colnames(dates5)<- c("Dates")
lm_dates<- rbind(dates1,dates2,dates3,dates4,dates5)
lm_pr<- cbind(nor_lm1,nor_lm2,nor_lm3,nor_lm4,nor_lm5)
lm_precip<- data.frame(t(lm_pr),lm_dates)
lm_precip<- dplyr::filter(lm_precip,year(Dates)>1978)
colnames(lm_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
write.csv(lm_precip,"NOR_ESM2_LM.csv",row.names = FALSE)
#NOR_MM
setwd("E:/MW_ClimateChange/Data for frontiers paper/Nor_ESM2_MM/Pr")
mm<- list.files(pattern=".nc")
for (i in 1:5){
  pa<- rotate(brick(mm[i]))
  np<- paste0("nor_mm",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}
mm_dates1<- data.frame(getZ(brick(mm[1])))
mm_dates2<- data.frame(getZ(brick(mm[2])))
mm_dates3<- data.frame(getZ(brick(mm[3])))
mm_dates4<- data.frame(getZ(brick(mm[4])))
mm_dates5<- data.frame(getZ(brick(mm[5])))
colnames(mm_dates1)<- colnames(mm_dates2)<- colnames(mm_dates3)<- colnames(mm_dates4)<- colnames(mm_dates5)<-c("Dates")
mm_date<- rbind(mm_dates1,mm_dates2,mm_dates3,mm_dates4,mm_dates5)
mm_pr<- cbind( nor_mm1,nor_mm2,nor_mm3,nor_mm4,nor_mm5)
mm_precip<- data.frame(t(mm_pr),mm_date)
mm_precip<- dplyr::filter(mm_precip,year(Dates)>1978)
colnames(mm_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
write.csv(mm_precip,"NOR_ESM2_MM.csv",row.names = FALSE)
#EC_Earth3
setwd("E:/MW_ClimateChange/Data for frontiers paper/EC_Earth3/Pr")
ec<- list.files(pattern=".nc")
for (i in 1:36){
  pa<- rotate(brick(ec[i]))
  np<- paste0("ec",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
  
}
for (i in 1:36){
  pa<- (brick(ec[i]))
  np<- paste0("ec_date",i)
  assign(np,data.frame(raster::getZ(pa)))
  
}
ec_dates<- rbind(ec_date1,ec_date2,ec_date3,ec_date4,ec_date5,ec_date6,ec_date7,ec_date8,ec_date9,ec_date10,ec_date11,ec_date12,ec_date13,ec_date14,ec_date15,ec_date16,ec_date17,ec_date18,ec_date19,ec_date20,ec_date21,ec_date22,ec_date23,ec_date24,ec_date25,ec_date26,ec_date27,ec_date28,ec_date29,ec_date30,ec_date31,ec_date32,ec_date33,ec_date34,ec_date35,ec_date36)
ec_pr<- cbind(ec1,ec2,ec3,ec4,ec5,ec6,ec7,ec8,ec9,ec10,ec11,ec12,ec13,ec14,ec15,ec16,ec17,ec18,ec19,ec20,ec21,ec22,ec23,ec24,ec25,ec26,ec27,ec28,ec29,ec30,ec31,ec32,ec33,ec34,ec35,ec36)
ec_precip<- data.frame(t(ec_pr),ec_dates)
colnames(ec_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior","Dates")
write.csv(ec_precip,"EC_Earth3.csv",row.names = FALSE)
#Global Precip
setwd("G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Global_Precip")
files<- list.files(pattern = ".nc")


for (i in 1:5){
  pa<- rotate(brick(ncar3[i]))
  np<- paste0("ncar_cesm2_wa",i)
  assign(np,data.frame(raster::extract(pa,region,fun=mean)))
}




for(i in 1:36){
  f<-rotate(brick(files[i]))
  name<- paste0("year",i)
  assign(name,data.frame(raster::extract(f,region,fun=mean,na.rm=TRUE)))

  
}

great_lakes_precip<- cbind(year1,year2,year3,year4,year5,year6,year7,year8,year9,year10,year11,year12,year13,year14,year15,year16,year17,year18,year19,year20,year21,year22,year23,year24,year25,year26,year27,year28,year29,year30,year31,year32,year33,year34,year35,year36)
great_lakes_precip<- t(great_lakes_precip)
glk_precip<- as.data.frame(great_lakes_precip)
colnames(glk_precip)<- c("Lk_Erie","Lk_Huron","Lk_Michigan","Lk_Ontario","Lk_Superior")
Date<- c(seq(as.Date("1979/01/01"),as.Date("2014/12/31"),by="days"))
glk_precip$Date<- Date
write.csv(glk_precip,"G:/LCS-Filer/ekcarter_r/MW_ClimateChange/Data for frontiers paper/Great_Lakes_Precipitation.csv", row.names = FALSE)

