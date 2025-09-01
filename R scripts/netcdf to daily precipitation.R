
#setwd("C:/Users/tamee/Downloads/greatlakes")
#reads the greatlakes shapefile
region<- st_read("greatlakes_subbasins.shp") 
shp =readOGR(dsn=".",layer = "greatlakes_subbasins")
e<- extent(region)
setwd("C:/Users/tamee/Downloads/historical/model/")
#list all netcdf files for a specific model
list<- list.files(pattern = ".nc")

#calculates mean daily precipitation for each 5 lake basins
for (i in 1:8){
  p<- rotate(brick(list[i]))
  n<- paste0("mpi_hist_",i)
  assign(n,t(as.data.frame(raster::extract(p,region,fun=mean))))
  
}

#combines all the dataframe 
mpi_hist<- rbind(mpi_hist_1, mpi_hist_2, mpi_hist_3,mpi_hist_4,mpi_hist_5,mpi_hist_6,mpi_hist_7,mpi_hist_8)

#converts to mm/day
for (i in 1:14610){
  mpi_hist[i,1:5] <- 86400*mpi_hist[i,1:5]
}

#extracts the date from the netcdf files
for (i in 1:8){
  pa<- (brick(list[i]))
  np<- paste0("mpi_date_hist_",i)
  assign(np,data.frame(date(getZ(pa))))
  
}

#combines all the date
mpi_hist_date<- rbind(mpi_date_hist_1,mpi_date_hist_2,mpi_date_hist_3,mpi_date_hist_4,mpi_date_hist_5,mpi_date_hist_6,mpi_date_hist_7,mpi_date_hist_8)
#combines date and precip
mpi_historical<- cbind(mpi_hist_date,mpi_hist)
mpi_historical<- mpi_historical
colnames(mpi_historical)<- lakes
#filters data from 1979-2014
mpi_historical_filtered <- mpi_historical %>%
  filter(year(Dates) > 1978)
#calculate average over all lake basins

mpi_mean <- rowMeans(mpi_historical_filtered[,3:7])

#continue this for all 12 models and combine them in 'mean.csv'