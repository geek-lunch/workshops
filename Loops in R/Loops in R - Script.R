# Rscript by Martin Leclerc
# Geek's lunch
# How to do loops
# Alternative to loops
# How to get faster loops



# Loops are easy to create 
# Loops are useful when you repeat a procedure multiple times
# For instance
print(paste("The year is", 2010))
print(paste("The year is", 2011))
print(paste("The year is", 2012))
print(paste("The year is", 2013))
print(paste("The year is", 2014))
print(paste("The year is", 2015))

# can be replaced by
for (year in 2010:2015){
  print(paste("The year is", year))
}

# We can change the marker
for (i in 2010:2015){
  print(paste("The year is", i))
}

# See another example of what we can do with loops (here a loop in a loop)
mat <- matrix(data = seq(10, 21, by=1), nrow = 6, ncol =2)
# Create the loop with r and c to iterate over the matrix
for (r in 1:nrow(mat)){
    for (c in 1:ncol(mat)){
         print(paste("Row", r, "and column",c, "have values of", mat[r,c]))  
		}}


# Loops can be used to extract info from a ftp website
for (i in 2000:2018){
	download.file(paste("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/vwnd.10m.",i,".nc",sep=""),                 #url du fichier
                    paste("uwnd.",i,".nc",sep=""),mode="wb")}


# What is the results of that loop?
mig=data.frame()
for (i in 1:length(date[,1])){
	pol=data[data$IDAnimal == date$ind[i],]
	pal=pol[which(pol$DateUTC >= as.POSIXct(date$Start[i],format="%Y-%m-%d",tz="UTC") & pol$DateUTC <= as.POSIXct(date$End[i],format="%Y-%m-%d",tz="UTC")),]
	mig=rbind(mig,pal)
	}




# Here, I created a loop to remove data before and after the spring migration so I can have a clean dataset (mig) containing only spring migration GPS locations.





#### Your turn to build a loop
# I want the mean temperature per day
# First create a database

start <- as.POSIXct("2000-01-01 00:00:00", tz="UTC")
end   <- as.POSIXct("2020-01-01 00:00:00", tz="UTC")
Date=format(seq(start,to=end, by="hour"), "%Y-%m-%d %H%:%M:%S")
Year=substr(Date,1,4)
Month=substr(Date,6,7)
Day=substr(Date,9,10)
YMD=substr(Date,1,10)
Data=as.data.frame(cbind(Date,Year,Month,Day,YMD))
Data$Year=as.numeric(as.character(Data$Year));Data$Month=as.numeric(as.character(Data$Month));Data$Day=as.numeric(as.character(Data$Day))
Data$Temp=rnorm(length(Data[,1]),5,8)
str(Data)

# Now I want you to create a loop to extract the mean temperature per day for 2000-2020





























# Answer
output=data.frame()
pg <- winProgressBar(title = paste("Iteration 0 out of",length(levels(Data$YMD))),min = 0, max = length(levels(Data$YMD)), width = 300)
for (i in 1:length(levels(Data$YMD))){
	dat=Data[Data$YMD == levels(Data$YMD)[i],]
	Temperature=mean(dat$Temp)
	dat2=cbind(as.character(dat$YMD[1]),Temperature)
	output=rbind(output,dat2)
	setWinProgressBar(pg, value=i, title = paste(i, "iterations done out of",length(levels(Data$YMD))), label = NULL)
	}
close(pg)






# Useful, but sometimes loops can take a lot of time
# We have some alternative
# For instance, you can explore the dplyr 
library(dplyr)
Data %>%
  group_by(YMD) %>%
  summarise(meanTemp = mean(Temp)) 

# Much faster with dplyr in this case
# Could also use doBy
library(doBy)
summaryBy(Temp~YMD,Data,FUN=mean)


# Loops are usefull, but sometimes you can use other tools such as dplyr-summaryBy and all the Sapply-Lapply
# Alternatives to loop can be faster by orders of magnitude
# Worth exploring!

# Sometimes, we need to use loop because alternatives are not available 
# But we can use different tactics to make a loop go faster
# For instance, a good use of rbind, lists, and subset
# Here I have extracted daily weather information for 5.5 million GPS locations 
data3=read.delim("Dataset_Empty_NoHudson_13h_Cleaned_20191125V2.txt")
my.list <- vector("list", length=length(levels(as.factor(substr(data3$date,1,10)))))
pg <- winProgressBar(title = paste("0 weather variable data extraction done out of",length(levels(as.factor(substr(data3$date,1,10))))),min = 0, max = length(levels(as.factor(substr(data3$date,1,10)))), width = 300)
for (i  in 1:length(levels(as.factor(substr(data3$date,1,10))))){
	data4=data3[substr(data3$date,1,10) == as.character(levels(as.factor(substr(data3$date,1,10)))[i]),]
	coords  <- data4[ , c("x1", "y1")] 
	dataP   <- data4[ , 3:16]
	crs     <- CRS("+init=epsg:3347")
	spdf    <- SpatialPointsDataFrame(coords = coords,data = dataP,proj4string = crs)
	spdf2   <- spTransform(spdf,CRS("+init=epsg:32198"))
	precp   <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/precip/raster/apcp_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	fonte   <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/fonte/raster/snom_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	snoco   <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/snowcover/raster/snowc_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	temp    <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/temp_surface/raster/air_sfc_",	as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	accu    <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/accum_snow/raster/weasd_",	as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	prof    <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/profondeur/raster/snod_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	UWind   <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/UWind/raster/uwnd_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	VWind   <- raster(paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/1 - Data/Meteo/VWind/raster/vwnd_",		as.character(substr(data4$date[1],1,4)),format(as.Date(as.character(((substr(data4$date[1],1,10)))),tz="UTC"),"%j"),".tif",sep=""))
	sum(proj4string(precp) == proj4string(fonte), proj4string(precp) == proj4string(snoco), proj4string(precp) == proj4string(temp), proj4string(precp) == proj4string(temp), proj4string(accu) == proj4string(prof),proj4string(precp) == proj4string(VWind),proj4string(precp) == proj4string(UWind),proj4string(precp) == proj4string(spdf2)) == 8
	
	meteo=cbind(spdf2@data$UID1,extract(precp, spdf2,df=TRUE),extract(fonte, spdf2,df=TRUE),extract(snoco, spdf2,df=TRUE),extract(temp, spdf2,df=TRUE),extract(accu, spdf2,df=TRUE),extract(prof, spdf2,df=TRUE),extract(UWind, spdf2,df=TRUE),extract(VWind, spdf2,df=TRUE))
	meteo=meteo[,c(1,3,5,7,9,11,13,15,17)]
	colnames(meteo)<-c("UID1","Precip","Fonte","Snowcover","Airtemp","Accumulation","Profondeur","UWind","VWind")
	my.list[[i]] <- meteo
	setWinProgressBar(pg, value=i, title = paste(i, "weather variable data extraction done out of",length(levels(as.factor(substr(data3$date,1,10))))), label = NULL)
	}
close(pg)
library(data.table)
weatherinfo=rbindlist(my.list)
weatherinfo2=as.data.frame(weatherinfo)


# The good combination of rbind and write.table and list could save you a lot a time (from 48h to 30 minutes)
# rbind is fast when not a lot of rows, but list is faster at larger number of rows
# Subsetting a large dataset takes a lot of time
pg <- winProgressBar(title = paste("Random migration #",1,"out of 500"),min = 0, max = 500, width = 300)
seed=1
for(i in 1:length(levels(droplevels(real3$id)))){
	
	pol=real3[real3$id == levels(droplevels(real3$id)) [i],]
	rownames(pol) <- NULL
	dat=data.frame()

	for (w in 1:500){	
		set.seed(seed)
		pp=sample(rownames(pol),length(pol[,1]),replace=F)
		pal=pol[as.numeric(as.character(pp)),]
		
		pal$x1[1] <- pol$x1[1] ; pal$y1[1] <- pol$y1[1] ; pal$date[1] <- pol$date[1]  
		pal$x2[1] = pal$x1[1] + pal$dx[1] ; pal$y2[1] = pal$y1[1] + pal$dy[1] ; pal$enddate[1] = pal$date[1] + pal$dt[1] 
		
		for (g in 2:length(pal[,1])){
			pal$x1[g] <- pal$x2[g-1] ; pal$y1[g] <- pal$y2[g-1] ; pal$date[g] <- pal$enddate[g-1] 
			pal$x2[g] <- pal$x1[g] + pal$dx[g] ; pal$y2[g] <- pal$y1[g] + pal$dy[g] ; pal$enddate[g] <- pal$date[g] + pal$dt[g] 
			} 
		pal$traj=paste(pal$id,"random",w,sep="-")	
		pal$timestep=c(1:length(pal[,1]))
		pal$id=as.character(pal$id)
		dat=rbind(dat,pal)
		seed=seed+1
		setWinProgressBar(pg, value=w, title = paste(w, "random migration out of 500 for caribou",i,"out of",length(levels(droplevels(real3$id)))), label = NULL)
		}
	dat[,1]=round(dat[,1]);dat[,2]=round(dat[,2]);dat[,4]=round(dat[,4]);dat[,5]=round(dat[,5]);dat[,6]=round(dat[,6]);dat[,9]=round(dat[,9]);dat[,10]=round(dat[,10]);
	dat$date=as.POSIXct(as.numeric(dat$date), origin="1970-01-01",tz="UTC") 
	dat$enddate=as.POSIXct(as.numeric(dat$enddate), origin="1970-01-01",tz="UTC")
	write.table(dat,paste("C:/Users/god_e/OneDrive - Université Laval/Post-doc 2/2 - Script/PathSF/Random/13h/",levels(droplevels(real3$id)) [i],".txt",sep=""),sep="\t",dec=".", row.names=F)
	}
close(pg)