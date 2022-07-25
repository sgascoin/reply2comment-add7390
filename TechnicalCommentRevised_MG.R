# Technical comment of "From white to green: Snow cover loss and increased vegetation productivity in the European Alps#
# *correspond author : arthur.bayle.env@gmail.com
# with some corrections and comments to improve clarity

# Load packages
library(terra)
library(DescTools)
library(mblm)
library(Kendall)
library(zoo)
library(raster)

#setwd(...) # Your path to "TOSHARE/...."
# getwd()

# -----------------------------------#
# ---------- PREPARE DATA ---------- #
# -----------------------------------#
# DO NOT LAUNCH !!!!!

# Proj : +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
# ETRS89-extended / LAEA Europe (EPSG:3035)
# ETRS89 = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# # Prepare standard grid by reprojecting to ETRS89 and aggregate to 300 m to limit processing time.
# GRID = rast("TOSHARE/DATA/ORIGINAL_DATA/PROB/alps_Prob_2021.tif")
# GRID = project(GRID,ETRS89)
# GRID = aggregate(GRID,fact=13.3333333)
# GRID[] = 1
# writeRaster(GRID,"TOSHARE/DATA/COMMENT_DATA/GRID.tif")

# # Loading GRID
# GRID = rast("TOSHARE/DATA/COMMENT_DATA/GRID.tif")
# 
# # Reprojecting all data on the GRID
# # a. COUNT
# LIST = list.files("TOSHARE/DATA/ORIGINAL_DATA/COUNT/",full.names=T)
# 
# YEAR = 1984:2021
# for(i in 1:length(LIST)){
#   print(YEAR[i])
#   NDVI = rast(LIST[i])
#   NDVI = project(NDVI,GRID,method="near")
#   writeRaster(NDVI,paste0("TOSHARE/DATA/COMMENT_DATA/COUNT/alps_imageCount_",YEAR[i],".tif"),overwrite=T)
# }
# 
# # b. NDVI
# LIST = list.files("TOSHARE/DATA/ORIGINAL_DATA/NDVI/",full.names=T)
# 
# YEAR = 1984:2021
# for(i in 1:length(LIST)){
#   print(YEAR[i])
#   NDVI = rast(LIST[i])
#   NDVI = project(NDVI,GRID)
#   writeRaster(NDVI,paste0("TOSHARE/DATA/COMMENT_DATA/NDVI/alps_NDVI_",YEAR[i],".tif"))
# }
# 
# # c. PROB
# LIST = list.files("TOSHARE/DATA/ORIGINAL_DATA//PROB/",full.names=T)
# 
# YEAR = 1984:2021
# for(i in 1:length(LIST)){
#   print(YEAR[i])
#   NDVI = rast(LIST[i])
#   NDVI = project(NDVI,GRID)
#   writeRaster(NDVI,paste0("TOSHARE/DATA/COMMENT_DATA/PROB/alps_Prob_",YEAR[i],".tif"))
# }
# 
# # d. PERM
# LIST = list.files("TOSHARE/DATA/ORIGINAL_DATA/PERM/",full.names=T)
# 
# YEAR = 1984:2021
# for(i in 1:length(LIST)){
#   print(YEAR[i])
#   NDVI = rast(LIST[i])
#   NDVI = project(NDVI,GRID,method = "near")
#   writeRaster(NDVI,paste0("TOSHARE/DATA/COMMENT_DATA/PERM/alps_Perm_",YEAR[i],".tif"),overwrite=T)
# }

-------------------------------#
---------- FIGURE 1 ---------- #
-------------------------------#
GRID = terra::rast("TOSHARE/DATA/COMMENT_DATA/GRID.tif")
GRID2 = raster::raster("TOSHARE/DATA/COMMENT_DATA/GRID.tif")

# Proj : +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
# ETRS89-extended / LAEA Europe (EPSG:3035)
ETRS89 = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

sampleSize <- 10000

# DEM 
DEM = rast("TOSHARE/DATA/COMMENT_DATA/DEM_EU-ALPS.tif")
DEM2 = raster::raster("TOSHARE/DATA/COMMENT_DATA/DEM_EU-ALPS.tif")
DEM = project(DEM,GRID)
DEM2 = project(DEM,GRID)

# -----------------------#
# ----- FIGURE 1 A ----- #
# -----------------------#

# We randomly selected 100.000 pixels around the Alps to conduct the analysis (so the final count is less than 100.000 because of NA)


n = sample(which(!is.na(GRID[])),sampleSize) # WRONG
n2 = sample(which(!is.na(DEM2[])),sampleSize) # suboptimal
elevMask <- DEM2<1700

# overwrite
DEM2[elevMask] = NA
n = sample(which(!is.na(DEM2[])),sampleSize)
values <- DEM2[n]

# Load 'Image Count' on data.frame (Per year)
LIST.IC = list.files("TOSHARE/DATA/COMMENT_DATA/COUNT/",full.names=T)
NUMB = sds(LIST.IC)
DF.N = data.frame(ID = 1:sampleSize)
YEAR = 1984:2021
for(i in 1:length(nlyr(NUMB))){
  print(YEAR[i])
  CUR = NUMB[i]
  CUR[which(CUR[] == 0)] = NA
  DF.N[,i] = CUR[n][,1]
  colnames(DF.N)[i] = paste0("NUMB_",YEAR[i])
}

# Plot
# YOU SHOULD BE FORBIDDEN TO USE CAPITAL
png("TOSHARE/FIGURES/FIGURE1A.png",width=1500,height=2500,res = 300)

# 1984 to 1996
X1 = apply(DF.N[,1:13],MARGIN = 1,FUN=function(x){mean(x,na.rm=T)})
plot(y=DEM[n][,1], x=X1,pch=".",cex=1.2,col=adjustcolor("red",alpha.f = 0.3),
     xlim=c(1,10),xaxs="i",xlab="Mean number of images per year",
     ylab="Altitude (m)",ylim=c(1600,3200),yaxs="i") 
abline(h=seq(1000,4000,500),v=seq(0,10,2),lty=2,col="grey") # grid 
LOESS1 = loess.smooth(X1,DEM[n][,1], span = 2/3, degree = 2)


# 1997 to 2009
X2 = apply(DF.N[,14:26],MARGIN = 1,FUN=function(x){mean(x,na.rm=T)})
points(y=DEM[n][,1], x=X2,pch=".",cex=1.2,col=adjustcolor("darkgreen",alpha.f = 0.3),xlim=c(1,8),xaxs="i")
LOESS2 = loess.smooth(X2,DEM[n][,1], span = 2/3, degree = 2)


# 2010 to 2021
X3 = apply(DF.N[,27:38],MARGIN = 1,FUN=function(x){mean(x,na.rm=T)})
points(y=DEM[n][,1], x=X3,pch=".",cex=1.2,col=adjustcolor("royalblue",alpha.f = 0.3),xlim=c(1,8),xaxs="i")
LOESS3 = loess.smooth(X3,DEM[n][,1], span = 2/3, degree = 2)



lines(LOESS1$x,LOESS1$y,col="red",lwd=4,lty=3)
lines(LOESS2$x,LOESS2$y,col="darkgreen",lwd=4,lty=3)
lines(LOESS3$x,LOESS3$y,col="royalblue",lwd=4,lty=3)

legend("topright",legend=c("1984 - 1996","1997 - 2009","2010 - 2021"),col=c("red","darkgreen","blue"),lwd=4,lty=3,bty="n",cex=1.5)
abline(h=1700,lwd=2)
text(3.5,1740,"Rumpf et al. (1) study limit")
polygon(x=c(1,10,10,1),y=c(1600,1600,1700,1700),col=adjustcolor("grey",alpha.f = 0.7)) # cut of bos
dev.off()

# !!!!
# Loess curves suggest drastic increases in number of images as the altitude goes down
# but it is that much only because we restrained the analysis above 1700m
# !!!!

# -----------------------#
# ----- FIGURE 1 B ----- #
# -----------------------#

png("TOSHARE/FIGURES/FIGURE1B.png",width=1500,height=1400,res = 300)

# Compute the mean and sd of number of images (same as Rumpf et al. Fig S?)
Y = apply(DF.N,MARGIN=2,function(x){mean(x,na.rm=T)})
Y.se = apply(DF.N,MARGIN=2,function(x){sd(x,na.rm=T)})
X = 1984:2021

# Plot the time series with sd and 2sd
plot(X,Y,type="l",lwd=4,ylim=c(0,12),yaxs="i",xaxs="i",
     xlab="Time",ylab="",lty=3)
polygon(x=c(X,rev(X)),y=c(Y-2*Y.se,rev(Y+2*Y.se)),border = NA,col = "grey90")
polygon(x=c(X,rev(X)),y=c(Y-Y.se,rev(Y+Y.se)),border = NA,col = "grey")
lines(X,Y,lwd=4,lty=3)

# Landsat 5 observation period
arrows(x0=1984,x1=2011,y0=8,y1=8,col = "red",lwd=2,angle = 90,length = 0.1)
arrows(x0=2011,x1=1984,y0=8,y1=8,col = "red",lwd=2,angle = 90,length = 0.1)
text(x=1992,y=9,"L5 TM",col="red",cex=1.5)

# Landsat 7 observation period
arrows(x0=2021,x1=1999,y0=9,y1=9,col = "black",lwd=2,angle = 90,length = 0.1)
text(x=2006,y=10,"L7 ETM+",col="black",cex=1.5)

# Landsat 8 observation period
arrows(x0=2021,x1=2013,y0=10,y1=10,col = "orange",lwd=2,angle = 90,length = 0.1)
text(x=2017,y=11,"L8 OLI",col="orange",cex=1.5)
box()
dev.off()

# -----------------------------------#
# ----- FIGURE BIAS - LOAD DATA ---- #
# -----------------------------------#
# the 100.000 samples were randomly selected before (under FIGURE 1A)

# SELECT 100.000 SAMPLES FROM VEGETATION PRODUCTIVITY
NDVIL =  list.files("TOSHARE/DATA/COMMENT_DATA",recursive = T,full.names=T,pattern="NDVI")
YEAR = 1984:2021

# Load all data as stack
NDVI = sds(NDVIL)
DF.NDVI = data.frame(ID = 1:sampleSize)
YEAR = 1984:2021

# Convert to data.frame for the selected samples
for(i in 1:length(nlyr(NUMB))){
  print(YEAR[i])
  DF.NDVI[,i] = NDVI[i][n][,1]
  colnames(DF.NDVI)[i] = paste0("NDVI_",YEAR[i])
}

# SELECT 100.000 SAMPLES FROM SUMMER SNOW
SSL =  list.files("TOSHARE/DATA/COMMENT_DATA",recursive = T,full.names=T,pattern="Prob")
# SSL =  list.files("TOSHARE/DATA/ORIGINAL_DATA/PERM",recursive = T,full.names=T,pattern="Perm")
YEAR = 1984:2021

# Load all data as stack
SS = sds(SSL)
DF.SS = data.frame(ID = 1:sampleSize)
YEAR = 1984:2021

names(NUMB)
# Convert to data.frame for the selected samples
for(i in 1:length(nlyr(NUMB))){
  print(YEAR[i])
  DF.SS[,i] = SS[i][n][,1]
  colnames(DF.SS)[i] = paste0("SS_",YEAR[i])
}

# SELECT 100.000 SAMPLES FROM PERMANENT SNOW
PERML =  list.files("TOSHARE/DATA/COMMENT_DATA",recursive = T,full.names=T,pattern="Perm")
YEAR = 1984:2021

# Load all data as stack
PERM = sds(PERML)
DF.PERM = data.frame(ID = 1:sampleSize)
YEAR = 1984:2021

# Convert to data.frame for the selected samples
for(i in 1:length(nlyr(NUMB))){
  print(YEAR[i])
  DF.PERM[,i] = PERM[i][n][,1]
  colnames(DF.PERM)[i] = paste0("PERM_",YEAR[i])
}


# ------------------------#
# ----- FIGURE 1 C ------ #
# ------------------------#

png("TOSHARE/FIGURES/FIGURE1c_ndvi.png",width=1500,height=1400,res = 300)

# Prepare data.frame
DF.N2 = round(data.frame(newcol = c(t(DF.N)), stringsAsFactors=FALSE))
DF.NDVI2 = data.frame(newcol = c(t(DF.NDVI)), stringsAsFactors=FALSE)
DF.ALL = data.frame(NUMB = DF.N2, NDVI = DF.NDVI2, YEAR = rep(1984:2021, each=sampleSize))
colnames(DF.ALL) = c("NUMB","NDVI","YEAR")

# Shows only between 1 and 10 observations
DF.ALL[which(DF.ALL$NUMB == 0),]=NA
DF.ALL[which(DF.ALL$NUMB > 10),]=NA

# Boxplot
boxplot(DF.ALL$NDVI ~ DF.ALL$NUMB,xaxs="i",col=adjustcolor("darkgreen",alpha.f = 0.5),
        xlab="Mean number of images per year",outline=F,ylim=c(0,1),
        ylab="Vegetation productivity")
LOESS = loess.smooth(DF.ALL$NUMB,DF.ALL$NDVI, span = 1, degree = 2)
lines(LOESS$x,LOESS$y, col="red",lwd=4,lty=1)

dev.off()






values <- DEM2[n]

DF.DEM = data.frame(ID = 1:sampleSize)#data.frame(newcol = c(t(values)), stringsAsFactors=FALSE)
for(i in 1:length(nlyr(NUMB))){
  
  DF.DEM[,i] = values
  colnames(DF.DEM)[i] = paste0("ELEV_",YEAR[i])
}


# Prepare data.frame
DF.N2 = round(data.frame(newcol = c(t(DF.N)), stringsAsFactors=FALSE))
DF.DEM2 = round(data.frame(newcol = c(t(DF.DEM)), stringsAsFactors=FALSE))
DF.ALL = data.frame(NUMB = DF.N2, DEM = DF.DEM2, YEAR = rep(1984:2021, each=sampleSize))
colnames(DF.ALL) = c("NUMB","DEM","YEAR")

# Shows only between 1 and 10 observations
DF.ALL[which(DF.ALL$NUMB == 0),]=NA
DF.ALL[which(DF.ALL$NUMB > 10),]=NA

png("TOSHARE/FIGURES/FIGURE_DEM.png",width=1500,height=1400,res = 300)

# Boxplot
boxplot(DF.ALL$DEM ~ DF.ALL$NUMB,xaxs="i",col=adjustcolor("darkgreen",alpha.f = 0.5),
        xlab="Mean number of images per year",outline=F,
        ylab="Elevation")
LOESS = loess.smooth(DF.ALL$NUMB,DF.ALL$DEM, span = 1, degree = 2)
lines(LOESS$x,LOESS$y, col="red",lwd=4,lty=1)

dev.off()
# ------------------------#
# ----- FIGURE 1 D ------ #
# ------------------------#

png("TOSHARE/FIGURES/FIGURE1d_prob.png",width=1500,height=1400,res = 300)

# Prepare data.frame
DF.N2 = round(data.frame(newcol = c(t(DF.N)), stringsAsFactors=FALSE))
DF.SS2 = data.frame(newcol = c(t(DF.SS)), stringsAsFactors=FALSE)
DF.ALL = data.frame(NUMB = DF.N2, SS = DF.SS2)
colnames(DF.ALL) = c("NUMB","SS")

# Shows only between 1 and 10 observations
DF.ALL[which(DF.ALL$NUMB == 0),]=NA
DF.ALL[which(DF.ALL$NUMB > 10),]=NA

# Remove pixels that are almost never snowy (most of it so it streches the plot to keep it and make it impossible to understand)
# The result is the same by changing the value, maybe it would be more logical to only remove when == 0 ?
# Might be better with 0.01 than 0.1 (0.1 was used in the technical comment)
DF.ALL$SS[which(DF.ALL$SS < 0.01)] = NA

boxplot(DF.ALL$SS ~ DF.ALL$NUMB,xaxs="i",col=adjustcolor("royalblue",alpha.f = 0.5),
        xlab="Mean number of images per year",outline=F,ylim=c(0,1),
        ylab="Summer snow proportion")
LOESS = loess.smooth(DF.ALL$NUMB,DF.ALL$SS, span = 1, degree = 2)
lines(LOESS$x,LOESS$y, col="red",lwd=4)

dev.off()

# ------------------------#
# ----- FIGURE 1 E ------ #
# ------------------------#

png("TOSHARE/FIGURES/FIGURE_PERMobs.png",width=1500,height=1400,res = 300)

# Prepare data
DF.N2 = round(data.frame(newcol = c(t(DF.N)), stringsAsFactors=FALSE))
DF.PERM2 = data.frame(newcol = c(t(DF.PERM)), stringsAsFactors=FALSE)
DF.ALL = data.frame(NUMB = DF.N2, PERM = DF.PERM2, YEAR = rep(1984:2021, each=sampleSize))
colnames(DF.ALL) = c("NUMB","PERM","YEAR")

# Range between 1 to 10 images
DF.ALL[which(DF.ALL$NUMB == 0),]=NA
DF.ALL[which(DF.ALL$NUMB > 10),]=NA

# Compute percentage of permanent snow cover
AGG = aggregate(DF.ALL$PERM,by=list(DF.ALL$NUMB),function(x){table(x)})
AGG = data.frame(NUMB = AGG$Group.1,NOSNOW=AGG$x[,1],SNOW=AGG$x[,2])
AGG$PERC = (AGG$SNOW/(AGG$NOSNOW+AGG$SNOW))*100

BP=barplot(AGG$PERC ~ AGG$NUMB,col=adjustcolor("darkblue",alpha.f = 1),
           xlab="Mean number of images per year",ylim=c(0,20),
           ylab="Proportion of permanent snow",xaxt="n")
axis(1,at = BP[,1],labels = 1:10)
box()

# It has actually no sense to plot a loess on this plot, should be removed !!!
#LOESS = loess.smooth(AGG$NUMB,AGG$PERC, span = 1, degree = 2)
#lines(LOESS$x,LOESS$y, col="red",lwd=4)

dev.off()
























# --------------------------#
# ----- SELECT PIXELS ----- #
# --------------------------#
# Starting from here, we used the data from RANDOM_DATA

LIST = list.files("TOSHARE/DATA/RANDOM_DATA/NDVI/",full.names=T,pattern="NDVI")
NAMES = list.files("TOSHARE/DATA/RANDOM_DATA/NDVI/",pattern="NDVI")

# Keep the names of each images
NAMES = StrRight(NAMES,29)

# SELECT IMAGE TO REPROJECT ALL IMAGES ON
# it is because all data are not exactly on the same extent
GRID = rast(LIST[503])

# Remove values (useless but cleaner)
GRID[which(GRID[] == 0)] = NA

# Select randomly 10.000 pixels (might be done with more but it takes some time to run...)
SAMPLE = 10000  # => this result in around 500 pixel
n = which(!is.na(GRID[]))
SAMPLE=length(n)

Sample# -----------------------------------------------------------------------#
# ----- COMPUTE TRENDS WITH ALL IMAGES ("ALL" in Figure 1F and 1G) ----- #
# -----------------------------------------------------------------------#

numberOfSamplePerYear=3;
qtype=1;

# LIST IMAGES
LIST.NDVI = list.files("TOSHARE/DATA/RANDOM_DATA/NDVI/",full.names=T,pattern="NDVI")
NAMES = list.files("TOSHARE/DATA/RANDOM_DATA/NDVI/",pattern="NDVI")
NAMES = StrRight(NAMES,29)

# We only keep the main path/row to not account for overlapping tiles.
# Overall the bias will be lower on overlapped surfaces, but it introduces a spatial bias (areas more or less affected by the bias)
# It is something we did not developed in the technical comment
PR = substr(NAMES,6,11)
LIST.NDVI = LIST.NDVI[which(PR == "195028")]

# Keep the year of each images
LISTYEAR = as.numeric(substr(StrRight(LIST.NDVI,17),1,4))

# The time series
YEARS = 1984:2021

# Prepare dataframe
# This one is for slope
DF.SLP.NDVI.BIASED = data.frame(ID = 1:SAMPLE)
DF.SLP.NDVI_original.BIASED = data.frame(ID = 1:SAMPLE)
# This one is for p-values
DF.PVAL.NDVI.BIASED = data.frame(ID = 1:SAMPLE)
DF.PVAL.NDVI_original.BIASED = data.frame(ID = 1:SAMPLE)
# This one is for number of images per year
DF.NUMBTOT.BIASED = data.frame(YEAR = 1984:2021)
z=2 # it does not change dynamically here (will be used for iterations on the next loop)

# Intermediates data.frame
DF.NUMB.BIASED = data.frame(ID = 1:SAMPLE)
DF.NDVI.BIASED = data.frame(ID = 1:SAMPLE)
DF.NDVI_original.BIASED = data.frame(ID = 1:SAMPLE)

# The Loop to compute trends, p-values and number of images for each year (NO RANDOMIZATION !!!!)
for(i in 1:length(YEARS)){
  # i<-1
  print(paste0(YEARS[i]))
  year = YEARS[i]
  
  # SELECT PER YEAR
  LIST.NDVIcur = LIST.NDVI[which(LISTYEAR == year)]
  
  ############################################## NDVI 0.75 QUANTILE ----
  
  # LOAD IMAGES AND KEEP ONLY SAMPLES
  DF.Nndvi = data.frame(ID = 1:SAMPLE)
  STACK = sds(LIST.NDVIcur)
  for(v in 1:length(nlyr(STACK))){
    CUR = STACK[v] # image 1, image 2 etc. 
    CUR[which(CUR[] == 0)] = NA
    DF.Nndvi[,v] = CUR[n][,1]
    colnames(DF.Nndvi)[v] = paste0("NDVI")
  }
  
  # COMPUTE 0.75 NDVI QUANTILE
  # the very important line
  QUANT = apply(DF.Nndvi,1,function(x){quantile(na.omit(na.omit(x)[sample(1:length(na.omit(x)), replace=F)[1:numberOfSamplePerYear]]),0.75,na.rm=T, type = qtype)})
  QUANT_original = apply(DF.Nndvi,1,function(x){stats::quantile(x,0.75,na.rm=T, type = qtype)})
  DF.NDVI.BIASED[,i+1] = QUANT
  DF.NDVI_original.BIASED[,i+1] = QUANT_original
  colnames(DF.NDVI.BIASED)[i+1] = as.character(year)
  colnames(DF.NDVI_original.BIASED)[i+1] = as.character(year)
  
  # COMPUTE NUMBER OF VALUES
  for(k in 1:ncol(DF.Nndvi)){DF.Nndvi[which(!is.na(DF.Nndvi[,k])),k] = 1}
  DF.NUMB.BIASED[,i+1] = apply(DF.Nndvi,1,function(x){sum(x,na.rm=T)})
  colnames(DF.NUMB.BIASED)[i+1] = as.character(year)
  
}

# Take number of images
for(k in 1:ncol(DF.NUMB.BIASED)){DF.NUMB.BIASED[which(DF.NUMB.BIASED[,k]==0),k] = NA}
DF.NUMBTOT.BIASED[,z] = as.numeric(apply(DF.NUMB.BIASED[,-1],2,function(x){mean(x,na.rm=T)}))

# ----- COMPUTE TRENDS ----- #

# One iteration per pixel ==> for each sample location???
for(i in 1:nrow(DF.NDVI.BIASED)){
  print(paste0(i))
  # i<-1
  
  # Store data properly in data.frame
  dat.ndvi = DF.NDVI.BIASED[i,-1]
  dat.ndvi = data.frame(YEAR = as.numeric(colnames(dat.ndvi)), NDVI = as.numeric(dat.ndvi))
  dat.ndvi$NDVI[which(dat.ndvi$NDVI == 0)] = NA 
  dat.ndvi = as.data.frame(na.omit(dat.ndvi)) # remove no Data and 0 values

  dat.ndvi_original = DF.NDVI_original.BIASED[i,-1]
  dat.ndvi_original = data.frame(YEAR = as.numeric(colnames(dat.ndvi_original)), NDVI = as.numeric(dat.ndvi_original))
  dat.ndvi_original$NDVI[which(dat.ndvi_original$NDVI == 0)] = NA 
  dat.ndvi_original = as.data.frame(na.omit(dat.ndvi_original)) # remove no Data and 0 values
  
  # Keep only pixel if length > 12 as in Rumpf et al. 
  if(length(dat.ndvi$YEAR) > 12){
    
    # Compute mblm
    MOD.NDVI = mblm(NDVI ~ YEAR,dataframe = dat.ndvi)
    # Keep slope
    DF.SLP.NDVI.BIASED[i,z] = MOD.NDVI$coefficients[[2]]
    # Convert to time series
    dat.ts = as.ts(read.zoo(dat.ndvi))
    # Compute Mann-Kendall slope and directly store it
    DF.PVAL.NDVI.BIASED[i,z] = MannKendall(dat.ts)[[2]][1]

    MOD.NDVI_original = mblm(NDVI ~ YEAR,dataframe = dat.ndvi_original)
    # Keep slope
    DF.SLP.NDVI_original.BIASED[i,z] = MOD.NDVI_original$coefficients[[2]]
    # Convert to time series
    dat.ts_original = as.ts(read.zoo(dat.ndvi_original))
    # Compute Mann-Kendall slope and directly store it
    DF.PVAL.NDVI_original.BIASED[i,z] = MannKendall(dat.ts_original)[[2]][1]
    
  } else {
    DF.SLP.NDVI.BIASED[i,z] = NA; DF.PVAL.NDVI.BIASED[i,z] = NA
    DF.SLP.NDVI_original.BIASED[i,z] = NA; DF.PVAL.NDVI_original.BIASED[i,z] = NA
  }
}






# -----------------------#
# ----- FIGURE 1 G ----- #
# -----------------------#

# WITH THE BIAS !!!
# We compute the % of greening by classes (also browning but we did not use it)
PERC.PVAL.BIASED = na.omit(DF.PVAL.NDVI_original.BIASED)
PERC.SLP.BIASED = na.omit(DF.SLP.NDVI_original.BIASED)
PERC.PVAL.SAMPLED = na.omit(DF.PVAL.NDVI.BIASED)
PERC.SLP.SAMPLED  = na.omit(DF.SLP.NDVI.BIASED)
# explain: negative P value? How? 
PERC.BIASED.POS0.01 = (length(which(PERC.PVAL.BIASED$V2 < 0.01 & PERC.SLP.BIASED$V2 > 0))/ length(PERC.PVAL.BIASED$V2))*100
PERC.BIASED.POS0.05 = (length(which(PERC.PVAL.BIASED$V2 > 0.01 & PERC.PVAL.BIASED$V2 < 0.05 & PERC.SLP.BIASED$V2 > 0))/length(PERC.PVAL.BIASED$V2))*100
PERC.BIASED.NEG0.01 = (length(which(PERC.PVAL.BIASED$V2 < 0.01 & PERC.SLP.BIASED$V2 < 0))/length(PERC.PVAL.BIASED$V2))*100
PERC.BIASED.NEG0.05 = (length(which(PERC.PVAL.BIASED$V2 > 0.01 & PERC.PVAL.BIASED$V2 < 0.05 & PERC.SLP.BIASED$V2 < 0))/length(PERC.PVAL.BIASED$V2))*100

PERC.SAMPLED.POS0.01 = (length(which(PERC.PVAL.SAMPLED$V2 < 0.01 & PERC.SLP.SAMPLED$V2 > 0))/ length(PERC.PVAL.SAMPLED$V2))*100
PERC.SAMPLED.POS0.05 = (length(which(PERC.PVAL.SAMPLED$V2 > 0.01 & PERC.PVAL.SAMPLED$V2 < 0.05 & PERC.SLP.SAMPLED$V2 > 0))/length(PERC.PVAL.SAMPLED$V2))*100
PERC.SAMPLED.NEG0.01 = (length(which(PERC.PVAL.SAMPLED$V2 < 0.01 & PERC.SLP.SAMPLED$V2 < 0))/length(PERC.PVAL.SAMPLED$V2))*100
PERC.SAMPLED.NEG0.05 = (length(which(PERC.PVAL.SAMPLED$V2 > 0.01 & PERC.PVAL.SAMPLED $V2 < 0.05 & PERC.SLP.SAMPLED$V2 < 0))/length(PERC.PVAL.SAMPLED$V2))*100


# We make a matrix with (ALL) and without (3MAX) biased
MAT = matrix(c(PERC.BIASED.POS0.01 , PERC.BIASED.POS0.05,
               PERC.SAMPLED.POS0.01 , PERC.SAMPLED.POS0.05),ncol=2,nrow=2)
colnames(MAT) = c("ALL","3MAX")
rownames(MAT) = c("pos001","pos005")

# We make a matrix with (ALL) and without (3MAX) biased
# MATNEG = matrix(c(PERC.BIASED.NEG0.01 , PERC.BIASED.NEG0.05,
#                mean(PERC.NEG0.01) , mean(PERC.NEG0.05)),ncol=2,nrow=2)
# colnames(MATNEG) = c("ALL","3MAX")
# rownames(MATNEG) = c("neg001","neg005")


png("TOSHARE/FIGURES/FIGURE1G_q1_3.png",width=1500,height=1400,res = 300)

bp = barplot(MAT,ylim=c(0,100),col=c(adjustcolor("darkgreen",alpha.f = 0.5),
                                     adjustcolor("palegreen",alpha.f = 0.5)),
             ylab="% of pixels",las=1,
             width=4,xlim=c(0,10))
text(bp[1],50,paste0(round(PERC.BIASED.POS0.01,2),"%"),cex=2)
text(bp[1],40,"(P-val < 0.01)",cex=0.7)

text(bp[2],50,paste0(round(PERC.SAMPLED.POS0.01,2),"%"),cex=2)
text(bp[2],40,"(P-val < 0.01)",cex=0.7)

text(bp[1],20,paste0(round(PERC.BIASED.POS0.01+PERC.BIASED.POS0.05,2),"%"),cex=2)
text(bp[1],10,"(P-val < 0.05)",cex=0.7)

text(bp[2],20,paste0(round(PERC.SAMPLED.POS0.01+PERC.SAMPLED.POS0.05,2),"%"),cex=2)
text(bp[2],10,"(P-val < 0.05)",cex=0.7)

box(which="plot", bty="]")
dev.off()















