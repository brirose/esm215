#Use this script to perform a Mutual Information Analysis relating
#environmental factors to land use/land cover pattern in a region of
#northern Santa Barbara County
#
#Data are extracted from GIS grids in the ESM215\data\sy_data folder 
#
#load required packages
  library(entropy)
  library(dplyr)
  library(here)
##read data
  sydat <- read.csv(here("week_2/ex2_sample_data.csv"),header=TRUE,na.strings=c(-9999)) %>% na.omit()
##list variables
  str(sydat)
##define variables as factors and set levels for ordinal data (not required for MI analysis but
##useful for other classifier methods)
  sydat[,c(2:6)] <- lapply(sydat[,c(2:6)],factor)
  levels(sydat$winter_rad3) <- c(1,2,3)
  levels(sydat$all_rad3) <- c(1,2,3)
  levels(sydat$flowcum3) <- c(1,2,3)
##entropy of the dependent variable (landcover)
  i_landcov <- entropy.empirical(table(sydat$landcover))
##First level MIA: calculate mutusl information of landcover with
##geology, flow accumulation classes and solar radiation classes
  mi_landcov_geol <- table(sydat$landcover,sydat$geology) %>% mi.empirical()
  mi_landcov_flow <- table(sydat$landcover,sydat$flowcum3) %>% mi.empirical()
  mi_landcov_allrad <- table(sydat$landcover,sydat$all_rad3) %>% mi.empirical()
  mi_landcov_winrad <- table(sydat$landcover,sydat$winter_rad3) %>% mi.empirical()
  mia_level1 <- rbind(mi_landcov_geol,mi_landcov_flow,mi_landcov_allrad,mi_landcov_winrad)
##
##Redundancy of geology and landcover? 
    r_landcov_geol <- mi_landcov_geol/i_landcov
##Proceed with geology as the first splitting variable.
##Loop through each of 5 geology classes to calculate the MI of landcover with
##flow accumulation, annual and winter radiation.
  #
  mi_flow_2 <- NULL
  for(i in c(1:5)){
    y <- filter(sydat,geology == i)
    x <- table(y$landcover,y$flowcum3) %>% mi.empirical()
    mi_flow_2 <- c(mi_flow_2,x)
  }
  #
  mi_allrad_2 <- NULL
  for(i in c(1:5)){
    y <- filter(sydat,geology == i)
    x <- table(y$landcover,y$all_rad3) %>% mi.empirical()
    mi_allrad_2 <- c(mi_allrad_2,x)
  }
  #
  mi_winrad_2 <- NULL
  for(i in c(1:5)){
    y <- filter(sydat,geology == i)
    x <- table(y$landcover,y$winter_rad3) %>% mi.empirical()
    mi_winrad_2 <- c(mi_winrad_2,x)
  }
 # 
  
  
