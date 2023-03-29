####################
##  Housekeeping  ##
####################

rm(list = ls())

dir <- getwd()
setwd(dir)

## Set working directory (WD)
setwd(dir)


## ----load-libraries------------------------------------------------------
library(raster)# work with raster data
library(rgdal)# export GeoTIFFs and other core GIS functions
library(ggplot2)
library(magicfor)
library(nadiv)
library(segmented)
library(tidyverse)
require(lubridate)
require(reshape2)


#################################
########## BLOCK 1 ##############
########## INPUTS ##############
#################################

DPM_Model = 'LOESS' # 'SEG' , 'LOESS' 
rgb_idx = "GLI" ## GLI selected
vi_method = 'mean' ## Mean selected
threshold <- as.numeric(0.06) ## Threshold selected: 0.06
planting_date = '06-08-2022'  # '06-17-2020' , '06-05-2020'
AdjJul_31 = '07-31-2022'

MaturityData = read.csv("C:/Users/leoag/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2022/HURON_Mat/h._VIs_Method2/VIs/2022_HURON_RGB_VIs.csv",h=T) #ONLY FOR TEST THE 90 PLOTS


MaturityData = MaturityData %>% 
  separate(Plot_ID, into = 'Trial', extra = 'drop', remove = FALSE) %>% 
  mutate(Trial = str_sub(Trial[], 1, 4))

str(MaturityData)

MaturityData_sel1 = MaturityData %>% 
  select(Flight_date, Plot_ID, GLI_mean) #GLI_median

head(MaturityData_sel1)


MaturityData_sel1 = MaturityData_sel1 %>% 
  pivot_wider(names_from = Plot_ID, values_from = GLI_mean) #GLI_median

dim(MaturityData_sel1)

MaturityData_sel1 = MaturityData_sel1 %>% 
  filter(!row_number() %in% c(1))

dim(MaturityData_sel1)


# Accordgin to the lenght of the flights (data points collected or each mission of flights performed)

Dates<- MaturityData_sel1[,1]

Dates<- MaturityData_sel1[,1] %>% 
  mutate(Flight_date = str_sub(Flight_date[], 1, 8)) %>% 
  filter(nzchar(Flight_date))


planting_date <- as.Date(mdy(planting_date))
jul_date_pl <- yday(planting_date)


AdjJul_31 <- as.Date(mdy(AdjJul_31))
AdjJul_31 <- yday(AdjJul_31)

date_list <- list() 
for (i in 1:nrow(Dates)) {
  new_date <- as.Date(mdy(Dates[i,]))
  flight_date <- yday(new_date)
  MAT_ADJ1 <- flight_date - jul_date_pl
  
  MAT_ADJ2 <- AdjJul_31 - jul_date_pl
  
  MAT_ADJ3 <- MAT_ADJ1 - MAT_ADJ2
  
  print(MAT_ADJ3)
  date_list[[i]] <- MAT_ADJ3
  
  if (i == 1) {
    flight_ini <-MAT_ADJ3
    
    
  } else if ( i == nrow(Dates)) {
    flight_end <- MAT_ADJ3
  }
  
}

days<- seq(flight_ini,flight_end,by=1)

head(MaturityData_sel1)

IDplots <- colnames(MaturityData_sel1)
flights <- as.numeric(unlist(date_list))


#################################
########## BLOCK 2 ##############
########## LOESS ##############
#################################

if ( DPM_Model == "LOESS") {
  
  magic_for(print, silent = TRUE)
  ptm <- proc.time()
  
  for (k in IDplots[2:ncol(MaturityData_sel1)]){
    data_x <- as.numeric(unlist(MaturityData_sel1[,k]))
    nge_loess_loop<-loess(data_x ~ flights)
    fitted.nge_loop<-predict(nge_loess_loop, days)
    list_date_pred<-approx(fitted.nge_loop, days, xout = threshold) #xout = threshold selected
    print(round(list_date_pred$y)) 
  }
  proc.time() - ptm
  
  
  Mat_Est<- magic_result_as_dataframe()
  colnames(Mat_Est)[1:2] <- c("Plots_ID", paste0("Mat_",DPM_Model,'_',rgb_idx,'_',vi_method, '_', threshold))
  
  if ( NA %in% Mat_Est[,2]) {
    message("Consider changing the threshold - NAs found:")
    print(sum(is.na(Mat_Est)))
    
  }
  
  write.csv(Mat_Est, paste0("Mat_",DPM_Model,'_',rgb_idx,'_',vi_method,'_',threshold,".csv"), quote = F, row.names = F)
  
  
  ##########################
  #######  SEG. REG. #######
  ##########################
  
}  else if( DPM_Model == "SEG") {
  
  ptm <- proc.time()
  lm_all<-list()
  
  for (j in IDplots[2:ncol(MaturityData_sel1)]){
    #print(k)
    
    data_x <- as.numeric(unlist(MaturityData_sel1[,j]))
    mod<-lm(data_x ~ flights) #glm also can be used to fit generalized linear models, but check if the script will work before)
    attempts = 0
    if.false <- F
    
    while(if.false == F){
      attempts <- attempts + 1
      tryCatch({
        if(nrow(MaturityData_sel1) > 7 && attempts < 100){
          #Volpato et al., 2020 paper recomendation is at least 8 flights to use 2 breaks points -- (3 optimum phases in the regression model = vegetative, senescence and maturity)
          seg_loop<-try(segmented(mod, seg.Z = ~ flights, npsi = 2, control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T)
          
          if("try-error" %in% class(seg_loop)) {
            seg_loop<-lm(data_x ~ flights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((threshold - ncpt) / slps)
            #print(DPM)
            lm_all[[j]] <- DPM
            
          } else if (!is.null(seg_loop$psi)) {
            
            slps <- slope(seg_loop)$flights
            ncpt <- intercept(seg_loop)$flights
            
            if ( rgb_idx == "GLI" || rgb_idx == "TGI" ) {
              slope <- min(slps[,1])
            } else if (rgb_idx == "HI") {
              #i.e., RGBindex == "HI"
              slope <- max(slps[,1])
            } else {
              print("Vegetation index not present in the formula")
            }
            slope_interc <- which(slps[,1] == slope)
            B1_interc <- ncpt[slope_interc,1]
            
            DPM <- round((threshold - B1_interc) / slope)
            #print(DPM)
            lm_all[[j]] <- DPM
            
          } else { 
            seg_loop<-lm(data_x ~ flights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((threshold - ncpt) / slps)
            #print(DPM)
            lm_all[[j]] <- DPM
          }
          
          
        } else {
          
          seg_loop<-try(segmented(mod, seg.Z = ~ flights, npsi = 1,  control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T)
          
          if("try-error" %in% class(seg_loop)) {
            seg_loop<-lm(data_x ~ flights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((threshold - ncpt) / slps)
            #print(DPM)
            lm_all[[j]] <- DPM
          
          } else if (!is.null(seg_loop$psi)) {
            
            slps <- slope(seg_loop)$flights
            ncpt <- intercept(seg_loop)$flights
            
            if ( rgb_idx == "GLI" || rgb_idx == "TGI" ) {
              slope <- min(slps[,1])
            } else if (rgb_idx == "HI") {
              #i.e., RGBindex == "HI"
              slope <- max(slps[,1])
            } else {
              print("Vegetation index not present in the formula")
            }
            slope_interc <- which(slps[,1] == slope)
            B1_interc <- ncpt[slope_interc,1]
            
            DPM <- round((threshold - B1_interc) / slope)
            #print(DPM)
            lm_all[[j]] <- DPM
            
          } else { 
            seg_loop<-lm(data_x ~ flights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((threshold - ncpt) / slps)
            #print(DPM)
            lm_all[[j]] <- DPM
          }
        }
      }, error = function(e){
      }, finally = {})
      
      if.false <- T	
    }
    
  }
  
  lm_all<-melt(lm_all)
  lm_all <- lm_all %>% 
    relocate(L1)
  
  colnames(lm_all)<- c("Plots_ID", paste0("Mat_",DPM_Model,'_',rgb_idx,'_',vi_method, '_',threshold))
  
  if ( NA %in% lm_all[,2]) {
    message("Consider changing the threshold - NAs found:")
    print(sum(is.na(lm_all)))
  }
  
  write.csv(lm_all, paste0("Mat_",DPM_Model,'_',rgb_idx,'_',vi_method,'_',threshold,".csv"), quote = F, row.names = F)
  
  proc.time() - ptm
}
