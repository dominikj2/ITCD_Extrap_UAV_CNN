rm(list=ls())
options(digits = 12)
options(warn=1) #options(warn=2, error=recover)

library(lidR) #
# library(gstat)
library(rgdal) #
library(stats) #
library (pastecs)
# library(lattice)
library (maptools) #
#library (boolean3)
# library(boot)
# library(plotrix)
# library(fields)
# library(VoxR)
library(raster) #
library(spatstat) #
# library(plyr)
library(dplyr) #
# library(e1071)
library(rgeos) #
# library(rgl)
# library(clusterCrit)
# library(dbscan)
# library(data.table)
#library(SDraw)
library(MASS) #
library(stringr) #
# library(scatterplot3d)
# library(fossil)
# library(deldir)
#library(EBImage)
#library(animation)
#library(magick)
#library(spatialEco)
# library(DescTools)
#library(randomcoloR)
# library(RColorBrewer)
# library(SDMTools)
# library(rgdal)
#library(fastmatch)
# library(tidyr)
# library(Comp_Nameare)
# library(DGVM3D) 
# library(randomcoloR) #
library(alphashape3d)
# source("https://bioconductor.org/biocLite.R")
# biocLite("EBImage")
library(EBImage) #
# library(BBmisc)
# library(mapview)
# library(spdep)
#library(Comp_Nameare)
# library(NCmisc)
library(tidyr) #
# library(sp) #
library(fields) #
library(data.table) # 
library(sf)
library(lwgeom)
library(spdep) #
library(units)
library(randomcoloR)
library(oceanmap)
library(geosphere)

#Comp_Name <- "/home/dj806/"
Comp_Name <- "//rcgdata/dj806/"

#list.functions.in.file("D:/CODE/CNN/R_Code/CNN_PLOT_SAMPLE_PREP_V20.R", alphabetic = TRUE)
#browser()

#install.packages(c("Comp_Nameare", "RColorBrewer", "randomcoloR", "magick", "animation", "SDraw"))

##################
# MAIN PARAMETERTS
##################

Para_TriShpParaCnt = 10

Para_MaxZ_Shrink <- 38.99
Para_plot_Res <- 15 # MINUS ONE IS A WORK AROUND TO AVOID IT BEING 16!!

Para_Samples <- 1

Para_Sl_Z <- 0.4 # THIS WAS USED TO SLICE THE ALS AND ASSIGN THE VALUES OF USE. NOW I AM USI
Para_Vx_R_Res <- 0.4
Para_Above_Below_Sl <- 1
Para_Dist_ZeroCloseTID <- 1.4 # 1 + 0.4
Para_Min_Portion_Grid <- 0.8

Para_Faces_Used <-  4
Para_Vox_Res <- 1

# PARAMETERS
Para_Base_WL <- 0.5
Para_K_BandWidth <- 0.4             # c(0.1, 0.2, 0.3, 0.4, 0.5) # 0.1 
Para_Gap_Threshold_Percent_U <- 0.2 # 0.4   # c(0.1, 0.2, 0.3, 0.4) # 0.2 
Para_Gap_Threshold_Percent_U2 <- 0.1 # 0.4   # c(0.1, 0.2, 0.3, 0.4) # 0.2 

Para_C_Res <- 0.5
Para_Mid_Res <- 0.2

Para_C_Base_BW <- 0.5
Para_C_Base_Thresh <- 0.2

Para_MinStrata <- 6
Para_strata_Res <-  0.5
Para_Under_Strata_Z <- 7

Para_BBox_Overlap <- 0.7
Para_BBox_Overlap_ZRng <- 5
Para_ZRng_Loc_BBox <- 3

Para_BBox_Z_MergeOverlap <- 5

Plot <- "Yes"

#Proj_Sys <- "+proj=utm +zone=55 +south +ellps=WGS84 +units=m +no_defs"
# Proj_Sys <- "+proj=utm +zone=55"
Proj_Sys <- "+proj=utm +zone=55 +south +ellps=WGS84 +units=m +no_defs" # st_crs(4326) 
#Proj_Sys <- Proj_Sys$input

Para_Base_WL <- 0.5 # BASE LENGTH AND WIDTH ... THIS IS NORMALISED WHEN EVERYTHING ELSE GETS NORMALISED

################
# MAIN DIRECTORY
################

#Comp_Name <- "//rcgdata/dj806/" # HPC SERVER  # Comp_Name <- "O:/research/ForestWater/BACKUP_Y_Drive/" # BACKUP SERVER

# source('//rcgdata/dj806/CNN/R_Code/SUPER_Comp_Name__CNN_SAMPLE_GENERATOR_V3.R')

###########
# FUNCTIONS
###########

Folder_Version <- "_V21" ### DOM DOM DOM MAKE SURE YOU CHANGE THIS INTO MOST UPDATED R FUNCTION FILE
R_Code_Version <- "_V21"

# source(paste(Comp_Name, "/R_Code/FUNCTIONS_CANOPY_SEG_V1.R", sep="")) # FUNCTIONS_TREE_DETECTION_Comp_NameLETE_DEVELOPING_V5_Desktop_UnderNEW_V5.R

source(paste(Comp_Name, "/CNN/R_Code/TORCH_ITCD_EXTRAP",R_Code_Version,"/TORCH_FUNCTIONS_CNN_MASTER",R_Code_Version, ".R", sep=""))


# source("D:/CODE/TINN_R/LIDAR_TREE_DETECT/FUNCTIONS_TREE_DETECTION_Comp_NameLETE_DEVELOPING_V5.R")

###############
# INPUT FOLDERS
###############

# ALS DATA
FOLDER_ALS <- paste(Comp_Name, "CNN/AEROPLANE_LAS_OVER_UAS_FLIGHT", sep="")  # NOTE THAT ALS DATA IN THIS FOLDER DOES NOT BUFFER EACH FLIGHT AND HAS MULTIPLE FILES PER FLIGHT
# FOLDER_ALS_O <- paste(Comp_Name, "CNN/ALS_OVER_UAS_FLIGHT_BUFF", sep="") # O
FOLDER_ALS_O_RmOut <- paste(Comp_Name, "CNN/ALS_OVER_UAS_FLIGHT_BUFF_RmOutlier", sep="") # O

FOLDER_UAS_SHP <- paste(Comp_Name, "CNN/SHP", sep="")
FOLDER_CSV <- paste(Comp_Name, "CNN/CSV", sep="")
FOLDER_ITCD_I <- paste(Comp_Name, "CNN/OUTPUT_V3", sep="") # REPLACE THIS IF RUNNING SEGMENTATION AGAIN....
FOLDER_GIS_I <- paste(Comp_Name, "CNN/FOLDER_GIS_INPUT", sep="") #Y:\CNN\FOLDER_GIS_INPUT\Aeroplane_Grids_WithUASFlight.shp


##############################################
# OPEN CSV (SCE PARAMETERS AND FLIGHT OfSETS)
##############################################
# REPLACE THIS IF RUNNING SEGMENTATION AGAIN....
SCE_Para_I <- read.csv(paste(Comp_Name, "CANOPY_SEGMENTATION/SCE_OPTIM_V60_GIS/ANALYSIS_Output_FIELD/Ridge_Regression_SCE_Para_Min_Z_SCE_StemOnly_FSCORE.csv", sep=""), row.names = NULL)
SCE_Para_I$TID <- SCE_Para_I$PSID

ALS_Offset_Mean <- read.csv(paste(FOLDER_CSV, "/All_Flights_Offset_02_BEST.csv", sep=""), row.names = NULL)

######################
# LOOP THROUGH FLIGHTS
######################
# Flights <- SCE_Para_I$TID #list.files(FOLDER_UAS_SHP, pattern = "Flight_")
# Flights <- c(1 ,  2,  3, 4,  5, 41)  # 1,
# Flights <- 2
#Flights <- c(6,  7,  8,  9, 10, 44)
#Flights <- c(11, 12, 13, 15, 17)
#Flights <- c( 19, 18,21, 22, 26)
#Flights <- c( 28, 27, 29, 30, 33, 43 ) # 
#Flights <- c(35, 36, 37, 39, 40)

###############
# OUTPUT FOLDER 
###############


dir.create(file.path(paste(Comp_Name, "CNN", sep=""),  paste("THINNED_SAMPLES_TORCH", Folder_Version, sep="")), showWarnings = FALSE)


# RANDOMLY ASSIGN FLIGHTS TO TRAINING AND VALIDATION
#All_Flights <- c(1, 2, 3, 4,5,6,7,8,9,10,11,12,13,15,17,18,19,20,21,22,26,27,28,29,30,32,33,34,35,36,37,39,40,41,43,44,46,47,48,49, 51, 52, 53) 
#Train_Flights <- sample(All_Flights, round(0.8*length(All_Flights))) # SAMPLE 80% of the flights 
Train_Flights <- c(3, 12, 44,  1,  4, 27, 19,  6, 40, 52, 18, 29, 37, 17, 20, 11, 53, 22, 32, 26, 33,  9, 43, 41, 36,  2, 28, 51, 21, 30,  8, 15, 46, 35)
#Valid_Flights <- setdiff(All_Flights, Train_Flights) 
Valid_Flights <- c(5, 7, 10, 13, 34, 39, 47, 48, 49)
#Flights <- c(17, 18, 19, 2) #47, 48,49, 51,
#Flights <- c(13, 34, 39)
#Flights <- c(47, 48, 49, 5)
#Flights <- c(21,22,26, 27, 28,20)
#Flights <- c( 5,49, 9, 8, 53, 52)
Flights <- c( 40, 41, 43, 44, 46)
if(Flights %in% Valid_Flights){
  dir.create(file.path(paste(Comp_Name, "CNN/THINNED_SAMPLES_TORCH", Folder_Version, sep=""),  "VALIDATE_DATA"), showWarnings = FALSE)
  FOLDER_O <- paste(Comp_Name, "CNN/THINNED_SAMPLES_TORCH", Folder_Version, "/VALIDATE_DATA", sep="")
}
if(Flights %in% Train_Flights){
  dir.create(file.path(paste(Comp_Name, "CNN/THINNED_SAMPLES_TORCH", Folder_Version, sep=""),  "TRAIN_DATA"), showWarnings = FALSE)
  FOLDER_O <- paste(Comp_Name, "CNN/THINNED_SAMPLES_TORCH", Folder_Version, "/TRAIN_DATA", sep="")
}


for (f in 1:length(Flights)){
  
  FID <- numextract(Flights[f])

  ###############
  # CREAT FOLDERS 
  ###############

  # FLIGHT FOLDER FOR OUTPUT DIRECTORY
  dir.create(file.path(paste(FOLDER_O, sep=""),  paste("Flight_",  FID, sep="")), showWarnings = FALSE)
  
  dir.create(file.path(paste(FOLDER_O, "/Flight_",  FID, sep=""),  "LAS"), showWarnings = FALSE)
  FOLDER_LAS_O <- paste(FOLDER_O, "/Flight_",  FID, "/LAS", sep="")

  dir.create(file.path(paste(FOLDER_O, "/Flight_",  FID, sep=""),  "CSV"), showWarnings = FALSE)
  FOLDER_CSV_O <- paste(FOLDER_O, "/Flight_",  FID, "/CSV", sep="")
  
  dir.create(file.path(paste(FOLDER_O, "/Flight_",  FID, sep=""),  "SHP"), showWarnings = FALSE)
  FOLDER_SHP_O <- paste(FOLDER_O, "/Flight_",  FID, "/SHP",sep="")

  dir.create(file.path(paste(FOLDER_O, "/Flight_",  FID, sep=""),  "PDF"), showWarnings = FALSE)
  FOLDER_PDF_O <- paste(FOLDER_O, "/Flight_",  FID, "/PDF",sep="")

  ################################################################################################################################################### 1
  ################################################################################################################################################### 1
  # PERFORM OFFSET SHIFT OF EACH FLIGHT TO ALIGN WITH ALS
  # REMOVE BUFFERS FROM GRID LAS AND TID TABLES
    # LOOP THROUGH GRIDS AND IDENTIFY TID WITHIN EACH GRID (WITHOUT BUFFER) AND GIVE THEM UNIQUE TID STARTING FROM 2
    # TID STARTS FROM 2 FOR EACH FLIGHT. YOU WILL NEED TO INCREMENTALLY INCREASE AFTER YOU PERFORM HPC ON EACH FLIGHT SEPERATELY 
  ################################################################################################################################################### 1
  ################################################################################################################################################### 1

  ########################################
  # GET FLIGHT OFFSET AND APPLY TO THE ALS
  ########################################
  
  ALS_Offset_oneF <- ALS_Offset_Mean[which(ALS_Offset_Mean$Flights ==  FID),]

  ####################
  # SHIFT (OFFSET) ALS 
  ####################
  LAS_ALS_oneF_allP <- lidR::readLAS(paste(FOLDER_ALS_O_RmOut, "/F_",  FID, "_Buff.laz", sep=""), select = "xyzp0")
  LAS_ALS_oneF_allP@data$PointSourceID <- as.integer(1)
  LAS_ALS_oneF_allP <- add_lasattribute(LAS_ALS_oneF_allP, x=LAS_ALS_oneF_allP@data$PointSourceID, name="TID", desc ="TID")
  LAS_ALS_oneF_allP <- add_lasattribute(LAS_ALS_oneF_allP, x=as.integer(0), name="LAS_Type", desc ="LAS_Type")
  
  # crs(LAS_ALS_oneF_allP) <- Proj_Sys ### DOM DOM DOM !!! REMOVED FOR SUPERCOMPUTER 31/5/21

  LAS_ALS_oneF_Corr <- LAS_ALS_oneF_allP
  LAS_ALS_oneF_Corr$X <- LAS_ALS_oneF_Corr$X + ALS_Offset_oneF$X_Offset
  LAS_ALS_oneF_Corr$Y <- LAS_ALS_oneF_Corr$Y + ALS_Offset_oneF$Y_Offset

  # LAS_ALS_oneF_Corr@data$PointSourceID <- as.integer(1)
  #LAS_ALS_oneF_Corr <- add_lasattribute(LAS_ALS_oneF_Corr, x=LAS_ALS_oneF_Corr@data$PointSourceID, name="TID", desc ="TID")
  
  ####################################
  # OPEN POLYGON WITH GRIDS FOR FLIGHT
  ####################################

  Poly_Ext_allG <- st_read( 
    dsn= paste(FOLDER_UAS_SHP, "/Flight_",FID, sep="") , 
    layer=paste("F" ,  FID, "_Extent_CHull_NotBuffer", sep="")
  )  %>% st_set_crs(Proj_Sys)
  
  Poly_Ext_allG$GridID <- Poly_Ext_allG$ID
  G_ID <- Poly_Ext_allG$GridID
  
  Poly_oneF_Boundary <- st_read( 
    dsn= paste(FOLDER_UAS_SHP, "/Flight_",FID, sep="") , 
    layer=paste("F" ,  FID, "_Extent_CHull", sep="")
  )  %>% st_transform (Proj_Sys)

  ####################
  # LOOP THROUGH GRIDS (EACH FLIGHT IS GRIDDED INTO 60X60)
  ####################  
  
  # REMOVE PREVIOUS FLIGHTS DATA 
  if(exists("AttTID_oneF_allG")){rm(AttTID_oneF_allG)} #in GG LOOP 
  if(exists("LAS_oneF_allG")){rm(LAS_oneF_allG)}     #in GG LOOP 
  
  # LOOP THROUGH EACH GRID
  max_TID <- 1 # MAKES SURE THAT FIRST TID IS 2 SO ALL UNDER/RESIDUALS ARE 1 AND ALL EMPTY ARE 0
  for(GG in 1:length(G_ID)){
    
    ############################################
    # OPEN GRID LAS FILES (TID, Zero and UNDER)
    ############################################

    # FILES GENERATED IN ITCD
    Files_LAS_G <- list.files(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/LAS/F",  FID, "_Grid_", G_ID[GG], sep=""), pattern = "BackupFINAL")
    ### DOM DOM DOM !!! FIND OUT WHICH CODE GENERATED THIS GRIDDED OUTPUT THAT IS USED AS INPUT BELOW?
    
    if(length(Files_LAS_G) > 0){
    
      # LAS_Zero
      LAS_oneF_oneG_Zero <- lidR::readLAS(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/LAS/F",  FID, "_Grid_", G_ID[GG],"/F",  FID, "_LAS_Zero_BackupFINAL", ".laz",  sep=""), select = "xyzp0")
      LAS_oneF_oneG_Zero@data$PointSourceID <- as.integer(1)
      LAS_oneF_oneG_Zero <- add_lasattribute(LAS_oneF_oneG_Zero, x=as.integer(1), name="TID", desc ="TID")
      LAS_oneF_oneG_Zero <- add_lasattribute(LAS_oneF_oneG_Zero, x=as.integer(2), name="LAS_Type", desc ="LAS_Type")
      LAS_oneF_oneG_Zero <- add_lasattribute(LAS_oneF_oneG_Zero, x=as.integer(G_ID[GG]), name="GridID", desc ="GridID")
      
      # LAS_TID
      LAS_oneF_oneG_TID <- lidR::readLAS(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/LAS/F",  FID, "_Grid_", G_ID[GG],"/F",  FID, "_LAS_PSID_BackupFINAL", ".laz",  sep=""),   select = "xyzp0")
      LAS_oneF_oneG_TID@data$PointSourceID <- as.integer(LAS_oneF_oneG_TID@data$PointSourceID) + as.integer(1) 
      LAS_oneF_oneG_TID <- add_lasattribute(LAS_oneF_oneG_TID, x=as.integer(LAS_oneF_oneG_TID@data$PointSourceID), name="TID", desc ="TID")
      LAS_oneF_oneG_TID <-add_lasattribute(LAS_oneF_oneG_TID, x=as.integer(1), name="LAS_Type", desc ="LAS_Type")
      LAS_oneF_oneG_TID <-add_lasattribute(LAS_oneF_oneG_TID, x=as.integer(G_ID[GG]), name="GridID", desc ="GridID")
      
      # LAS_Under
      Files_LAS_Und <- list.files(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/LAS/F",    FID, "_Grid_", G_ID[GG],sep="") , "_LAS_IDW_Under_Backup14")
      LAS_oneF_oneG_Und <- lidR::readLAS(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/LAS/F", FID, "_Grid_", G_ID[GG],"/", Files_LAS_Und,sep=""))
      LAS_oneF_oneG_Und@data$PointSourceID <- as.integer(1)
      Index_Keep <- which(colnames(LAS_oneF_oneG_Und@data) %in% colnames(LAS_oneF_oneG_TID@data))
      LAS_oneF_oneG_Und@data <- LAS_oneF_oneG_Und@data[, ..Index_Keep]
      LAS_oneF_oneG_Und <- add_lasattribute(LAS_oneF_oneG_Und, x=as.integer(LAS_oneF_oneG_Und@data$PointSourceID), name="TID", desc ="TID")
      LAS_oneF_oneG_Und <- add_lasattribute(LAS_oneF_oneG_Und, x=as.integer(3), name="LAS_Type", desc ="LAS_Type")
      LAS_oneF_oneG_Und <- add_lasattribute(LAS_oneF_oneG_Und, x=as.integer(G_ID[GG]), name="GridID", desc ="GridID")
      
      
      ##################################
      # GET TID ATTRIBUTES FOR ALL TID
      ##################################
      
      # GET ATTRIBUTE DATA FOR THE GRID CELL CLUSTERS (INCLUDES THE BUFFER)
      Files_AttTID <- list.files(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/CSV/F",FID, "_Grid_",G_ID[GG] ,sep=""),  pattern = "_St_Attributes_60X60.csv") # DOM DOM DOM !! IS THIS FILE CORRECT
      AttTID <- read.csv(paste(FOLDER_ITCD_I, "/Flight_",  FID, "/CSV/F",FID, "_Grid_",G_ID[GG] ,"/",  Files_AttTID, sep=""), row.names = NULL)
      AttTID$TID <- AttTID$PSID
      AttTID$TID <- AttTID$TID + 1 # ALL TID ARE INCREMENTED BY ONE SO VOX EMPTY HAS VALUE Zero 
      
      # GET MINIMUM VALUE LOCATION OF EACH CLUSTER
      Loc_T <- data.frame(GridID = rep(G_ID[GG], nrow(AttTID)),
                                TID = AttTID$TID, 
                                TID_X = AttTID$St_Base_X, 
                                TID_Y =AttTID$St_Base_Y)
      
      Loc_TID_SF <- st_as_sf(Loc_T, coords = c("TID_X", "TID_Y")) %>% st_set_crs(Proj_Sys)

      #################################################
      # IDENTIFY TID in Grid using Point Locations (NOT IN BUFFER)
      #################################################
      
      Poly_Ext_oneG <- Poly_Ext_allG[Poly_Ext_allG$GridID ==  G_ID[GG],]

      Loc_TID_G <- st_intersection(Loc_TID_SF, Poly_Ext_oneG)
      
      ########################################################################################
      # REMOVING BUFFER FOR CLUSTER LOCATIONS, CLUSTER ATTRIBUTES AND LAS CLUSTERS WITHIN GRID
      ########################################################################################

      if(length(Loc_TID_G$TID) > 0){
        Index_Coords<- which(round(st_coordinates(Loc_TID_SF)[,1],3) %in% round(st_coordinates(Loc_TID_G)[,1],3) &
                round(st_coordinates(Loc_TID_SF)[,2],3) %in% round(st_coordinates(Loc_TID_G)[,2],3)  )
        Loc_TID_G_SFDF <- Loc_TID_SF[Index_Coords,]
        
        # GET LAS FOR CLUSTERS WITHIN GRID (WITHOUT BUFFER)
        LAS_oneF_oneG_TID_NBuff <- filter_poi(LAS_oneF_oneG_TID, TID %in% Loc_TID_G_SFDF$TID)
        
        # ATTRIBUTE TABLE OF ALL TID WITHIN ALL THE GRIDS MERGED (WITHOUT BUFFER)
        AttTID_oneG <- AttTID[which(AttTID$TID %in% Loc_TID_G_SFDF$TID),]
        AttTID_oneG <- data.frame(FID =  FID,
                                  GridID = G_ID[GG],
                                  AttTID_oneG)
      }else{
        LAS_oneF_oneG_TID_NBuff <- NULL
        Work_Around <- data.table(AttTID[1,])
        Work_Around[1,] <- as.data.frame(Work_Around[1,lapply(.SD,function(x) x[NA])])
        AttTID_oneG <- data.frame(FID =  FID,
                                  GridID = G_ID[GG],
                                  Work_Around)
       }
      
      ######################################################################
      # CLIP LAS TO REMOVE BUFFER (Zero AND CLUSTERS NOT IN ATTRIBUTE TABLE) 
      ######################################################################
      
      # GET XY COORDS OF ONE GRID POLYON
      Poly_Ext_oneG <- Poly_Ext_oneG %>% st_set_crs(Proj_Sys)
      XY_oneG <-  st_coordinates(Poly_Ext_oneG)[,1:2] 

      # CLIP Zero TID AND Zero WITHIN GRID CELL
      # crs(LAS_oneF_oneG_Zero) <- Proj_Sys ### DOM DOM DOM !!! REMOVED FOR SUPERCOMPUTER 31/5/21
      # browser() # source('R_Code/SupComp_Name_V8/C_SEG_CNN_F1.R')
 
      LAS_oneF_oneG_Zero_NBuff <- lidR::clip_roi(LAS_oneF_oneG_Zero, Polygon(data.frame(XY_oneG))) 
      LAS_oneF_oneG_Und_NBuff <- lidR::clip_roi(LAS_oneF_oneG_Und, Polygon(data.frame(XY_oneG))) 
  
      # CLIP TID NOT IN ATTRIBUTE TABLE BUT IN GRID CELL
      Att_Missing <- setdiff(sort(unique(LAS_oneF_oneG_TID@data$TID)), sort(AttTID$TID))
      LAS_oneF_oneG_NotAtt <- filter_poi(LAS_oneF_oneG_TID, TID %in% Att_Missing)
      if(nrow(LAS_oneF_oneG_NotAtt@data) > 0) { # (!is.null(LAS_oneF_oneG_NotAtt))
        LAS_oneF_oneG_NotAtt@data$TID <- as.integer(1) # ONE REPRESENTS TID THAT IS EITHER UNDERSTOREY OR Zero
        LAS_oneF_oneG_TID_NotAtt_NBuff <- clip_roi(LAS_oneF_oneG_NotAtt, Polygon(data.frame(XY_oneG))) 
      }else{
        LAS_oneF_oneG_TID_NotAtt_NBuff <- NULL 
        }
      
      ####################################
      # MERGING ALL THE LAS FILES TOGETHER
      ####################################
      LAS_Merge <- list(LAS_oneF_oneG_Und_NBuff, 
                           LAS_oneF_oneG_Zero_NBuff,
                           LAS_oneF_oneG_TID_NBuff,
                           LAS_oneF_oneG_TID_NotAtt_NBuff)
  
      List_Null <- which(sapply(LAS_Merge, is.null))
      if(length(List_Null) > 0){
        LAS_Merge <-  LAS_Merge[-which(sapply(LAS_Merge, is.null))]
      }
      
      if(exists("LAS_oneF_oneG")){rm(LAS_oneF_oneG)}
      for(LJ in 1:length(LAS_Merge)){
        LAS_Merge_one <- LAS_Merge[[LJ]]
        if(nrow(LAS_Merge_one@data) > 0){ #  (!is.null(LAS_Merge_one))
          if(!exists("LAS_oneF_oneG")){
            LAS_oneF_oneG <-  LAS_Merge[LJ][[1]]
          }else{
            LAS_Add <- LAS_Merge[LJ][[1]]
            LAS_oneF_oneG@data <- rbind(LAS_oneF_oneG@data, LAS_Add@data )
            }
          }
        } # LJ LOOP
      if(!exists("LAS_oneF_oneG")){
        LAS_oneF_oneG <- NULL
      }
      
      ######################################################################################
      # MERGE ALL THE GRID CELLS (WITH ALL LAS) FOR ONE FLIGHT AND UPDATE CLUSTER ATTRIBUTES
      ######################################################################################    
  
      # CHANGE TID SO EACH GRID HAS UNIQUE VALUE
      if(!is.null(LAS_oneF_oneG)){
          if(nrow(LAS_oneF_oneG@data) > 0) { # (!is.null(LAS_oneF_oneG))
            Unique_TID<- unique(LAS_oneF_oneG@data$TID)
            Unique_TID <- Unique_TID[which(Unique_TID > 1)] # ONE REPRESENTS TID THAT IS EITHER UNDERSTOREY OR Zero
            if(length(Unique_TID)>0){
              # UPDATE TID SO UNIQUE FOR ALL GRID CELLS
              New_TID <- seq(max_TID+1, max_TID+length(Unique_TID),1) 
              DF_TID <- data.frame(Unique_TID, New_TID)
              Index_1 <- which(LAS_oneF_oneG@data$TID %in%  DF_TID$Unique_TID )
              Index_2 <- match(LAS_oneF_oneG@data$TID[Index_1],  DF_TID$Unique_TID )
              LAS_oneF_oneG@data$TID[Index_1] <- as.integer(DF_TID$New_TID[Index_2])
              
              # UPDATING CLUSTER ATTRIBUTE TID SO EACH GRID HAS UNIQUE TIDs
              Index_1 <- which(AttTID_oneG$TID %in%  DF_TID$Unique_TID )
              Index_2 <- match(AttTID_oneG$TID[Index_1],  DF_TID$Unique_TID )
              
              AttTID_oneG$TID[Index_1] <- as.integer(DF_TID$New_TID[Index_2])
              max_TID <- max(New_TID)
            }
          }
          
        # MERGING ALL THE GRIDS 
          if(!exists("LAS_oneF_allG")){
            if(nrow(LAS_oneF_oneG@data) > 0) {  # (!is.null(LAS_oneF_oneG))
              LAS_oneF_allG  <-  LAS_oneF_oneG
              AttTID_oneF_allG <- AttTID_oneG
            }
          }else{
            if(nrow(LAS_oneF_oneG@data) > 0) { # (!is.null(LAS_oneF_oneG))
              LAS_oneF_allG@data  <-  rbind(LAS_oneF_allG@data, LAS_oneF_oneG@data)  
              AttTID_oneF_allG <- rbind(AttTID_oneF_allG, AttTID_oneG)  
            }
          }# ELSE 
        } # IF LOOP FOR CHANGING UNIQUE TID
    }
  #plot(LAS_oneF_oneG) 

  print(paste("Doing Flight: ",  FID, "   Done Grid: GG", GG, sep=""))
  } # GG loop for Grids

  
  # REMOVE TID WITH ATTRIBUTE St_Base NA 
  Index_AttLoc_RmNA <- which(is.na(AttTID_oneF_allG$St_Base_X))
  if(length(Index_AttLoc_RmNA) > 0){
    AttTID_oneF_allG <- AttTID_oneF_allG[-Index_AttLoc_RmNA,]
  }

  ##########################################
  # OUTPUT LAS AND ATTRIBUTES FOR ALL GRIDS (WITHOUT BUFFERS OVERLAPPING AND WITH UNIQUE TID)
  ##########################################
  LAS_oneF_allG@data$PointSourceID <- as.integer(LAS_oneF_allG@data$TID)
  writeLAS(LAS_oneF_allG, paste(FOLDER_LAS_O,"/F",  FID, "_rmBuff.laz",sep=''))
  write.csv(AttTID_oneF_allG, paste(FOLDER_CSV_O,"/F",  FID, "_TID_AttTID.CSV",sep=''),
            row.names = FALSE)

  # BACKUP LAS_oneF_allG
  LAS_oneF_allG_Orig <- LAS_oneF_allG
  ###########################
  # OUTPUT CLUSTER ATTRIBUTES (AS POINT SHAPEFILE USING EACH STEM LOCATION)
  ###########################
  
  AttTID_oneF_allG_SF <-  st_as_sf(AttTID_oneF_allG, coords = c("St_Base_X", "St_Base_Y")) %>% st_set_crs(Proj_Sys)
  
  st_write(AttTID_oneF_allG_SF, 
           dsn = paste(FOLDER_SHP_O, sep=""), 
           layer = paste("F",FID, "_AttTID_Pnts",sep=""), 
           driver = "ESRI Shapefile",
           append=FALSE, overwrite = TRUE)
  
  ################################################################################################################################################### 2
  ################################################################################################################################################### 2
  # ALS SEGMENT (i.e. GIVE TID) WITH TID "SOCKS" FROM UAS DATA
      # THIS PROCEDURE ALSO ASSIGNS ALL ZERO LAS POINTS TO TID
  ################################################################################################################################################### 2
  ################################################################################################################################################### 2

  ######################################
  # CLUSTER ALS USING SOCK FROM UAS TID
  ###################################### 
  
  LAS_oneF_allT <- filter_poi(LAS_oneF_allG, TID > 1)
  LAS_oneF_zeroT <- filter_poi(LAS_oneF_allG, TID <= 1 & LAS_Type != 3)  # WHAT HAPPENS TO LAS_Type 3
  LAS_oneF_Under <- filter_poi(LAS_oneF_allG, TID <= 1 & LAS_Type == 3) 
  
  # SEQUENCE OF SLICE HEIGHTS (USING THE SEGMENTED STEMS ONLY)
  
  Z_Inc_Sl <- seq(floor(min(LAS_oneF_allT$Z)/Para_Vx_R_Res)*Para_Vx_R_Res, ceiling(max(LAS_oneF_allT$Z)/Para_Vx_R_Res)*Para_Vx_R_Res, Para_Vx_R_Res)
  
  # GET ALL DATA BELOW THE FIRST SLICE
  LAS_ALS_BelowTID <- filter_poi(LAS_ALS_oneF_Corr, Z <= Z_Inc_Sl[1])  
  LAS_ALS_BelowTID@data$TID <- as.integer(0)
  LAS_ALS_BelowTID@data$LAS_Type <- as.integer(3)
  LAS_ALS_BelowTID <- merge_spatial(LAS_ALS_BelowTID, Poly_Ext_allG, attribute = "GridID")
  
  R_Z_Under<-   grid_metrics(LAS_ALS_BelowTID, max(Z), res = Para_Vx_R_Res)
  
  # CREATE AN EMPTY LAS FOR UPDATED RESULTS
  LAS_oneF_allT_FixZero <- LAS_oneF_allT
  LAS_oneF_allT_FixZero@data <- LAS_oneF_allT_FixZero@data[0,]
  LAS_oneF_zeroT_FixZero <- LAS_oneF_zeroT
  LAS_oneF_zeroT_FixZero@data <- LAS_oneF_zeroT_FixZero@data[0,]
  
  ###############################
  # SLICE LOOP FOR SEGMENTING ALS 
  ###############################

  
  # REMOVE PREVIOUS FLIGHT DATA
  if(exists("LAS_ALS_oneF")){rm(LAS_ALS_oneF)}     #in hh LOOP 
  for(hh in 1:length(Z_Inc_Sl)){
    
    if(exists("xyZ_TID_All")){rm(xyZ_TID_All)} 
    
    # SLICE LAS (UAS AND ALS)
    LAS_UAS_oneF_oneSl_allT <-  filter_poi(LAS_oneF_allT, Z > (Z_Inc_Sl[hh]) & Z <= (Z_Inc_Sl[hh] + Para_Vx_R_Res))
    LAS_UAS_oneF_oneSl_zeroT <-  filter_poi(LAS_oneF_zeroT, Z > (Z_Inc_Sl[hh]) & Z <= (Z_Inc_Sl[hh] + Para_Vx_R_Res))

    if(nrow(LAS_UAS_oneF_oneSl_allT@data) > 0){
      ###################################################################
      # CLEARN THE UAS ZERO VALUES BY ASSIGNING THEM WITH THE CLOSEST TID
      ###################################################################
      Start_Zero_Count <- nrow(LAS_UAS_oneF_oneSl_zeroT@data)
      print(paste("STARTED ZERO COUNT : ", nrow(LAS_UAS_oneF_oneSl_zeroT@data)))
      
      if(nrow(LAS_UAS_oneF_oneSl_zeroT@data) > 0){ # (!is.null(LAS_UAS_oneF_oneSl_zeroT))
        
        LAS_oneF_oneSl_Ab <-  filter_poi(LAS_oneF_allT, Z > (Z_Inc_Sl[hh]) + Para_Vx_R_Res & Z <= (Z_Inc_Sl[hh] + Para_Vx_R_Res + Para_Above_Below_Sl))
        LAS_oneF_oneSl_Bl <-  filter_poi(LAS_oneF_allT, Z > (Z_Inc_Sl[hh]) - Para_Above_Below_Sl & Z <= (Z_Inc_Sl[hh]))
        
        List_LAS_TID_CorrZero <- list(LAS_UAS_oneF_oneSl_allT, LAS_oneF_oneSl_Ab, LAS_oneF_oneSl_Bl)
        
        # LOOP SUBJECT SLICE AND LAS JUST BELOW AND JUST ABOVE SLICE (FOR FILLING IN Non_TID VALUES)
        for(LL in 1:length(List_LAS_TID_CorrZero)){
          if(nrow(LAS_UAS_oneF_oneSl_zeroT@data) == 0){break()}
          LAS_TID_CHECK <- List_LAS_TID_CorrZero[[LL]]
          if(nrow(LAS_TID_CHECK@data) > 0){
            
            R_oneSl_TID<-   grid_metrics(LAS_TID_CHECK, ~as.numeric(names(table(TID)[which.max(table(TID))])), res = Para_Sl_Z) # MAKE IT THE MOST COMMON TID VALUE
            xy_TID <- as.data.frame(rasterToPoints(R_oneSl_TID))
            colnames(xy_TID) <- c("X", "Y","TID")
            
            R_oneSl_Zero<-   grid_metrics(LAS_UAS_oneF_oneSl_zeroT, min(TID),  res = Para_Sl_Z) # TID IS ONE AT THIS STAGE
            R_oneSl_Zero_Cl <- clump(R_oneSl_Zero)
            xy_Cl <- as.data.frame(rasterToPoints(R_oneSl_Zero_Cl))
            colnames(xy_Cl) <- c("X", "Y","Cl")
            
            Dist_TID_Zero_R  <- round(rdist(xy_TID[,1:2],
                                            xy_Cl[,1:2]), 2) # ROW (TID), COL (Zero)  
            
            # GET THE ZeroS THAT ARE WITHIN 0.4 m OF TID  
            Closest_Index_TID <- apply(Dist_TID_Zero_R, 2, which.min)
            Closest_Dist_TID <- apply(Dist_TID_Zero_R, 2, min)
            Close_Zeros <- which(Closest_Dist_TID <= Para_Dist_ZeroCloseTID)
            
            # ASSIGN CLOSE ZeroS WITH CORRECT TID. PUT NEW TID (FROM ZERO) INTO 
            if(length(Close_Zeros) > 0){
              
              # GET THE PREDICTED TID INTO RASTER AND THEN PUT INTO LAS FILE
              xy_Cl$TID <- 0
              xy_Cl$TID[Close_Zeros] <- xy_TID$TID[Closest_Index_TID[Close_Zeros]]
              xy_Cl_p <- SpatialPointsDataFrame(coords = xy_Cl[,1:2], data = xy_Cl[,3:4])
              R_oneSl_Cl_TID <- rasterize(xy_Cl_p, R_oneSl_TID, field="TID")
              
              # UPDATING TID FOR zeroT CLOSE TO TID POINT
              LAS_UAS_oneF_oneSl_zeroT<- merge_spatial(LAS_UAS_oneF_oneSl_zeroT, R_oneSl_Cl_TID, attribute = "TID")
              LAS_UAS_oneF_oneSl_FillTID <- filter_poi(LAS_UAS_oneF_oneSl_zeroT, TID >= 1)
              LAS_UAS_oneF_oneSl_allT@data <- rbind(LAS_UAS_oneF_oneSl_allT@data, LAS_UAS_oneF_oneSl_FillTID@data)
              LAS_UAS_oneF_oneSl_zeroT <- filter_poi(LAS_UAS_oneF_oneSl_zeroT, TID == 0)
              
            } # IF THERE ARE ZEROS CLOSE TO VALUES
          } # IF LAS FILE HAS DATA
        } # LOOP THROUGH EACH LAS (BELOW, WITHIN and ABOVE)
      } # IF THERE ARE ZEROS IN 
      
      print(paste("ENDED ZERO COUNT : ", nrow(LAS_UAS_oneF_oneSl_zeroT@data)))
      print(paste("FIXED_ZERO :", Start_Zero_Count - nrow(LAS_UAS_oneF_oneSl_zeroT@data)))
      
      # UPDATE LAS FILEs FOR UAS 
      LAS_oneF_zeroT_FixZero@data <- rbind(LAS_oneF_zeroT_FixZero@data, LAS_UAS_oneF_oneSl_zeroT@data)
      LAS_oneF_allT_FixZero@data <- rbind(LAS_oneF_allT_FixZero@data, LAS_UAS_oneF_oneSl_allT@data)
      
      ##############################
      # GIVE ALS DATA THE UAS VALUES (USING MORPHOLOGICAL RECONSTRUCTION)
      ##############################
      
      LAS_ALS_oneF_oneSl <- filter_poi(LAS_ALS_oneF_Corr, Z > (Z_Inc_Sl[hh]) & Z <= (Z_Inc_Sl[hh] + Para_Vx_R_Res))
      if(nrow(LAS_ALS_oneF_oneSl@data) > 0){ # !is.null(LAS_ALS_oneF_oneSl)
        
        # BINARY TO GET PIXEL LOCATION OF UAS LAS DATA
        R_oneSl_Binary<-   grid_metrics(LAS_UAS_oneF_oneSl_allT, ~as.numeric(max(LAS_Type/LAS_Type)), res = Para_Vx_R_Res)
        
        ###############################################################################
        #WORK OUT WHICH R PIXELS ACTUALLY HAVE AN ALS (ONLY COMPUTE DISTANCE FOR THOSE)
        ###############################################################################
        LAS_ALS_MorphDil <- LAS_ALS_oneF_oneSl
        R_oneSl_CellID <- R_oneSl_Binary
        values(R_oneSl_CellID) <- 1:ncell(R_oneSl_CellID)
        LAS_ALS_MorphDil<- merge_spatial(LAS_ALS_MorphDil, R_oneSl_CellID, attribute = "Morph_Dilate")
        CellID_ALS <- na.omit(unique(LAS_ALS_MorphDil@data$Morph_Dilate))
        
        ################################################################################
        # PERFORM MORPHOLOGICAL RECONSTRUCTION TO DEVELOP A BUFFER AROUND EACH TID PIXEL
        ################################################################################
        R_oneSl_Binary_Orig <- R_oneSl_Binary
        A_oneSl_Binary <- t(as.matrix(R_oneSl_Binary))
        A_oneSl_Binary[is.na(A_oneSl_Binary)] <- 0
        img_oneSl_Binary <-Image(A_oneSl_Binary)
        
        kern = makeBrush(9, shape='disc')
        Dilated_Area <- dilate(img_oneSl_Binary , kern)
        values(R_oneSl_Binary) <- as.vector(Dilated_Area)
        
        ##################################################
        # GET UAS VALUES (TID, GridID, Type) FOR THE SLICE 
        ##################################################
        R_oneSl_GridID <- grid_metrics(LAS_UAS_oneF_oneSl_allT, ~max(GridID), res = Para_Vx_R_Res) # MAKE IT THE MOST COMMON TID VALUE
        R_oneSl_GridID[is.na(R_oneSl_GridID)] <- 0
        values(R_oneSl_GridID) <- values(R_oneSl_GridID) + values(R_oneSl_Binary)
        
        R_oneSl_Type <-   grid_metrics(LAS_UAS_oneF_oneSl_allT, ~max(LAS_Type), res = Para_Vx_R_Res) # MAKE IT THE MOST COMMON TID VALUE
        R_oneSl_Type[is.na(R_oneSl_Type)] <- 0
        values(R_oneSl_Type) <- values(R_oneSl_Type) + values(R_oneSl_Binary)
        
        R_oneSl_TIDMx <-  grid_metrics(LAS_UAS_oneF_oneSl_allT, ~as.numeric(names(table(TID)[which.max(table(TID))])), res = Para_Vx_R_Res) # MAKE IT THE MOST COMMON TID VALUE
        R_oneSl_TIDMx[is.na(R_oneSl_TIDMx)] <- 1
        values(R_oneSl_TIDMx) <- values(R_oneSl_TIDMx) * values(R_oneSl_Binary)
        
        XY_oneSl_Fill <- data.frame(xyFromCell(R_oneSl_TIDMx, cell = CellID_ALS),
                                    TIDMx = values(R_oneSl_TIDMx)[CellID_ALS], 
                                    Type = values(R_oneSl_Type)[CellID_ALS],
                                    GridID = values(R_oneSl_Type)[CellID_ALS])
        XY_oneSl_Morph <- XY_oneSl_Fill[which(XY_oneSl_Fill$TIDMx == 1),]
        XY_oneSl_TID <- XY_oneSl_Fill[which(XY_oneSl_Fill$TIDMx > 1),]
        
        Dist_TID_Morph  <- round(rdist(XY_oneSl_TID[,1:2],
                                       XY_oneSl_Morph[,1:2]), 2) # ROW (TID), COL (Zero)  
        
        # GET THE Zeros THAT ARE WITHIN 0.4 m OF TID  
        Closest_Index_TID <- apply(Dist_TID_Morph, 2, which.min)
        Closest_Dist_TID <- apply(Dist_TID_Morph, 2, min)
        Close_Zeros <- which(Closest_Dist_TID <= Para_Dist_ZeroCloseTID)
        
        # UPDATE (FILL IN) THE RASTER VALUES FOR THE MORPH DILATED PIXELS 
        XY_oneSl_Morph$TIDMx[Close_Zeros] <- XY_oneSl_TID$TIDMx[Closest_Index_TID[Close_Zeros]]
        XY_oneSl_Fill$TIDMx[as.numeric(rownames(XY_oneSl_Morph))] <- XY_oneSl_Morph$TIDMx
        values(R_oneSl_TIDMx)[CellID_ALS] <- XY_oneSl_Fill$TIDMx
        
        XY_oneSl_Morph$Type[Close_Zeros] <- XY_oneSl_TID$Type[Closest_Index_TID[Close_Zeros]]
        XY_oneSl_Fill$Type[as.numeric(rownames(XY_oneSl_Morph))] <- XY_oneSl_Morph$Type
        values(R_oneSl_Type)[CellID_ALS] <- XY_oneSl_Fill$Type -1
        
        XY_oneSl_Morph$GridID[Close_Zeros] <- XY_oneSl_TID$GridID[Closest_Index_TID[Close_Zeros]]
        XY_oneSl_Fill$GridID[as.numeric(rownames(XY_oneSl_Morph))] <- XY_oneSl_Morph$TIDMx
        values(R_oneSl_GridID)[CellID_ALS] <- XY_oneSl_Fill$GridID -1
        
        # UPDATE ALS LAS USING RASTERS WITH MORPH DILATION
        LAS_ALS_oneF_oneSl<- merge_spatial(LAS_ALS_oneF_oneSl, R_oneSl_GridID, attribute = "GridID")
        LAS_ALS_oneF_oneSl<- merge_spatial(LAS_ALS_oneF_oneSl, R_oneSl_Type, attribute = "LAS_Type")
        LAS_ALS_oneF_oneSl<- merge_spatial(LAS_ALS_oneF_oneSl, R_oneSl_TIDMx, attribute = "TID")
        
        LAS_ALS_oneF_oneSl@data$TID[is.na(LAS_ALS_oneF_oneSl@data$TID)] <- 1
        LAS_ALS_oneF_oneSl@data$TID[which(LAS_ALS_oneF_oneSl@data$TID <= 1)] <- 1 # CONVERTING THE ZEROS TO 1
        LAS_ALS_oneF_oneSl@data$PointSourceID <- as.integer(LAS_ALS_oneF_oneSl@data$TID)
        
        # APPEND SLICE ALS TO FLIGHT ALS
        if(!exists("LAS_ALS_oneF")){
          LAS_ALS_oneF <-  LAS_ALS_oneF_oneSl
        }else{
          LAS_ALS_oneF@data <- rbind(LAS_ALS_oneF@data , LAS_ALS_oneF_oneSl@data)
        }
      } # IF NOT POINTS IN ALS SLICE 
    } # IF NO POINTS IN UAS SLICE
  print(paste("Done hh: ",hh,  "out of ", length(Z_Inc_Sl)))  
  } # hh Loop

  #####################
  # UPDATE OBJECT NAMES FOR UAS (FOR NEW ASSIGNED ZERO )
  #####################
  LAS_oneF_zeroT_NotFizedZero <-  LAS_oneF_zeroT
  LAS_oneF_allT_NotFizedZero <- LAS_oneF_allT
  LAS_oneF_allT <- LAS_oneF_allT_FixZero
  LAS_oneF_zeroT <- LAS_oneF_zeroT_FixZero
  
  # MERGE TID, ZERO AND UNDER INTO ONE LAS FILE
  LAS_oneF_allG <- LAS_oneF_allT
  LAS_oneF_allG@data <- rbind(LAS_oneF_allG@data, LAS_oneF_zeroT@data, LAS_oneF_Under@data)

  ###################################################################################
  # OUTPUT ALS WITH TID VALUES ASSIGNED (INCLUDED THE BELOW TID POINTS (understorey))
  ###################################################################################
  
  LAS_ALS_oneF@data <- rbind(LAS_ALS_oneF@data, LAS_ALS_BelowTID@data)
  LAS_ALS_oneF@data$PointSourceID <- as.integer(LAS_ALS_oneF@data$TID)
  writeLAS(LAS_ALS_oneF, paste(FOLDER_LAS_O,"/All_ALS_F",  FID, ".laz",sep=''))
  LAS_ALS_oneF_noBuff <- clip_polygon(LAS_ALS_oneF, xpoly=st_coordinates(Poly_oneF_Boundary)[,1], ypoly=st_coordinates(Poly_oneF_Boundary)[,2])
  
  LAS_ALS_oneF_noBuff_Zeros <- filter_poi(LAS_ALS_oneF_noBuff, !(TID > 1))
  LAS_oneF_allG_Zeros <- filter_poi(LAS_oneF_allG, !(TID > 1))
  ############################################################################################################################################### 3
  ############################################################################################################################################### 3
  # THIN UAS FLIGHT TID CLUSTERS
  # GENERATE TREE DESCRIPTORS OF EACH THINNED TREE
  # BOUNDING BOXES OF TID CLUSTERS FROM UAS (THIS IS WHAT I PREDICT IN CNN!)
  ############################################################################################################################################### 3
  ############################################################################################################################################### 3
  
  dir.create(file.path(FOLDER_LAS_O,  paste("LAS_ALS",  sep="")), showWarnings = FALSE)
  FOLDER_LAS_ALS_O <- paste(FOLDER_LAS_O, "/LAS_ALS",sep="")
  
  dir.create(file.path(FOLDER_LAS_O,  paste("LAS_TID",  sep="")), showWarnings = FALSE)
  FOLDER_LAS_TID_O <- paste(FOLDER_LAS_O, "/LAS_TID",sep="")
  
  dir.create(file.path(FOLDER_LAS_O,  paste("LAS_ALS_TID",  sep="")), showWarnings = FALSE)
  FOLDER_ALS_TID_O <- paste(FOLDER_LAS_O, "/LAS_ALS_TID",sep="") 
  
  dir.create(file.path(FOLDER_LAS_O,  paste("LAS_UNDER",  sep="")), showWarnings = FALSE)
  FOLDER_LAS_UNDER_O <- paste(FOLDER_LAS_O, "/LAS_UNDER",sep="")

  # EMPTY DATAFRAME FOR GENERATING TREE DESCRIPTORS THAT CNN WILL TRY AND FIND
  DF_LocBox_oneF_allT <- data.frame(TID = numeric(), 
                                 Sample = numeric(), 
                                 Quantile = numeric(), 
                                 X = numeric(), 
                                 Y = numeric(), 
                                 Z = numeric())
  
  DF_LocBox_oneT <- DF_LocBox_oneF_allT
  DF_LocBox_Empty <- DF_LocBox_oneF_allT
  
  # pdf(paste(FOLDER_PDF_O, "/F",  FID, "_Orig_BBox_DensityPlots.pdf", sep=""))
  # par(mfrow=c(2,2)) 
  
  ##########################################
  # MAKE SURE ALS AND UAS HAVE SAME COLNAMES
  ##########################################

  AddCol_ToALS <- setdiff(colnames(LAS_oneF_allG@data), colnames(LAS_ALS_oneF_noBuff@data))
  if(length(AddCol_ToALS) > 0){
    for(CN in 1:length(AddCol_ToALS)){
      LAS_ALS_oneF_noBuff <- add_lasattribute(LAS_ALS_oneF_noBuff, x=as.integer(0), name=AddCol_ToALS[CN], desc =AddCol_ToALS[CN])
    }
  }
  AddCol_ToUAS<- setdiff(colnames(LAS_ALS_oneF_noBuff@data),colnames(LAS_oneF_allG@data))
    if(length(AddCol_ToUAS) > 0){
    for(CN in 1:length(AddCol_ToUAS)){
      LAS_oneF_allG <- add_lasattribute(LAS_oneF_allG, x=as.integer(0), name=AddCol_ToUAS[CN], desc =AddCol_ToUAS[CN])
    }
  }

  if(exists("XYZWLHR_oneF_allT")){rm(XYZWLHR_oneF_allT)}
  
  Unique_TID <- as.numeric(names(rev(sort(table(LAS_oneF_allG@data$TID)))))
  Unique_TID <- Unique_TID[which(Unique_TID > 1)] # ONE REPRESENTS TID THAT IS EITHER UNDERSTOREY OR Zero
  
  # # LOOP THROUGH EACH TID
  # for (TT in 1:length(Unique_TID)){
  #   # LOOP S SAMPLES OF EACH TID
  # 
  #     # ONE TID FROM BOTH UAS AND ALS
  #     LAS_UAS_oneT <- filter_poi(LAS_oneF_allG, TID == Unique_TID[TT])
  #     LAS_ALS_oneT <- filter_poi(LAS_ALS_oneF_noBuff, TID == Unique_TID[TT])
  #     plot(LAS_ALS_oneT, size=10)
  #     plot(LAS_UAS_oneT)
  #     browser()
  # }
  
  # LOOP THROUGH EACH TID 
  for (TT in 1:length(Unique_TID)){
    # LOOP S SAMPLES OF EACH TID 
    for(S in 1:Para_Samples){
      # START WITH EMPTY oneTID
      DF_LocBox_oneT <- DF_LocBox_Empty
      
      # ONE TID FROM BOTH UAS AND ALS
      LAS_UAS_oneT <- filter_poi(LAS_oneF_allG, TID == Unique_TID[TT])
      LAS_ALS_oneT <- filter_poi(LAS_ALS_oneF_noBuff, TID == Unique_TID[TT])

      ################################################################################################################
      # IF ALS_TID HAS NOT BEEN PICKED UP IN SOCK THEN GET NON_TID VALUES WITHIN BOUNDING BOX OF UAS_TID (WORK AROUND)
      ################################################################################################################
      ### DOM DOM DOM !!! YOU NEED TO DEAL WITH CIRCUMSTANCES WHERE ALS DOES NOT CAPTURE THE TREES. (THE THINNING PROCESS WILL NATURALLY REMOVE A CERTAIN NUMBER OF THEM)

      if(nrow(LAS_ALS_oneT@data) == 0) {
        LAS_ALS_oneT <- filter_poi(LAS_ALS_oneF_noBuff, X > min(LAS_UAS_oneT$X) & 
                                     X < max(LAS_UAS_oneT$X) & 
                                     Y > min(LAS_UAS_oneT$Y) & 
                                     Y < max(LAS_UAS_oneT$Y) & 
                                     Z > 3 &
                                     TID == 1)
        
        LAS_ALS_oneT@data$TID <- Unique_TID[TT]
        LAS_ALS_oneF_noBuff@data$TID[which( LAS_ALS_oneF_noBuff$X > min(LAS_UAS_oneT$X) & 
                                     LAS_ALS_oneF_noBuff$X < max(LAS_UAS_oneT$X) & 
                                     LAS_ALS_oneF_noBuff$Y > min(LAS_UAS_oneT$Y) & 
                                     LAS_ALS_oneF_noBuff$Y < max(LAS_UAS_oneT$Y) & 
                                     LAS_ALS_oneF_noBuff$Z > 3 &
                                     LAS_ALS_oneF_noBuff@data$TID == 1)] <- Unique_TID[TT]
      }
      
      ################################################################################################
      # IF ALS HAS VERY FEW DATA POINTS FOR TREE THEN USE THE UAS THINNED USING RATIO FOR WHOLE FLIGHT
      ################################################################################################
      oneF_Ratio_ALS_UAS <- nrow(LAS_ALS_oneF_noBuff@data)/nrow(LAS_oneF_allG@data)
      oneT_Ratio_ALS_UAS <- nrow(LAS_ALS_oneT@data)/nrow(LAS_UAS_oneT@data)
      if(length(LAS_ALS_oneT$Z) < 30){
        Sample_Count <- ceiling(oneF_Ratio_ALS_UAS*nrow(LAS_UAS_oneT@data))
      }else{
        Sample_Count <- length(LAS_ALS_oneT$Z)

      }
      if(oneT_Ratio_ALS_UAS > 1){
        Sample_Count <- min(Sample_Count, length(LAS_UAS_oneT$Z))
      }
      
      
      if(nrow(LAS_ALS_oneT@data) > 0) { 
        ################################################
        # THIN USING THE DENSITY PROFILE OF THE UAS STEM  
        ################################################   
        Index_S <- sample(seq(1, length(LAS_UAS_oneT$Z), 1), Sample_Count, replace = FALSE)
        LAS_UAS_Thin_oneT <- filter_poi(LAS_UAS_oneT, seq(1, length(LAS_UAS_oneT$Z), 1) %in% Index_S)

        # OUTPUT LAS FILE FOR EACH SAMPLE  (DOM DOM DOM !!! BOTH FILE NAMES BELOW NEED TO BE THE SAME)
        LAS_UAS_Thin_oneT@data$PointSourceID <- as.integer(LAS_UAS_Thin_oneT@data$TID)
        writeLAS(LAS_UAS_Thin_oneT, paste( FOLDER_LAS_TID_O, "/LAS_F",  FID, "_TID", Unique_TID[TT], "_S", S, ".laz", sep=""))
        writeLAS(LAS_ALS_oneT, paste( FOLDER_ALS_TID_O, "/LAS_F",  FID, "_TID", Unique_TID[TT], "_S", S, ".laz", sep=""))
  
        ##############################################################################################################################################
        ###############################################################
        # CALC THE TWO BBOXES AND STEM LOCATION XYZ VALUES FOR EACH TID 
        ###############################################################
        ##############################################################################################################################################

        ############################################
        # STEM PROFILE TO CALCULATE TID CANOPY BASE
        ############################################

        Profile_oneT <- GAP_DENSITY_FUNCTION_NEW2(LAS_UAS_Thin_oneT$Z,
                                                Para_BW = Para_C_Base_BW ,
                                                Para_Threshold_Percent = Para_C_Base_Thresh ,
                                                Plot = "Yes",
                                                Plot_Heading = paste( FID, "XYZWLHR_oneF_oneT", Unique_TID[TT], "_S", S, sep=""))
       
    
        
        ###############################
        # CALCULATING START AND END GAP
        ###############################
        
        # IF BOTH Start_Largest_Gap and End_Largest_Gap ARE THE SAME
        if(Profile_oneT$End_Largest_Gap == Profile_oneT$Start_Largest_Gap){
          Index_Dip <- which(Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary$Peak_Dip == "Dip")
          if(length(Index_Dip) >= 1){
            Profile_oneT$End_Largest_Gap <- Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary$Z[Index_Dip[1]]
            Profile_oneT$Start_Largest_Gap <- min(LAS_UAS_Thin_oneT$Z)
          }else{
            Index_Peak <- which(Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary$Peak_Dip == "Peak")[1]
            Profile_oneT$End_Largest_Gap <- Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary$Z[Index_Peak]
            Profile_oneT$Start_Largest_Gap <- Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary$Z[(Index_Peak-1)]
            }
          }
        
        # WORK AROUND FOR WHEN ONE OF THEM IS INF
        Inf_Gap <- which(c(Profile_oneT$Start_Largest_Gap,Profile_oneT$End_Largest_Gap) == Inf)
        if(length(Inf_Gap) > 0){
          if(length(Inf_Gap) == 2){
            flag <- 9
            browser()
            }
          oneGap <- c(Profile_oneT$Start_Largest_Gap,Profile_oneT$End_Largest_Gap)[-Inf_Gap]
          
          Profile_oneT$Start_Largest_Gap <- oneGap
          PeakDips <- Profile_oneT$Peak_Dip_Summary$Peak_Dip_Summary
          Profile_oneT$End_Largest_Gap <- PeakDips$Z[which(PeakDips$Z > oneGap)[1]]
        }else{
          flag <-0
          Z_C_Base <- max(Profile_oneT$End_Largest_Gap, Profile_oneT$Start_Largest_Gap)
        }
        
        #############
        # GENERATE GT  # OUTPUT IS "XYZWLHR_oneF_allT"
        #############
        source(paste(Comp_Name, "/CNN/R_Code/TORCH_ITCD_EXTRAP", R_Code_Version, "/TORCH_GT_PROCEDURE_PARA", Para_TriShpParaCnt,  R_Code_Version, ".R", sep=""))  
        
        # WORK AROUND TO MAKE SURE L AND W ARE NOT ZERO
        Index_W_Zero <- which(XYZWLHR_oneF_allT$W_TopBox == 0)
        if(length(Index_W_Zero) > 0){
          XYZWLHR_oneF_allT$W_TopBox[Index_W_Zero] <- 0.1
        }
        Index_L_Zero <- which(XYZWLHR_oneF_allT$L_TopBox == 0)
        if(length(Index_L_Zero) > 0){
          XYZWLHR_oneF_allT$L_TopBox[Index_L_Zero] <- 0.1
        }
        
      }else{ # IF ALS HAS NO TID ( SOCK DIDN'T WORK WELL)
        DF_LocBox_oneT <- cbind(Unique_TID[TT], S, NA , NA, NA, NA)
        colnames(DF_LocBox_oneT) <- c("TID", "Sample", "Quantile", "X", "Y", "Z") 
        DF_LocBox_oneT <- as.data.frame(DF_LocBox_oneT)
        DF_LocBox_oneT = apply(DF_LocBox_oneT, 2, function(x) as.numeric(as.character(x)))
        DF_LocBox_oneF_allT <- rbind(DF_LocBox_oneF_allT, DF_LocBox_oneT)
        DF_LocBox_oneT <- rbind(DF_LocBox_oneT, DF_LocBox_oneT)
        }
      } # SAMPLE LOOP S
      print(paste("DONE TID", TT, " out of ",length(Unique_TID), sep="")) 
      #browser()
    } # TID LOOP FOR BBox and LOC CALCULATION
    # dev.off() # END BBox_DENSITY PLOT

  writeLAS(LAS_ALS_oneF_noBuff, paste(FOLDER_LAS_O,"/All_ALS_F",  FID, "noBuff.laz",sep=''))
  # OUTPUT THE BOUNDING BOXES AND TID LOCATION FOR EACH THINNED TREE
  write.csv(DF_LocBox_oneF_allT, paste(FOLDER_CSV_O, "/F",  FID, "_allTID_LocBox.csv", sep=""), row.names=FALSE)   # DF_LocBox_oneF_allT <- read.csv(paste(FOLDER_CSV_O, "/F",  FID, "AllTID_100Sample_CNN_Loc_BoundBox.csv", sep="") )

  
  ########################################################################
  # Remove NA (BOTH BBOX ARE NA) AND CORRECT NAs (ONLY BOTTOM BBOX IS NA)
      # FINAL CLEAN BUT SHOULD NOT BE NECESSARY 
  ########################################################################
  Index_Remove_NA <- which(is.na(XYZWLHR_oneF_allT$R_TopBox) & is.na(XYZWLHR_oneF_allT$X_TopBox))
  if(length(Index_Remove_NA) > 0){
    browser()
    XYZWLHR_oneF_allT <- XYZWLHR_oneF_allT[-Index_Remove_NA,]
  }
  Index_Correct_NA <- which(is.na(XYZWLHR_oneF_allT$X_TopBox))
  if(length(Index_Correct_NA) > 0){
    browser()
    XYZWLHR_oneF_allT$Z_TopBox[Index_Correct_NA] <- XYZWLHR_oneF_allT$Z_Base[Index_Correct_NA] + 0.5*(XYZWLHR_oneF_allT$Z_BotBox [Index_Correct_NA] - XYZWLHR_oneF_allT$Z_Base[Index_Correct_NA])
    XYZWLHR_oneF_allT[Index_Correct_NA, which(colnames(XYZWLHR_oneF_allT) %in% c("X_TopBox","Y_TopBox" ,"L_TopBox","W_TopBox", "R_TopBox"))] <- XYZWLHR_oneF_allT[Index_Correct_NA, which(colnames(XYZWLHR_oneF_allT) %in% c("R_TopBox","Y_BotBox" ,"L_BotBox","W_BotBox", "R_BotBox"))]
  }
  
  # OUTPUT XYZWLHR FORMAT OF  BOUNDING BOXES AND TID LOCATION FOR EACH THINNED TREE
  write.csv(XYZWLHR_oneF_allT, paste(FOLDER_CSV_O, "/F",  FID, "_allTID_XYZWLHR.csv", sep=""), row.names=FALSE)

# # ###############################################################################################################################################
# # ###############################################################################################################################################
# # # TESTING RESULTS
# # ###############################################################################################################################################
# # ###############################################################################################################################################
# # 
  ### DOM DOM DOM !!! GridID FOR ALS IS WRONG!!! BELOW PROCESS TRIED TO FIX BUT PROBABLY NOT NECESSARY

  # R_Flight_Binary<-   grid_metrics(LAS_ALS_oneF_noBuff, ~as.numeric(max(LAS_Type/LAS_Type)), res = Para_Vx_R_Res)
  # names(R_Flight_Binary@data) <- "GridID"
  # r_Poly_Ext_oneG <- rasterize(Poly_Ext_oneG, R_Flight_Binary, 'GridID')
  # Test<- merge_spatial(LAS_ALS_oneF_noBuff, Poly_Ext_oneG, attribute = "GridID")
  
  
#   for(GP in 1:length(G_ID)){
#     LAS_Plot_oneG <- filter_poi(LAS_oneF_allG, GridID == G_ID[GP])
#     LAS_Plot_ALS <- filter_poi(LAS_ALS_oneF_noBuff, GridID == G_ID[GP])
#     PLOT_LAS_FUN(LAS_Plot_oneG, Title_Plot = GP)
#     PLOT_LAS_FUN(LAS_Plot_ALS, Title_Plot = GP)
#     browser()
#   }
#   
# Test_DF <- read.csv(paste(FOLDER_CSV_O, "/F",  FID, "_allTID_LocBox.csv", sep=""))
# Unique_EE <- unique(Test_DF$TID)
# for(EE in 1:length(Unique_EE)){
#   Test_oneT <- Test_DF[which(Test_DF$TID == Unique_EE[EE]),]
#   # ONE TID FROM BOTH UAS AND ALS
#   LAS_UAS_oneT_TEST <- filter_poi(LAS_oneF_allG, TID == Unique_EE[EE])
#   LAS_ALS_oneT_TEST <- filter_poi(LAS_ALS_oneF_noBuff, TID == Unique_EE[EE])
# 
#   plot(LAS_UAS_oneT_TEST)
#   Shift_X <- min(LAS_UAS_oneT_TEST$X)
#   Shift_Y <- min(LAS_UAS_oneT_TEST$Y)
#   Vert_Loc <- Test_oneT[1,4:6]
#   Vert_Loc$X <- Vert_Loc$X -Shift_X; Vert_Loc$Y <- Vert_Loc$Y -Shift_Y
#   points3d(Vert_Loc, fill=FALSE, col="yellow", size=10)
# 
#   Vert_BBox1 <- Test_oneT[2:6,4:6]
#   Vert_BBox1[,1] <- Vert_BBox1[,1] - Shift_X
#   Vert_BBox1[,2] <- Vert_BBox1[,2] - Shift_Y
#   polygon3d(Vert_BBox1, fill=FALSE, col="red", size=5)
# 
#   Vert_BBox2 <- Test_oneT[7:11,4:6]
#   Vert_BBox2[,1] <- Vert_BBox2[,1] - Shift_X
#   Vert_BBox2[,2] <- Vert_BBox2[,2] - Shift_Y
#   polygon3d(Vert_BBox2, fill=FALSE, col="green", size=5)
# 
#   plot(LAS_ALS_oneT_TEST)
#   Shift_X <- min(LAS_UAS_oneT_TEST$X)
#   Shift_Y <- min(LAS_UAS_oneT_TEST$Y)
#   Vert_Loc <- Test_oneT[1,4:6]
#   Vert_Loc$X <- Vert_Loc$X -Shift_X; Vert_Loc$Y <- Vert_Loc$Y -Shift_Y
#   points3d(Vert_Loc, fill=FALSE, col="yellow", size=10)
# 
#   Vert_BBox1 <- Test_oneT[2:6,4:6]
#   Vert_BBox1[,1] <- Vert_BBox1[,1] - Shift_X
#   Vert_BBox1[,2] <- Vert_BBox1[,2] - Shift_Y
#   polygon3d(Vert_BBox1, fill=FALSE, col="red", size=5)
# 
#   Vert_BBox2 <- Test_oneT[7:11,4:6]
#   Vert_BBox2[,1] <- Vert_BBox2[,1] - Shift_X
#   Vert_BBox2[,2] <- Vert_BBox2[,2] - Shift_Y
#   polygon3d(Vert_BBox2, fill=FALSE, col="green", size=5)
#   browser()
# }

###############################################################################################################################################

  ###############################################################################################################################################
  #####################
  # THIN UNDER AND Zero .... THIN USING THE DENSITY PROFILE OF THE UAS STEM  (NOTE I TRIED TO MAKE SAME PROFILE AS ALS BUT DIDN'T WORK)
  #####################
  # DOM DOM DOM !!! YOU DO NOT DO ANYTHING WITH THESE ?? INVESTIGATE
  LAS_UAS_oneF_Under_Zero <- filter_poi(LAS_oneF_allG, TID <= 1)
  LAS_ALS_oneF_Under_Zero <- filter_poi(LAS_ALS_oneF_noBuff, TID <=1)
  for(SU in 1:Para_Samples){
    Index_S <- sample(seq(1, length(LAS_UAS_oneF_Under_Zero$Z), 1), length(LAS_ALS_oneF_Under_Zero$Z), replace = FALSE)
    LAS_UnderZero_oneF_oneS <- filter_poi(LAS_UAS_oneF_Under_Zero, seq(1, length(LAS_UAS_oneF_Under_Zero$Z), 1) %in% Index_S)
    LAS_UnderZero_oneF_oneS@data$PointSourceID <- as.integer(LAS_UnderZero_oneF_oneS@data$TID)
    writeLAS(LAS_UnderZero_oneF_oneS, paste(FOLDER_LAS_UNDER_O, "/LAS_UnderZero_F",  FID, "_S", SU, ".laz", sep=""))
    
    writeLAS(LAS_ALS_oneF_Under_Zero, paste(FOLDER_LAS_UNDER_O, "/LAS_ALS_UnderZero_F",  FID, "_S", SU, ".laz", sep=""))
    
    } # UNDER SAMPLE LOOP
  
  ############################################################################################################################################### 4
  ############################################################################################################################################### 4  
  ############################################################################################################################################### 4
  ############################################################################################################################################### 4  
  ####################
  # GENERATE CNN PLOT (15*15) INFORMATION (i.e. CSV AND SHAPEFILES OF TID THAT GOES INTO EACH PLOT) 
  ####################
  ############################################################################################################################################### 4
  ############################################################################################################################################### 4  
  ############################################################################################################################################### 4
  ############################################################################################################################################### 4  
  
  #############################################################
  # IDENTIFY CLUSTERS THAT ARE STEMS USING SCE CONSTRAINTS 
  # Note: some TID may be in attribute table but are not Trees
  #############################################################
  
  # TWO ITCD PARAMETERS
  SCE_Para_oneF <- SCE_Para_I[which(SCE_Para_I$TID ==  FID),] # DOM DOM DOM !!! IF YOU RE-RUN ITCD YOU WILL HAVE TO CHANGE THIS FILE
  Para_Z_Range <- SCE_Para_oneF$SCE_Para_Z_Range
  Para_Min_Z <- SCE_Para_oneF$SCE_Para_Min_Z
  # 
  #   
  # # ERROR IN ATTRIBUTE RANGE !!! DOM DOM DOM !!! THIS COULD BE A HUGE SOURCE OF ERROR IN YOUR FINAL RESULTS. MAKE SURE THIS IS FIXED
  # Test_Att_Range <- AttTID_oneF_allG[,which(colnames(AttTID_oneF_allG) %in% c("TID", "min_z", "St_Height", "Range_z"))]
  # Test_Att_Range$Range_Test <- Test_Att_Range$St_Height - Test_Att_Range$min_z
  # Test_Att_Range$Range_Diff <- Test_Att_Range$Range_z - Test_Att_Range$Range_Test
  # Test_Att_Range$Error <- 0
  # Index_Error <- which(Test_Att_Range$Range_Diff > 1 |Test_Att_Range$Range_Diff <= - 1)
  # Test_Att_Range$Error[Index_Error] <- 1
  # TID_ERROR <- Test_Att_Range$TID[Index_Error]
  # 
  # # BELOW SHOWS HOW THE ERROR LOOKS !!!! 
  # # for(TE in 1:length(TID_ERROR)){
  # #   LAS_Test <- filter_poi(LAS_oneF_allG, TID == TID_ERROR[TE])
  # #   LAS_Range <- range(LAS_Test@data$Z)
  # #   LAS_Range2 <- LAS_Range[2]- LAS_Range[1]
  # #   print(paste("LAS_RANGE", LAS_Range2, "min_Z:", LAS_Range[1], "max_Z:", LAS_Range[2]))
  # #   print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  # #   print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  # #   print(Test_Att_Range[TE,])
  # #   browser()
  # # }
  # 
  # 
  # ONLY GET THE TID THAT ARE TREES DUE TO SCE CONSTRAINS (Z_Range and Min_Z)
  AttTID_SCETrees <- AttTID_oneF_allG[which(AttTID_oneF_allG$Range_z > Para_Z_Range &  AttTID_oneF_allG$min_z < Para_Min_Z),]
  write.csv(AttTID_SCETrees, paste(FOLDER_CSV_O,"/F",  FID, "_SCEOptim_Cluster_Attributes.CSV",sep=''), row.names=FALSE)

  AttTID_SCETrees_SF <- st_as_sf(AttTID_SCETrees, coords = c("St_Base_X", "St_Base_Y")) %>% st_set_crs(Proj_Sys)
  
  st_write(AttTID_SCETrees_SF,
           dsn = paste(FOLDER_SHP_O, sep=""),
           layer = paste("F",FID, "_AttTID_SCETrees_Pnts",sep=""),
           driver = "ESRI Shapefile",
           append=FALSE, overwrite = TRUE)

  ##################################
  # GENERATE PLOTS ACROSS THE FLIGHT (i.e. Para_plot_Res = 15 m)
  ##################################
  
  # Extent OF FLIGHT
  Poly_oneF_Bound <- st_union(Poly_Ext_allG)
  #Poly_oneF_Bound$ID <- matrix(rep(1, length(Poly_Ext_allG$ID))) ### DOM DOM DOM !!! SF ... REMOVED THIS FROM CODE 

  st_write(Poly_oneF_Bound, 
           dsn = paste(FOLDER_SHP_O, sep=""), 
           layer = paste("F",FID, "_Poly_Bound",sep=""),
           driver = "ESRI Shapefile",
           append=FALSE, overwrite = TRUE)
  
  # Extent OF FLIGHT REDUCED BY BUFFER WIDTH
  Poly_Bound_Reduced10mBuff <- st_buffer(Poly_oneF_Bound, dist=-10)
  st_write(Poly_Bound_Reduced10mBuff, 
           dsn = paste(FOLDER_SHP_O, sep=""), 
           layer = paste("F",FID, "_Poly_Bound_Reduced10mBuff",sep=""), 
           driver = "ESRI Shapefile",
           append=FALSE, overwrite = TRUE)
  R_oneF_Rect <- raster()
  F_Ext <- raster::extent(Poly_Ext_allG)
  F_Ext[1] <- mround_Down(F_Ext[1],Para_plot_Res)-Para_plot_Res 
  F_Ext[2] <- mround_Up(F_Ext[2],Para_plot_Res) + Para_plot_Res
  F_Ext[3] <- mround_Down(F_Ext[3],Para_plot_Res) -Para_plot_Res 
  F_Ext[4] <- mround_Up(F_Ext[4],Para_plot_Res) + Para_plot_Res
  
  # GENERATES ONE WALL-TO-WALL SET OF PLOTS (fishnet)
  raster::extent(R_oneF_Rect) <- F_Ext
  res(R_oneF_Rect) <-Para_plot_Res
  R_oneF_Rect <- setExtent(R_oneF_Rect, F_Ext)
  R_oneF_Rect <- setValues(R_oneF_Rect, 1:ncell(R_oneF_Rect))
  Poly_allMP_fishnet <- rasterToPolygons(R_oneF_Rect)  %>% st_as_sf() # %>% st_set_crs(Proj_Sys)
  
  ############################################################
  # LOOP O and OO: SHIFT PLOTS UP AND DOWN WITH 5 M INCREMENTS 
  # Above involved 60X60 m grids whereas below its 15X15 m grids
  ############################################################

  # DOM DOM DOM !!! I AM TESTING WITH NO SHIFTS OF PLOT AT FIRST .... CHANGE BELOW LINE FOR 25 GRID SHIFTS (i.e. 5X5 in x and y direction)
  
  Dist_Move_G <- 0 # seq(0, Para_plot_Res, by=Para_plot_Res/5)[-1]
  max_Grid_ID <- 0
  Move_Position <- 0

  # REMOVE FOR NEXT FLIGHT


  if(exists("Prior_Loc_oneF")){rm(Prior_Loc_oneF)}              
  if(exists("Prior_BBox_oneF")){rm(Prior_BBox_oneF)}            
  if(exists("Prior_XYZWLHR_oneF")){rm(Prior_XYZWLHR_oneF)}  
  
  LocBox_Empty <- data.frame( Flight = numeric(),
                                       Plot_ID = numeric(),
                                       TID = numeric(), 
                                       Quantile = numeric(), 
                                       X = numeric(), 
                                       Y = numeric(), 
                                       Z = numeric())
  
  XYZWLHR_Empty <- data.frame( Flight = numeric(),
                               Plot_ID = numeric(),
                               TID = numeric(), 
                               X_Base = numeric(), 
                               Y_Base = numeric(), 
                               Z_Base  = numeric(),
                               X_BotBox  = numeric(),
                               Y_BotBox  = numeric(),
                               L_BotBox  = numeric(),
                               W_BotBox = numeric(), 
                               Z_BotBox = numeric(), 
                               R_BotBox = numeric(),
                               X_TopBox = numeric(), 
                               Y_TopBox = numeric(), 
                               L_TopBox = numeric(), 
                               W_TopBox = numeric(), 
                               Z_TopBox = numeric(), 
                               R_TopBox = numeric(),
                               Z_TopTree = numeric())
  
  GT_XYZWLHR_oneF_allMP_allP15X15 <- XYZWLHR_Empty

  Plot_Stocking <- data.frame(Flight = numeric(),
                              Plot_ID = numeric(),
                              Move_Position = numeric(),
                              Stocking = numeric())
  
  for(O in 1:length(Dist_Move_G)){
    print(paste("Doing ", O ," In total O Plots of ", length(Dist_Move_G), sep=""))
    for(OO in 1:length(Dist_Move_G)){
      print(paste("MP:............................................................", Move_Position))
      print(paste("Doing ", OO ," In total OO Plots of ", length(Dist_Move_G), sep=""))
      
      Move_Position <- Move_Position + 1

      ######################################################################################################################################
      #####################################
      # GENERATE MP FOLDERS FOR CSV AND SHP
      #####################################
      dir.create(file.path(FOLDER_CSV_O,  paste("CSV_MP", Move_Position, sep="")), showWarnings = FALSE)
      FOLDER_CSV_MP_O <- paste(FOLDER_CSV_O, "/CSV_MP", Move_Position,sep="") 
      
      dir.create(file.path(FOLDER_SHP_O,  paste("SHP_MP", Move_Position, sep="")), showWarnings = FALSE)
      FOLDER_SHP_MP_O <- paste(FOLDER_SHP_O, "/SHP_MP", Move_Position,sep="") 
      
      dir.create(file.path(FOLDER_PDF_O,  paste("PDF_MP", Move_Position, sep="")), showWarnings = FALSE)
      FOLDER_PDF_MP_O <- paste(FOLDER_PDF_O, "/PDF_MP", Move_Position,sep="") 

      #########################
      # GENERATE SHP SUBFOLDERS
      #########################
      dir.create(file.path(paste(FOLDER_SHP_MP_O, sep=""), 
                           paste("SCE_P_S_TID", sep="")), showWarnings = FALSE)
      FOLDER_SHP_SCE_MP_O <- paste(FOLDER_SHP_MP_O, "/SCE",  sep="")
      
      dir.create(file.path(paste(FOLDER_SHP_MP_O, sep=""), 
                           paste("ALLTID_P_S", sep="")), showWarnings = FALSE)
      FOLDER_SHP_TID_MP_O <- paste(FOLDER_SHP_MP_O, "/TID",sep="") 
      
      dir.create(file.path(paste(FOLDER_SHP_MP_O, sep=""), 
                           paste("All_P",sep="")), showWarnings = FALSE)
      FOLDER_SHP_Plots_MP_O <- paste(FOLDER_SHP_MP_O, "/PLOTS",sep="")   
      
      dir.create(file.path(paste(FOLDER_SHP_MP_O, sep=""), 
                           paste("One_Plot",sep="")), showWarnings = FALSE)
      FOLDER_SHP_OnePlot_MP_O <- paste(FOLDER_SHP_MP_O, "/One_Plot", sep="")
      
      #########################
      # GENERATE CSV SUBFOLDERS
      #########################
      dir.create(file.path(paste(FOLDER_CSV_MP_O, sep=""), 
                           paste("SCE", sep="")), showWarnings = FALSE)
      FOLDER_CSV_SCE_MP_O <- paste(FOLDER_CSV_MP_O, "/SCE",sep="")
      
      dir.create(file.path(paste(FOLDER_CSV_MP_O, sep=""), 
                           paste("TID", sep="")), showWarnings = FALSE)
      FOLDER_CSV_TID_MP_O <- paste(FOLDER_CSV_MP_O, "/TID",sep="") 
      
      dir.create(file.path(paste(FOLDER_CSV_MP_O, sep=""), 
                           paste("LocBox_allP15X15", sep="")), showWarnings = FALSE)
      FOLDER_CSV_P15X15_MP_O <- paste(FOLDER_CSV_MP_O, "/LocBox_allP15X15",sep="") 
      
      dir.create(file.path(FOLDER_CSV_MP_O, "VOX_DF"), showWarnings = FALSE)
      FOLDER_CSV_VOX_MP_O <- paste(FOLDER_CSV_MP_O, "/VOX_DF", sep="")
      
      dir.create(file.path(FOLDER_CSV_MP_O, "VOX_DF_ALS"), showWarnings = FALSE)
      FOLDER_CSV_VOX_ALS_MP_O <- paste(FOLDER_CSV_MP_O, "/VOX_DF_ALS", sep="")
      
      dir.create(file.path(FOLDER_CSV_MP_O, "Prior_LocBox"), showWarnings = FALSE)
      FOLDER_CSV_PRIOR_MP_O <- paste(FOLDER_CSV_MP_O, "/", "Prior_LocBox", sep="")
      
      dir.create(file.path(FOLDER_CSV_MP_O,  "Sample_Miss"), showWarnings = FALSE)
      FOLDER_CSV_MISS_MP_O <- paste(FOLDER_CSV_MP_O, "/Sample_Miss",sep="") 
      
      dir.create(file.path(FOLDER_CSV_MP_O,  "TriangShp"), showWarnings = FALSE)
      FOLDER_CSV_TRISHP_MP_O <- paste(FOLDER_CSV_MP_O, "/TriangShp",sep="") 
      
      dir.create(file.path(FOLDER_CSV_MP_O,  "IoU_Prior_GT"), showWarnings = FALSE)
      FOLDER_CSV_IoU_MP_O <- paste(FOLDER_CSV_MP_O, "/IoU_Prior_GT",sep="") 
      
      dir.create(file.path(FOLDER_CSV_MP_O,  "Stocking_VoxCnt_GT"), showWarnings = FALSE)
      FOLDER_CSV_STOCK_MP_O <- paste(FOLDER_CSV_MP_O, "/Stocking_VoxCnt_GT",sep="") 
      
      dir.create(file.path(FOLDER_CSV_MP_O,  "ALS"), showWarnings = FALSE)
      FOLDER_CSV_ALS_MP_O <- paste(FOLDER_CSV_MP_O, "/ALS",sep="") 
      
      ##########################
      # GENERATE LAS SUBFOLDERS
      ##########################
      dir.create(file.path(FOLDER_LAS_O,  paste("LAS_MP", Move_Position, sep="")), showWarnings = FALSE)
      FOLDER_LAS_MP_O <- paste(FOLDER_LAS_O, "/LAS_MP", Move_Position,sep="") 
      
      dir.create(file.path(FOLDER_LAS_MP_O,  paste("LAS_ALS",  sep="")), showWarnings = FALSE)
      FOLDER_LAS_ALS_MP_O <- paste(FOLDER_LAS_MP_O, "/LAS_ALS",sep="")
      
      dir.create(file.path(FOLDER_LAS_MP_O,  paste("Vox_P",  sep="")), showWarnings = FALSE)
      FOLDER_LAS_VOX_MP_O <- paste(FOLDER_LAS_MP_O, "/Vox_P",  sep="")
      
      dir.create(file.path(FOLDER_LAS_MP_O,  paste("LAS_P",  sep="")), showWarnings = FALSE)
      FOLDER_LAS_Pnt_MP_O <- paste(FOLDER_LAS_MP_O, "/LAS_P",sep="") 
      
      dir.create(file.path(FOLDER_LAS_MP_O,  paste("LAS_P15X15",  sep="")), showWarnings = FALSE)
      FOLDER_LAS_P15X15_MP_O <- paste(FOLDER_LAS_MP_O, "/LAS_P15X15",sep="") 
      
      dir.create(file.path(FOLDER_LAS_MP_O,  paste("LAS_P15X15_ResTriShp",  sep="")), showWarnings = FALSE)
      FOLDER_LAS_P15X15_ResTriShp_MP_O <- paste(FOLDER_LAS_MP_O, "/LAS_P15X15_ResTriShp",sep="") 
      
      ######################################################################################################################################
      ############################
      # SHIFTING FISHNET PROCEDURE
      ############################
      R_oneF_Rect_Move <- raster::shift(R_oneF_Rect, dx=Dist_Move_G[O], dy=Dist_Move_G[OO])
      R_oneF_Rect_Move <- setValues(R_oneF_Rect_Move, (max_Grid_ID+1):(max_Grid_ID+ncell(R_oneF_Rect_Move)))
      max_Grid_ID <- max_Grid_ID+ncell(R_oneF_Rect_Move)
      
      Poly_oneMP_allP <- rasterToPolygons(R_oneF_Rect_Move)  %>% st_as_sf() %>% st_set_crs(Proj_Sys)
      
      # CLIP ALL GRID CELLS USING BOUNDARY OF FLIGHT
      Poly_oneMP_allP_Bound <- st_intersection(Poly_oneMP_allP,Poly_oneF_Bound)
      Poly_oneMP_allP_Bound <- Poly_oneMP_allP_Bound %>% st_set_crs(Proj_Sys)
    
      st_write(Poly_oneMP_allP_Bound, 
               dsn = paste(FOLDER_SHP_O, sep=""), 
               layer = paste("F",FID, "_MovPos", Move_Position, "_allPlots",sep=""),
               driver = "ESRI Shapefile",
               append=FALSE, overwrite = TRUE)
      
      Poly_oneMP_allP_Bound <- Poly_oneMP_allP_Bound  %>% st_set_crs(Proj_Sys)
      colnames(Poly_oneMP_allP_Bound)[1] <- "Grid_ID" 
      Poly_oneMP_allP_Bound$MP_ID <-  Move_Position
      Range_oneMP_allP <- range(Poly_oneMP_allP_Bound$Grid_ID)
      
      # REMOVE GRIDS THAT ARE NOT WHOLE (i.e. 15*15)
      Index_fullGrid <- which(st_area(Poly_oneMP_allP_Bound) == as_units(225, "m^2")) 
      Poly_oneMP_allP_Bound <- Poly_oneMP_allP_Bound[Index_fullGrid,]

      ######################################################################################################################################
      ###########################################
      # MERGE SHP OF MP FOR ALL THE SHIFTED GRIDS
      ###########################################
      if(OO == 1 & O == 1){
        Poly_allMP_allP <- Poly_oneMP_allP
        Poly_allMP_fishnet <- Poly_oneMP_allP_Bound
        Range_allMP_allP <-t(data.frame(as.numeric(c(Move_Position, Range_oneMP_allP))))
        row.names(Range_allMP_allP) <- ""
        colnames(Range_allMP_allP) <- c("MovPos_ID", "Min_GID", "Max_GID" )
      }else{
        Poly_allMP_allP <- rbind(Poly_allMP_allP, Poly_oneMP_allP, makeUniqueIDs = TRUE)
        Poly_allMP_fishnet <- rbind(Poly_allMP_fishnet, Poly_oneMP_allP_Bound, makeUniqueIDs = TRUE)
        Range_allMP_allP <- rbind(Range_allMP_allP, as.numeric(c(Move_Position, Range_oneMP_allP)))
      }

      ######################################################################################################################################
      
      ###########################################################
      # LOOP THROUGH EACH P15X15 TO ID TID IN PLOT  (CSV AND SHP)
      ###########################################################

      # DOM DOM DOM !!! NOT SURE IF ANY OF THIS IS NECESSARY FOR ALGORITHM (IN THAT CASE HOW DOES Para_Z_Range GET USED TO IDENTIFY TARGETS)
      GID_oneMP_allP <- Poly_oneMP_allP_Bound$Grid_ID
      for(GG2 in 1:length(GID_oneMP_allP)){
        Poly_oneMP_oneP_Bound <- Poly_oneMP_allP_Bound[which(Poly_oneMP_allP_Bound$Grid_ID == GID_oneMP_allP[GG2]),]

        # IDENTIFY THE TID FOUND IN EACH SAMPLE PLOT
        AttTID_oneG_allT_SF <- st_intersection(AttTID_oneF_allG_SF, Poly_oneMP_oneP_Bound)
        AttTID_oneG_SCETrees_SF <- st_intersection(AttTID_SCETrees_SF, Poly_oneMP_oneP_Bound)

        st_is_valid(AttTID_oneF_allG_SF)

        # OUTPUT CSV OF Plots POINTS
        DF_AttTID_oneG_allT <- as.data.frame(AttTID_oneG_allT_SF)
        DF_AttTID_oneG_allT <- DF_AttTID_oneG_allT[,-which(colnames(DF_AttTID_oneG_allT) == "geometry")]

        DF_AttTID_oneG_SCETrees <- as.data.frame(AttTID_oneG_SCETrees_SF)
        DF_AttTID_oneG_SCETrees <- DF_AttTID_oneG_SCETrees[,-which(colnames(DF_AttTID_oneG_SCETrees) == "geometry")]

        write.csv(DF_AttTID_oneG_allT, paste(FOLDER_CSV_TID_MP_O, "/", "F",FID, "_P", GID_oneMP_allP[GG2], "_allT_AttTID.csv",sep=""), row.names = FALSE)
        write.csv(DF_AttTID_oneG_SCETrees, paste(FOLDER_CSV_SCE_MP_O, "/", "F",FID, "_P", GID_oneMP_allP[GG2], "_SCEOptimTrees_AttTID.csv",sep=""), row.names = FALSE)

        print(paste("DOING GG2: ", GG2, " out of ", length(GID_oneMP_allP)))
        }
      # END GG2 LOOP
      print("DONE GG2 LOOPS")

      ############################################################################################################################################### 5  
      #########################################
      # GENERATE CNN PLOTS WITH LAS INFORMATION
      #########################################
      ############################################################################################################################################### 5

      ################################################
      # PUT UNDERSTOREY LAS (WHOLE FLIGHT) INTO A LIST (FOR UAS AND ALS DATA) 
      ################################################  
      
      # UAS DATA
      Under_Samples <- list.files(FOLDER_LAS_UNDER_O, pattern ="LAS_UnderZero_F")
      List_LAS_UnderZero <- list()
      for(US in 1:length(Under_Samples)){
        LAS_oneUnderZero <- lidR::readLAS(paste(FOLDER_LAS_UNDER_O, "/", Under_Samples[US], sep=""), 
                                      select = "xyzp0")
        List_LAS_UnderZero <- c(List_LAS_UnderZero, LAS_oneUnderZero)
      }
      
      # ALS DATA
      Under_ALS_Samples <- list.files(FOLDER_LAS_UNDER_O, pattern ="LAS_ALS_UnderZero_F")
      List_ALS_UnderZero <- list()
      for(US in 1:length(Under_ALS_Samples)){
        LAS_oneALS_UnderZero <- lidR::readLAS(paste(FOLDER_LAS_UNDER_O, "/", Under_ALS_Samples[US], sep=""), 
                                          select = "xyzp0")

        List_ALS_UnderZero <- c(List_ALS_UnderZero, LAS_oneALS_UnderZero)
      }
      
      ######################################################################
      # LOOP THROUGH EACH UNDERSTOREY SAMPLE AND CLASSIFY THEM FOR EACH MOVE
      ######################################################################
      for(LL in 1:length(List_LAS_UnderZero)){
        LAS_LL <- List_LAS_UnderZero[[LL]]
        LAS_LL<- merge_spatial(LAS_LL, Poly_oneMP_allP_Bound, attribute = "Grid_ID")                    # LAS_LL<- merge_spatial(LAS_LL, Poly_oneMP_allP_Bound, attribute = "MP_ID")   #lasclassify(LAS_LL, Poly_oneMP_allP_Bound, paste("MovPos", MovPos_Number, sep=""))
        LAS_LL <-add_attribute(LAS_LL, x=Move_Position, name=paste("MovPos", Move_Position,sep=""))           # LAS_LL <- add_lasattribute(LAS_LL, x=Move_Position, name=paste("MovPos", Move_Position,sep=""), desc =paste("MovPos", Move_Position,sep=""))
        LAS_LL <- add_attribute(LAS_LL, x=LAS_LL@data$Grid_ID, name=paste("Grid_MP", Move_Position,sep=""))  # LAS_LL <-add_attribute(LAS_LL, x=LAS_LL@data$Grid_ID, name=paste("Grid_MP", Move_Position,sep=""))    # 
        List_LAS_UnderZero[[LL]] <- LAS_LL
        print(paste("DOING LL: ", LL, " out of ", length(List_LAS_UnderZero)))
        } # LOOP THROUGH LAS FILES
      print("DONE LL LOOPS")
      
      for(LL in 1:length(List_ALS_UnderZero)){
        LAS_LL <- List_ALS_UnderZero[[LL]]
        LAS_LL<- merge_spatial(LAS_LL, Poly_oneMP_allP_Bound, attribute = "Grid_ID")                    # LAS_LL<- merge_spatial(LAS_LL, Poly_oneMP_allP_Bound, attribute = "MP_ID")   #lasclassify(LAS_LL, Poly_oneMP_allP_Bound, paste("MovPos", MovPos_Number, sep=""))
        LAS_LL <-add_attribute(LAS_LL, x=Move_Position, name=paste("MovPos", Move_Position,sep=""))           # LAS_LL <- add_lasattribute(LAS_LL, x=Move_Position, name=paste("MovPos", Move_Position,sep=""), desc =paste("MovPos", Move_Position,sep=""))
        LAS_LL <- add_attribute(LAS_LL, x=LAS_LL@data$Grid_ID, name=paste("Grid_MP", Move_Position,sep=""))  # LAS_LL <-add_attribute(LAS_LL, x=LAS_LL@data$Grid_ID, name=paste("Grid_MP", Move_Position,sep=""))    # 
        List_ALS_UnderZero[[LL]] <- LAS_LL
        print(paste("DOING ALS LL: ", LL, " out of ", length(List_ALS_UnderZero)))
      } # LOOP THROUGH LAS FILES
      print("DONE ALS LL LOOPS")
      
      ##################################################################
      # GENERATING PLOT LAS FILES (WITH DIFFERENT SAMPLES FOR EACH PLOT)
      ##################################################################
      
      # GET ALL THE TID FOR EACH PLOT (Eech with Para_Samples samples)
      Files_P_S_CSV <- list.files(FOLDER_CSV_SCE_MP_O)
      Range_allMP_allP <- as.data.frame(Range_allMP_allP)  
      
      # GENERATE EMPTY DATAFRAME FOR OUTPUTTING ALL PLOT (TRACK TID AND SAMPLE FOR EACH TID USED SO THAT BOUNDING BOXES CAN BE MATCHED)
      DF_oneMP_allP_S_TID <- data.frame("FID" = as.numeric(),
                                       "Plot_ID" = as.numeric(),
                                       "Under_ID" = as.numeric(),
                                       "TID" = as.numeric(),
                                       "Sample_ID" = as.numeric())
      
      DF_oneMP_Miss_allP_S_TID <- data.frame("FID" = as.numeric(),
                                 "Plot_ID" = as.numeric(),
                                 "Under_ID" = as.numeric(),
                                 "TID" = as.numeric(),
                                 "Sample_ID" = as.numeric())

      # EMPTY DATAFRAME FOR GENERATING TREE DESCRIPTORS THAT CNN WILL TRY AND FIND
      #LocBox_oneF_oneMP_allP15X15 <- LocBox_Empty
      
      GT_XYZWLHR_oneF_oneMP_allP15X15 <- XYZWLHR_Empty

      ####################################
      # LOOP THROUGH EACH PLOT SAMPLE FILE
      ####################################
      pdf(paste(FOLDER_PDF_MP_O, "/F",  FID, "_MP", Move_Position, "TID_LEVEL_PLOT_DENSITIES.pdf", sep=""))
      par(mfrow=c(2,2)) 
      
      Count_Priors <- c()
      Files_Priors <- c()
      Comb_allPrior <- data.frame(Plot_ID = numeric(), 
                                          Loc = numeric(),
                                          Bot_BBox_Cnt = numeric(),
                                          Top_BBox_Cnt = numeric(),
                                          Combinations = numeric(),
                                          Removed_Dup_H2= numeric(),
                                          Removed_Dup_H3= numeric(),
                                          countMiss_H2= numeric(),
                                          countMiss_H3= numeric()) # , Final_Combinations= numeric()

      start.time <- Sys.time()
      
      # browser()
      
      # XYZWLHR_To_XYZWLHR_N_FUN 90 YES
      # XYZWHR_TO_VERT_FUN YES
      
      # XYZWLHR_To_Goffset_FUN 90
      # SCALE_RoI2Plot_Coord_FUN 90
      # XYZWLHR_FUN                # YES, IN GT PRODUCTION
      # rad2deg
      # deg2rad
      # VERT_To_XYZWLHR_FUN
      
      for(PP in 1:length(Files_P_S_CSV)){
 
        # GET TID IN EACH PLOT
        CSV_oneP <- read.csv(paste(FOLDER_CSV_SCE_MP_O, "/", Files_P_S_CSV[PP], sep=""), row.names = NULL)
        TID_oneP <- CSV_oneP$TID
        
        if(nrow(CSV_oneP) > 0){
          start.time <- Sys.time()
          max_Prior_TID <- 0
          Removed_Dup_H2 <- 0
          Removed_Dup_H3 <- 0
          countMiss_H2 <- 0
          countMiss_H3 <- 0
          
          # REMOVE OBJECTS AT THE START OF PP LOOP
          if(exists("Ext_oneP_plotPrior")){rm(Ext_oneP_plotPrior)}  
          if(exists("Loc_oneF_oneMP_oneP_allPriors")){rm(Loc_oneF_oneMP_oneP_allPriors)} #@  
          if(exists("Prior_BBox_oneF_oneMP_oneP")){rm(Prior_BBox_oneF_oneMP_oneP)}  #@
          if(exists("XYZWLHR_oneF_oneMP_oneP_Boxes")){rm(XYZWLHR_oneF_oneMP_oneP_Boxes)} #@
          if(exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){rm(XYZWLHR_oneF_oneMP_oneP_allPrior)} #@
          
          # GENERATE EMPTY DATAFRAME FOR EACH PLOT (TRACK TID AND SAMPLE FOR EACH TID USED SO THAT BOUNDING BOXES CAN BE MATCHED)
          DF_oneMP_oneP_allT <- data.frame("FID" = as.numeric(),
                                           "Plot_ID" = as.numeric(),
                                           "Under_ID" = as.numeric(),
                                           "TID" = as.numeric(),
                                           "Sample_ID" = as.numeric())
          
          DF_oneMP_Miss_oneP_allT <- data.frame("FID" = as.numeric(),
                                                "Plot_ID" = as.numeric(),
                                                "Under_ID" = as.numeric(),
                                                "TID" = as.numeric(),
                                                "Sample_ID" = as.numeric())
          
          # GET PLOT ID
          Plot_ID  <- as.numeric(numextract_all(Files_P_S_CSV[PP])[2])
          # MovPos_ID <- Range_allMP_allP$MovPos_ID[which(Range_allMP_allP$Min_GID <= Plot_ID & Range_allMP_allP$Max_GID >= Plot_ID)]
          
          #########################################
          # GET ONE UNDERSTOREY/ZERO LAS SAMPLE FOR PLOT
          #########################################
          Under_ID <- sample(1:Para_Samples,1)
          LAS_oneP_UnderZero <- List_LAS_UnderZero[[Under_ID]] 
          LAS_oneP_ALS_UnderZero <- List_ALS_UnderZero[[Under_ID]]
          
          # MAKE SURE ALS AND UAS HAVE SAME COLUMNS
          AddCol_ToALS <- setdiff(colnames(LAS_oneP_ALS_UnderZero@data), colnames(LAS_oneP_UnderZero@data))
          if(length(AddCol_ToALS)>0){
            for(CN in 1:length(AddCol_ToALS)){
              LAS_oneP_UnderZero <- add_lasattribute(LAS_oneP_UnderZero, x=as.integer(0), name=AddCol_ToALS[CN], desc =AddCol_ToALS[CN])
            }
          }
          
          AddCol_ToUAS<- setdiff(colnames(LAS_oneP_UnderZero@data),colnames(LAS_oneP_ALS_UnderZero@data))
          if(length(AddCol_ToUAS)>0){
            for(CN in 1:length(AddCol_ToUAS)){
              LAS_oneP_ALS_UnderZero <- add_lasattribute(LAS_oneP_ALS_UnderZero, x=as.integer(0), name=AddCol_ToUAS[CN], desc =AddCol_ToUAS[CN])
            }
          }
          
          Index_Col <- which(colnames(LAS_oneP_UnderZero@data) == paste("Grid_MP", Move_Position, sep=""))
          Index_oneP_Pnts <-which(as.vector(LAS_oneP_UnderZero@data[,..Index_Col]) == Plot_ID)
          
          Index_ALS_Col <- which(colnames(LAS_oneP_ALS_UnderZero@data) == paste("Grid_MP", Move_Position, sep=""))
          Index_ALS_oneP_Pnts <-which(as.vector(LAS_oneP_ALS_UnderZero@data[,..Index_ALS_Col]) == Plot_ID)
          
          if(length(Index_oneP_Pnts) > 0){
            LAS_oneP_UnderZero@data <- LAS_oneP_UnderZero@data[Index_oneP_Pnts,]
            
            # WORK AROUND WHERE ALS DOESN'T HAVE UNDERSTOREY THEN YOU JUST GET UNDERSTOREY FROM UAS
            if(length(Index_ALS_oneP_Pnts) > 0){
              LAS_oneP_ALS_UnderZero@data <- LAS_oneP_ALS_UnderZero@data[Index_ALS_oneP_Pnts,]
            }else{
              LAS_oneP_ALS_UnderZero <- LAS_oneP_UnderZero
              LAS_oneP_ALS_UnderZero@data <- LAS_oneP_UnderZero@data[Index_oneP_Pnts,]
            }
            
            # EXTRACT UNDER FOR ONE PLOT
            Index_ColKeep <- which(substr(colnames(LAS_oneP_UnderZero@data),1,3) != "Mov" ) # & substr(colnames(LAS_oneP_UnderZero@data),1,7) != "Grid_MP"  
            LAS_oneP_UnderZero@data <- LAS_oneP_UnderZero@data[,..Index_ColKeep]
            
            Index_ALS_ColKeep <- which(substr(colnames(LAS_oneP_ALS_UnderZero@data),1,3) != "Mov" ) # & substr(colnames(LAS_oneP_UnderZero@data),1,7) != "Grid_MP"  
            LAS_oneP_ALS_UnderZero@data <- LAS_oneP_ALS_UnderZero@data[,..Index_ALS_ColKeep]
            
            ###############################################################################################################################################################
            # ###############################################################
            # LOOP THROUGH GT TID FOR THE ONE P15X15 AND MERGE TO UNDERSTOREY
            # ###############################################################
            ###############################################################################################################################################################
            
            onePlot_Stocking <- data.frame(Flight = FID, Plot_ID = Plot_ID, Move_Position = Move_Position, Stocking = length(TID_oneP))
            Plot_Stocking <- rbind(Plot_Stocking, onePlot_Stocking)
            
            # GENERATE one plot LAS (STARTING WITH UNDERSTOREY)
            LAS_oneP_allT <- LAS_oneP_UnderZero
            LAS_ALS_oneP_allT <- LAS_oneP_ALS_UnderZero
            
            ##########################################
            # MAKE SURE ALS AND UAS HAVE SAME COLNAMES
            ##########################################
            
            AddCol_ToALS <- setdiff(colnames(LAS_oneP_allT@data), colnames(LAS_ALS_oneP_allT@data))
            if(length(AddCol_ToALS) > 0){
              for(CN in 1:length(AddCol_ToALS)){
                LAS_ALS_oneP_allT <- add_lasattribute(LAS_ALS_oneP_allT, x=as.integer(0), name=AddCol_ToALS[CN], desc =AddCol_ToALS[CN])
              }
            }
            AddCol_ToUAS<- setdiff(colnames(LAS_ALS_oneP_allT@data),colnames(LAS_oneP_allT@data))
            if(length(AddCol_ToUAS) > 0){
              for(CN in 1:length(AddCol_ToUAS)){
                LAS_oneP_allT <- add_lasattribute(LAS_oneP_allT, x=as.integer(0), name=AddCol_ToUAS[CN], desc =AddCol_ToUAS[CN])
              }
            }
            
            ################################################################################################################################################################################################################
            ####################################################
            # LOOP THROUGH UAS AND ALS DATA TO GENERATE THE PLOT
            ####################################################
            
            LIST_LAS_TYPES <- list(LAS_ALS_oneP_allT, LAS_oneP_allT)
            
            Suffix_LAS_TYPE <- c("_ALS", "")
            Folder_Get_oneT <- c(FOLDER_ALS_TID_O, FOLDER_LAS_TID_O)
            Folder_AU_LAS_TYPE <- c(FOLDER_LAS_ALS_MP_O, FOLDER_LAS_Pnt_MP_O)
            Folder_AU_P15X15_LAS_TYPE <- c(FOLDER_LAS_ALS_MP_O, FOLDER_LAS_P15X15_MP_O)
            Folder_AU_ResTriShp_LAS_TYPE <- c(FOLDER_LAS_ALS_MP_O, FOLDER_LAS_P15X15_ResTriShp_MP_O)
            Folder_AU_VOX_LAS_TYPE <- c(FOLDER_LAS_ALS_MP_O, FOLDER_LAS_VOX_MP_O)
            Folder_AU_CSV_MISS_TYPE <- c(FOLDER_CSV_ALS_MP_O, FOLDER_CSV_MISS_MP_O)
            Folder_AU_CSV_TID_TYPE <- c(FOLDER_CSV_ALS_MP_O,FOLDER_CSV_TID_MP_O) 
            Folder_AU_CSV_P15X15_TYPE <- c(FOLDER_CSV_ALS_MP_O,FOLDER_CSV_P15X15_MP_O)
            
            # LOOP AU (LOOP UAS AND ALS)
            for(AU in 1:length(LIST_LAS_TYPES)){
              LAS_oneP_oneType_allT <- LIST_LAS_TYPES[[AU]]
              
              LAS_oneP_oneType_allT <- add_lasattribute(LAS_oneP_oneType_allT, x= as.integer(LAS_oneP_oneType_allT@data$PointSourceID), name="TID", desc = "TID")  # LAS_oneP_oneType_allT <-add_attribute(LAS_oneP_oneType_allT, x= as.integer(LAS_oneP_oneType_allT@data$PointSourceID), name="TID") 
              LAS_oneP_oneType_allT <- add_attribute(LAS_oneP_oneType_allT, x=Move_Position, name="MP_ID")
              
              ###################################
              # LOOP THROUGH EACH TID OF ONE PLOT
              ###################################
              if(length(TID_oneP) > 0){
                for(PS in 1:length(TID_oneP)){
                  S_ID<- sample(1:Para_Samples,1)
                  
                  ###############################################
                  # GET LAS DATA FOR THE ONE TID AND PUT IN P15X15 
                  ###############################################
                  File_LAS_TID <- paste("LAS_F",FID,"_TID", TID_oneP[PS],"_S", S_ID, ".laz", sep="")
                  if(length(list.files(Folder_Get_oneT[AU],File_LAS_TID)) == 1){
                    # ADD ONE TID LAS TO THE GRID
                    LAS_oneT <-  lidR::readLAS(paste(Folder_Get_oneT[AU], "/", File_LAS_TID, sep=""), select = "xyzp0")
                    # print(table(LAS_oneT@data$PointSourceID))
                    LAS_oneT <- add_attribute(LAS_oneT, x= TID_oneP[PS], name=paste("Grid_MP", Move_Position,sep=""))
                    LAS_oneT <- add_attribute(LAS_oneT, x=Move_Position, name="MP_ID")
                    LAS_oneT <- add_lasattribute(LAS_oneT, x= TID_oneP[PS], name="Grid_ID", desc = "Grid_ID") # LAS_oneT@data$Grid_ID <- TID_oneP[PS] #
                    LAS_oneT <- add_lasattribute(LAS_oneT, x= as.integer(LAS_oneT@data$PointSourceID), name="TID", desc = "TID")
                    LAS_oneT <- add_lasattribute(LAS_oneT, x= as.integer(0), name="C_Base", desc = "C_Base")
                    # LAS_oneT <- add_lasattribute(LAS_oneT, x= as.integer(0), name="Inside_TriShp", desc = "Inside_TriShp")
                    
                    if(AU == 1){
                      LAS_oneT <- add_lasattribute(LAS_oneT, x= as.integer(0), name="Centroid_ID", desc = "Centroid_ID")
                    }
                    
                    # WORK AROUND AS I LOOSE THIS ATTRIBUTE WHEN I READ IT BACK IN ... NOT SURE WHY!!!
                    if(AU == 2){
                      LAS_oneT <- add_lasattribute(LAS_oneT, x= as.integer(0), name="LAS_Type", desc = "LAS_Type")
                    }
                    
                    # # SANITY CHECK!!
                    # if(length(setdiff(colnames(LAS_oneP_oneType_allT@data), colnames(LAS_oneT@data))) != 0 |
                    #    length(setdiff( colnames(LAS_oneT@data), colnames(LAS_oneP_oneType_allT@data))) != 0){browser()}
                    
                    LAS_oneP_oneType_allT@data <- rbind(LAS_oneP_oneType_allT@data, LAS_oneT@data)
                    
                    ##########################################################
                    # IF LAS TID EXISTS STORE THE INFO IN DF_oneMP_oneP_allT
                    ##########################################################
                    DF_oneMP_oneP_oneT <- t(as.data.frame(as.numeric(c(FID, Plot_ID, Under_ID, TID_oneP[PS], S_ID))))
                    rownames(DF_oneMP_oneP_oneT ) <- ""
                    colnames(DF_oneMP_oneP_oneT) <- c("FID", "Plot_ID", "Under_ID", "TID", "Sample_ID")
                    DF_oneMP_oneP_allT <-  rbind(DF_oneMP_oneP_allT, DF_oneMP_oneP_oneT)
                    
                  }else{
                    ################################################################
                    # IF LAS TID NOT EXIST STORE THE INFO IN DF_oneMP_Miss_oneP_allT
                    ################################################################
                    DF_oneMP_Miss_oneP_oneT <- t(as.data.frame(as.numeric(c(FID, Plot_ID, Under_ID, TID_oneP[PS], S_ID))))
                    rownames(DF_oneMP_Miss_oneP_oneT ) <- ""
                    colnames(DF_oneMP_Miss_oneP_oneT) <- c("FID", "Plot_ID", "Under_ID", "TID", "Sample_ID")
                    DF_oneMP_Miss_oneP_allT <-  rbind(DF_oneMP_Miss_oneP_allT, DF_oneMP_Miss_oneP_oneT)
                  }
                  #browser()
                } # PS LOOP .... LOOPING THROUGH TID TREES WITHIN PLOTS
              }else{# IF THERE ARE NO TID IN PLOT
                DF_oneMP_oneP_oneT <- t(as.data.frame(as.numeric(c(FID, Plot_ID, Under_ID, NA, NA))))
                rownames(DF_oneMP_oneP_oneT ) <- ""
                colnames(DF_oneMP_oneP_oneT) <- c("FID", "Plot_ID", "Under_ID", "TID", "Sample_ID")
                DF_oneMP_oneP_allT <-  rbind(DF_oneMP_oneP_allT, DF_oneMP_oneP_oneT)
                DF_oneMP_Miss_oneP_oneT <- t(as.data.frame(as.numeric(c(FID, Plot_ID, Under_ID, NA, NA))))
                rownames(DF_oneMP_Miss_oneP_oneT ) <- ""
                colnames(DF_oneMP_Miss_oneP_oneT) <- c("FID", "Plot_ID", "Under_ID", "TID", "Sample_ID")
                DF_oneMP_Miss_oneP_allT <-  rbind(DF_oneMP_Miss_oneP_allT, DF_oneMP_Miss_oneP_oneT)
              }
              
              # OUTPUTING LAS FILE FOR PLOT AND CSV FILE FOR SAMPLED TREES IN PLOT (FOR GETTING BOUNDING BOXES LATER)
              LAS_oneP_oneType_allT@data$TID <- LAS_oneP_oneType_allT@data$PointSourceID #<- as.integer(LAS_oneP_oneType_allT@data$TID)
              writeLAS(LAS_oneP_oneType_allT, paste(Folder_AU_LAS_TYPE[AU],"/F",  FID, "_MP", Move_Position, "_P", Plot_ID, Suffix_LAS_TYPE[AU], ".laz", sep="")) # SUFFIX
              
              write.csv(DF_oneMP_Miss_oneP_allT, paste( Folder_AU_CSV_MISS_TYPE[AU],"/F",  FID, "_P", Plot_ID, "_Missed_Trees",Suffix_LAS_TYPE[AU], ".CSV",sep=''), row.names=FALSE) # SUFFIX
              write.csv(DF_oneMP_oneP_allT, paste(Folder_AU_CSV_TID_TYPE[AU],"/F",  FID, "_P", Plot_ID, "_allS_allT", Suffix_LAS_TYPE[AU],".CSV",sep=''), row.names=FALSE) # SUFFIX
              
              # MERGE ALL ONE MP CSV OUTPUT
              DF_oneMP_allP_S_TID <- rbind(DF_oneMP_allP_S_TID, DF_oneMP_oneP_allT)
              DF_oneMP_Miss_allP_S_TID <- rbind(DF_oneMP_Miss_allP_S_TID, DF_oneMP_Miss_oneP_allT)
              
              ############################################################################################################################################### 6
              ############################################################################################################################################### 6  
              #####################################
              # SHRINK PLOT (I.E. BOUND LAS VALUES WITHIN 15*15*40)
              #####################################  
              ############################################################################################################################################### 6
              ############################################################################################################################################### 6      
              LAS_oneP_oneType_allT_Orig <- LAS_oneP_oneType_allT
              
              LAS_oneP_oneType_allT <- add_lasattribute(LAS_oneP_oneType_allT, x=as.integer(0), name="Inside_TriShp", desc ="Inside_TriShp")
              ExtOrig_oneP_allT <- raster::extent(LAS_oneP_oneType_allT) 
              Poly_oneG <- Poly_allMP_allP[which(Poly_allMP_allP$layer == Plot_ID),] # Poly_allMP_allP
              Poly_ExtTarget <- raster::extent(Poly_oneG)
              
              # REDUCE X AND Y VALUE 
              extent_Diff <- as.vector(Poly_ExtTarget) - as.vector(ExtOrig_oneP_allT) # GRID - ORIG_LAS
              X_Range_Shrink <- as.vector(ExtOrig_oneP_allT)[1:2]
              if(extent_Diff[1] > 0){X_Range_Shrink[1] <- Poly_ExtTarget[1]} # IF MinX ORIG_LAS IS WITHIN GRID
              if(extent_Diff[2] < 0){X_Range_Shrink[2] <- Poly_ExtTarget[2]} # IF MaxX ORIG_LAS IS WITHIN GRID
              
              Y_Range_Shrink <- as.vector(ExtOrig_oneP_allT)[3:4]
              if(extent_Diff[3] > 0){Y_Range_Shrink[1] <- Poly_ExtTarget[3]}
              if(extent_Diff[4] < 0){Y_Range_Shrink[2] <- Poly_ExtTarget[4]}
              
              x_oneP <- LAS_oneP_oneType_allT$X
              origRng_oneP_x <- range(x_oneP)
              x_New <- BBmisc::normalize(x_oneP, method = "range", range = X_Range_Shrink) # range(LAS_oneG_All_Under$X)
              LAS_oneP_oneType_allT$X <- x_New
              
              y_oneP <- LAS_oneP_oneType_allT$Y
              origRng_oneP_y <- range(y_oneP)
              y_New <- BBmisc::normalize(y_oneP, method = "range", range = Y_Range_Shrink) # range(LAS_oneG_All_Under$Y)
              LAS_oneP_oneType_allT$Y <- y_New
              
              # REDUCE Z VALUE 
              ZExt_oneP_allT <-range(LAS_oneP_oneType_allT$Z)
              z_oneP <- LAS_oneP_oneType_allT$Z
              origRng_oneP_z <- range(z_oneP)
              if(ZExt_oneP_allT[2] > Para_MaxZ_Shrink){
                Z_Range_Shrink <- c(0.1, Para_MaxZ_Shrink)
              } else{
                Z_Range_Shrink <- c(0.1, range(LAS_oneP_oneType_allT$Z)[2]) 
              }
              Z_New <- BBmisc::normalize(z_oneP, method = "range", range = Z_Range_Shrink)
              LAS_oneP_oneType_allT$Z <- Z_New
              writeLAS(LAS_oneP_oneType_allT, paste( Folder_AU_P15X15_LAS_TYPE[AU],"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID, "_15X15X40_N", Suffix_LAS_TYPE[AU],".laz", sep=""))
              
              # DOM DOM DOM !!! GENERATE CSV FILE OF THE LAS INFORMATION
              DF_oneP_oneType_allT <- LAS_oneP_oneType_allT@data
              write.csv(DF_oneP_oneType_allT, paste(FOLDER_CSV_VOX_MP_O,"/F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_15X15X40_N",Suffix_LAS_TYPE[AU],".csv", sep=""))
              
              # STORING THE SHRINK FACTOR FOR ALS
              if(Suffix_LAS_TYPE[AU] == "_ALS"){ 
                X_origRng_ALS <- origRng_oneP_x
                Y_origRng_ALS <- origRng_oneP_y
                Z_origRng_ALS <- origRng_oneP_z
              }
              
              #####################################################
              # ADJUSTING THE XYZWLHR VALUES USING THE 15X15 SHRINK
              #####################################################
              
              Width_X_Portion <- (Poly_ExtTarget[2]-Poly_ExtTarget[1])/(ExtOrig_oneP_allT[2] - ExtOrig_oneP_allT[1])
              Length_Y_Portion <- (Poly_ExtTarget[4]-Poly_ExtTarget[3])/(ExtOrig_oneP_allT[4] - ExtOrig_oneP_allT[3])
              Height_Z_Portion <- (Z_Range_Shrink[2]- Z_Range_Shrink[1])/(ZExt_oneP_allT[2] - ZExt_oneP_allT[1])
              
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15 <- XYZWLHR_oneF_allT[which(XYZWLHR_oneF_allT$TID %in% TID_oneP),]
              Index_X <- which(substr(colnames(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15),1,1)  == "X")
              Index_Y <- which(substr(colnames(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15),1,1)  == "Y")
              Index_Z <- which(substr(colnames(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15),1,1)  == "Z")
              Index_L <- which(substr(colnames(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15),1,1)  == "L")
              Index_W <- which(substr(colnames(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15),1,1)  == "W")
              
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_X] <- Poly_ExtTarget[1] + Width_X_Portion*(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_X] - ExtOrig_oneP_allT[1])   # BBmisc::normalize(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_X], method = "range", range = X_Range_Shrink)Poly_ExtTarget[1]
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Y] <- Poly_ExtTarget[3] + Length_Y_Portion*(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Y] - ExtOrig_oneP_allT[3])     # BBmisc::normalize(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Y], method = "range", range = Y_Range_Shrink)
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Z] <-  Z_Range_Shrink[1]  + Height_Z_Portion*(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Z] - ZExt_oneP_allT[1])   # BBmisc::normalize(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_Z], method = "range", range = Z_Range_Shrink)
              
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_L] <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_L] * Length_Y_Portion  # SWITCH Length_Y_Portion with Width_X_Portion (18/03/21)
              GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_W] <-  GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[,Index_W] * Width_X_Portion  # SWITCH Width_X_Portion with Width_X_Portion (18/03/21)
              
              write.csv(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15, paste(Folder_AU_CSV_P15X15_TYPE[AU], "/F",  FID, "_P",Plot_ID,"_Ext15X15_allT_LocBox_XYZWLHR", Suffix_LAS_TYPE[AU],".csv", sep=""), row.names=FALSE)   # LocBox_oneF_oneMP_oneP_Ext15X15 <- read.csv(paste(FOLDER_CSV_O, "/F",  FID, "AllTID_100Sample_Loc_BoundBox.csv", sep="") )
              
              # UPDATE THE FILE WILL ALL THE XYZWLHR FOR ALL PLOTS IN  MP AND FLIGHT
              GT_XYZWLHR_oneF_oneMP_allP15X15 <- rbind(GT_XYZWLHR_oneF_oneMP_allP15X15, GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15)
              
              ###################################################################
              # RE-ASSIGN RESIDUALS WITHIN GT TRI-SHAPE INTO TID (BEFORE VOXELISING)
              ###################################################################  
              
              Triangles_ID_All <-TRIANG_ID_ALL_FUN(Para_Faces_Used = 4, Box_Levels = 3)
              Triangles_ID_All_Mx <- matrix(Triangles_ID_All, ncol=3, byrow= TRUE)
              Point_DF <- data.frame(LAS_oneP_oneType_allT@data[,1:3])
              
              Unique_T <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID
              
              for(RT in 1:length(Unique_T)){ 
                Index_T <- which(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID ==Unique_T[RT])
                Vert_oneT <- XYZWHR_TO_VERT_FUN(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[Index_T,3:ncol(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15)], 
                                                Base_WL = Para_Base_WL, Normalised = "No", Para_Cnt = Para_TriShpParaCnt)
                Vert_oneT <- na.omit(Vert_oneT)
                Vert_oneT <- VERTICIES_ORDER_FUN(Vert_oneT, Box_Levels = 3) # ADDED 11/05/2021... 
                
                #### NOTE THAT WE DON'T HAVE THE GROUND BOX (Z=0) IN THIS COMPUTATION
                
                # Vert_oneT_DF <- as.data.frame(Vert_oneT)
                # Shift_X <- min(LAS_PLOT$X)
                # Shift_Y <- min(LAS_PLOT$Y) 
                # points3d(as.vector(Vert_oneT_DF$X[1:4])-Shift_X,
                #          as.vector(Vert_oneT_DF$Y[1:4]) -Shift_Y,
                #          as.vector(Vert_oneT_DF$Z[1:4]),
                #          col="Yellow", size=10)
                # polygon3d(as.vector(Vert_oneT_DF$X[5:8])-Shift_X,
                #           as.vector(Vert_oneT_DF$Y[5:8])-Shift_Y,
                #           as.vector(Vert_oneT_DF$Z[5:8]),
                #           fill = FALSE, col="orange", lwd=2)
                # polygon3d(as.vector(Vert_oneT_DF$X[9:12])-Shift_X,
                #           as.vector(Vert_oneT_DF$Y[9:12])-Shift_Y,
                #           as.vector(Vert_oneT_DF$Z[9:12]),
                #           fill = FALSE, col="red", lwd=2)
                # polygon3d(as.vector(Vert_oneT_DF$X[13:16])-Shift_X,
                #           as.vector(Vert_oneT_DF$Y[13:16])-Shift_Y,
                #           as.vector(Vert_oneT_DF$Z[13:16]),
                #           fill = FALSE, col="blue", lwd=2)
                
                #######################################
                # FILL IN NA VALUES WITHIN THE TRISHAPE 
                #######################################
                
                # IF THE TREE  (TID) IS MORE THAN 2 M TALL (ALMOST ALWAYS)
                if((range(Vert_oneT[,3])[2]-range(Vert_oneT[,3])[1]) > 2){
                  # CALCULATE INSIDE VECTORS
                  nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vert_oneT, Point_DF)
                  insideVect <- list(nbIntersect%%2 != 0)
                  
                  # UPDATE NA VALUES TO TID VALUE
                  LAS_oneP_oneType_allT@data$Inside_TriShp[insideVect[[1]]] <- Unique_T[RT] # Summary_onePlot$PointSourceID[UPR]
                  LAS_oneP_oneType_allT@data$TID[which(LAS_oneP_oneType_allT@data$TID == 1 &
                                                         LAS_oneP_oneType_allT@data$Inside_TriShp == Unique_T[RT]) ] <- Unique_T[RT] # as.integer(Summary_onePlot$PointSourceID[UPR])
                }
              } # RT LOOP (REASSIGN RESIDUALS TO EACH GT TRISHAPE)
              
              #OUTPUT PLOT WITH RES_TRISHP
              writeLAS(LAS_oneP_oneType_allT, paste(Folder_AU_ResTriShp_LAS_TYPE[AU],"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID, "_15X15X40_ResTriShpAssigned", Suffix_LAS_TYPE[AU],".laz", sep=""))
              
              # ### TESTING PLOTTING BOUNDING BOXES TO CHECK THAT THEY WORK
              # PLOT_LAS_XYZWLHR_FUN(LAS_oneP_oneType_allT, GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15, Para_Base_WL, Normalised = "No")
              # Shift_X <- min(LAS_oneP_oneType_allT@data$X)
              # Shift_Y <- min(LAS_oneP_oneType_allT@data$Y)
              # maxZ <- max(LAS_oneP_oneType_allT@data$Z)
              # XY_Poly_oneG <- as.data.frame(st_coordinates(Poly_oneG)[,1:2])
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(0,0,0,0, 0)),fill = FALSE, col="Yellow", size=20)
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(maxZ,maxZ,maxZ,maxZ, maxZ)),fill = FALSE, col="Yellow", size=20)
              # print("Post Shift")
              
              
              ############################################################################################################################################### 7  
              #########################
              # GENERATE VOXELISED GRID
              #########################  
              ############################################################################################################################################### 7
              
              XYZ_oneG <- data.frame(st_coordinates(Poly_oneG)[,1:2], Z = 0)
              XYZ_oneG <- round(XYZ_oneG, 0)
              colnames(XYZ_oneG) <- c("X", "Y", "Z")
              
              # # THIS MAKES SURE THERE ARE ONLY 15 VOXELS IN X AND Y DIRECTION
              # XYZ_oneG$X[which(XYZ_oneG$X %in% min(XYZ_oneG$X))] <- XYZ_oneG$X[which(XYZ_oneG$X %in% min(XYZ_oneG$X))] + 0.1 
              # XYZ_oneG$Y[which(XYZ_oneG$Y %in% min(XYZ_oneG$Y))] <- XYZ_oneG$Y[which(XYZ_oneG$Y %in% min(XYZ_oneG$Y))] + 0.1 
              
              XYZ_oneG_LAS <- LAS_oneP_oneType_allT@data[1,4:ncol(LAS_oneP_oneType_allT@data)]
              XYZ_oneG_LAS[,] <- as.integer(0)
              XYZ_oneG_LAS <- XYZ_oneG_LAS[rep(1,nrow(XYZ_oneG)),]
              LAS_Corner_Pnts <- LAS(cbind(XYZ_oneG, XYZ_oneG_LAS))
              # LAS_Corner_Pnts <- las_quantize(LAS_Corner_Pnts)
              crs(LAS_Corner_Pnts) <- crs(LAS_oneP_oneType_allT)
              
              # MAKING SURE ALL POINTS ARE WITHIN A RANGE THAT ALLOWS FOR VOXELISATION TO BE CORRECT
              LAS_oneP_oneType_allT <- filter_poi(LAS_oneP_oneType_allT, X < (min(X)+ Para_plot_Res))
              LAS_oneP_oneType_allT <- filter_poi(LAS_oneP_oneType_allT, Y < (min(Y)+ Para_plot_Res))
              
              # flag <- "ff"
              
              LAS_oneP_oneType_allT <- rbind(LAS_oneP_oneType_allT, LAS_Corner_Pnts)
              LAS_oneP_oneType_allT@data$Inside_TriShp <- 0
              
              # flag <- "fff"
              # if( (range(LAS_oneP_oneType_allT$Y)[2]- range(LAS_oneP_oneType_allT$Y)[1]) != (range(LAS_oneP_oneType_allT$X)[2]- range(LAS_oneP_oneType_allT$X)[1])) {browser()}
              # 
              Vox_TID <- lidR::voxel_metrics(LAS_oneP_oneType_allT, 
                                             names(table(TID))[which.max(table(TID))], 
                                             res = Para_Vox_Res)
              Vox_Count <- lidR::voxel_metrics(LAS_oneP_oneType_allT, 
                                               max(table(TID)), 
                                               res = Para_Vox_Res)
              Vox_TID_Count <- cbind(Vox_TID, Vox_Count[,4])
              colnames(Vox_TID_Count) <- c("X", "Y", "Z", "TID", "Count")
              Vox_TID_Count$TID <- as.integer(Vox_TID_Count$TID)
              
              # GENERATE EMPTY VOXELS
              Empty_Voxels <- EMPTY_VOX_FUN(Vox_TID_Count, Para_Vox_Res, Para_MaxZ_Shrink)    
              
              # FINAL VOXEL FOR TENSORFLOW (i.e. EMPYT VOXELS ASSIGNED TID =0 AND COUNT =0)
              Vox_Final <- rbind(Vox_TID_Count, Empty_Voxels)
              # ORDER THE VOXEL SO THAT IT IS FIRST Z (min to max), then Y and then X
              Vox_Final <- Vox_Final[order( Vox_Final$Z, Vox_Final$Y, Vox_Final$X),]
              LAS_Vox <- LAS(Vox_Final)
              LAS_Vox@data$PointSourceID <- as.integer(LAS_Vox@data$TID)
              LAS_Vox <- add_lasattribute(LAS_Vox, x=as.integer(LAS_Vox@data$Count), name="Count", desc ="Count")
              LAS_Vox <- add_lasattribute(LAS_Vox, x=as.integer(0), name="Prior", desc ="Prior")
              
              writeLAS(LAS_Vox, paste(Folder_AU_VOX_LAS_TYPE[AU],"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox", Suffix_LAS_TYPE[AU],".laz", sep="")) 
              
              flag <- "fffff"
              if( (range(LAS_Vox$Y)[2]- range(LAS_Vox$Y)[1]) != (range(LAS_Vox$X)[2]- range(LAS_Vox$X)[1])) {browser()}
              flag <- "f"
              ############################
              # NORMALISE THE ALS VOX DATA 
              ############################
              if(Suffix_LAS_TYPE[AU] == "_ALS"){
                
                # PREPARING THE LAS HEADER FOR SCALING   # https://gis.stackexchange.com/questions/387535/shrinking-coordinates-of-las-file-to-fit-in-0-1-using-lidr
                
                Shift_Vox_X_ALS <- min(LAS_Vox$X) 
                Shift_Vox_Y_ALS <- min(LAS_Vox$Y)
                Shift_Vox_Z_ALS <- min(LAS_Vox$Z)
                
                LAS_Vox_N <-SCALE_LAS_FUN(LAS_Vox, Scale_Factor = 0.01/1, 
                                          Offset_Factor_X = as.integer(min(LAS_Vox$X)), 
                                          Offset_Factor_Y= as.integer(min(LAS_Vox$Y)), 
                                          Offset_Factor_Z= as.integer(min(LAS_Vox$Z)) ) 
                
                
                ### DOM DOM DOM !!! ABOVE FUNCTION SOLVES FOR WHEN YOU IMPORT THIS FILE INTO R THEN IT LOOSE THE Y DIMENSION (NOT SURE WHY!!!!) see below import... I.E. unique(LAS_VoxTest$Y) or plot(LAS_VoxTest)
                ##################### LAS_VoxTest <- readLAS(paste(FOLDER_LAS_VOX_MP_O,"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox_N.laz", sep=""), select = "xyzp0")
                
                
                LAS_Vox_N$X <- LAS_Vox_N$X - Shift_Vox_X_ALS
                LAS_Vox_N$Y <- LAS_Vox_N$Y - Shift_Vox_Y_ALS
                LAS_Vox_N$Z <- LAS_Vox_N$Z - Shift_Vox_Z_ALS
                
                Shift_Vox_N_X_ALS <- max(LAS_Vox_N$X)
                Shift_Vox_N_Y_ALS <- max(LAS_Vox_N$Y)
                Shift_Vox_N_Z_ALS <- max(LAS_Vox_N$Z)
                
                LAS_Vox_N$X <- LAS_Vox_N$X/Shift_Vox_N_X_ALS
                LAS_Vox_N$Y <- LAS_Vox_N$Y/Shift_Vox_N_Y_ALS
                LAS_Vox_N$Z <- LAS_Vox_N$Z/Shift_Vox_N_Z_ALS
                
                # NORMALISE THE COUNT FIELD 
                maxDen_N_ALS <- max(LAS_Vox_N@data$Count)
                
                #LAS_Vox_N@data$Count_Norm <- LAS_Vox_N@data$Count/max(LAS_Vox_N@data$Count)
                LAS_Vox_N <- add_lasattribute(LAS_Vox_N, x=as.numeric(LAS_Vox_N@data$Count/maxDen_N_ALS), name="Count_Norm", desc ="Count_Norm")
                writeLAS(LAS_Vox_N, paste(Folder_AU_VOX_LAS_TYPE[AU],"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox_N",Suffix_LAS_TYPE[AU],".laz", sep="")) 
                
                #browser()
                # DOM DOM DOM !!! GENERATE CSV FILE OF THE LAS INFORMATION
                DF_Vox_N <- LAS_Vox_N@data
                write.csv(DF_Vox_N, paste(FOLDER_CSV_VOX_MP_O,"/F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox_N",Suffix_LAS_TYPE[AU],".laz", sep=""))
                
                #######################################
                # OUTPUT NORMALISED LAS VOXEL DATAFRAME
                #######################################
                DF_Vox_N <- as.data.frame(LAS_Vox_N@data)
                #DF_Vox_N <- DF_Vox_N[,match(c("X", "Y", "Z", "TID", "Count_Norm"),colnames(DF_Vox_N))]
                DF_Vox_N <- DF_Vox_N[order( DF_Vox_N[,3], DF_Vox_N[,2], DF_Vox_N[,1]),]
                write.csv(DF_Vox_N, paste(FOLDER_CSV_VOX_ALS_MP_O,"/F",  FID,"_P",   Plot_ID, "_LAS_Vox_N", Suffix_LAS_TYPE[AU],".csv",sep=''), row.names=FALSE) 
              }
              
              print(paste("VOXEL_Completed :", PP, " of a total of ", length(Files_P_S_CSV), sep=""))
              
            } # AU LOOP THAT LOOPS THROUGH BOTH ALS AND UAS DATA TO GENERATE VOXELISED OUTPUT
            
            #### DOM DOM DOM !!! BELOW HERE YOU ONLY RUN CODE WITH UAS DATA (ALS IS NOT USED TO GENERATE THE TRI-SHAPES AND ALL OTHER OUTPUT)
            # MAKE SURE LAS_oneP_allT IS LAS_oneP_oneType_allT FOR THE REST OF THE BELOW CODE
            LAS_oneP_allT <- LAS_oneP_oneType_allT
            
            ############################################################################################################################################### 9
            ############################################################################################################################################### 9
            ###############################################################
            # CALCULATE PORTION OF EACH GT CAPTURED WITHIN EACH GT TRISHAPE
            ###############################################################
            ############################################################################################################################################### 9
            ############################################################################################################################################### 9
            print("FINISHED AU")
            
            ### TESTING PLOTTING BOUNDING BOXES TO CHECK THAT THEY WORK
            LAS_Vox_Trees <- filter_poi(LAS_Vox, TID > 1)
            
            if(length(LAS_Vox_Trees$X) > 0){
              # PLOT_LAS_XYZWLHR_FUN(LAS_Vox_Trees, GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15, Para_Base_WL, Normalised = "No")
              # Shift_X <- min(LAS_oneP_oneType_allT@data$X)
              # Shift_Y <- min(LAS_oneP_oneType_allT@data$Y)
              # maxZ <- max(LAS_oneP_oneType_allT@data$Z)
              # XY_Poly_oneG <- as.data.frame(st_coordinates(Poly_oneG)[,1:2])
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(0,0,0,0, 0)),fill = FALSE, col="Yellow", size=20)
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(maxZ,maxZ,maxZ,maxZ, maxZ)),fill = FALSE, col="Yellow", size=20)
              # print("Post Vox")
              
              LAS_Vox <- add_lasattribute(LAS_Vox, x=as.integer(0), name="Inside_TriShp", desc ="Inside_TriShp")
              
              Summary_oneVox <- as.data.frame(LAS_Vox@data %>%
                                                dplyr::group_by(TID) %>%
                                                dplyr::summarise(Zmin = min(Z),
                                                                 Zmax = max(Z), .groups = 'drop'))
            }
            
            
            #DF_oneMP_oneP_allT<- as.data.frame(DF_oneMP_oneP_allT) GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15
            #DF_oneMP_oneP_allT <- DF_oneMP_oneP_allT[which(DF_oneMP_oneP_allT$Plot_ID == Plot_ID),]
            #onePlot_Data <- LocBox_oneF_oneMP_oneP_Ext15X15[which(LocBox_oneF_oneMP_oneP_Ext15X15$TID   %in% DF_oneMP_oneP_allT$TID),]
            
            Point_DF <- data.frame(LAS_Vox@data[,1:3])
            Triangles_ID_All_Mx <- matrix(Triangles_ID_All, ncol=3, byrow= TRUE)
            
            unique_GT <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID
            if(length(unique_GT) > 0){
              # LOOP THROUGH EACH TID AND ASSIGN VOXELS 
              for(GT in 1:length(unique_GT)){
                XYZWLHR_oneGT <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[which(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID == unique_GT[GT]),]
                if(nrow(XYZWLHR_oneGT) > 0){
                  TID_ID <- unique_GT[GT]
                  Vertices_oneGT <- XYZWHR_TO_VERT_FUN(XYZWLHR_oneGT[,3:ncol(XYZWLHR_oneGT)], Base_WL = Para_Base_WL, Normalised = "No", Para_Cnt = Para_TriShpParaCnt)
                  Vertices_oneGT_Mx <- as.matrix(Vertices_oneGT)
                  Vertices_oneGT_Mx <- na.omit(Vertices_oneGT_Mx)
                  Vertices_oneGT_Mx <- VERTICIES_ORDER_FUN(Vertices_oneGT_Mx, Box_Levels = 3) # ADDED 11/05/2021...
                  
                  if(GT == 1){
                    Vertices_Triang_onePlot_allGT <- cbind(Vertices_oneGT_Mx, Flight =FID, Plot_ID = Plot_ID, TID = unique_GT[GT])
                  }else{
                    Vertices_Triang_onePlot_oneGT <- cbind(Vertices_oneGT_Mx, Flight =FID, Plot_ID = Plot_ID, TID = unique_GT[GT])
                    Vertices_Triang_onePlot_allGT <- rbind(Vertices_Triang_onePlot_allGT, Vertices_Triang_onePlot_oneGT)
                  }
                  
                  # CALCULATE INSIDE VECTORS
                  nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vertices_oneGT_Mx, Point_DF)
                  insideVect <- list(nbIntersect%%2 != 0)
                  # SAVE RESULTS IN LAS FILE
                  LAS_Vox@data$Inside_TriShp[insideVect[[1]]] <- unique_GT[GT]
                  
                } # IF TID HAS INFORMATION TO PROCESS
              } # LOOP THROUGH EACH GT
              write.csv(Vertices_Triang_onePlot_allGT, paste(FOLDER_CSV_TRISHP_MP_O,"/F",  FID,"_MP", Move_Position, "_P", Plot_ID,"_3D_Vertices_Triang_onePlot_allGT.csv", sep="")) 
              
              #####################################################################
              # SUMMARISING THE QUALITY OF THE TRISHAPES CAPTURING THE VOXELISED GT
              #####################################################################
              
              Summary_LAS_TID <- as.data.frame(LAS_Vox@data %>%
                                                 dplyr::group_by(TID) %>%
                                                 dplyr::summarise(TotalVox = length(Z), .groups = 'drop'))
              Summary_LAS_TID_TriShp <- as.data.frame(LAS_Vox@data %>%
                                                        dplyr::group_by(TID, Inside_TriShp) %>%
                                                        dplyr::summarise(Count = length(Z), .groups = 'drop'))
              
              Summary_LAS_TID_TriShp2 <- as.data.frame(Summary_LAS_TID_TriShp %>%
                                                         dplyr::group_by(TID) %>%
                                                         dplyr::summarise(Correct_inTriShp = sum(Count[Inside_TriShp  == TID]),
                                                                          Missed_outTriShp = sum(Count[Inside_TriShp  == 0]),
                                                                          Portion_TID_Corr = (Correct_inTriShp/(Missed_outTriShp+Correct_inTriShp)), .groups = 'drop'))
              
              Summary_LAS_TID_TriShp3 <- as.data.frame(Summary_LAS_TID_TriShp %>%
                                                         dplyr::group_by(Inside_TriShp ) %>%
                                                         dplyr::summarise(Extra_InTriShp = sum(Count[TID   == 0]), .groups = 'drop'))
              Summary_LAS_TID_TriShp_Final <- merge(Summary_LAS_TID_TriShp2, Summary_LAS_TID_TriShp3, 
                                                    by.x = "TID",
                                                    by.y = "Inside_TriShp", all.x = TRUE,
                                                    all == TRUE)
              Summary_LAS_TID_TriShp_Final <- merge(Summary_LAS_TID_TriShp_Final, Summary_LAS_TID, 
                                                    by.x = "TID",
                                                    by.y = "TID", all.x = TRUE,
                                                    all == TRUE)
              Summary_LAS_TID_TriShp_Final[is.na(Summary_LAS_TID_TriShp_Final)] <- 0
              Summary_LAS_TID_TriShp_Final <- Summary_LAS_TID_TriShp_Final[which(Summary_LAS_TID_TriShp_Final$TID > 1),]
              
              if(nrow(Summary_LAS_TID_TriShp_Final) > 0){
                Summary_LAS_TID_TriShp_Final$Portion_Extr_Within <- Summary_LAS_TID_TriShp_Final$Extra_InTriShp / (Summary_LAS_TID_TriShp_Final$Extra_InTriShp+Summary_LAS_TID_TriShp_Final$Correct_inTriShp)
                Summary_LAS_TID_TriShp_Final$FID <-  FID
                Summary_LAS_TID_TriShp_Final$Plot_ID <-  Plot_ID
                Summary_LAS_TID_TriShp_Final <- Summary_LAS_TID_TriShp_Final[, match( c("FID", "Plot_ID", 
                                                                                        "TID", "TotalVox",
                                                                                        "Correct_inTriShp", "Missed_outTriShp", "Extra_InTriShp",
                                                                                        "Portion_TID_Corr", "Portion_Extr_Within"), colnames(Summary_LAS_TID_TriShp_Final))]
              }
              write.csv(Summary_LAS_TID_TriShp_Final, paste(FOLDER_CSV_TRISHP_MP_O,"/F",  FID,"_MP", Move_Position, "_P", Plot_ID, "_SUMMARY_Vox_TriangShp_VOXELS_InOut_GT.csv",sep=''), row.names=FALSE)
              
              print("COMPLETED SUMMARY OF EACH GT'S WITHIN THE TRISHAPE FOR SUBJECT GT .......................................................")
              
              ### DOM DOM DOM !!! THINK ABOUT REMOVING ALL THE NON_TID (ZERO POINTS THAT ARE ABOVE THE UNDERSTOREY... AFTER THEY ARE ASSIGNED TO THE STEMS)
              
              ############################################################################################################################################### 8
              ############################################################################################################################################### 8
              ############################################################################################################################################### 8
              # PRIOR BOUNDING BOXES OF EACH PLOT (THIS IS AFTER THE PLOT Extent HAS BEEN REDUCED TO 15X15)
              ############################################################################################################################################### 8      
              ############################################################################################################################################### 8
              ############################################################################################################################################### 8
              
              # EMPTY IoU DF
              Output_IoU_allPrior_allGT_onePlot <- data.frame(Plot_ID = numeric(), 
                                                              Prior = as.numeric(),
                                                              TID = as.numeric(),
                                                              Count_IoU = as.numeric(),
                                                              Total_Vox = as.numeric() ,
                                                              Portion_IoU_CorrecIn = as.numeric(),
                                                              Portion_IoU_WrongIn = as.numeric(),
                                                              TP = as.numeric(),
                                                              FN = as.numeric(),
                                                              FP = as.numeric(),
                                                              Recall = as.numeric(),
                                                              Precision = as.numeric(),
                                                              FScore = as.numeric())
              
              source(paste(Comp_Name, "/CNN/R_Code/TORCH_ITCD_EXTRAP", R_Code_Version, "/TORCH_PRIOR_PROCEDURE_PARA", Para_TriShpParaCnt, R_Code_Version, ".R", sep=""))  
              
              # COMPUTE THE BBox HEIGHT FOR EACH GT IN THE OUTPUT
              Output_IoU_allPrior_allGT_onePlot$TID_Z_TopBox <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$Z_TopBox[match(Output_IoU_allPrior_allGT_onePlot$TID, 
                                                                                                                   GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID)]
              
              
              ########################################################################################################################################### 10
              ########################################################################################################################################### 10
              ###########################
              # OUTPUTING ALL THE RESULTS
              ###########################
              ########################################################################################################################################### 10
              ########################################################################################################################################### 10
              
              
              

              
              ############################################
              # OUTPUT PLOT PRIORS (AND MERGE FOR EACH MP)
              ############################################
              if(exists("Loc_oneF_oneMP_oneP_allPriors")){
                write.csv(Loc_oneF_oneMP_oneP_allPriors, paste(FOLDER_CSV_PRIOR_MP_O ,  "/F" , FID, "_P", Plot_ID, "_Prior_Loc.csv"  , sep=""),row.names=FALSE)
                if(exists("Prior_Loc_oneF_oneMP")){
                  Prior_Loc_oneF_oneMP <- rbind(Prior_Loc_oneF_oneMP, Loc_oneF_oneMP_oneP_allPriors)
                }else{
                  Prior_Loc_oneF_oneMP <- Loc_oneF_oneMP_oneP_allPriors
                }
              }
              
              if(exists("Prior_BBox_oneF_oneMP_oneP")){
                write.csv(Prior_BBox_oneF_oneMP_oneP, paste(FOLDER_CSV_PRIOR_MP_O , "/F" , FID, "_P", Plot_ID, "_Prior_BBox.csv" , sep=""),row.names=FALSE)
                # if(exists("Prior_BBox_oneF_oneMP")){
                #   Prior_BBox_oneF_oneMP <- rbind(Prior_BBox_oneF_oneMP, Prior_BBox_oneF_oneMP_oneP)
                # }else{
                #   Prior_BBox_oneF_oneMP <- Prior_BBox_oneF_oneMP_oneP
                #   }
              }
              if(exists("XYZWLHR_oneF_oneMP_oneP_Boxes")){
                write.csv(XYZWLHR_oneF_oneMP_oneP_Boxes, paste(FOLDER_CSV_PRIOR_MP_O , "/F" , FID, "_P", Plot_ID, "_UniquePriors_BBox_XYZWLHR.csv" , sep=""),row.names=FALSE)
              }
              
              # Comp_NameUTE ALL PRIOR_COMBINATIONS (FOR ALL PLOTS)
              if(exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){
                
                Comb_allPrior_onePlot <- data.frame(Plot_ID = Plot_ID,
                                                    Loc = nrow(unique(XYZWLHR_oneF_oneMP_oneP_allPrior[,c('X_Base','Y_Base','Z_Base')])),
                                                    Bot_BBox_Cnt =  length(which(XYZWLHR_oneF_oneMP_oneP_Boxes$Height_Strata == 2)), 
                                                    Top_BBox_Cnt = length(which(XYZWLHR_oneF_oneMP_oneP_Boxes$Height_Strata == 3)),
                                                    Combinations = nrow(XYZWLHR_oneF_oneMP_oneP_allPrior),
                                                    Removed_Dup_H2 = Removed_Dup_H2,
                                                    Removed_Dup_H3 = Removed_Dup_H3,
                                                    countMiss_H2 = countMiss_H2,
                                                    countMiss_H3 = countMiss_H3)
              }else{
                Comb_allPrior_onePlot <- data.frame(Plot_ID = Plot_ID,
                                                    Loc = 0,
                                                    Bot_BBox_Cnt =  0, 
                                                    Top_BBox_Cnt = 0,
                                                    Combinations = 0,
                                                    Removed_Dup_H2 = Removed_Dup_H2,
                                                    Removed_Dup_H3 = Removed_Dup_H3,
                                                    countMiss_H2 = countMiss_H2,
                                                    countMiss_H3 = countMiss_H3)
              }
              
              Comb_allPrior <- rbind(Comb_allPrior, Comb_allPrior_onePlot)
              # END OF PRIOR
              
              #############################################################################################################################################################################
              #############################################################################################################################################################################
              ############### 1b
              # NORMALISE VOX
              ############### 1b
              
              writeLAS(LAS_Vox, paste(FOLDER_LAS_VOX_MP_O,"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox.laz", sep="") )
              
              flag <- "gggg"
              if( (range(LAS_Vox$Y)[2]- range(LAS_Vox$Y)[1]) != (range(LAS_Vox$X)[2]- range(LAS_Vox$X)[1])) {browser()}
              flag <- "g"
              
              Shift_Vox_X <- min(LAS_Vox$X)
              Shift_Vox_Y <- min(LAS_Vox$Y)
              Shift_Vox_Z <- min(LAS_Vox$Z)
              
              LAS_Vox_N <-SCALE_LAS_FUN(LAS_Vox, Scale_Factor = 0.01/1, 
                                        Offset_Factor_X = as.integer(min(LAS_Vox$X)), 
                                        Offset_Factor_Y= as.integer(min(LAS_Vox$Y)), 
                                        Offset_Factor_Z= as.integer(min(LAS_Vox$Z)) )
              
              LAS_Vox_N$X <- LAS_Vox_N$X - Shift_Vox_X
              LAS_Vox_N$Y <- LAS_Vox_N$Y - Shift_Vox_Y
              LAS_Vox_N$Z <- LAS_Vox_N$Z - Shift_Vox_Z
              
              Shift_Vox_N_X <- max(LAS_Vox_N$X)
              Shift_Vox_N_Y <- max(LAS_Vox_N$Y)
              Shift_Vox_N_Z <- max(LAS_Vox_N$Z)
              
              LAS_Vox_N$X <- LAS_Vox_N$X/Shift_Vox_N_X
              LAS_Vox_N$Y <- LAS_Vox_N$Y/Shift_Vox_N_Y
              LAS_Vox_N$Z <- LAS_Vox_N$Z/Shift_Vox_N_Z
              
              # NORMALISE THE COUNT FIELD 
              #LAS_Vox_N@data$Count_Norm <- LAS_Vox_N@data$Count/max(LAS_Vox_N@data$Count)
              maxDen_N <- max(LAS_Vox_N@data$Count)
              LAS_Vox_N <- add_lasattribute(LAS_Vox_N, x=as.numeric(LAS_Vox_N@data$Count/maxDen_N), name="Count_Norm", desc ="Count_Norm")
              
              flag <- "hhhh"
              if( (range(LAS_Vox_N$Y)[2]- range(LAS_Vox_N$Y)[1]) != (range(LAS_Vox_N$X)[2]- range(LAS_Vox_N$X)[1])) {browser()}
              flag <- "h"
              
              writeLAS(LAS_Vox_N, paste(FOLDER_LAS_VOX_MP_O,"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox_N.laz", sep="")) 
              
              ### DOM DOM DOM !!! FOR SOME REASON WHEN YOU IMPORT THIS FILE INTO R THEN IT LOOSE THE Y DIMENSION (NOT SURE WHY!!!!) see below import... I.E. unique(LAS_VoxTest$Y) or plot(LAS_VoxTest)
              ##################### LAS_VoxTest <- readLAS(paste(FOLDER_LAS_VOX_MP_O,"/LAS_F",  FID, "_MP", Move_Position, "_P", Plot_ID,"_Vox_N.laz", sep=""), select = "xyzp0")
              
              # ###############################
              # # PLOT ALL PRIOR BOXES FOR PLOT
              # ###############################
              # LAS_oneP_Vox_N <- filter_poi(LAS_Vox_N, TID > 1)
              # plot(LAS_oneP_Vox_N, color="TID")
              # Unique_BBox <- unique(BBox_allCl$Box_ID)
              # for(rrr in 1:length(Unique_BBox)){
              #   BBox_oneCl <- BBox_allCl[which(BBox_allCl$Box_ID == Unique_BBox[rrr]),]
              #   PLOT3_Vert_oneBBox <- BBox_oneCl[,2:4]
              #   PLOT3_Vert_oneBBox$X <- (PLOT3_Vert_oneBBox$X - Shift_Vox_X)/Shift_Vox_N_X
              #   PLOT3_Vert_oneBBox$Y <- (PLOT3_Vert_oneBBox$Y - Shift_Vox_Y)/Shift_Vox_N_Y
              #   PLOT3_Vert_oneBBox$Z <- (PLOT3_Vert_oneBBox$Z - Shift_Vox_Z)/Shift_Vox_N_Z
              #   Z_CB_N <- (Z_CB- Shift_Vox_Z)/Shift_Vox_N_Z
              #   if(PLOT3_Vert_oneBBox$Z[1] < Z_CB_N){Color = "green"}else{Color = "white"}
              #   polygon3d(PLOT3_Vert_oneBBox, fill=FALSE, col=Color, lwd=1)
              # }
              # # browser()
              
              #######################################
              # OUTPUT NORMALISED LAS VOXEL DATAFRAME
              #######################################
              DF_Vox_N <- as.data.frame(LAS_Vox_N@data)
              #DF_Vox_N <- DF_Vox_N[,match(c("X", "Y", "Z", "TID", "Count_Norm"),colnames(DF_Vox_N))]
              DF_Vox_N <- DF_Vox_N[order( DF_Vox_N[,3], DF_Vox_N[,2], DF_Vox_N[,1]),]
              write.csv(DF_Vox_N, paste(FOLDER_CSV_VOX_MP_O,"/F",  FID,"_P",   Plot_ID, "_LAS_Vox_N.csv",sep=''), row.names=FALSE) 
              
              #####################################
              # OUTPUTING SHIFT AND NORMALISED DATA
              #####################################
              
              # UAS DATA
              Shifts_LAS_XYZWLHR <- data.frame(FID,  Plot_ID,
                                               Shift_Vox_X, Shift_Vox_Y, Shift_Vox_Z, 
                                               Shift_Vox_N_X, Shift_Vox_N_Y, Shift_Vox_N_Z, Shift_Rad = 90, 
                                               X_origRng = origRng_oneP_x[2]-origRng_oneP_x[1], X_orig_Bot = origRng_oneP_x[1], X_orig_Top = origRng_oneP_x[2],
                                               Y_origRng = origRng_oneP_y[2]-origRng_oneP_y[1], Y_orig_Bot = origRng_oneP_y[1], Y_orig_Top = origRng_oneP_y[2], 
                                               Y_origRng = origRng_oneP_z[2]-origRng_oneP_z[1], Z_orig_Bot = origRng_oneP_z[1], Z_orig_Top = origRng_oneP_z[2],
                                               maxDen_N) #,
              
              if(!exists("Shifts_LAS_XYZWLHR_AllP")){
                Shifts_LAS_XYZWLHR_AllP <- Shifts_LAS_XYZWLHR
              }else{
                Shifts_LAS_XYZWLHR_AllP <- rbind(Shifts_LAS_XYZWLHR_AllP, Shifts_LAS_XYZWLHR)
              }
              # ALS DATA
              Shifts_LAS_XYZWLHR_ALS <- data.frame(FID,  Plot_ID,
                                                   Shift_Vox_X_ALS, Shift_Vox_Y_ALS, Shift_Vox_Z_ALS, 
                                                   Shift_Vox_N_X_ALS, Shift_Vox_N_Y_ALS, Shift_Vox_N_Z_ALS, Shift_Rad = 90, 
                                                   X_origRng_ALS = X_origRng_ALS[2]-X_origRng_ALS[1], X_orig_Bot_ALS = X_origRng_ALS[1], X_orig_Top_ALS = X_origRng_ALS[2],
                                                   Y_origRng_ALS = Y_origRng_ALS[2]-Y_origRng_ALS[1], Y_orig_Bot_ALS = Y_origRng_ALS[1], Y_orig_Top_ALS = Y_origRng_ALS[2],
                                                   Z_origRng_ALS = Z_origRng_ALS[2]-Z_origRng_ALS[1], Z_orig_Bot_ALS = Z_origRng_ALS[1], Z_orig_Top_ALS = Z_origRng_ALS[2],
                                                   maxDen_N_ALS) #,
              
              if(!exists("Shifts_LAS_XYZWLHR_AllP_ALS")){
                Shifts_LAS_XYZWLHR_AllP_ALS <- Shifts_LAS_XYZWLHR_ALS
              }else{
                Shifts_LAS_XYZWLHR_AllP_ALS <- rbind(Shifts_LAS_XYZWLHR_AllP_ALS, Shifts_LAS_XYZWLHR_ALS)
              }
              
              ### DOM DOM DOM !!! SEE IF YOU NEED TO SaVE ANY OTHER CHANGES IN THIS FILE
              
              LAS_Vox_Trees_Coord <- filter_poi(LAS_Vox, TID > 1)
              # # XYZWLHR_oneF_oneMP_oneP_allPrior[15,]
              # PLOT_LAS_XYZWLHR_FUN(LAS_Vox_Trees_Coord, XYZWLHR_oneF_oneMP_oneP_allPrior[1:15,], Para_Base_WL, Normalised = "No", Para_Cnt = Para_TriShpParaCnt) 
              # Shift_X <- min(LAS_oneP_oneType_allT@data$X)
              # Shift_Y <- min(LAS_oneP_oneType_allT@data$Y)
              # maxZ <- max(LAS_oneP_oneType_allT@data$Z)
              # XY_Poly_oneG <- as.data.frame(st_coordinates(Poly_oneG)[,1:2])
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(0,0,0,0, 0)),fill = FALSE, col="Yellow", size=20)
              # polygon3d(as.vector(XY_Poly_oneG$X)-Shift_X,
              #           as.vector(XY_Poly_oneG$Y)-Shift_Y,
              #           as.vector(c(maxZ,maxZ,maxZ,maxZ, maxZ)),fill = FALSE, col="Yellow", size=20)
              
              
              
              #####################################
              # NORMALISE IoU PRIOR TID Z_Top_VALUE
              #####################################
              
              Output_IoU_allPrior_allGT_onePlot$PRIOR_Z_TopBox <- (Output_IoU_allPrior_allGT_onePlot$PRIOR_Z_TopBox - Shift_Vox_Z)/Shift_Vox_N_Z
              Output_IoU_allPrior_allGT_onePlot$TID_Z_TopBox <-  (Output_IoU_allPrior_allGT_onePlot$TID_Z_TopBox - Shift_Vox_Z)/Shift_Vox_N_Z
              
              if(nrow(Output_IoU_allPrior_allGT_onePlot) > 0){
                Output_IoU_allPrior_allGT_onePlot <- data.frame(Plot_ID = Plot_ID, Output_IoU_allPrior_allGT_onePlot)
              }else{
                Output_IoU_allPrior_allGT_onePlot[1,] <- c(Plot_ID, rep(NA, (ncol(Output_IoU_allPrior_allGT_onePlot)-1)))
              }
              
              write.csv(Output_IoU_allPrior_allGT_onePlot, paste(FOLDER_CSV_IoU_MP_O,"/F",  FID,"_P",   Plot_ID, "_All_IoU_Prior_GroundTruth_PRE_REMOVAL_Zero.csv",sep=''), row.names=FALSE)
              
              # REMOVING RESIDUAL AND GROUND ESTIMATES (i.e. NOT IN TriShp) AS THEY ARE IRRELVANT AFTER IoU and FScore CALCULATION
              Output_IoU_allPrior_allGT_onePlot <- Output_IoU_allPrior_allGT_onePlot[which(Output_IoU_allPrior_allGT_onePlot$TID > 1),]
              
              # SAVE OUTPUT
              write.csv(Output_IoU_allPrior_allGT_onePlot, paste(FOLDER_CSV_IoU_MP_O,"/F",  FID,"_P",   Plot_ID, "_All_IoU_Prior_GroundTruth.csv",sep=''), row.names=FALSE)
              
              if(exists("Ext_oneP_plotPrior")){
                write.csv(Ext_oneP_plotPrior, paste(FOLDER_CSV_IoU_MP_O,"/F",  FID,"_P",   Plot_ID, "_All_Prior_XYZ_Extent.csv",sep=''), row.names=FALSE)
              }
             
              
              ############################## 1b
              # NORMALISE XYZWLHR_plotPriors
              ############################## 1b
              if(exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){
                XYZWLHR_plotPriors_N <- XYZWLHR_To_XYZWLHR_N_FUN(XYZWLHR = XYZWLHR_oneF_oneMP_oneP_allPrior, Shift = Shifts_LAS_XYZWLHR, Para_Cnt = Para_TriShpParaCnt)
                colnames(XYZWLHR_plotPriors_N)[1] <- "PriorID"
                
                write.csv(XYZWLHR_plotPriors_N, paste(FOLDER_CSV_PRIOR_MP_O , "/F" , FID, "_P", Plot_ID, "_Ext15X15_AllPRIORS_N.csv" , sep=""),row.names=FALSE)
              }

              
              LAS_Vox_Trees_N <- filter_poi(LAS_Vox_N, TID > 0)
              # PLOT_LAS_XYZWLHR_FUN(LAS_Vox_Trees_N, XYZWLHR_plotPriors_N[1:15,], Para_Base_WL/16, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15
              # Shift_X <- min(LAS_Vox_Trees_N@data$X)
              # Shift_Y <- min(LAS_Vox_Trees_N@data$Y)
              # maxZ <- max(LAS_Vox_Trees_N@data$Z)
              # XY_Poly_oneG <- as.data.frame(st_coordinates(Poly_oneG)[,1:2])
              # polygon3d(((as.vector(XY_Poly_oneG$X)-Shifts_LAS_XYZWLHR$Shift_Vox_X)/Shifts_LAS_XYZWLHR$Shift_Vox_N_X),
              #           ((as.vector(XY_Poly_oneG$Y)-Shifts_LAS_XYZWLHR$Shift_Vox_Y)/Shifts_LAS_XYZWLHR$Shift_Vox_N_Y),
              #           (as.vector(c(0,0,0,0, 0))),fill = FALSE, col="Yellow", size=20)
              # polygon3d(((as.vector(XY_Poly_oneG$X)-Shifts_LAS_XYZWLHR$Shift_Vox_X)/Shifts_LAS_XYZWLHR$Shift_Vox_N_X),
              #           ((as.vector(XY_Poly_oneG$Y)-Shifts_LAS_XYZWLHR$Shift_Vox_Y)/Shifts_LAS_XYZWLHR$Shift_Vox_N_Y),
              #           as.vector(c(1,1,1,1, 1)),fill = FALSE, col="Yellow", size=20)
              print("Post Vox")
              
              
              # # LOGICAL CONTRAINT TO REMOVE IMPOSSIBLE PRIORS (i.e. REDUCE POTENTIAL PRIORS SO BOTTOM IS BELOW MID WHICH IS BELOW TOP WHICH IS BELOW MAX HEIGHT)
              # #                                               (### DOM DOM DOM !!! YOU CAN IMPOSE HORIZONTAL DISTANCE CONSTRAINT BASED ON WHAT GT VARIATION LOOKS LIKE)
              # # CHECK THERE ARE NO WRONG PRIORS
              # 
              # Test <- which(XYZWLHR_plotPriors_N$Z_Base < XYZWLHR_plotPriors_N$Z_BotBox &
              #                 XYZWLHR_plotPriors_N$Z_BotBox < XYZWLHR_plotPriors_N$Z_TopBox &
              #                 XYZWLHR_plotPriors_N$Z_TopBox < XYZWLHR_plotPriors_N$Z_TopTree)
              # if(dim(XYZWLHR_plotPriors_N)[1] != length(Test)){
              #    # browser()
              #    # REMOVE POOR PRIORS (SHOULD HAVE ANY THOUGH...)
              #     XYZWLHR_plotPriors_N <- XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$Z_Base < XYZWLHR_plotPriors_N$Z_BotBox &
              #                                                    XYZWLHR_plotPriors_N$Z_BotBox < XYZWLHR_plotPriors_N$Z_TopBox &
              #                                                    XYZWLHR_plotPriors_N$Z_TopBox < XYZWLHR_plotPriors_N$Z_TopTree),]}
              
              
              ############################
              # NORMALISE EXTENT OF PRIORS
              ############################
              if(exists("Ext_oneP_plotPrior")){
                Ext_oneP_plotPrior_N <- Ext_oneP_plotPrior
                Ext_oneP_plotPrior_N$X <- (Ext_oneP_plotPrior_N$X- Shift_Vox_X)/Shift_Vox_N_X              
                Ext_oneP_plotPrior_N$Y<- (Ext_oneP_plotPrior_N$Y- Shift_Vox_Y)/Shift_Vox_N_Y        
                Ext_oneP_plotPrior_N$Z <- (Ext_oneP_plotPrior_N$Z- Shift_Vox_Z)/Shift_Vox_N_Z  
                write.csv(Ext_oneP_plotPrior_N, paste(FOLDER_CSV_IoU_MP_O,"/F",  FID,"_P",   Plot_ID, "_All_Prior_XYZ_Extent_N.csv",sep=''), row.names=FALSE)
              }

              
              ########################## 1b
              # NORMALISE XYZWLHR_plotGT
              ########################## 1b
              
              XYZWLHR_allGT_N <- XYZWLHR_To_XYZWLHR_N_FUN(XYZWLHR = GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15, Shift = Shifts_LAS_XYZWLHR, Para_Cnt = Para_TriShpParaCnt)
              write.csv(XYZWLHR_allGT_N, paste(FOLDER_CSV_P15X15_MP_O , "/F" , FID, "_P", Plot_ID, "_Ext15X15_allT_LocBox_XYZWLHR_N.csv" , sep=""),row.names=FALSE)
              
              
              # SANITY CHECK
              Index_ZeroA <- which(XYZWLHR_allGT_N$W_TopBox == 0)
              if(length(Index_ZeroA) > 0) {
                XYZWLHR_allGT_N$W_TopBox[Index_ZeroA] <- 1/Para_plot_Res
              }
              
              Index_ZeroB <- which(XYZWLHR_allGT_N$L_TopBox == 0)
              if(length(Index_ZeroB) > 0) {
                XYZWLHR_allGT_N$L_TopBox[Index_ZeroB] <- 1/Para_plot_Res
              }
              
              # if(length(Index_ZeroA) > 0 | length(Index_ZeroB) > 0) {browser()}
              # browser()
              
              
              # PLOT_LAS_XYZWLHR_FUN(LAS_Vox_Trees_N, XYZWLHR_allGT_N, Para_Base_WL/15, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt)
              # XY_Poly_oneG <- as.data.frame(st_coordinates(Poly_oneG)[,1:2])
              # polygon3d(((as.vector(XY_Poly_oneG$X)-Shifts_LAS_XYZWLHR$Shift_Vox_X)/Shifts_LAS_XYZWLHR$Shift_Vox_N_X),
              #           ((as.vector(XY_Poly_oneG$Y)-Shifts_LAS_XYZWLHR$Shift_Vox_Y)/Shifts_LAS_XYZWLHR$Shift_Vox_N_Y),
              #           (as.vector(c(0,0,0,0, 0))),fill = FALSE, col="Yellow", size=20)
              # polygon3d(((as.vector(XY_Poly_oneG$X)-Shifts_LAS_XYZWLHR$Shift_Vox_X)/Shifts_LAS_XYZWLHR$Shift_Vox_N_X),
              #           ((as.vector(XY_Poly_oneG$Y)-Shifts_LAS_XYZWLHR$Shift_Vox_Y)/Shifts_LAS_XYZWLHR$Shift_Vox_N_Y),
              #           as.vector(c(1,1,1,1, 1)),fill = FALSE, col="Yellow", size=20)
              # print("FINAL NORMALISED")
              
            } # IF THERE ARE UNIQUE GT
            
            
            end.time <- Sys.time()
            time.taken <- end.time - start.time
            print(paste("PP:",PP, "  Time Taken:   ",  time.taken))  
            
            # OUTPUT THE IoU OF ALL PRIORS AND ALL GT (ONE PLOT)
            write.csv(Output_IoU_allPrior_allGT_onePlot, paste(FOLDER_CSV_IoU_MP_O,"/F",  FID, "_MP", Move_Position, "_P", Plot_ID,  "_IoU_allPrior_allGT_onePlot.CSV",sep=''), row.names=FALSE)
            print(paste(" ...................................................   PP:",PP, "Out of", length(Files_P_S_CSV), sep=""))
            
            #browser()
            
            ###################
            # VISUALISE RESULTS
            ###################
            
          } # IF THERE IS UNDERSTOREY_ZERO DATA IN PLOT
          
          
          ####################
          # MERGING FOR FLIGHT (CSV)
          ####################
          GT_XYZWLHR_oneF_allMP_allP15X15 <- rbind(GT_XYZWLHR_oneF_allMP_allP15X15, GT_XYZWLHR_oneF_oneMP_allP15X15)
          
          print(paste("SHOULD BE 16:", length(table(LAS_Vox_N$Y))))
          print(paste("SHOULD BE 16:", length(table(LAS_Vox_N$X))))
          # browser()
        } # IF THERE ARE TREES IN THE PLOT
        
        #### DOM DOM DOM !!! IF THERE ARE NO TREES IN PLOT YOU HAVE TO DO SOMETHING FOR THE CNN TO IDENTIFY THIS... WORK ON LATER
        
        
        } # LOOP PP (PLOT CSV FILES) # (PP+1)
      
      dev.off() # TID LEVEL DENSITY PLOTS
      ################      
      ################
      # MP LOOP OUTPUT (CSV)
      ################
      ################     
      
      write.csv(Shifts_LAS_XYZWLHR_AllP, paste(FOLDER_CSV_O, "/F",  FID, "_Shifts_LAS_XYZWLHR.csv",sep=''), row.names=FALSE)
      write.csv(Shifts_LAS_XYZWLHR_AllP_ALS, paste(FOLDER_CSV_O, "/F",  FID, "_Shifts_LAS_XYZWLHR_ALS.csv",sep=''), row.names=FALSE)
      
      write.csv(Comb_allPrior, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_CountComb_allPrior_Remove_Dup_ManyWS.CSV",sep=''), row.names=FALSE)
      
      Count_Priors_Test_DF <- data.frame(Plot=Files_Priors, Count_Priors)
      write.csv(Count_Priors_Test_DF, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_Count_Priors_in_Plot.CSV",sep=''), row.names=FALSE)
      
      
      write.csv(DF_oneMP_Miss_allP_S_TID, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_Missed_allP_S_TID.CSV",sep=''), row.names=FALSE)
      write.csv(DF_oneMP_allP_S_TID, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_allP_S_TID.CSV",sep=''), row.names=FALSE)

      #write.csv(LocBox_oneF_oneMP_allP15X15, paste(FOLDER_CSV_MP_O, "/F",  FID,"_MP", Move_Position, "_allP_Ext15X15_allT_LocBox.csv", sep=""), row.names=FALSE)   # LocBox_oneF_oneMP_oneP_Ext15X15 <- read.csv(paste(FOLDER_CSV_O, "/F",  FID, "AllTID_100Sample_Loc_BoundBox.csv", sep="") )
      write.csv(GT_XYZWLHR_oneF_oneMP_allP15X15, paste(FOLDER_CSV_MP_O, "/F",  FID,"_MP", Move_Position, "_allP_Ext15X15_allGT_XYZWLHR.csv", sep=""), row.names=FALSE)   # LocBox_oneF_oneMP_oneP_Ext15X15 <- read.csv(paste(FOLDER_CSV_O, "/F",  FID, "AllTID_100Sample_Loc_BoundBox.csv", sep="") )
      
      # ######################################
      # # MERGING ALL MP INTO ONE F FOR PRIORS
      # ######################################
      # if(!exists("Prior_Loc_oneF")){
      #   Prior_Loc_oneF <- Prior_Loc_oneF_oneMP
      # }else{
      #   Prior_Loc_oneF <- rbind(Prior_Loc_oneF,  Prior_Loc_oneF_oneMP)
      # }
      # if(!exists("Prior_BBox_oneF")){
      #   Prior_BBox_oneF <- Prior_BBox_oneF_oneMP
      # }else{
      #   Prior_BBox_oneF <- rbind(Prior_BBox_oneF,  Prior_BBox_oneF_oneMP) 
      # }
      # if(!exists("Prior_XYZWLHR_oneF")){
      #   Prior_XYZWLHR_oneF <- Prior_XYZWLHR_oneF_oneMP
      # }else{
      #   Prior_XYZWLHR_oneF <- rbind(Prior_XYZWLHR_oneF,  Prior_XYZWLHR_oneF_oneMP) 
      # }
      # 
      # write.csv(Prior_Loc_oneF_oneMP, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_allP_allPrior_Loc.CSV",sep=''), row.names=FALSE)
      # write.csv(Prior_BBox_oneF_oneMP, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_allP_allPrior_BBox.CSV",sep=''), row.names=FALSE)
      # write.csv(Prior_XYZWLHR_oneF_oneMP, paste(FOLDER_CSV_MP_O,"/F",  FID, "_MP", Move_Position, "_allP_allPrior_BBox_XYZWLHR.CSV",sep=''), row.names=FALSE)
      # 
      
      ################      
      ################
      # MP LOOP OUTPUT (SHP)
      ################
      ################ 
      
      # OUTPUT FOR ONE MP (i.e. ONE MOVING PLOTS (GRIDS) ACROSS PLOT)
      
      # OUTPUT THE Plots THAT WILL BE USED TO SAMPLE FOR EACH PLOT.
      Poly_oneMP_intersect <- st_intersection(Poly_oneMP_allP_Bound, Poly_oneF_Bound) 
      Poly_oneMP_intersect <- Poly_oneMP_intersect %>% st_set_crs(Proj_Sys)
      

      Area_oneMP <- st_area(Poly_oneMP_intersect)
      Index_Area80Percent_oneMP <- which(Area_oneMP > as_units(Para_Min_Portion_Grid*Para_plot_Res*Para_plot_Res, "m^2"))
      Poly_oneMP_intersect_Area80Percent <- Poly_oneMP_intersect[Index_Area80Percent_oneMP,]
      
      # OUTPUT SHAPEFILES FOR ONE MP
      st_write(Poly_oneMP_intersect, 
               dsn = paste(FOLDER_SHP_MP_O, sep=""), 
               layer = paste("F",FID, "_allP_Intersect",sep=""),
               driver = "ESRI Shapefile",
               append=FALSE, overwrite = TRUE)
      Poly_oneMP_intersect_Area80Percent <- Poly_oneMP_intersect_Area80Percent %>% st_set_crs(Proj_Sys)
      st_write(Poly_oneMP_intersect_Area80Percent, 
               dsn = paste(FOLDER_SHP_MP_O, sep=""), 
               layer = paste("F",FID, "_allP_80Percent_Intersect",sep=""),
               driver = "ESRI Shapefile",
               append=FALSE, overwrite = TRUE)
      Poly_oneMP_allP <- Poly_oneMP_allP %>% st_set_crs(Proj_Sys)
      st_write(Poly_oneMP_allP, 
               dsn = paste(FOLDER_SHP_MP_O, sep=""),
               layer = paste("F",FID, "_allP_allG",sep=""),
               driver = "ESRI Shapefile",
               append=FALSE, overwrite = TRUE)

      } # LOOP OO
    } # O LOOP
    
    ###################
    # OUTPUT FOR FLIGHT (SHP)
    ###################  
    Poly_allMP_intersect <- st_intersection(Poly_allMP_fishnet, Poly_oneF_Bound)  

    Poly_allMP_intersect <- Poly_allMP_intersect %>% st_set_crs(Proj_Sys)
    Area_allMP <- st_area(Poly_allMP_intersect)
    Index_allMP_Area80Percent <- which(Area_allMP > as_units(Para_Min_Portion_Grid*Para_plot_Res*Para_plot_Res, "m^2"))
    Poly_allMP_intersect_Area80Percent <- Poly_allMP_intersect[Index_allMP_Area80Percent,]
    

    st_write(Poly_allMP_intersect, 
             dsn = paste(FOLDER_SHP_O, sep=""), 
             layer = paste("F",FID, "_allMP_allP_Intersect",sep=""),
             driver = "ESRI Shapefile",
             append=FALSE, overwrite = TRUE)
    Poly_allMP_intersect_Area80Percent <- Poly_allMP_intersect_Area80Percent %>% st_set_crs(Proj_Sys)
    st_write(Poly_allMP_intersect_Area80Percent, 
             dsn = paste(FOLDER_SHP_O, sep=""), 
             layer = paste("F",FID, "_allMP_allP_80Percent_Intersect",sep=""),
             driver = "ESRI Shapefile",
             append=FALSE, overwrite = TRUE)
    Poly_allMP_allP <- Poly_allMP_allP %>% st_set_crs(Proj_Sys)
    st_write(Poly_allMP_allP, 
             dsn = paste(FOLDER_SHP_O, sep=""), 
             layer = paste("F",FID, "_allMP_allP_allG",sep=""), 
             driver = "ESRI Shapefile",
             append=FALSE, overwrite = TRUE)
    ############################################################################################
    # CLIP ALL MP ALL GRIDS WITH THE BUFFER (Reduced 10 m) TO GET INNER PLOTS WITH GOOD COVERAGE
    ############################################################################################
    
    Poly_allMP_G_ReducedBuff <-st_intersection(Poly_allMP_allP, Poly_Bound_Reduced10mBuff)
    Poly_allMP_G_ReducedBuff <- Poly_allMP_G_ReducedBuff %>% st_set_crs(Proj_Sys)
    Poly_allMP_G_ReducedBuff$Area <- st_area(Poly_allMP_G_ReducedBuff)  
    
    st_write(Poly_allMP_G_ReducedBuff, 
             dsn = paste(FOLDER_SHP_O, sep=""), 
             layer = paste("F",FID, "_allMP_P_Reduced10mBuff",sep=""),
             driver = "ESRI Shapefile",
             append=FALSE, overwrite = TRUE)
    # USE THESE PLOTS IN THE CNN
    Poly_allMP_G_ReducedBuff_80Pc <- Poly_allMP_G_ReducedBuff[which(Poly_allMP_G_ReducedBuff$Area > as_units(((Para_plot_Res*Para_plot_Res)*0.8), "m^2")),]
    st_write(Poly_allMP_G_ReducedBuff_80Pc, 
             dsn = paste(FOLDER_SHP_O, sep=""), 
             layer = paste("F",FID, "_allMP_P_Reduced10mBuff_80PcArea",sep=""),
             driver = "ESRI Shapefile",
             append=FALSE, overwrite = TRUE)
    Plots_Area_InBuff <- data.frame(Poly_allMP_G_ReducedBuff_80Pc) ### DOM DOM DOM !!! SF ...MAKE SURE THIS PRODUCES CORRECT RESULTS
    Plots_Area_InBuff <- Plots_Area_InBuff[,c(1,3)]
    write.csv(Plots_Area_InBuff, paste(FOLDER_CSV_O, "/PLOTS_FINAL_Reduced10mBuff_80PcArea.csv" , sep=""), row.names=FALSE)
    
    # SAVE LAS_UNDERZero WITH UPDATED FIELDS FOR MovPos 
    Under_Samples <- list.files(FOLDER_LAS_UNDER_O, pattern ="LAS_UnderZero_F")
    for(LS in 1:length(List_LAS_UnderZero)){
      LAS_LL <- List_LAS_UnderZero[[LS]]
      #LAS_LL@data$PointSourceID <- as.integer(LAS_LL@data$TID)
      writeLAS(LAS_LL, paste(FOLDER_LAS_UNDER_O, "/", Under_Samples[LS], sep=""))
    }
    
    #########################
    # OUTPUT FOR FLIGHT (CSV)
    #########################
    write.csv(Plot_Stocking, paste(FOLDER_CSV_O,"/F",FID,"_Plot_Stocking.csv" , sep=""),
              row.names=FALSE)
    # write.csv(Prior_Loc_oneF, paste(FOLDER_CSV_O,"/F",FID,"_Prior_Loc_XYZWLHR.csv" , sep=""),
    #           row.names=FALSE) #@
    # write.csv(Prior_BBox_oneF, paste(FOLDER_CSV_O,"/F",FID,"_Prior_BBox.csv", sep=""),
    #           row.names=FALSE) #@
    # write.csv(Prior_XYZWLHR_oneF, paste(FOLDER_CSV_O,"/F",FID,"_Prior_BBox_XYZWLHR.csv", sep=""),
    #           row.names=FALSE) #@
    write.csv(Range_allMP_allP, paste(FOLDER_CSV_O,"/F",FID,"_Range_allMP_allP.csv", sep=""),
              row.names=FALSE) #@
    
    write.csv(GT_XYZWLHR_oneF_allMP_allP15X15, paste(FOLDER_CSV_O,"/F",FID,"_GT_XYZWLHR_oneF_allMP_allP15X15.csv", sep=""),
              row.names=FALSE) #@
    
  } # f Loop for Flights 
  
  
  # DOM DOM DOM !!!  (NOTE I TRIED TO MAKE SAME PROFILE AS ALS BUT DIDN'T WORK)
  # Den_F <- density(LAS_UAS_oneT$Z)
  # bw_F <- Den_F$bw
  # Den_ALS <- density(LAS_ALS_oneT$Z)
  # bw_Den_ALS <- Den_ALS$bw 
  # plot(Den_F, col="blue", lwd=3)
  # lines(Den_ALS, col="red", lwd=3)
  # lines(Den_ALS_Synth, col="green", lwd=3)
  # lines(Den_Synth_ExtractUAS, col="black", lwd=3, lty=2)
  # 
  # legend("topleft", legend = c("UAS Density", "ALS Density", "ALS_Synthetic", "ALS_Synth_ExtractUAS"), col=c("blue", "red", "green", "black"),
  #        lty = c(1,1,1,2),
  #        lwd= 3,
  #        bty = "n",# turn of border
  #        cex = 1)
  
  


###############
# PLOTTING CODE
###############

# # LAS_PLOT<- LAS_oneP_allT
# # Shift_X <- min(LAS_PLOT$X)
# # Shift_Y <- min(LAS_PLOT$Y)
# # LAS_PLOT$X  <-  LAS_PLOT$X- Shift_X
# # LAS_PLOT$Y  <-  LAS_PLOT$Y- Shift_Y
# # 
# # n <- length(unique(LAS_PLOT@data$TID))
# # palette <- sample(distinctColorPalette(n))
# # 
# # # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
# # LAS_PLOT@data$Color <- as.character("")
# # Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TID),
# #                        ID_TID = palette)
# # index_Color_TID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_TID)
# # index_Color_TID_2 <- match(LAS_PLOT@data$TID[index_Color_TID_1],
# #                             Color_ID$Unique_TID)
# # LAS_PLOT@data$Color[index_Color_TID_1] <- as.character(Color_ID$ID_TID[index_Color_TID_2])
# 
# # plot(LAS_PLOT, color="Color")
# #text3d(min(LAS_PLOT$X),min(LAS_PLOT$Y),max(LAS_PLOT$Z), paste("P",Plot_ID, sep=""), col="white")
# 
  
    ########################
  # PLOTTING_TRAINGULATION 
  ########################
  # Vertices_Mx_Plot <- Vertices_oneGT_Mx
  # Vertices_Mx_Plot[,1] <- Vertices_Mx_Plot[,1] -Shift_X
  # Vertices_Mx_Plot[,2] <- Vertices_Mx_Plot[,2] -Shift_Y 
  #triangles3d(Vertices_Mx_Plot[Triangles_ID_All,], col=palette) 
  # browser()




# ## TEST PLOTTING TO CHECK AFTER THE FILL IN
# LAS_PLOT <- LAS_oneP_allT
# n <- length(unique(LAS_PLOT@data$TID))
# palette <- sample(distinctColorPalette(n))
# # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
# LAS_PLOT@data$Color <- as.character("")
# Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TID),
#                        ID_TID = palette)
# index_Color_TID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_TID)
# index_Color_TID_2 <- match(LAS_PLOT@data$TID[index_Color_TID_1],
#                            Color_ID$Unique_TID)
# LAS_PLOT@data$Color[index_Color_TID_1] <- as.character(Color_ID$ID_TID[index_Color_TID_2])
# plot(LAS_PLOT, color="Color", size=2)
# print(table(LAS_PLOT@data$TID))
# Unique_T <- GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID
# for(RT in 1:length(Unique_T)){ ### DOM DOM DOM YOU CHANGED THE INDEX TO 2 (PUT BACK TO 1)
#   Index_T <- which(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15$TID ==Unique_T[RT])
#   Vert_oneT <- XYZWHR_TO_VERT_FUN(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15[Index_T,3:ncol(GT_XYZWLHR_oneF_oneMP_oneP_Ext15X15)], Normalised = "Yes")
# 
#   Vert_oneT_DF <- as.data.frame(Vert_oneT)
#   Shift_X <- min(LAS_PLOT$X)
#   Shift_Y <- min(LAS_PLOT$Y) 
#   points3d(as.vector(Vert_oneT_DF$X[1:4])-Shift_X,
#            as.vector(Vert_oneT_DF$Y[1:4]) -Shift_Y,
#            as.vector(Vert_oneT_DF$Z[1:4]),
#            col="Yellow", size=10)
#   polygon3d(as.vector(Vert_oneT_DF$X[5:8])-Shift_X,
#             as.vector(Vert_oneT_DF$Y[5:8])-Shift_Y,
#             as.vector(Vert_oneT_DF$Z[5:8]),
#             fill = FALSE, col="orange", lwd=2)
#   polygon3d(as.vector(Vert_oneT_DF$X[9:12])-Shift_X,
#             as.vector(Vert_oneT_DF$Y[9:12])-Shift_Y,
#             as.vector(Vert_oneT_DF$Z[9:12]),
#             fill = FALSE, col="red", lwd=2)
#   polygon3d(as.vector(Vert_oneT_DF$X[13:16])-Shift_X,
#             as.vector(Vert_oneT_DF$Y[13:16])-Shift_Y,
#             as.vector(Vert_oneT_DF$Z[13:16]),
#             fill = FALSE, col="blue", lwd=2)
# }

  
