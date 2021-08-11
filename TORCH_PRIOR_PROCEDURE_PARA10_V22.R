
# start_time <- Sys.time()

Count_TriShps_allP <- data.frame(PlotID = numeric(),
                            Prior_Cnt = numeric(),
                            Loc_Cnt = numeric(),
                            Box_Cnt = numeric(),
                            UnderTop = numeric(),
                            CBase = numeric())

#for(PP in 1:length(File_List)){
if(exists("BBox_allCl")){rm(BBox_allCl)} #@
if(exists("Poly_allBBox")){rm(Poly_allBBox)} #@

# # REMOVE OBJECTS AT THE START OF PP LOOP
# if(exists("Ext_oneP_plotPrior")){rm(Ext_oneP_plotPrior)}  
# if(exists("Loc_oneF_oneMP_oneP_allPriors")){rm(Loc_oneF_oneMP_oneP_allPriors)} #@  
# if(exists("Prior_BBox_oneF_oneMP_oneP")){rm(Prior_BBox_oneF_oneMP_oneP)}  #@
# if(exists("XYZWLHR_oneF_oneMP_oneP_Boxes")){rm(XYZWLHR_oneF_oneMP_oneP_Boxes)} #@
# if(exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){rm(XYZWLHR_oneF_oneMP_oneP_allPrior)} #@

# # EMPTY IoU DF
# Output_IoU_allPrior_allGT_onePlot <- data.frame(Plot_ID = numeric(), 
#                                                 Prior = as.numeric(),
#                                                 TID = as.numeric(),
#                                                 Count_IoU = as.numeric(),
#                                                 Total_Vox = as.numeric() ,
#                                                 Portion_IoU_CorrecIn = as.numeric(),
#                                                 Portion_IoU_WrongIn = as.numeric(),
#                                                 TP = as.numeric(),
#                                                 FN = as.numeric(),
#                                                 FP = as.numeric(),
#                                                 Recall = as.numeric(),
#                                                 Precision = as.numeric(),
#                                                 FScore = as.numeric())
# 
# # REMOVE 3 below lines ONCE PRIOR PROCEDURE INTEGRATED 
# FID <- as.numeric(numextract_all(File_List[PP])[1])
# Plot_ID <- as.numeric(numextract_all(File_List[PP])[3])
# H <- 1

# GET DISTRIBUTION OF Z VALUES IN PLOT
#LAS_oneP_allT <- readLAS(paste(Comp_Name, Folder_Location, "/LAS_P/", File_List[PP], sep=""))
XYZ_oneP <- LAS_oneP_allT@data # LAS_oneP_allT
Z_Val_Plot <- XYZ_oneP$Z
pdens <- density(Z_Val_Plot, bw= Para_K_BandWidth)


# # GET DF OF VOX SPACE WITH GT AND SUMMARISE GT
# Vox_File <- paste("LAS_", substr(File_List[PP],1, nchar(File_List[PP])-4), "_Vox.laz", sep="")
# LAS_Vox <- readLAS(paste(Comp_Name, Folder_Location, "/Vox_P/", Vox_File, sep=""))
# LAS_Vox@data$TID <- LAS_Vox@data$PointSourceID
# Point_DF <- data.frame(LAS_Vox@data[,1:3])

# GET DF OF VOX SPACE WITH GT AND SUMMARISE GT
Summary_Vox_TID <- data.frame(TID = names(table(LAS_Vox@data$TID)),
                              Vox_Count = as.vector(table(LAS_Vox@data$TID)))

# OUTPUT STOCK AND VOX COUNT FOR PLOT AND PUT INTO FLIGHT OUTPUT
write.csv(Summary_Vox_TID, paste(FOLDER_CSV_STOCK_MP_O, "/F" , FID, "_P", Plot_ID, "_Stocking_VoxCnt.csv" , sep=""),row.names=FALSE)

# TID_oneP <- unique(LAS_Vox@data$TID)
# TID_oneP <- sort(TID_oneP[which(TID_oneP > 1)])


############################
# CALCULATE PLOT CANOPY BASE (USED TO IDENTIFY BotBBOX THAT NEEDS TO BE BELOW THIS VALUE)
############################
Plot_CanopyBase <- GAP_DENSITY_FUNCTION_NEW2(Z_Val_Plot, Plot = "No")
Summary_Plot_CB <- Plot_CanopyBase$Peak_Dip_Summary$Peak_Dip_Summary
Z_CB_Temp <- Summary_Plot_CB$Z[which(Summary_Plot_CB$Z > Plot_CanopyBase$End_Largest_Gap)[1]]

# DOM DOM DOM IF Z_CB IS NEAR TOP OF PLOT HEIGHT THEN CORRECT IT SO IT IS JUST BELOW BIGGEST GAP BETWEEN TWO PEAKS 

# GET Peak_Dip_Summary
Peak_Dip_Summary <- Peak_Dip_FUN(pdens) # Peak_Dip_Summary$Peak_Dip_Summary
Peak_Dip_DF <- Peak_Dip_Summary$Peak_Dip_Summary 
Peak_Dip_DF$Rise <- c(0,Peak_Dip_DF$Density[-1] - Peak_Dip_DF$Density[-nrow(Peak_Dip_DF)])
Peak_Dip_DF <- Peak_Dip_DF[order(Peak_Dip_DF$Rise),]

# REMOVE UNDERSTOREY PEAK
# IF THE SECOND RISE HAS A LOT OF POINTS BEFORE IT DIPS AGAIN THEN REMOVE IT
# GET POINTS BETWEEN FIRST AND SECOND DIP 
Peak_Dip_DF_Und <- Peak_Dip_DF[which(Peak_Dip_DF$Z < Para_Under_Strata_Z),] # Peak_Dip_DF$Peak_Dip == "Peak" & 
Peak_Dip_DF_Und <- Peak_Dip_DF_Und[order(Peak_Dip_DF_Und$Z),]
Index_Dip <- which(Peak_Dip_DF_Und$Peak_Dip == "Dip")

if(length(Index_Dip) >= 2){
  Las_Under_Bot <- lasfilter(LAS_oneP_allT, Z > Peak_Dip_DF_Und$Z[Index_Dip[1]] & Z < Peak_Dip_DF_Und$Z[Index_Dip[2]])
  if(length(Las_Under_Bot$Z) > 100){
    #browser()
    Z_Under_Strata <- Peak_Dip_DF_Und$Z[Index_Dip[2]]
  }else{
    Z_Under_Strata <- Peak_Dip_DF_Und$Z[Index_Dip[1]]
  }
}else{ # IF 0
  if(length(Index_Dip) == 0){
    Z_Under_Strata <- Para_Under_Strata_Z
  }else{ # IF 1
    Z_Under_Strata <- Peak_Dip_DF_Und$Z[Index_Dip[1]] 
  }
  
}

Peak_Dip_DF_RmUnd <- Peak_Dip_DF[-which(Peak_Dip_DF$Z < Z_Under_Strata),] # Peak_Dip_DF$Peak_Dip == "Peak" & 

# # SUBSET THE DIPS WITH HEIGHEST DENSITY
Peak_Dip_DF_RmUnd$Split <- 0
if(nrow(Peak_Dip_DF_RmUnd) > Para_MinStrata*2){
  Dip_Large_Den <- Peak_Dip_DF_RmUnd[which(Peak_Dip_DF_RmUnd$Peak_Dip == "Dip"),]
  Dip_Large_Den <- Dip_Large_Den[rev(order(Dip_Large_Den$Density)),]
  Z_Split <- sort(Dip_Large_Den$Z[1:Para_MinStrata])
  Peak_Dip_DF_RmUnd$Split[which(Peak_Dip_DF_RmUnd$Z %in% Z_Split)] <- 1
  Peak_Dip_DF_RmUnd <-  Peak_Dip_DF_RmUnd[order(Peak_Dip_DF_RmUnd$Z),]
}else{
  Z_Split <- sort(Peak_Dip_DF_RmUnd$Z[which(Peak_Dip_DF_RmUnd$Peak_Dip == "Dip")])
  Peak_Dip_DF_RmUnd <-  Peak_Dip_DF_RmUnd[order(Peak_Dip_DF_RmUnd$Z),]
}

# ADD MIN HEIGHT AND MAX HEIGHT IN THE SPLIT
Z_Split <- unique(c(Z_Under_Strata, Z_Split, max(Peak_Dip_DF$Z)))

# SPLIT THE LARGEST STRATA IF GREATER THAN 10 m IN Z RANGE 
Z_GAPS <- Z_Split[-1] - Z_Split[-length(Z_Split)]
Index_Large_Gap <- which(Z_GAPS > 10)[1]
if(!is.na(Index_Large_Gap)){
  Z_Split <- c(Z_Split[1:Index_Large_Gap], 
               (Z_Split[Index_Large_Gap]+Z_GAPS[Index_Large_Gap]/2),
               Z_Split[(Index_Large_Gap+1):length(Z_Split)])
}

###############
# SPLIT PROFILE
###############
Strata_ID <- seq(1,length(Z_Split),1)
Strata <- cut(Z_Val_Plot, breaks = c(min(Z_Val_Plot), Z_Split), label = Strata_ID)
LAS_oneP_allT@data$Strata <- as.numeric(Strata)

# IF NUUMBER OF STRATAS GREATER THAN 6 THEN MERGE THE ONES WITH SMALLEST NUMBER OF POINTS.
# table(Strata)

  while(length(Strata_ID)> 6){
    Strata_Den <- table(LAS_oneP_allT@data$Strata)
    Index_Min <- which.min(Strata_Den)
    Index_Merge <-which.min(c(Strata_Den[Index_Min-1], Strata_Den[Index_Min+1]))
    Z_Split <- Z_Split[-Index_Min]
    Strata_ID <- seq(1,length(Z_Split),1)
    Strata <- cut(Z_Val_Plot, breaks = c(min(Z_Val_Plot), Z_Split), label = Strata_ID)
    LAS_oneP_allT@data$Strata <- as.numeric(Strata)
    # browser()
    }

#########################
# COMPUTING CANOPY HEIGHT (Z_CB IS START OF BIGGEST RISE BETWEEN TOP OF CANOPY AND BIGGEST PEAK IN PLOT)
#########################
Peak_Dip_DF_RmUnd$Split <- 0
Peak_Dip_DF_RmUnd$Split[which(Peak_Dip_DF_RmUnd$Z %in% Z_Split)] <- 1
Index_denMax <- which.max(Peak_Dip_DF_RmUnd$Density)
Index_maxMrise_belowdenMax <- which.max(Peak_Dip_DF_RmUnd$Rise[1:(Index_denMax)])
Z_CB <- Peak_Dip_DF_RmUnd$Z[Index_maxMrise_belowdenMax-1] # -1 to get to bottom of rise

#################################
# PLOT THE STRATIFIED VEG PROFILE 
#################################
if(Plot == "Yes"){
  plot(pdens$x, pdens$y, type="l", ylim=c(0, 0.6),
       main= paste(Plot_ID),
       ylab = "Density", 
       xlab = "Height (m)", 
       cex.lab = 1.5,
       cex.axis = 2)
  abline(v = Peak_Dip_DF_RmUnd$Z[which(Peak_Dip_DF_RmUnd$Peak_Dip == "Peak")], col="blue", lwd = 1, lty=3)
  abline(v = Z_Split, col="red", lwd = 3)
  abline(v = Z_CB, col="orange", lwd = 3)
  abline(v = Z_CB_Temp, col="orange", lwd = 2, lty=3)
  abline(v = Z_Under_Strata, col="blue", lwd = 2)
}


# DOM DOM DOM !!! Z_CB MAY HAVE A HUGE EFFECT ON THE PRIORS USED (YOU COULD MISS GT THAT HAVE NO STEM ... WHICH COULD BE GOOD.... )
    # TO INCREASE PRIOR CNT YOU MAYBE MOVE Z_CB UP .... AS FIRST Z_Split ABOVE Z_CB ...  which(Z_Split > Z_CB)[1]
    # TO MAKE SURE IT DOESN'T GO TOO HIGH USE CONTRAINT... ONLY IF THE Z VALUE IS LESS THAN THE HIGHEST DENSITY POINT  
          # using Peak_Dip_DF_RmUnd$Z[which(Peak_Dip_DF_RmUnd$Peak_Dip == "Peak")] Z LOCATIONS TO FIND HIGHEST DENSITY

  
# }


####################################
# COMBINATIONS THAT WILL BE ASSESSED (NOT INCLUDING UNDERSTOREY ... Strata_ID[-1])
####################################

# GENERATE WS PROFILES
Strata_Comb_all <- list(1, 2, 3, 4, 5, 6,
                        c(1, 2), c(2,3), c(3,4), c(4,5), c(5,6),
                        c(1,2,3), c(2,3,4), c(3,4,5), c(4,5,6),
                        c(1,2,3,4), c(2,3,4,5), c(3,4,5,6),
                        c(1,2,3,4,5), c(2,3,4,5,6))

Strata_Comb_oneP <- Strata_Comb_all[unlist(lapply(Strata_Comb_all,function(x) all(x %in% Strata_ID[-1])))]

# # PLOT LAS 3D
# plot(LAS_oneP_allT, color="Strata", size=5)
# bg3d("white")
# Shift_X <- min(LAS_oneP_allT@data$X)
# Shift_Y <- min(LAS_oneP_allT@data$Y)

##############################
# LOOP EACH STRATa COMBINATION
##############################
Box_ID <- 0
for(ST in 1:length(Strata_Comb_oneP)){

  LAS_oneP_oneStr <- filter_poi(LAS_oneP_allT, Strata %in% Strata_Comb_oneP[[ST]])
  if(nrow(LAS_oneP_oneStr@data) > 0){
    
    LAS_oneP_oneStr@data$Srata <- ST
    
    ###########################################################################################################################################################
    ##########################################
    # EXTRACT POTENTIAL STEM BASE COORDINATES (USING LOWEST STRATA ONLY)
    ##########################################
    if(ST == 1){
      
      # GENERATE RASTERS OF LOCATION WS
      R_Rng <- grid_metrics(LAS_oneP_oneStr, max(Z) - min(Z) , res = Para_strata_Res)
      Ext_R <- raster::extent(R_Rng)
      A_Rng <-as.matrix(R_Rng)
      A_WS <- EBImage::watershed(A_Rng, tol = 1, ext=5)
      R_WS <- raster(A_WS, xmn=Ext_R[1], xmx=Ext_R[2], ymn=Ext_R[3], ymx=Ext_R[4])
      XY_WS <- coordinates(R_WS)
      # browser()
      # GET DATAFRAME OF WS
      WS_Cl <- raster::extract(R_WS, XY_WS)
      Rng <- raster::extract(R_Rng, XY_WS)
      XY_Cl_Rng <- data.frame(XY_WS,WS_Cl, Rng)
      XY_Cl_Rng <- XY_Cl_Rng[which(XY_Cl_Rng$WS_Cl > 0),]
      
      if(nrow(XY_Cl_Rng) > 0){
        colnames(XY_Cl_Rng)[1:2] <- c("X", "Y")
        
        # GET BBOX REPRESENTING BASE LOCATIONS
        maxZ <- max(LAS_oneP_oneStr$Z)
        Loc_Base <- as.data.frame(XY_Cl_Rng %>%
                                    dplyr::group_by(WS_Cl) %>%
                                    dplyr::summarise(X_Min = X[which.max(Rng)]-(Para_Base_WL*2),
                                                     X_Max = X[which.max(Rng)]+(Para_Base_WL*2),
                                                     Y_Min = Y[which.max(Rng)]-(Para_Base_WL*2),
                                                     Y_Max = Y[which.max(Rng)]+(Para_Base_WL*2),
                                                     Z_Min = maxZ - max(Rng),
                                                     .groups = 'drop'))
        
        Loc_Cl <- data.frame(WS_Cl =rep(Loc_Base$WS_Cl, 5),
                             X=c(Loc_Base$X_Min, Loc_Base$X_Max , Loc_Base$X_Min, Loc_Base$X_Max, Loc_Base$X_Max),
                             Y=c(Loc_Base$Y_Min, Loc_Base$Y_Max, Loc_Base$Y_Max, Loc_Base$Y_Min, Loc_Base$Y_Min),
                             Z= rep(Loc_Base$Z_Min, 5))
        Loc_Cl <- Loc_Cl[order(Loc_Cl$WS_Cl),]
        
        Unique_WS_Cl <- unique(Loc_Cl$WS_Cl)
        for(CB in 1:length(Unique_WS_Cl)){
          Box_ID <- Box_ID + 1
          XY_oneCl <- Loc_Cl[which(Loc_Cl$WS_Cl == Unique_WS_Cl[CB]),]
          M_XY_oneCl <- matrix(c(XY_oneCl$X,  XY_oneCl$Y), ncol=2)
          Loc_oneCl <- as.data.frame(BBOX_PNTS_FUN(M_XY_oneCl))
          colnames(Loc_oneCl) <- c("X", "Y")
          Loc_oneCl <- data.frame(Box_ID = Box_ID,
                                  Loc_oneCl,
                                  Z = Loc_Cl$Z[which(Loc_Cl$WS_Cl == Unique_WS_Cl[CB])],
                                  Strata = ST,
                                  WS_Cl=Unique_WS_Cl[CB])
          
          # # PLOT POLYGON
          # polygon3d(as.vector(Loc_oneCl$X)-Shift_X,
          #           as.vector(Loc_oneCl$Y)-Shift_Y,
          #           as.vector(Loc_oneCl$Z),
          #           fill = FALSE, col="yellow", lwd=5)
          
          # GENERATE SP POLYGON DATAFRAMES
          SpP = SpatialPolygons(list(Polygons(list(Polygon(Loc_oneCl[,2:3])),as.character(Loc_oneCl[1,1]))), proj4string=CRS(Proj_Sys))
          DF <- Loc_oneCl[1,c(1,4,5,6)]
          row.names(DF) <- as.character(Loc_oneCl[1,1])
          Poly_Loc_oneCl <- SpatialPolygonsDataFrame(SpP, DF)
          
          # STORE THE BBOX IN DF
          if(!exists("BBox_allCl")){
            BBox_allCl <- Loc_oneCl # list(Loc_oneCl)
            Poly_allBBox  <- Poly_Loc_oneCl
          }else{
            BBox_allCl <- rbind(BBox_allCl,Loc_oneCl) # c(BBox_allCl,list(Loc_oneCl))
            Poly_allBBox  <- rbind(Poly_allBBox, Poly_Loc_oneCl)
          }
          
        } # LOOP CANOPY BASE CLUSTERS
      } # IF THERE ARE NO Loc_Cl
    } # FIRST ST FOR CANOPY BASE CLUSTERS
    ###########################################################################################################################################################
    
    ####################################################################
    # GENERATING WS_Cl USING THE DENSITY PROFILE OF Z VALUES (IN STRATA)
    ####################################################################
    R_Den <- grid_metrics(LAS_oneP_oneStr, ~length(Z) , res = Para_strata_Res)
    Ext_R <- raster::extent(R_Den)
    A_Den <-as.matrix(R_Den)
    
    A_WS <- EBImage::watershed(A_Den, tol = 1, ext=5)
    R_WS_Box <- raster(A_WS, xmn=Ext_R[1], xmx=Ext_R[2], ymn=Ext_R[3], ymx=Ext_R[4])
    
    XY_WS <- coordinates(R_WS_Box)
    WS_Cl <- raster::extract(R_WS_Box, XY_WS)
    XY_Cl <- data.frame(XY_WS, WS_Cl)
    XY_Cl <- XY_Cl[which(XY_Cl$WS_Cl > 0),]
    
    LAS_oneP_Cl <- merge_spatial(LAS_oneP_oneStr, R_WS_Box, attribute = "WS_Cl")
    Loc_Cl <- as.data.frame(LAS_oneP_Cl@data %>%
                              dplyr::group_by(WS_Cl) %>%
                              dplyr::summarise(Zmedian = median(Z), .groups = 'drop'))
    
    ####################################
    # GENERATE BOXES AROUND EACH CLUSTER
    ####################################
    Unique_WS_Cl <- unique(XY_Cl$WS_Cl)
    for(CB2 in 1:length(Unique_WS_Cl)){
      Box_ID <- Box_ID + 1
      XY_oneCl <- XY_Cl[which(XY_Cl$WS_Cl == Unique_WS_Cl[CB2]),]
      if(nrow(distinct(XY_oneCl)) >2){
        XY_oneCl <- matrix(c(XY_oneCl$x,  XY_oneCl$y), ncol=2)
        BBox_oneCl <- BBOX_PNTS_FUN(XY_oneCl)
        BBox_oneCl <- data.frame(Box_ID = Box_ID,
                                 BBox_oneCl,
                                 Z = Loc_Cl$Zmedian[which(Loc_Cl$WS_Cl == Unique_WS_Cl[CB2])],
                                 Strata = ST,
                                 WS_Cl=Unique_WS_Cl[CB2])
        colnames(BBox_oneCl)[2:3] <- c("X", "Y")
        
        # # PLOT POLYGON
        # polygon3d(as.vector(BBox_oneCl$X)-Shift_X,
        #           as.vector(BBox_oneCl$Y)-Shift_Y,
        #           as.vector(BBox_oneCl$Z),
        #           fill = FALSE, col=ST, lwd=3)
        
        # GENERATE SP POLYGON DATAFRAMES
        SpP = SpatialPolygons(list(Polygons(list(Polygon(BBox_oneCl[,2:3])),as.character(BBox_oneCl[1,1]))), proj4string=CRS(Proj_Sys))
        DF <- BBox_oneCl[1,c(1,4,5,6)]
        row.names(DF) <- as.character(BBox_oneCl[1,1])
        Poly_BBox_oneCl <- SpatialPolygonsDataFrame(SpP, DF)
        
        
        # STORE THE BBOX IN DF
        if(!exists("BBox_allCl")){
          BBox_allCl <- BBox_oneCl# list(BBox_oneCl)
          Poly_allBBox  <- Poly_BBox_oneCl
        }else{
          BBox_allCl <- rbind(BBox_allCl,BBox_oneCl) # c(BBox_allCl,list(BBox_oneCl))
          Poly_allBBox  <- rbind(Poly_allBBox, Poly_BBox_oneCl)
        }
      } # IF WS HAS ENOUGH POINTS TO PROCESS
    } # LOOP WS_Cl
  } 
    
  } # LOOP ST (STRATAS)
  
#browser()
# INTERSECTION oF BBox(s) (AREAS AND OVERLAPS)
Poly_allBBox$Area <- round(raster::area(Poly_allBBox),2)
Poly_Int_allP <- intersect(Poly_allBBox, Poly_allBBox)
Poly_Int_allP <- Poly_Int_allP[-which(Poly_Int_allP$Box_ID.1 == Poly_Int_allP$Box_ID.2),]  # REMOVE INTERSECTION OF POLYGON AGAINST ITS OWN POLYGON
Poly_Int_allP$Int_Area <- raster::area(Poly_Int_allP)   # INTERSECTION AREA
Poly_Int_allP$PortionOverlap_1 <- Poly_Int_allP$Int_Area/Poly_Int_allP$Area.1
Poly_Int_allP$PortionOverlap_2 <- Poly_Int_allP$Int_Area/Poly_Int_allP$Area.2

########################
# REMOVE SIMILAR BBOX(s)
########################

# IDENTIFY ALL PAIRED POLYGONS THAT HAVE OVERLAP > 70% (EACH WAY) AND Z VALUES <5 m
Poly_Int_allP_DF <- Poly_Int_allP@data
Poly_Int_allP_DF_Similar <- Poly_Int_allP_DF[which(Poly_Int_allP_DF$PortionOverlap_1 > Para_BBox_Overlap &
                                                             Poly_Int_allP_DF$PortionOverlap_2 > Para_BBox_Overlap &
                                                             abs(Poly_Int_allP_DF$Z.1 - Poly_Int_allP_DF$Z.2) < Para_BBox_Overlap_ZRng),]

ID_PolyRemove <- unique(Poly_Int_allP_DF_Similar$Box_ID.1)

# USE POLYGON ID TO GET COMPLETE BBOX OF THOSE POLYGONS
Poly_allCl_RmSimilar <- Poly_allBBox[-which(Poly_allBBox$Box_ID %in% ID_PolyRemove),]

POLY_FINAL <- Poly_allCl_RmSimilar

##################################################################
# MERGING POLYGONS THAT INTERSECT IF Z DIFFERENCE IS MORE THAN 5 m (THIS GENERATES LARGER BBox(s))
##################################################################
Overlap_BBox <- TRUE
Poly_Merge <- Poly_allCl_RmSimilar
Poly_Merge$Area <- raster::area(Poly_Merge)
while(Overlap_BBox == TRUE){
  Poly_Int_Merge <- intersect(Poly_Merge, Poly_Merge)

  
  # REMOVE POLYGONS THAT REPRESENT SAME INTERSECTING BBOX OR HAVE DIFFERENT HEIGHTS
  Index_Not_Overlap <- which(Poly_Int_Merge$Box_ID.1 == Poly_Int_Merge$Box_ID.2 | abs(Poly_Int_Merge$Z.1 - Poly_Int_Merge$Z.2) > Para_BBox_Z_MergeOverlap)
  if(length(Index_Not_Overlap) > 0){
    Poly_Int_Merge <- Poly_Int_Merge[-Index_Not_Overlap,]
    }

  # CREATE NEW BBOX USING ONES THAT OVERLAP
  if(length(Poly_Int_Merge) > 0){

    # IDENTIFY THE POLYGON WITH LARGEST INTERSECT FOR EACH BBox (using either Box_ID.1 and Box_ID.2)
    Poly_Int_Merge$Int_Area <- raster::area(Poly_Int_Merge)   # INTERSECTION AREA
    Largest_Overlap_DF <- as.data.frame(Poly_Int_Merge@data %>%
                              dplyr::group_by(Box_ID.1) %>%
                              dplyr::summarise(ID_Largest_Overlap = Box_ID.2[which.max(Int_Area)], .groups = 'drop'))

    # USE POLYGON ID TO GET COMPLETE BBOX OF THOSE POLYGONS
    Poly_Merge <- Poly_Merge[which(Poly_Merge$Box_ID %in% unique(Poly_Int_Merge$Box_ID.1)),]
    Poly_Merge_New <- Poly_Merge[0,]

    # MERGE_OVERLAPPING_BBOXS
    for(ii in 1:nrow(Largest_Overlap_DF)){
      Index_BBox1 <- which(Poly_Merge$Box_ID == Largest_Overlap_DF$Box_ID.1[ii])
      Index_BBox2 <- which(Poly_Merge$Box_ID == Largest_Overlap_DF$ID_Largest_Overlap[ii])
      Coord_Int <- rbind(Poly_Merge@polygons[[Index_BBox1]]@Polygons[[1]]@coords,
                         Poly_Merge@polygons[[Index_BBox2]]@Polygons[[1]]@coords)
      BBox_Int <- BBOX_PNTS_FUN(as.matrix(Coord_Int))
      Box_ID <- Box_ID + 1
      BBox_Int <- data.frame(Box_ID = Box_ID,
                             BBox_Int,
                             Z = mean(Poly_Merge@data$Z[c(Index_BBox1, Index_BBox2)]),
                             Strata = ST,
                             WS_Cl=NA)
      colnames(BBox_Int)[2:3] <- c("X", "Y")

      # GENERATE SP POLYGON DATAFRAMES
      SpP = SpatialPolygons(list(Polygons(list(Polygon(BBox_Int[,2:3])),as.character(BBox_Int[1,1]))), proj4string=CRS(Proj_Sys))
      DF <- BBox_Int[1,c(1,4,5,6)]
      row.names(DF) <- as.character(BBox_Int[1,1])
      Poly_BBox_Int <- SpatialPolygonsDataFrame(SpP, DF)
      Poly_BBox_Int$Area <- raster::area(Poly_BBox_Int)

      Poly_Merge_New  <- rbind(Poly_Merge_New, Poly_BBox_Int)
      if(ii == 1){BBox_Int_all <- BBox_Int} else{BBox_Int_all <- rbind(BBox_Int_all, BBox_Int)}
    } # LOOP ii (LOOP FOR MERGING BBoxes THAT OVERLAP)

    ########################
    # REMOVE SIMILAR BBOX(s)
    ########################

    # INTERSECTION oF BBox(s) (AREAS AND OVERLAPS)
    Poly_Merge_New$Area <- round(raster::area(Poly_Merge_New),2)
    Poly_Int_Merge_New <- intersect(Poly_Merge_New, Poly_Merge_New)
    Poly_Int_Merge_New <- Poly_Int_Merge_New[-which(Poly_Int_Merge_New$Box_ID.1 == Poly_Int_Merge_New$Box_ID.2),]  # REMOVE INTERSECTION OF POLYGON AGAINST ITS OWN POLYGON
    Poly_Int_Merge_New$Int_Area <- raster::area(Poly_Int_Merge_New)   # INTERSECTION AREA
    Poly_Int_Merge_New$PortionOverlap_1 <- Poly_Int_Merge_New$Int_Area/Poly_Int_Merge_New$Area.1
    Poly_Int_Merge_New$PortionOverlap_2 <- Poly_Int_Merge_New$Int_Area/Poly_Int_Merge_New$Area.2

    # IDENTIFY ALL PAIRED POLYGONS THAT HAVE OVERLAP > 70% (EACH WAY) AND Z VALUES <5 m
    Poly_Merge_New_DF <- Poly_Int_Merge_New@data
    Poly_Merge_New_DF_Similar <- Poly_Merge_New_DF[which(Poly_Merge_New_DF$PortionOverlap_1 > Para_BBox_Overlap &
                                                           Poly_Merge_New_DF$PortionOverlap_2 > Para_BBox_Overlap &
                                                         abs(Poly_Merge_New_DF$Z.1 - Poly_Merge_New_DF$Z.2) < Para_BBox_Overlap_ZRng),]

    ID_PolyRemove <- unique(Poly_Merge_New_DF_Similar$Box_ID.1)
    # USE POLYGON ID TO GET COMPLETE BBOX OF THOSE POLYGONS
    Poly_Merge_New_RmSimilar <- Poly_Merge_New[-which(Poly_Merge_New$Box_ID %in% ID_PolyRemove),]
    if(length(Poly_Merge_New_RmSimilar) == 0){
      Overlap_BBox <- FALSE
    }else{
      BBox_Int_RmSimilar <- BBox_Int_all[which(BBox_Int_all$Box_ID %in% Poly_Merge_New_RmSimilar$Box_ID),]

      # STORE THE BBOX IN DF
      BBox_allCl <- rbind(BBox_allCl, BBox_Int_RmSimilar) # c(BBox_allCl,list(BBox_oneCl))
      Poly_allBBox  <- rbind(Poly_allBBox, Poly_Merge_New_RmSimilar)
    }

  }else{
    Overlap_BBox <- FALSE
    Poly_Merge_New_RmSimilar <- Poly_Merge
  }
  Poly_Merge <- Poly_Merge_New_RmSimilar
} # WHILE LOOP

################################################################################
# GENERATE ALL COMBINATIONS (pairs) WHERE ONE NEEDS TO OVERLAP THE OTHER.
################################################################################
allBBox_DF <- Poly_allBBox@data

allBBox_DF <- allBBox_DF[order(allBBox_DF$Z),]

botBBox_DF <- allBBox_DF[which(allBBox_DF$Z < Z_CB),]

# IF THERE ARE NO BOUNDING BOXES BELOW CANOPY BASE MAKE HALF OF THEM botBBox_DF
if(nrow(botBBox_DF) < 1){
  botBBox_DF <- allBBox_DF[which(allBBox_DF$Z < median(allBBox_DF$Z)),] 
}

Prior_ID <- 0
for(CM in 1:nrow(botBBox_DF)){

  ### DOM DOM DOM !!! YOU DETERMINE THE INTERSECT OF LOC (WHICH WAS A BOX) WITH ABOVE BOXES.... WHY NOT USE THE ORIGINAL BOX ???
      # OR CONVERT THE LOCATION INTO A SQUARE AND USE IT FOR THE INTERSECT COMPUTATION....
  
  onebotBBox_DF <- botBBox_DF[CM,]
  Poly_onebotBBox <- Poly_allBBox[which(Poly_allBBox$Box_ID == onebotBBox_DF$Box_ID),]
  xy_oneBBox <- Poly_onebotBBox@polygons[[1]]@Polygons[[1]]@coords
  xy_oneBBox_Cent <- as.data.frame(t(colMeans(xy_oneBBox[1:4,])))


  # REDUCE THE BOTTOM BBOX TO A SQUARE AROUND CENTRE
  Loc_oneBBox_Sqr <-BBox_Sqr_FUN( Cent_X = xy_oneBBox_Cent$X,
                                 Cent_Y = xy_oneBBox_Cent$Y,
                                 Cent_Z = onebotBBox_DF$Z,
                                 Sqr_Offset = Para_Base_WL/2)
  colnames(Loc_oneBBox_Sqr) <- c("X_Base", "Y_Base", "Z_Base")
  Loc_oneBBox_Sqr$Strata <- 1
  Loc_oneBBox_Sqr$Plot <- Plot_ID
  Loc_oneBBox_Sqr$Flight <- FID

  # Loc_oneF_oneMP_oneP_allPriors
  # LOCATION OF EACH BASE COORDINATE
  #  TID     X_Base      Y_Base Z_Base Strata Plot Flight
  if(!exists("Loc_oneF_oneMP_oneP_allPriors")){
    Loc_oneF_oneMP_oneP_allPriors <- Loc_oneBBox_Sqr
  }else{
    Loc_oneF_oneMP_oneP_allPriors <- rbind(Loc_oneF_oneMP_oneP_allPriors,  Loc_oneBBox_Sqr)
  }

  xy_oneBBox_Cent_SP <- xy_oneBBox_Cent
  colnames(xy_oneBBox_Cent) <-  c("X_Base","Y_Base") # colnames(xy_oneBBox_Cent) <-  c("X","Y")
  coordinates(xy_oneBBox_Cent_SP) <- ~ X+Y

  Index_Above <- which(Poly_allBBox$Z > onebotBBox_DF$Z + Para_ZRng_Loc_BBox) #

  if(length(Index_Above) > 0){
    Poly_IntBBox_Abv <- intersect(Poly_allBBox[Index_Above,], xy_oneBBox_Cent_SP)
    if(length(Poly_IntBBox_Abv) > 0){
      for(TR in 1:length(Poly_IntBBox_Abv)){
        Prior_ID <- Prior_ID + 1

        # GENERATE XYZWLHR WITH BotBox NA (THIS IS FOR 10 PARAMETER PROCEDURE)
        XYZWLHR_Loc <- data.frame(Prior_ID, xy_oneBBox_Cent, Z_Base = onebotBBox_DF$Z)
        XYZWLHR_oneF_oneMP_oneP_onePrior_Temp <- data.frame(XYZWLHR_Loc,
                                                            X_BotBox=NA,
                                                            Y_BotBox=NA,
                                                            L_BotBox=NA,
                                                            W_BotBox=NA, Z_BotBox=NA, R_BotBox=NA)

        # GENERATE TRISHAPE USING THE TWO BBoxes
        Poly_oneAbvBBox <- Poly_IntBBox_Abv[TR,]
        Prior_BBox_WS <- as.data.frame(Poly_oneAbvBBox@polygons[[1]]@Polygons[[1]]@coords)
        Prior_BBox_WS$Z <- Poly_oneAbvBBox$Z
        Prior_BBox_WS$Flight <- FID
        Prior_BBox_WS$Plot <- Plot_ID
        Prior_BBox_WS$TID <- Poly_oneAbvBBox$Box_ID
        Prior_BBox_WS$Strata <- 2
        Prior_BBox_WS$Para_WS_Ext <- NA

        ######################
        # COMPUTE TOP OF PRIOR.
        ######################
        #############################################
        Prior_BBox_Top <- Prior_BBox_WS
        LAS_Prior_BBox <- merge_spatial(LAS_oneP_oneStr, Poly_oneAbvBBox, attribute = "BBox")
        Index_WithinBox <- which(LAS_Prior_BBox@data$BBox == TRUE)
        # IF THERE ARE POINTS WITHIN THE BOX (OTHERWISE DO NO PRODUCE THE PRIOR)
        if(length(Index_WithinBox) > 0){
          Prior_BBox_Top$Z <- max(LAS_Prior_BBox$Z[Index_WithinBox])
          Prior_BBox_Top$Strata <- 4 # DOM DOM DOM !!! NOT SURE IF THIS SHOULD BE 3 (may impact results later)

          Prior_Both_BBox <- rbind(Prior_BBox_WS, Prior_BBox_Top)
          
          # Prior_BBox_oneF_oneMP_oneP
          # VERT VALUES FOR EACH BBOX (ONLY TopBox AND TopHeightBox)
          #  X  Y  Z Z_TopTree Flight Plot TID Strata Para_WS_Et
          if(!exists("Prior_BBox_oneF_oneMP_oneP")){
            Prior_BBox_oneF_oneMP_oneP <- Prior_Both_BBox
          }else{
            Prior_BBox_oneF_oneMP_oneP <- rbind(Prior_BBox_oneF_oneMP_oneP,  Prior_Both_BBox)
          }
        

          ###################################
          # OUTPUTTING DATA IN FORMAT XYZWLHR (THIS REQUIRES A MERGER OF ALL THE COMPONENTS OF A TRISHAPE...)
          ###################################
          if(nrow(Prior_BBox_WS) > 0){
            Prior_BBox_XYZWLHR <- XYZWLHR_FUN(Prior_BBox_WS, Prefix = "Top", Height_Strata = 3, Sample_Att = "No")
            Prior_BBox_XYZWLHR$Z_TopTree <- max(LAS_Prior_BBox$Z[which(LAS_Prior_BBox@data$BBox == TRUE)])# Z_Priors$Max_Z[which(Z_Priors$TID ==Unique_WS[W])]

            # GENERATES UNIQUE BBOXES AND THEIR STRATA HEIGHT             # TID  Height_Strata    X_Box   Y_Box   L_Box   W_Box   Z_Box   R_Box   Z_Tree
            if(!exists("XYZWLHR_oneF_oneMP_oneP_Boxes")){
              XYZWLHR_oneF_oneMP_oneP_Boxes <- Prior_BBox_XYZWLHR
            }else{
              XYZWLHR_oneF_oneMP_oneP_Boxes <- rbind(XYZWLHR_oneF_oneMP_oneP_Boxes,  Prior_BBox_XYZWLHR)

            }
            XYZWLHR_oneF_oneMP_oneP_onePrior <- cbind(XYZWLHR_oneF_oneMP_oneP_onePrior_Temp, Prior_BBox_XYZWLHR[,3:9])
          }

          # XYZWLHR_oneF_oneMP_oneP_allPrior
          # XYZWLHR OF ALL PRIORS
          # PriorID     X_Base      Y_Base Z_Base      X_BotBox      Y_BotBox       L_BotBox      W_BotBox      Z_BotBox      R_BotBox      X_TopBox      Y_TopBox
          # cont....  L_TopBox      W_TopBox      Z_TopBox      R_TopBox Z_TopTree
          if(!exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){
            XYZWLHR_oneF_oneMP_oneP_allPrior <-  XYZWLHR_oneF_oneMP_oneP_onePrior
          }else{
            #XYZWLHR_oneF_oneMP_oneP_onePrior$TID <- max( XYZWLHR_oneF_oneMP_oneP_allPrior$TID) + 1
            XYZWLHR_oneF_oneMP_oneP_allPrior <- rbind( XYZWLHR_oneF_oneMP_oneP_allPrior,  XYZWLHR_oneF_oneMP_oneP_onePrior)
          }

          ############################################################################################################################
          ##############################################################################
          # PERFORM INTERSECTION OF UNION BETWEEN PRIOR AND ALL GROUND TRUTHS FOR A PLOT
          ##############################################################################
          Vert_onePrior <- XYZWHR_TO_VERT_FUN(XYZWLHR_oneF_oneMP_oneP_onePrior[,-1], Base_WL = Para_Base_WL, Normalised = "No", Para_Cnt = 10) # NOT THAT THIS PARAMETERS IS NORMALISED
   
          # Vert_onePrior_Orig <- Vert_onePrior
          # 
          # Scale_X <- min(Vert_onePrior[,1])
          # Scale_Y <- min(Vert_onePrior[,2])
          # Vert_onePrior[,1] <- Vert_onePrior[,1]-Scale_X
          # Vert_onePrior[,2] <- Vert_onePrior[,2]-Scale_Y
 
          Vert_onePrior <- VERTICIES_ORDER_FUN(Vert_onePrior, Box_Levels = 3) # ADDED 11/05/2021... 
          Vert_onePrior <- as.data.frame(Vert_onePrior)
          # Ext_oneP_plotPrior
          # EXTENT OF EACH PRIOR (two lines per prior)
          # TID_Prior             X             Y      Z

          # KEEP TRACK OF THE Extent OF EACH PRIOR
          if(!exists("Ext_oneP_plotPrior")){
            Ext_oneP_plotPrior <- data.frame(TID_Prior = Prior_ID, apply(Vert_onePrior,2,range))
          }else{
            Ext_onePrior <- data.frame(TID_Prior = Prior_ID, apply(Vert_onePrior,2,range))
            Ext_oneP_plotPrior <- rbind(Ext_oneP_plotPrior, Ext_onePrior)
          }

          # IF THE TREE (TID) IS MORE THAN 2 M TALL (ALMOST ALWAYS)
          if((range(Vert_onePrior[,3])[2]-range(Vert_onePrior[,3])[1]) > 2){ # replaced Shift_15X15_N_Z with Shift_Vox_N_Z

            ######################### 2b
            # CLIP VOXELS IN TRISHAPE
            ######################### 2b
            #
            # lapply(Vert_onePrior, class)
            nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, as.matrix(Vert_onePrior), Point_DF) 
            insideVect <- list(nbIntersect%%2 != 0)

            LAS_Vox@data$Prior[insideVect[[1]]] <- Prior_ID

            # GET ALL VOXELS THAT ARE PRIOR AND GT
            LAS_Vox_GT_Prior <- filter_poi(LAS_Vox, Prior == Prior_ID | TID %in% TID_oneP) ### DOM DOM DOM !!! THE GT VOXELS ARE THE ACTUAL GT AND NOT THE TRISHP... DOES THIS MATTER
            
            # OUTPUT PRIOR AND GT
            Prior_TID <- data.frame(Prior = LAS_Vox_GT_Prior@data$Prior,
                                    TID = LAS_Vox_GT_Prior@data$TID )
            
            # ALL PRIORS THAT AREN'T SUBJECT PRIOR BECOMES ZERO
            Prior_TID$Prior[which(Prior_TID$Prior != Prior_ID)] <- 0
            
            # SUMMARISE onePRIOR AND allGT INTERACTION
            Summary_FSc_IoU_onePRIOR_AllComb <- as.data.frame(Prior_TID  %>%
                                                     dplyr::group_by(Prior, TID) %>%
                                                     dplyr::summarise(Count_IoU = length(Prior), .groups = 'drop'))

            Summary_FSc_IoU_onePRIOR <- Summary_FSc_IoU_onePRIOR_AllComb[which(Summary_FSc_IoU_onePRIOR_AllComb$TID > 1 & Summary_FSc_IoU_onePRIOR_AllComb$Prior > 0),]
            
            # # CHECKING INTERSECTION RESULTS RESULTS  
            # plot(LAS_Vox_GT_Prior, color="Prior")
            # bg3d("white")
            # Vert_onePrior_PLOT <- Vert_onePrior
            # Vert_onePrior_PLOT$X <- Vert_onePrior_PLOT$X - min(LAS_Vox_GT_Prior$X)
            # Vert_onePrior_PLOT$Y <- Vert_onePrior_PLOT$Y - min(LAS_Vox_GT_Prior$Y)
            # triangles3d(Vert_onePrior_PLOT[Triangles_ID_All_Mx,], col="red")
            
            if(nrow(Summary_FSc_IoU_onePRIOR) > 0){

              Summary_FSc_IoU_onePRIOR$Total_Vox <- Summary_Vox_TID$Vox_Count[match(Summary_FSc_IoU_onePRIOR$TID,Summary_Vox_TID$TID)] ### DOM DOM DOM !!! CHANGES THIS 16/11/20  which(Summary_Vox_TID$TID %in% Summary_FSc_IoU_onePRIOR$TID)

              # Comp_NameUTING FSCORE AND INTERSECTION OF UNION (IoU)
              Summary_FSc_IoU_onePRIOR$Portion_IoU_CorrecIn <- Summary_FSc_IoU_onePRIOR$Count_IoU/ Summary_FSc_IoU_onePRIOR$Total_Vox
              Summary_FSc_IoU_onePRIOR$Portion_IoU_WrongIn <- (Summary_FSc_IoU_onePRIOR$Total_Vox - Summary_FSc_IoU_onePRIOR$Count_IoU)/ Summary_FSc_IoU_onePRIOR$Total_Vox
              Summary_FSc_IoU_onePRIOR$TP <- Summary_FSc_IoU_onePRIOR$Count_IoU
              
              Index_Prior_FN <- which(Summary_FSc_IoU_onePRIOR_AllComb$Prior == 0)
              Summary_FSc_IoU_onePRIOR$FN <- 0
              if(length(Index_Prior_FN) > 0){
                FN <- Summary_FSc_IoU_onePRIOR_AllComb[Index_Prior_FN,]
                Summary_FSc_IoU_onePRIOR$FN <-  FN$Count_IoU[match(Summary_FSc_IoU_onePRIOR$TID, FN$TID)] # Summary_FSc_IoU_onePRIOR$Total_Vox  - Summary_FSc_IoU_onePRIOR$Count_IoU
                }
              
              Index_Prior_FP <- which(Summary_FSc_IoU_onePRIOR_AllComb$Prior == Prior_ID)
              Summary_FSc_IoU_onePRIOR$FP <- 0
              if(length(Index_Prior_FP) > 0){
                FP <- Summary_FSc_IoU_onePRIOR_AllComb[Index_Prior_FP,]
                FP$FP <- sum(FP$Count_IoU) - FP$Count_IoU
                Summary_FSc_IoU_onePRIOR$FP <-  FP$FP[match(Summary_FSc_IoU_onePRIOR$TID, FP$TID)]  # sum(Summary_FSc_IoU_onePRIOR$Count_IoU) -Summary_FSc_IoU_onePRIOR$Count_IoU
                }

              Summary_FSc_IoU_onePRIOR$Recall <- Summary_FSc_IoU_onePRIOR$TP/(Summary_FSc_IoU_onePRIOR$TP + Summary_FSc_IoU_onePRIOR$FN)
              Summary_FSc_IoU_onePRIOR$Precision <- Summary_FSc_IoU_onePRIOR$TP/(Summary_FSc_IoU_onePRIOR$TP + Summary_FSc_IoU_onePRIOR$FP)
              Summary_FSc_IoU_onePRIOR$FScore <- 2*((Summary_FSc_IoU_onePRIOR$Recall* Summary_FSc_IoU_onePRIOR$Precision)/(Summary_FSc_IoU_onePRIOR$Recall + Summary_FSc_IoU_onePRIOR$Precision))
              Summary_FSc_IoU_onePRIOR$PRIOR_Z_TopBox <- XYZWLHR_oneF_oneMP_oneP_onePrior$Z_TopBox

              # IoU BETWEEN EACH GT AND EACH PRIOR ....  columns area.... Prior TID Count_IoU Total_Vox Portion_IoU_CorrecIn Portion_IoU_WrongIn TP  FN FP          Recall      Precision         FScore
              Output_IoU_allPrior_allGT_onePlot <- rbind(Output_IoU_allPrior_allGT_onePlot, Summary_FSc_IoU_onePRIOR)
             
              # browser()
              }
            } # IF TID IS TALLER THAN 2 m
        
        } # IF THERE ARE POINTS WITHIN THE ABOVE BOX
          ### DOM DOM DOM !!! NOT SURE IF YOU NEED TO GENERATE THIS ....
          # browser()
          # oneTriShp <- rbind(Loc_oneBBox_Sqr, Prior_BBox_WS)

        #print(paste("TR", TR))
      } # LOOP THROUGH TOP BOX TO GENERATE XYZLHWR
    } # IF THERE ARE POLYGONS ABOVE THE LOCATION
  } # IF THERE ARE POLYGONS ABOVE THE LOCATION
  #print(paste("CM", CM))
} # LOOP CM (LOOPS THROUGH BOTTOM BOX (i.e. Loc))

########################################################################################################################################### 10
########################################################################################################################################### 10
###########################
# OUTPUTING ALL THE RESULTS
###########################
########################################################################################################################################### 10
########################################################################################################################################### 10

# ############################################
# # OUTPUT PLOT PRIORS (AND MERGE FOR EACH MP)
# ############################################
# if(exists("Loc_oneF_oneMP_oneP_allPriors")){
#   write.csv(Loc_oneF_oneMP_oneP_allPriors, paste(FOLDER_CSV_PRIOR_MP_O ,  "/F" , FID, "_P", Plot_ID, "_Prior_Loc.csv"  , sep=""),row.names=FALSE)
#   if(exists("Prior_Loc_oneF_oneMP")){
#     Prior_Loc_oneF_oneMP <- rbind(Prior_Loc_oneF_oneMP, Loc_oneF_oneMP_oneP_allPriors)
#   }else{
#     Prior_Loc_oneF_oneMP <- Loc_oneF_oneMP_oneP_allPriors
#   }
# }
#browser()

# KEEPING TRACK OF HOW MANY OF EACH THERE ARE

if(exists("XYZWLHR_oneF_oneMP_oneP_allPrior")){
  Prior_Cnt = nrow(XYZWLHR_oneF_oneMP_oneP_allPrior)
}else{
  Prior_Cnt = 0
}

if(exists("Loc_oneF_oneMP_oneP_allPriors")){
  Loc_Cnt = nrow(Loc_oneF_oneMP_oneP_allPriors)/5
}else{
  Loc_Cnt = 0
}
Count_TriShps_oneP <- data.frame(PlotID = Plot_ID,
                                 Prior_Cnt = Prior_Cnt,
                                 Loc_Cnt = Loc_Cnt,
                                 #botBox_Cnt = nrow(botBBox_DF),
                                 Box_Cnt = nrow(allBBox_DF),
                                 UnderTop = Z_Under_Strata,    
                                 CBase = Z_CB)
Count_TriShps_allP <- rbind(Count_TriShps_allP, Count_TriShps_oneP)

print(Count_TriShps_allP)
#  } # LOOP PLOT

# end_time <- Sys.time()
# Time_Taken <- end_time - start_time
# print(Time_Taken)




# Ext_oneP_plotPrior
# Loc_oneF_oneMP_oneP_allPriors
# Prior_BBox_oneF_oneMP_oneP
# XYZWLHR_oneF_oneMP_oneP_Boxes
# XYZWLHR_oneF_oneMP_oneP_allPrior



# # PLOT POLYGON
# plot(LAS_oneP_allT                     CBase = numeric(), color="Strata")
# Shift_X <- min(LAS_oneP_allT                     CBase = numeric()@data$X)
# Shift_Y <- min(LAS_oneP_allT                     CBase = numeric()@data$Y)
# for(tt in 1:length(Poly_allBBox)){
#   BBox_Test <- BBox_allCl[which(BBox_allCl$Box_ID == Poly_allBBox@data$Box_ID[tt]),]
#   polygon3d(as.vector(BBox_Test$X)-Shift_X,
#             as.vector(BBox_Test$Y)-Shift_Y,
#             as.vector(BBox_Test$Z),
#             fill = FALSE, col=tt, lwd=3)
# }




