

#######################################################################################################################################################
#######################################################################################################################################################
# LOOK AT RAW DATA. PLOT BEST PRIORS/TARGET/PREDICTION AND SEE WHAT THE RESULTS LOOK LIKE!!!!
#######################################################################################################################################################
#######################################################################################################################################################

# XYZ_RoI_Empty_N <- EMPTY_RoI_VOX_FUN(para_RoI_Pool_Dim_XY, para_RoI_Pool_Dim_Z)
# LAS_RoI_Empty_N <- LAS(XYZ_RoI_Empty_N)
# LAS_RoI_Empty_N@data$TID <- 1

#######################################################################################################################################################
#####################
# OPEN THE BEST MODEL
#####################
# DOM DOM DOM !!! PRESENTLY YOU ARE OPENING THE LAST MODEL RAN 
# BUT YOU MaY WANT TO OPEN UP CSV AND SEE WHEN MODEL VALIDATION STARTS DETERIORATING AND OPEN MODEL JSUT BEFORE THAT POINT

FOLDER_SAVED_MODEL <- paste(FOLDER_MAIN_DATA, "/SAVED_MODELS", sep="")
Model_Epoch <- list.files(FOLDER_SAVED_MODEL)
FOLDER_RUNS <- Model_Epoch[which.max(as.numeric(numextract_all(Model_Epoch)))]
# DOM DOM DOM !!! JUST GETTING THE 4th EPOCH 
Model_Epochs <- list.files(paste(FOLDER_SAVED_MODEL, "/", FOLDER_RUNS, sep=""), pattern= "model_Epoch") 
FILE_SAVED_MODEL <- Model_Epochs[which.max(as.numeric(numextract_all(Model_Epochs)))]
model_Saved <- torch_load(paste(FOLDER_SAVED_MODEL, "/",FOLDER_RUNS, "/", FILE_SAVED_MODEL, sep="")) 

model_Saved$eval()

### DOM DOM DOM !!! NOTE THAT YOU SHOULD USE VALIDATION DATA IN FUTURE
# eval_ds <- Valid_ds
# eval_dl <- Valid_dl
eval_ds <- Train_ds
eval_dl <- Train_dl

# GET BATCH FILE/FOLDER NAMES
batch <- eval_dl  %>% torch::dataloader_make_iter() %>% torch::dataloader_next()

# OPEN TENSORS 
INPUT_LIST <- batch[[1]] # list(oneP_Vox_Den_ExtP_T, oneP_RoI_Vox_T,  oneP_RoI_Dec_T, INPUT_PRIOR_XYZWLHR_T)
TARGET_LIST <- batch[[2]]  # list(oneP_Vox_TID_ExtP_T, TARGET_STOCK_T, TARGET_CLASS_T, TARGET_XYZWLHR_T, oneP_TID_T)

#######################################################################################################################################################
#########################
# LOOP THROUGH EACH BATCH
#########################

Batch_Count <- 0
for (b in enumerate(eval_dl)) { 
  Batch_Count <- Batch_Count + 1 

  INPUT_LIST <- b[[1]]
  TARGET_LIST <- b[[2]]
  FLIGHT_PLOT_ID_LIST <- b[[3]]

  Flight <- numextract_all(eval_ds$DIR[[1]][[8]][1])
  Flight <- Flight[length(Flight)]

  
  #######################################################################################################################################################
  ########################
  # LOOP THROUGH EACH PLOT
  ########################
  browser()
  Batch_Length <- dim(INPUT_LIST[[1]])[1]
  for(p in 3:Batch_Length){
    print(paste("Doing Plot: .....", p))
    BestWorst_IoU<- read.csv(paste(eval_ds$DIR[[1]][[6]][p], "/", eval_ds$DIR[[2]][[6]][[p]], sep=""))
    BestWorst_IoU <- BestWorst_IoU[,-2] # REMOVE SAMPLE COLUMN
    
    Plot_ID <- BestWorst_IoU$Plot_ID[1]
    
    Shift_Parameters <- read.csv(paste(eval_ds$DIR[[1]][[8]][1], "/CSV/F", as.numeric(Flight),"_Shifts_LAS_XYZWLHR.csv", sep=""))
    oneP_Shift_Parameters <- Shift_Parameters[which(Shift_Parameters$Plot_ID == Plot_ID),]
    
   # LAS_Vox_oneP_Orig <- readLAS(paste(eval_ds$DIR[[1]][[8]][1], "/LAS/LAS_MP1/LAS_P/F", Flight, "_MP1_P", Plot_ID, ".laz", sep=""))
    LAS_Vox_oneP_15X15_Coord_Orig <- readLAS(paste(eval_ds$DIR[[1]][[8]][1], "/LAS/LAS_MP1/LAS_P15X15/LAS_F", Flight, "_MP1_P", Plot_ID, "_15X15X40_N.laz", sep=""))
 
    ####################
    # GET DATA FOR BATCH
    ####################
  
    # INPUT_LIST[[1]] #_LAS_Vox_N is VOXEL LEVEL DF (0-1)                                 Pre_Master_FILE # VOX
    # INPUT_LIST[[2]] #_List_RoI_Vox_DF_S is VOXEL LEVEL (1-16 or 1:40)                   PrepDATA FILE   # ROI
    # INPUT_LIST[[3]] #_List_RoI_Dec_DF_S is DECIMAL LEVEL (0-1)                          PrepDATA FILE   # ROI
    # INPUT_LIST[[4]] #_XYZWLHR_BestWorst_plotPriors_S is NORMALISED (0-1)                PrepDATA FILE   # INPUT PRIORS
    # STOCK
    # CLASS
    # INPUT_LIST[[7]] #_Ext15X15_allT_LocBox_XYZWLHR_N                                     Pre_Master_FILE #GT
    # FOLDERS
    
    #INPUTS FOR DATA_PREP
    # LAS_Vox_N_DF         ..........IS ..............INPUT_LIST[[1]] #_LAS_Vox_N is VOXEL LEVEL DF (0-1)                                 Pre_Master_FILE # VOX
    # XYZWLHR_plotGT_N      ..........IS .............INPUT_LIST[[7]] #_Ext15X15_allT_LocBox_XYZWLHR_N 
    
    #INPUTS FOR PRE_MASTER
    # XYZWLHR_plotPriors_N .... IS ................  _Ext15X15_AllPRIORS_N  IN Pre_Master_FILE USE _LAS_Vox_N
    # Output_IoU_Prior_GT <- ... IS .............. INTERSECTION OF UNION ONLY (IT USE LAS FILE THAT STILL HAS ORIGINAL COORD)

    
    # INPUT LISTS
    oneP_Vox_Den_ExtP <- INPUT_LIST[[1]][p, .., drop = FALSE]         # oneP_Vox_Den_ExtP_T           1 1 16 16 40   1 CHANNEL PROVIDING DENSITY OF POINTS IN EACH VOXEL
    oneP_RoI_Vox <- INPUT_LIST[[2]][p, .., drop = FALSE]             # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
    oneP_RoI_Dec <- INPUT_LIST[[3]][p, .., drop = FALSE]             # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
    oneP_PRIOR_XYZWLHR_ExtP_Orig  <- INPUT_LIST[[4]][p, .., drop = FALSE]  # INPUT_PRIOR_XYZWLHR_T     1 64 16         64 UNIQUE PRIORS WITH XYZWLHR LCOATION
  
    oneP_Vox_TID_ExtP <- TARGET_LIST[[1]][p, .., drop = FALSE]    #  oneP_Vox_TID_ExtP_T   1  1 16 16 40
    oneP_TARGET_XYZWLHR_ExtP_Orig <- TARGET_LIST[[4]][p, .., drop = FALSE]
    oneP_TID <- TARGET_LIST[[5]][p, .., drop = FALSE]        #  oneP_TID_T       1 64
    
    FlightID <- as.array(FLIGHT_PLOT_ID_LIST[[1]])[p,]
    PlotID <- as.array(FLIGHT_PLOT_ID_LIST[[2]])[p,]
    FOLDER <- as.array(FLIGHT_PLOT_ID_LIST[[3]])[p]
  
    ######################################
    # CONVERT VOX_DEN TENSOR INTO LAS FILE 
    ######################################
    
    Vox_DenCount <- round(as.vector(as.array(oneP_Vox_Den_ExtP)), 4) #Shift_Data$Shift_Vox_X[1
    Vox_TID <- as.vector(as.array(oneP_Vox_TID_ExtP)) #Shift_Data$Shift_Vox_X[1
    Vox_TID_DenCount <- data.frame(Vox_DenCount, TID = Vox_TID)
    Vox_LASFormat <- EMPTY_VOX_FUN2(Vox_TID_DenCount, Para_Target_Base, Para_Target_Z_Height, Para_Vox_Res)
    LAS_Vox <- LAS(Vox_LASFormat) 
    LAS_Vox_Trees <-lasfilter(LAS_Vox, Vox_DenCount > 0)
    
    LAS_Orig <- readLAS(paste(FOLDER, "/LAS/LAS_MP1/Vox_P/LAS_F",FlightID,"_MP1_P",PlotID,"_Vox_N.laz", sep=""))
    LAS_Orig_Trees <- lasfilter(LAS_Orig, Count_Norm > 0)
    LAS_Orig_Trees@data$TID <- LAS_Orig_Trees@data$PointSourceID

    # LAS_Vox_oneP_15X15_Coord_Orig <- readLAS(paste(eval_ds$DIR[[1]][[8]][1], "/LAS/LAS_MP1/LAS_P15X15/LAS_F", Flight, "_MP1_P", Plot_ID, "_15X15X40_N.laz", sep=""))

    # PLOT THE LAS/VOXEL INPUT (NORMALISED)
    Title_Plot <- paste("LAS_Orig_Trees  P:", Plot_ID)
    PLOT_LAS_FUN(LAS_Orig_Trees, Title_Plot) # THIS IS LAS FILE FROM THE Pre_MASTER OUTPUT
    Title_Plot <- paste("LAS_Vox_Trees  P:", Plot_ID)
    PLOT_LAS_FUN(LAS_Vox_Trees, Title_Plot)  # THIS IS THE TENSOR (PLOTTED AS LAS FILE) THAT IS PUT INTO THE CNN (i.e. oneP_Vox_Den_ExtP ) 
    
    #############################################
    # MODEL INITIALISATION TO EXTRACT PREDICTIONS
    #############################################  
    OUTPUT_LIST <-model_Saved(oneP_Vox_Den_ExtP, oneP_RoI_Vox, oneP_RoI_Dec, oneP_PRIOR_XYZWLHR_ExtP_Orig, oneP_TARGET_XYZWLHR_ExtP_Orig, oneP_Vox_TID_ExtP, oneP_TID)
    
    OUTPUT_out4 <- OUTPUT_LIST[[1]]
    OUTPUT_out5 <- OUTPUT_LIST[[2]]
    
    oneP_TARGET_XYZWLHR_ExtR <- OUTPUT_out4[[1]] 
    oneP_TARGET_XYZWLHR_ExtR_Goffset <- OUTPUT_out4[[2]]      
    oneP_PRIOR_XYZWLHR_ExtR <- OUTPUT_out4[[3]]      # 16 64  16        # 64 ROIS, 16 offsets for locations
    oneP_Vox_Den_ExtR <- OUTPUT_out4[[4]] 
    oneP_MASK_ExtR <- OUTPUT_out4[[7]] 
    
    # SQUEEZE ALL AXIS
    oneP_PRIOR_XYZWLHR_ExtR <- torch_squeeze(oneP_PRIOR_XYZWLHR_ExtR)
    oneP_TARGET_XYZWLHR_ExtP_Orig <- torch_squeeze(oneP_TARGET_XYZWLHR_ExtP_Orig)
    oneP_TARGET_XYZWLHR_ExtR <- torch_squeeze(oneP_TARGET_XYZWLHR_ExtR)
    oneP_TARGET_XYZWLHR_ExtR_Goffset <- torch_squeeze(oneP_TARGET_XYZWLHR_ExtR_Goffset)
    oneP_Vox_Den_ExtR <- torch_squeeze(oneP_Vox_Den_ExtR)
    oneP_MASK_ExtR <- torch_squeeze(oneP_MASK_ExtR)
    oneP_RoI_Vox <- torch_squeeze(oneP_RoI_Vox)
    
    oneP_Pred_XYZWLHR_Goffset_ExtR <- OUTPUT_out5[[2]]      # 16 64  16        # 64 ROIS, 16 offsets for locations
    oneP_Pred_XYZWLHR_Goffset_ExtR <- torch_squeeze(oneP_Pred_XYZWLHR_Goffset_ExtR)
  
    # DECODER FUNCTION 
    oneP_Pred_XYZWLHR_ExtR <- Goffset_To_XYZWLHR_FUN(oneP_Pred_XYZWLHR_Goffset_ExtR, oneP_PRIOR_XYZWLHR_ExtR)
    
    browser()
    #############################################################################################################################################################
    #########################################################################################
    # RESCALE RESULTS (oneP_Pred_XYZWLHR_ExtR) TO THE SIZE OF THE PLOT oneP_Pred_XYZWLHR_ExtP
    #########################################################################################
    
    oneP_Pred_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
                                                                                 XYZWLHR = oneP_Pred_XYZWLHR_ExtR, 
                                                                                 batch=1, 
                                                                                 Shift=oneP_Shift_Parameters, 
                                                                                 ScaleRoI2Plot_First = "Yes")))
    colnames(oneP_Pred_XYZWLHR_ExtP_Coord) <- Colnames_XYZWLHR 
    
    oneP_TARGET_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
                                                                                   XYZWLHR = oneP_TARGET_XYZWLHR_ExtR, 
                                                                                   batch=1, 
                                                                                   Shift=oneP_Shift_Parameters, 
                                                                                   ScaleRoI2Plot_First = "Yes")))
    colnames(oneP_TARGET_XYZWLHR_ExtP_Coord) <- Colnames_XYZWLHR
    
    
    # SOMETHING WRONG WITH THIS !!!
    oneP_TARGET_XYZWLHR_ExtP_Orig_DF <- data.frame(as.array(oneP_TARGET_XYZWLHR_ExtP_Orig))
    colnames(oneP_TARGET_XYZWLHR_ExtP_Orig_DF) <- Colnames_XYZWLHR
    oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_DF <- XYZWLHR_NORM_to_COORD_FUN(oneP_TARGET_XYZWLHR_ExtP_Orig_DF, Shift = oneP_Shift_Parameters)
    if(!all(round(oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_DF,4) == round(oneP_TARGET_XYZWLHR_ExtP_Coord,4))){browser()}
    
    #browser()
    
    oneP_PRIOR_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
                                                                                  XYZWLHR = oneP_PRIOR_XYZWLHR_ExtR, 
                                                                                  batch=1, # batch=p, 
                                                                                  Shift=oneP_Shift_Parameters, 
                                                                                  ScaleRoI2Plot_First = "Yes")))
    colnames(oneP_PRIOR_XYZWLHR_ExtP_Coord) <- Colnames_XYZWLHR
    

    
    ###############################################################################################################################################################################
    ##############
    # SANITY CHECK (GETTING TO SAME RESULTS WITH DIFFEREND FORMATS AND FUNCTIONS TO CONVERT)
    ##############
    
    oneP_TARGET_XYZWLHR_ExtR_CHECK <- Goffset_To_XYZWLHR_FUN(oneP_TARGET_XYZWLHR_ExtR_Goffset, oneP_PRIOR_XYZWLHR_ExtR)
    
    oneP_TARGET_XYZWLHR_ExtP_Coord_CHECK <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
                                                                                         XYZWLHR = oneP_TARGET_XYZWLHR_ExtR_CHECK, 
                                                                                         batch=1, 
                                                                                         Shift=oneP_Shift_Parameters, 
                                                                                         ScaleRoI2Plot_First = "Yes")))
    colnames(oneP_TARGET_XYZWLHR_ExtP_Coord_CHECK) <- Colnames_XYZWLHR
    
    oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_CHECK <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = NA, 
                                                                                              XYZWLHR = oneP_TARGET_XYZWLHR_ExtP_Orig, 
                                                                                              batch=1, # batch=p, 
                                                                                              Shift=oneP_Shift_Parameters, 
                                                                                              ScaleRoI2Plot_First = "No")))
    colnames(oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_CHECK) <- Colnames_XYZWLHR
    
    if(!all(round(oneP_TARGET_XYZWLHR_ExtP_Coord_CHECK,4) == round(oneP_TARGET_XYZWLHR_ExtP_Coord,4))){browser()}
    
    
    if(!all(round(oneP_TARGET_XYZWLHR_ExtP_Coord_CHECK,4) == round(oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_CHECK,4))){browser()}

    if(!all(round(as.array(oneP_TARGET_XYZWLHR_ExtR_CHECK), 4) == round(as.array(oneP_TARGET_XYZWLHR_ExtR),4))){browser()}
    
    ###############################################################################################################################################################################
    # PLOT LEVEL PLOTTING
    #####################
    
    #############################################
    # GENERATE VERTICES FOR BEST PRIOR AND TARGET (FOR EACH TREE)
    #############################################
    
    oneP_RoI_Dec <-  torch_squeeze(oneP_RoI_Dec)
    oneP_RoI_Dec_Coord <- RoI_NORM_to_COORD_FUN(oneP_RoI_Dec,Shift= oneP_Shift_Parameters)
    
    Tree_ID <- unique(BestWorst_IoU$TID)
    
    # LOOP THROUGH EACH TREE
    for(TT in 1:length(Tree_ID)){
      INDEX_Tree <- which(BestWorst_IoU$TID == Tree_ID[TT])
      oneT_BestWorst_IoU <- BestWorst_IoU[INDEX_Tree,]
      INDEX_Best_PRIOR <- which.max(oneT_BestWorst_IoU$FScore)
      INDEX_FINAL <-INDEX_Tree[INDEX_Best_PRIOR]

      Best_Prior <- oneT_BestWorst_IoU[INDEX_FINAL,]
      
      # ONLY PLOT BEST PRIOR FOR EACH TREE_ID
      #BestPr_oneP_TARGET_XYZWLHR_ExtP_Coord <- oneP_TARGET_XYZWLHR_ExtP_Coord[INDEX_FINAL,] # oneP_TARGET_XYZWLHR_ExtP_Orig_Coord_DF
      BestPr_oneP_TARGET_XYZWLHR_ExtP_Coord <- oneP_TARGET_XYZWLHR_ExtP_Coord[INDEX_FINAL,]
      BestPr_oneP_PRIOR_XYZWLHR_ExtP_Coord <- oneP_PRIOR_XYZWLHR_ExtP_Coord[INDEX_FINAL,]
      BestPr_oneP_Pred_XYZWLHR_ExtP_Coord <- oneP_Pred_XYZWLHR_ExtP_Coord[INDEX_FINAL,]
      
      # GENERATE VERTICES FOR PLOTTING
      oneB_TARGET_Vert <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_TARGET_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL))
      oneB_PRIOR_Vert <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_PRIOR_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL))
      oneB_PRED_Vert <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_Pred_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL))
      
      # Test <- rbind(oneB_PRIOR_Vert, oneB_TARGET_Vert)
      # dplyr::sumarise(Test, c(X, Y, Z))
      # Test_Summary <- as.data.frame(Test %>%
      #               dplyr::summarise(Xmin = min(X),
      #                                Xmax = max(X),
      #                                Ymin = min(Y),
      #                                Ymax = max(Y),
      #                                Zmin = min(Z),
      #                                Zmax = max(Z), .groups = 'drop'))
      
      Prior_Type <-  BestWorst_IoU$Prior_Type[INDEX_FINAL]

      colours <- c( "red","green", "white")
      List_Vert <- list(oneB_PRIOR_Vert, oneB_TARGET_Vert, oneB_PRED_Vert)
      
      # PLOT ALL VOXELS ONLY FOR FIRST TREE
      if(TT == 1){
        Title_Plot <- paste("All P:", PlotID, "PT:", Prior_Type, "TID:",Tree_ID[TT], sep="")
        PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_oneP_15X15_Coord_Orig , List_Vert, Title_Plot, colours, Plot_Colour_Att = "TID") # oneB_Vox_Den_ExtR
      }
      
      # PLOT JUST THE TREE VOXELS
      LAS_Vox_oneP_Trees_15X15_Coord_Orig <- filter_poi(LAS_Vox_oneP_15X15_Coord_Orig, TID > 1)
      
      Title_Plot <- paste("TreeVox P:", PlotID, "PT:", Prior_Type, "TID:",Tree_ID[TT], sep="")
      PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_oneP_Trees_15X15_Coord_Orig , List_Vert, Title_Plot, colours, Plot_Colour_Att = "TID") #oneB_Vox_Den_ExtR

      browser()
      
      # Test_Length <- rbind(oneB_TARGET_Vert[1,], oneB_TARGET_Vert[1,])
      # Test_Length[2,1] <- Test_Length[2,1] +BestPr_oneP_TARGET_XYZWLHR_ExtP_Coord$L_BotBox
      # 
      # LAS_EXTENT<- extent(LAS_Vox_oneP_Trees_15X15_Coord_Orig)
      # lines3d(x = Test_Length$X - LAS_EXTENT@xmin, y = Test_Length$Y - LAS_EXTENT@ymin, z = Test_Length$Z, fill=FALSE, col="light blue", lwd=5)
      # 

      # PLOT ROI REGION AROUND EACH PLOT
      best_RoI_Vox <- as.array(oneP_RoI_Dec_Coord[INDEX_FINAL,][2:7])
      ROI_VERT <- GEN_ROI_VERTICES_FUN(best_RoI_Vox)
      ROI_VERT$X <- ROI_VERT$X - oneP_Shift_Parameters$Shift_Vox_X
      ROI_VERT$Y <- ROI_VERT$Y - oneP_Shift_Parameters$Shift_Vox_Y
      polygon3d(ROI_VERT[1:4,], fill=FALSE, col="yellow")
      polygon3d(ROI_VERT[5:8,], fill=FALSE, col="yellow")
      
      #############################################################################################################################################################
      # ROI LEVEL PLOTTING FOR EACH TREE
      ##################################
      
      oneP_oneT_RoI_Vox <- as.array(oneP_RoI_Vox [INDEX_FINAL,2:7])
      oneP_oneT_RoI_Dec <- c(oneP_oneT_RoI_Vox[1:4]/16, oneP_oneT_RoI_Vox[5:6]/40)
      
      # CLIP VOXELS
      LAS_Vox_oneT <- lasfilter(LAS_Vox_Trees, X >= oneP_oneT_RoI_Dec[1] &
                                  X <= oneP_oneT_RoI_Dec[2] &
                                  Y >= oneP_oneT_RoI_Dec[3] &
                                  Y <= oneP_oneT_RoI_Dec[4] &
                                  Z >= oneP_oneT_RoI_Dec[5] &
                                  Z <= oneP_oneT_RoI_Dec[6])
      
      if(length(which(LAS_Vox_Trees@data$TID == Tree_ID[TT])) != length(which(LAS_Vox_oneT@data$TID == Tree_ID[TT]))) {
        MISSING_POINTS <- length(which(LAS_Vox_Trees@data$TID == Tree_ID[TT])) - length(which(LAS_Vox_oneT@data$TID == Tree_ID[TT]))
        print(paste("RoI MISSING_POINTS: .... ", MISSING_POINTS, "out of ",length(which(LAS_Vox_Trees@data$TID == Tree_ID[TT])), "in tree's total" ))
        
      }

      oneP_TARGET_XYZWLHR_ExtR_DF <- as.data.frame(as.array(oneP_TARGET_XYZWLHR_ExtR))
      colnames(oneP_TARGET_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      oneP_PRIOR_XYZWLHR_ExtR_DF <- as.data.frame(as.array(oneP_PRIOR_XYZWLHR_ExtR))
      colnames(oneP_PRIOR_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      oneP_Pred_XYZWLHR_ExtR_DF <- as.data.frame(as.array(oneP_Pred_XYZWLHR_ExtR))
      colnames(oneP_Pred_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      
      BestPr_oneP_TARGET_XYZWLHR_ExtR <- oneP_TARGET_XYZWLHR_ExtR_DF[INDEX_FINAL,]
      BestPr_oneP_PRIOR_XYZWLHR_ExtR <- oneP_PRIOR_XYZWLHR_ExtR_DF[INDEX_FINAL,]
      BestPr_oneP_Pred_XYZWLHR_ExtR <- oneP_Pred_XYZWLHR_ExtR_DF[INDEX_FINAL,]
      
      BestPr_oneP_TARGET_XYZWLHR_ExtR[,c(9,15)] <- BestPr_oneP_TARGET_XYZWLHR_ExtR[,c(9,15)]*90
      BestPr_oneP_PRIOR_XYZWLHR_ExtR[,c(9,15)] <- BestPr_oneP_PRIOR_XYZWLHR_ExtR[,c(9,15)]*90
      BestPr_oneP_Pred_XYZWLHR_ExtR[,c(9,15)] <- BestPr_oneP_Pred_XYZWLHR_ExtR[,c(9,15)]*90
      
      oneB_TARGET_Vert_ExtR <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_TARGET_XYZWLHR_ExtR, Base_WL = Para_Base_WL/Para_Target_Base))
      oneB_PRIOR_Vert_ExtR <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_PRIOR_XYZWLHR_ExtR, Base_WL = Para_Base_WL/Para_Target_Base))
      oneB_PRED_Vert_ExtR <- as.data.frame(GEN_VERTICES_FROM_XYZWHRT_FUN(BestPr_oneP_Pred_XYZWLHR_ExtR, Base_WL = Para_Base_WL/Para_Target_Base))
      
      Prior_Type <-  BestWorst_IoU$Prior_Type[INDEX_FINAL]
      Title_Plot <- paste("TID (ROI ONLY):",Tree_ID[TT], sep="")
      
      colours <- c( "red","green", "white")
      List_Vert <- list(oneB_PRIOR_Vert_ExtR, oneB_TARGET_Vert_ExtR, oneB_PRED_Vert_ExtR)

      # PLOT JUST THE TREE VOXELS
      LAS_Vox_oneT_TreesOnly <- filter_poi(LAS_Vox_oneT, TID > 1)
      PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_oneT_TreesOnly , List_Vert, Title_Plot, colours, Plot_Colour_Att = "TID") #oneB_Vox_Den_ExtR

      print(paste("Done Plot: .....", p))
      browser()
      } # LOOP TREES
  } # LOOP PLOTS
} # LOOP BATCHES


# 
# oneB_Pred_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneB_Pred_XYZWLHR_ExtR))
# colnames(oneB_Pred_XYZWLHR_ExtR_DF) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
# 
# 
# 
# 
# XYZ_Empty_N <- EMPTY_RoI_VOX_FUN(para_RoI_Pool_Dim_XY, para_RoI_Pool_Dim_Z)
# 
# 
# Test <- EMPTY_VOX_FUN(Vox_Den_Count, Para_Vox_Res, Para_MaxZ_Shrink)
#   
# LAS <- TENSOR_LAS_PLOT_FUN(oneB_oneRoI_Vox_Den, Shift_Data, Vox_Den_Orig, LAS_Orig,
#                     Para_Vox_Res,
#                     Para_Target_Base,
#                     Para_MaxZ_Shrink)
# 
# 
# PLOT_Ver1_Vert2_VOX_FUN
# SCALE_PLOT2RoI_FUN
# PLOT_GT_PRIOR_VOX_FUN
# EMPTY_VOX_FUN
# TENSOR_LAS_RoI_FUN
# 
# 