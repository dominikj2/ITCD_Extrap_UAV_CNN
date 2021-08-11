

#####################
# OPEN THE BEST MODEL
#####################
# DOM DOM DOM !!! PRESENTLY YOU ARE OPENING THE LAST MODEL RAN 
    # BUT YOU MaY WANT TO OPEN UP CSV AND SEE WHEN MODEL VALIDATION STARTS DETERIORATING AND OPEN MODEL JSUT BEFORE THAT POINT
# browser()
FOLDER_SAVED_MODEL <- paste(FOLDER_MAIN_DATA, "/SAVED_MODELS/","RUN", Version_RCode, "_" , RUN_NAME, sep="")

dir.create(file.path(FOLDER_SAVED_MODEL,  "TOTAL_PARA_COUNT_OUTPUT"), showWarnings = FALSE)
Folder_TOTAL_PARA_COUNT <- paste(FOLDER_SAVED_MODEL,  "/TOTAL_PARA_COUNT_OUTPUT", sep="") 

Model_Epochs <- list.files(FOLDER_SAVED_MODEL)

Max_Model <- max(as.numeric(numextract_all(Model_Epochs)))
LastEpoch <- Model_Epochs[which(Model_Epochs == paste("model_Epoch", 300, ".pt", sep=""))]



# COMPUTE NUMBER OF PARAMETERS

Count_Mod_Para <- MODEL_PARA_COUNT_FUN(model_Saved_LIST)

Output_MODEL_STATE_ALL <- Count_Mod_Para[[1]]
Output_OPTIM_ALL <- Count_Mod_Para[[2]]
Summary_Total_Parmeter <- Count_Mod_Para[[3]]

write.csv(Output_MODEL_STATE_ALL, paste(Folder_TOTAL_PARA_COUNT, "/ParaCnt_MODEL", RUN_NAME,".csv", sep=""))
write.csv(Output_OPTIM_ALL, paste(Folder_TOTAL_PARA_COUNT, "/ParaCnt_OPTIM",RUN_NAME ,".csv", sep=""))
write.csv(Summary_Total_Parmeter, paste(Folder_TOTAL_PARA_COUNT, "/ParaCnt_SUMMARY",RUN_NAME ,".csv", sep=""))

# browser()

model_Saved <- model_Saved_LIST$model_Instance

model_Saved$eval()

# browser()
### DOM DOM DOM !!! NOTE THAT YOU SHOULD USE VALIDATION DATA IN FUTURE
# eval_ds <- Valid_ds
# eval_dl <- Valid_dl
eval_ds <- Train_ds
eval_dl <- Train_dl

# GET BATCH FILE/FOLDER NAMES
batch <- eval_dl  %>% torch::dataloader_make_iter() %>% torch::dataloader_next()

# OPEN TENSORS 
INPUT_LIST <- batch[[1]] # list(INPUT_VOX_Den_T, INPUT_ROI_VOX_T,  INPUT_ROI_DEC_T, INPUT_PRIOR_XYZWLHR_T)
TARGET_LIST <- batch[[2]]  # list(TARGET_VOX_TID_T, TARGET_STOCK_T, TARGET_CLASS_T, TARGET_XYZWLHR_T, TARGET_TID_T)

Test_XYZ_Empty_N <- EMPTY_RoI_VOX_FUN(Para_Target_Base, Para_Target_Z_Height)
Test_LAS_Empty_N <- LAS(Test_XYZ_Empty_N)
Test_LAS_Empty_N_all <- Test_LAS_Empty_N
Test_LAS_Empty_N@data$Tensor_Count_Norm  <- 1; Test_LAS_Empty_N@data$Mask  <- 1; 
Test_LAS_Empty_N_Zero <- filter_poi (Test_LAS_Empty_N, (X == 0 | X == 1) & 
                                       (Y == 0 | Y == 1) & 
                                       (Z == 0 | Z == 1))
Test_LAS_Empty_N_Zero@data$TID <- 1

############################
# LOOPING THROUGH EACH PLOT
############################
for (p in 1:batch_size){

  # GENERATE EMPTY VOX (LAS)
  Empty_Vox <- EMPTY_RoI_VOX_FUN(Para_Target_Base, Para_Target_Z_Height)
  LAS_Vox <- LAS(Empty_Vox)
  
  # PLOT SUMMARY    # 1"FlightID"    2 "PlotID"  3 "TID"  4 "Plot_ID"  5 "Prior" 6" Count_IoU"           
                    #[7] "Total_Vox" 8 "Portion_IoU_CorrecIn"  9 "Portion_IoU_WrongIn" 10 "TP" 11 "FN"  12  "FP"                  
                    #[13] "Recall"   14 "Precision"  15"FScore"  16 "PRIOR_Z_TopBox"  17"TID_Z_TopBox"  18 "Diff_ZTopBox"        
                    #[19] "Prior_Type"  20 "RoI_ID"  
  
  # device <- if (FALSE) torch_device("cuda:0") else "cpu"
  
  oneP_BestWorst_IoU <- torch_clone(TARGET_LIST[[3]][p, .., drop = TRUE])$to(device ="cpu") 
  Flight <- as.array(oneP_BestWorst_IoU[1,1]) 
  Plot_ID <- as.array(oneP_BestWorst_IoU[1,2]) 
  TID_RoI <- as.array(oneP_BestWorst_IoU[,3])
  ID_allPriors <- as.array(oneP_BestWorst_IoU[,5])
  
  ############################
  # GET PLOT SPECIFIC TENSORS
  ############################
  
  # INPUT LISTS
  oneP_Vox_Den_ExtP <- torch_clone(INPUT_LIST[[1]][p, .., drop = FALSE])$to(device ="cpu")          # INPUT_VOX_Den_T           1 1 16 16 40   1 CHANNEL PROVIDING DENSITY OF POINTS IN EACH VOXEL
  oneP_RoI_Vox <- torch_clone(INPUT_LIST[[2]][p, .., drop = FALSE])$to(device ="cpu")              # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
  oneP_RoI_Dec <- torch_clone(INPUT_LIST[[3]][p, .., drop = FALSE])$to(device ="cpu")             # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
  oneP_PRIOR_XYZWLHR_ExtP  <- torch_clone(INPUT_LIST[[4]][p, .., drop = FALSE])$to(device ="cpu")   # INPUT_PRIOR_XYZWLHR_T     1 64 16         64 UNIQUE PRIORS WITH XYZWLHR LCOATION
  
  # TARGET LISTS
  oneP_Vox_TID_ExtP <- torch_clone(TARGET_LIST[[1]][p, .., drop = FALSE])$to(device ="cpu")     #  TARGET_VOX_TID_T   1  1 16 16 40
  oneP_TARGET_XYZWLHR_ExtP <- torch_clone(TARGET_LIST[[3]][p, .., drop = FALSE])$to(device ="cpu") 
  oneP_TID <- torch_clone(TARGET_LIST[[4]][p, .., drop = FALSE])$to(device ="cpu")         #  TARGET_TID_T       1 64
  
  # CONVERT ExtP TENSOR TO DF
  oneP_PRIOR_XYZWLHR_ExtP_DF <-as.data.frame(as.array(torch_squeeze(oneP_PRIOR_XYZWLHR_ExtP)))
  colnames(oneP_PRIOR_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
  oneP_TARGET_XYZWLHR_ExtP_DF <-as.data.frame(as.array(torch_squeeze(oneP_TARGET_XYZWLHR_ExtP)))
  colnames(oneP_TARGET_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
  
  ##########################################
  # GENERATING PLOT LAS FILE AND PLOTTING IT (FROM TENSORS)
  ##########################################
  LAS_oneP_ExtP <- TENSOR_LAS_RoI_FUN(oneP_Vox_Den_ExtP,
                                           oneP_Vox_TID_ExtP,
                                           Para_Target_Base,
                                           Para_Target_Z_Height)
  
  LAS_oneP_Trees_ExtP <- filter_poi(LAS_oneP_ExtP, Tensor_Count_Norm > 0)
  LAS_oneP_Trees_ExtP@data$TID <- LAS_oneP_Trees_ExtP@data$Mask
  LAS_oneP_Trees_ExtP@data$GT <- 0
  LAS_oneP_Trees_ExtP@data$Pred <- 0
  Tree_Vox <- LAS_oneP_Trees_ExtP@data
  
  # ##########################################
  # PLOT INPUTS TO MAKE SURE THEY ARE CORRECT
  # ##########################################
  
  #browser()
  for(ggg in 1:oneP_RoI_Dec$size(2)){
    #oneP_TARGET_XYZWLHR_ExtP
    # ExtP DF
    oneP_PRIOR_XYZWLHR_ExtP_DF <- as.data.frame(as.array(torch_squeeze(oneP_PRIOR_XYZWLHR_ExtP)))[ggg,]
    colnames(oneP_PRIOR_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
    oneP_TARGET_XYZWLHR_ExtP_DF <-as.data.frame(as.array(torch_squeeze(oneP_TARGET_XYZWLHR_ExtP)))[ggg,]
    colnames(oneP_TARGET_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR

    oneGT_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_TARGET_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
    onePrior_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_PRIOR_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED

    # PLOT ExtR
    colours <- c( "red","green", "white")
    List_Vert_ExtR <- list(onePrior_Vert_ExtP, oneGT_Vert_ExtP)
    #Title_Plot <- paste("F", Flight,  ":P", Plot_ID,":TID", TID,  ":PT", Prior_Type, sep="")
    Title_Plot <- "Test"
    PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Trees_ExtP, List_Vert_ExtR, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "Tensor_Count_Norm")
    lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
    lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
    lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
    lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue

    oneP_oneRoI_Dec_Vec <- as.array(oneP_RoI_Dec[1,ggg,])[-1]
    Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec) # POLYGON OF ROI AREA
    polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
    polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
    #browser()
    }
  browser()
  #############################
  # GET THE SAVED MODEL OUTPUTS
  #############################
  
  ### DOM DOM DOM !!! FIRST YOU NEED TO RUN THE INITIALISE PROCEDURE
  
  checkpoint_fpath <- paste(FOLDER_SAVED_MODEL, "/",LastEpoch,  sep="")
  # INSTANCE WITH EPOCH = LOADED. USING STATE DICT PARAMETERS
  
  # LOAD SAVED MODEL
 
  model_Saved_LIST <- torch_load(checkpoint_fpath)
  model_Instance_Saved <- model_Saved_LIST$model_state_dict
  Optim_Instance_Saved <- model_Saved_LIST$optimizer_state_dict
  Scheduler_Instance_Saved <-model_Saved_LIST$scheduler_state_dict
  
  List_Loss_Output <- model_Saved_LIST$loss 
  train_loss_XYZWLHR <- List_Loss_Output[[1]] 
  train_loss_CONFIDENCE <- List_Loss_Output[[2]]
  train_loss_FSCORE <- List_Loss_Output[[3]]
  # train_loss_STOCKING <- List_Loss_Output[[4]]
  train_loss_MULTI_TASK <- List_Loss_Output[[4]] 
  
  # INITIALISING THE OPTIMISER
  if(grepl( "ADAM", RUN_NAME, fixed = TRUE)){ 
    OPTIMAL_LR <- 0.001
    optimizer = optim_adam(model_Instance$parameters, lr= OPTIMAL_LR, amsgrad = TRUE) # model_Instance_Para
  }else{
    OPTIMAL_LR <- 0.001
    optimizer <- optim_sgd(model_Instance$parameters, lr= OPTIMAL_LR,   momentum=Para_momentum) # model_Instance_Para
  }
  
  # UPDATE OPTIMISER AND SCHEDULER
  optimizer$load_state_dict(Optim_Instance_Saved)
  
  scheduler <- optimizer %>%
    lr_one_cycle(max_lr = OPTIMAL_LR, epochs = Para_num_epochs, steps_per_epoch = Train_dl$.length())
  scheduler$load_state_dict( Scheduler_Instance_Saved)
  
  # UPDATE MODELPARAMETRES
  model_Instance$load_state_dict(model_Instance_Saved)
  
  # UPDATE LOSSES
  List_Starting_Loss <- model_Saved_LIST$loss
  train_loss_XYZWLHR <- List_Starting_Loss[[1]] 
  train_loss_CONFIDENCE <- List_Starting_Loss[[2]]
  train_loss_FSCORE <- List_Starting_Loss[[3]]
  # train_loss_STOCKING <- List_Starting_Loss[[4]]
  train_loss_MULTI_TASK <- List_Starting_Loss[[4]] 
  
  # AFTER HERE YOU NEED TO RUN A TRAIN OR EVALUATION TO THEN YOU CAN PLOT THE RESULTS.
  
  
  
  
  # model_Saved$load_state_dict(model_Saved_LIST$model_state_dict)  #checkpoint['model_state_dict'])
  # 
  # OUTPUT_LIST <-model_Saved(oneP_Vox_Den_ExtP, 
  #                           oneP_RoI_Vox, 
  #                           oneP_RoI_Dec, 
  #                           oneP_PRIOR_XYZWLHR_ExtP, 
  #                           oneP_TARGET_XYZWLHR_ExtP, 
  #                           oneP_Vox_TID_ExtP, oneP_TID, TARGET_IoU_SUMMARY = oneP_BestWorst_IoU, epoch = model_Saved_LIST$epoch,  Batch_Count = 1)
  # model_Saved <- model_Saved_LIST$model_Instance(oneP_Vox_Den_ExtP, 
  #                                                oneP_RoI_Vox, 
  #                                                oneP_RoI_Dec, 
  #                                                oneP_PRIOR_XYZWLHR_ExtP, 
  #                                                oneP_TARGET_XYZWLHR_ExtP, 
  #                                                oneP_Vox_TID_ExtP, oneP_TID, TARGET_IoU_SUMMARY = oneP_BestWorst_IoU, epoch = model_Saved_LIST$epoch,  Batch_Count = 1)
  OUTPUT_out4 <- OUTPUT_LIST[[1]]
  OUTPUT_out5 <- OUTPUT_LIST[[2]]
  
  # OUT4
  oneP_TARGET_XYZWLHR_ExtR <- torch_squeeze(torch_clone(OUTPUT_out4[[1]]))
  oneP_TARGET_XYZWLHR_ExtR_Goffset <- torch_squeeze(torch_clone(OUTPUT_out4[[2]]))   # COMPUTED USING: XYZWLHR_To_Goffset_FUN(TARGET_XYZWLHR_oneP_allR_ExtR, PRIOR_XYZWLHR_oneP_allR_ExtR, Plot_Output = "No",  Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, use_Tensor = "Yes")    
  oneP_PRIOR_XYZWLHR_ExtR <- torch_squeeze(torch_clone(OUTPUT_out4[[3]]))           # 16 64  16        # 64 ROIS, 16 offsets for locations
  oneP_Vox_Den_ExtR <- torch_squeeze(torch_clone(OUTPUT_out4[[4]]))
  oneP_MASK_ExtR <- torch_squeeze(torch_clone(OUTPUT_out4[[7]]))
  
  # OUT5
  oneP_Pred_XYZWLHR_Goffset_ExtR <- torch_squeeze(torch_clone(OUTPUT_out5[[2]]))      # 16 64  16        # 64 ROIS, 16 offsets for locations

  ######################################################################
  # CONVERT GoffSET TO PREDICT_XYZWLHR (FOR ROI) AND SCALE TO PLOT LEVEL       ### DOM DOM DOM !!! Loc_Loss (smooth_l1) uses TARGET THAT IS XYZWLHR_Goffset # decoded_locs = cxcy_to_xy( gcxgcy_to_cxcy(predicted_locs[i], self.priors_cxcy))
  ######################################################################
  
  oneP_Pred_XYZWLHR_ExtR  <- Goffset_To_XYZWLHR_FUN(oneP_Pred_XYZWLHR_Goffset_ExtR, oneP_PRIOR_XYZWLHR_ExtR, 
                                                              Para_Cnt = Para_TriShpParaCnt, 
                                                              use_Tensor = "Yes")  # oneP_Pred_XYZWLHR_RevGoffset_ExtR
  
  oneP_Pred_XYZWLHR_ExtP <-  SCALE_RoI2PLOT_FUN(RoI_Dec = oneP_RoI_Dec, oneP_Pred_XYZWLHR_ExtR, 
                                                batch=1, Para_Cnt = Para_TriShpParaCnt, Normalised = "Yes",
                                                IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR")
  
  oneP_Pred_XYZWLHR_ExtP_DF <-as.data.frame(as.array(oneP_Pred_XYZWLHR_ExtP))
  colnames(oneP_Pred_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR

  # CONVERT ExtR TENSOR TO DF
  
  oneP_Pred_XYZWLHR_Goffset_ExtR_DF <- as.data.frame(as.array(oneP_Pred_XYZWLHR_Goffset_ExtR))
  colnames(oneP_Pred_XYZWLHR_Goffset_ExtR_DF) <- Colnames_XYZWLHR
  oneP_PRIOR_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_PRIOR_XYZWLHR_ExtR))
  colnames(oneP_PRIOR_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
  oneP_TARGET_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_TARGET_XYZWLHR_ExtR))
  colnames(oneP_TARGET_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
  oneP_Pred_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_Pred_XYZWLHR_ExtR))
  colnames(oneP_Pred_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
  
  ###################################################################################################################
  ###################################################################################################################
  # PERFORM IoU
  ###################################################################################################################
  ###################################################################################################################
  
  # EMPTY IoU DATA.FRAME
  Output_IoU_allPred_allGT_onePlot <- data.frame(Plot_ID = numeric(),
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

  Output_IoU_allPred_oneGT_onePlot_EMPTY <- Output_IoU_allPred_allGT_onePlot
  
  ######################
  # LOOP THROUGH EACH GT
  ######################
  unique_TID <- unique(TID_RoI)
  for(GT in 1:length(unique_TID)){
    Output_IoU_allPred_oneGT_onePlot <- Output_IoU_allPred_oneGT_onePlot_EMPTY
    oneGT_Value <- unique_TID[GT]

    # RESET THE Pred and GT values in EMPTY LAS FILE
    LAS_Vox@data$GT <- 0

    # GENERATE GT VERTICES
    Index_GT <- which(TID_RoI %in% unique_TID[GT])
    oneGT_XYZWLHR_ExtP_DF <- oneP_TARGET_XYZWLHR_ExtP_DF[Index_GT[1],] # ALL oneGT XYZWLHR ARE SAME 
    oneGT_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneGT_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
    oneGT_Vert_ExtP_oneGnd <- oneGT_Vert_ExtP[1:4,]
    oneGT_Vert_ExtP_oneGnd[,3] <- 0
    oneGT_Vert_ExtP_Gnd <- rbind(oneGT_Vert_ExtP_oneGnd, oneGT_Vert_ExtP)

    # GIVE VOXELS IN GT TRISHAPE TID VALUE
    oneGT_Intersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneGT_Vert_ExtP_Gnd), Empty_Vox)
    oneGT_insideVect <- which(oneGT_Intersect%%2 != 0)
    LAS_Vox@data$GT[oneGT_insideVect] <- unique_TID[GT] # 1

    # #######################################
    # # PLOT THE TRISHAPES AROUND THE GT ONLY
    # #######################################
    # 
    # # INTERSECTION WITH JUST TREE VOXELS 
    # LAS_oneP_Trees_ExtP@data$GT <-0
    # oneGT_Intersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneGT_Vert_ExtP_Gnd), Tree_Vox[,1:3])
    # oneGT_insideVect <- which(oneGT_Intersect%%2 != 0)
    # LAS_oneP_Trees_ExtP@data$GT[oneGT_insideVect] <- unique_TID[GT] #1
    # 
    # LAS_GT_Pnts <- filter_poi(LAS_oneP_Trees_ExtP, GT == oneGT_Value) # 1) unique_TID[GT]
    # GT_Count_Vox <- nrow(LAS_GT_Pnts@data)
    # 
    # if(nrow(LAS_GT_Pnts@data) > 0){
    #   LAS_GT_Pnts_Corners <- LAS_GT_Pnts
    #   Test_LAS_Empty_N_Zero@data$TID <- 0; Test_LAS_Empty_N_Zero@data$GT <- 0; Test_LAS_Empty_N_Zero@data$Pred <- 0;
    #   LAS_GT_Pnts_Corners <- rbind(LAS_GT_Pnts_Corners, Test_LAS_Empty_N_Zero)
    #   plot(LAS_GT_Pnts_Corners, size = 6)
    #   bg3d("white")
    # 
    #   #plot(LAS_GT_Pnts)
    #   oneGT_Vert_ExtP_Gnd <- as.matrix(oneGT_Vert_ExtP_Gnd)
    #   oneGT_Vert_ExtP_Gnd <- na.omit(oneGT_Vert_ExtP_Gnd)
    #   #Vertices_Mx_Plot <- oneGT_Vert_ExtP_Gnd
    # 
    #   oneGT_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneGT_Vert_ExtP_Gnd, Box_Levels = 4)
    # 
    #   triangles3d(oneGT_Vert_ExtP_Gnd[Triangles_ID_All_L4,], col="green")
    #   } # IF GT LAS HAS POINTS

  
    #################################
    # LOOP THROUGH EACH PREDICTED RoI
    #################################

    Index_oneGT_allPriors <- which(as.vector(as.array(oneP_TID)) %in% unique_TID[GT])
    Missed_Priors <- c()
    for(PR in 1:length(Index_oneGT_allPriors)){
      Index_GT_PR <- Index_oneGT_allPriors[PR]
      # PriorID <- 

      # VERTICES FOR PREDICTION (Inc GND)
      oneP_oneR_Pred_XYZWLHR_ExtP_DF <- oneP_Pred_XYZWLHR_ExtP_DF[Index_GT_PR,]
      onePred_Vert <- XYZWHR_TO_VERT_FUN(oneP_oneR_Pred_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
      onePred_Vert <- VERTICIES_ORDER_FUN(onePred_Vert, Box_Levels = 3)
      onePred_Vert_oneGnd <- onePred_Vert[1:4,]
      onePred_Vert_oneGnd[,3] <- 0
      onePred_Vert_Gnd <- rbind(onePred_Vert_oneGnd, onePred_Vert)
      
      # PLOT TRISHAPE
      # triangles3d(onePred_Vert_Gnd[Triangles_ID_All_L3,], col="red")

      # INTERSECT VOXELS WITHIN PREDICTED TRISHAPE
      onePred_IntGT = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L3, as.matrix(onePred_Vert_Gnd), Empty_Vox)
      onePred_insideVectGT <- which(onePred_IntGT%%2 != 0)
      LAS_Vox@data$Pred <- 0
      LAS_Vox@data$Pred[onePred_insideVectGT] <- 1

      ####################
      # IoU (PRED AND FGT)
      ####################
      
      # GET ALL VOXELS THAT ARE PRIOR AND GT
      LAS_Vox_GT_Pred <- filter_poi(LAS_Vox, Pred == 1 | GT == oneGT_Value)

      # SUMMARISE IoU (PRED AND FGT)
      IoU_Vox_Pred_GT<- data.frame(  Pred = LAS_Vox_GT_Pred@data$Pred,  GT = LAS_Vox_GT_Pred@data$GT) 
      Summary_onePredoneGT <- as.data.frame(IoU_Vox_Pred_GT  %>%
                                                      dplyr::group_by(Pred, GT) %>%
                                                      dplyr::summarise(Count = length(Pred), .groups = 'drop'))
      Index_TP <- which(Summary_onePredoneGT$Pred > 0 & Summary_onePredoneGT$GT > 0)
      Summary_FSc_IoU_onePredoneGT <- Summary_onePredoneGT[Index_TP,]

      ###############################
      # CALCULATE FSCORE (TP, FN, FP)
      ###############################
      if(nrow(Summary_FSc_IoU_onePredoneGT) > 0){
        Summary_FSc_IoU_onePredoneGT$PriorID <- PR
        Summary_FSc_IoU_onePredoneGT$Total_Vox <- sum(Summary_onePredoneGT$Count)
        Summary_FSc_IoU_onePredoneGT$Portion_IoU_CorrecIn <- Summary_FSc_IoU_onePredoneGT$Count/ Summary_FSc_IoU_onePredoneGT$Total_Vox
        Summary_FSc_IoU_onePredoneGT$Portion_IoU_WrongIn <- (Summary_FSc_IoU_onePredoneGT$Total_Vox -Summary_FSc_IoU_onePredoneGT$Count)/ Summary_FSc_IoU_onePredoneGT$Total_Vox

        Summary_FSc_IoU_onePredoneGT$TP <- 0
        if(length(Index_TP) > 0){  Summary_FSc_IoU_onePredoneGT$TP <- Summary_onePredoneGT$Count[Index_TP] }

        Index_FN <- which(Summary_onePredoneGT$Pred == 0 & Summary_onePredoneGT$GT > 0)
        Summary_FSc_IoU_onePredoneGT$FN <- 0
        if(length(Index_FN) > 0){ Summary_FSc_IoU_onePredoneGT$FN <- Summary_onePredoneGT$Count[Index_FN] }

        Index_FP <- which(Summary_onePredoneGT$Pred == 1 & Summary_onePredoneGT$GT == 0)
        Summary_FSc_IoU_onePredoneGT$FP <- 0
        if(length(Index_FP) > 0){ Summary_FSc_IoU_onePredoneGT$FP <- Summary_onePredoneGT$Count[Index_FP] }

        Summary_FSc_IoU_onePredoneGT$Recall <- Summary_FSc_IoU_onePredoneGT$TP/(Summary_FSc_IoU_onePredoneGT$TP + Summary_FSc_IoU_onePredoneGT$FN)
        Summary_FSc_IoU_onePredoneGT$Precision <- Summary_FSc_IoU_onePredoneGT$TP/(Summary_FSc_IoU_onePredoneGT$TP + Summary_FSc_IoU_onePredoneGT$FP)
        Summary_FSc_IoU_onePredoneGT$FScore <- 2*((Summary_FSc_IoU_onePredoneGT$Recall* Summary_FSc_IoU_onePredoneGT$Precision)/(Summary_FSc_IoU_onePredoneGT$Recall + Summary_FSc_IoU_onePredoneGT$Precision))
        Summary_FSc_IoU_onePredoneGT$Pred_Z_TopBox <- oneP_oneR_Pred_XYZWLHR_ExtP_DF$Z_TopBox
        Output_IoU_allPred_oneGT_onePlot <- rbind(Output_IoU_allPred_oneGT_onePlot, Summary_FSc_IoU_onePredoneGT)
      }else{
        Missed_Priors <- c(Missed_Priors, PR)
        } # nrow(Summary_FSc_IoU_onePredoneGT)
    } # PRIOR LOOP FOR ONE GT

    ###################################################################################################################
    #######
    # NMS
    #######  
    ###################################################################################################################
    
    # #############################
    # # PLOT THE BEST PR FOR EACH GT
    # #############################
    # 
    # # INDEXING BEST PRIOR
    # Index_oneGT_IoUPriors <- Index_oneGT_allPriors[-Missed_Priors]
    # Index_Best_FSc <- which.max(Output_IoU_allPred_oneGT_onePlot$FScore)
    # Index_Best_Prior <- Index_oneGT_IoUPriors[Index_Best_FSc]
    # 
    # Prior_Type <-  as.array(oneP_BestWorst_IoU[Index_Best_Prior,19]) # oneP_BestWorst_IoU$Prior_Type[Index_GT_PR]
    # TID <- as.array(oneP_BestWorst_IoU[Index_Best_Prior,3])
    # Title_Plot <- paste("F", Flight,  ":P", Plot_ID,":TID", TID,  ":PT", Prior_Type, sep="")
    # 
    # #############################
    # # PLOT BEST RoI FOR EACH PLOT
    # #############################
    # # CONVERT TENSOR  INTO LAS (VoxDen AND GT)
    # oneP_oneRoI_Vox_Den_ExtR <- oneP_Vox_Den_ExtR[Index_Best_Prior,]
    # oneP_oneRoI_MASK_ExtR <- oneP_MASK_ExtR[Index_Best_Prior,]
    # 
    # print("MASK TABLE")
    # print( table(as.array(oneP_oneRoI_MASK_ExtR)))
    # 
    # LAS_oneP_ExtR <- TENSOR_LAS_RoI_FUN(oneP_oneRoI_Vox_Den_ExtR,
    #                                     oneP_oneRoI_MASK_ExtR,
    #                                     para_RoI_Pool_Dim_XY,
    #                                     para_RoI_Pool_Dim_Z)
    # 
    # LAS_oneP_Tree_ExtR <- filter_poi(LAS_oneP_ExtR, Tensor_Count_Norm > 0)
    # LAS_oneP_Tree_ExtR@data$TID <- LAS_oneP_Tree_ExtR@data$Mask
    # LAS_oneP_Tree_ExtR@data$GT <- 0
    # LAS_oneP_Tree_ExtR@data$Pred <- 0
    # 
    # Test_LAS_Empty_N_Zero@data$TID <- 0; Test_LAS_Empty_N_Zero@data$GT <- 0; Test_LAS_Empty_N_Zero@data$Pred <- 0;
    # LAS_oneP_Tree_ExtR <- rbind(LAS_oneP_Tree_ExtR, Test_LAS_Empty_N_Zero)
    # 
    # # PREPARE XYZWLHR (ExtR and ExtP) for PRIOR TARGET and Pred
    # oneP_onePR_PRIOR_XYZWLHR_ExtR_DF <- oneP_PRIOR_XYZWLHR_ExtR_DF[Index_Best_Prior,]
    # oneP_onePR_TARGET_XYZWLHR_ExtR_DF <- oneP_TARGET_XYZWLHR_ExtR_DF[Index_Best_Prior,]
    # oneP_onePR_Pred_XYZWLHR_ExtR_DF<- oneP_Pred_XYZWLHR_ExtR_DF[Index_Best_Prior,]
    # 
    # # CONVERT XYZWLHR (TARGET AND PREDICTED) INTO VERTICES
    # oneP_PRIOR_Vert_ExtR <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_PRIOR_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # oneP_TARGET_Vert_ExtR <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_TARGET_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # oneP_Pred_Vert_ExtR <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_Pred_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # 
    # #####################
    # # PREPARING PLOT PLOT
    # #####################
    # 
    # # PLOT ExtR
    # colours <- c( "red","green", "white")
    # List_Vert_ExtR <- list(oneP_PRIOR_Vert_ExtR, oneP_TARGET_Vert_ExtR, oneP_Pred_Vert_ExtR)
    # PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Tree_ExtR, List_Vert_ExtR, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "Tensor_Count_Norm")
    # lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
    # lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
    # lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
    # lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue
    # points3d(oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$X_Base,
    #          oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$Y_Base,
    #          oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$Z_Base, col="yellow", size=7)
    # 
    # # PLOT THE OFFSET INFORMATION
    # PLOT_OFFSET_FUN(oneP_onePR_TARGET_XYZWLHR_ExtR_DF,
    #                 oneP_onePR_Pred_XYZWLHR_ExtR_DF,
    #                 oneP_onePR_PRIOR_XYZWLHR_ExtR_DF)
    # 
    # oneP_oneR_PRIOR_XYZWLHR_ExtP_DF <- oneP_PRIOR_XYZWLHR_ExtP_DF[Index_Best_Prior,]
    # oneP_onePR_TARGET_XYZWLHR_ExtP_DF <- oneP_TARGET_XYZWLHR_ExtP_DF[Index_Best_Prior,]
    # oneP_onePR_Pred_XYZWLHR_ExtP_DF<- oneP_Pred_XYZWLHR_ExtP_DF[Index_Best_Prior,]
    # 
    # oneP_PRIOR_Vert_ExtP <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_oneR_PRIOR_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # oneP_TARGET_Vert_ExtP <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_TARGET_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # oneP_Pred_Vert_ExtP <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_Pred_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt))
    # 
    # # GET VOX WITHIN ROI FOR PLOTTING ExtP
    # 
    # # PLOT ExtP
    # List_Vert_ExtP <- list(oneP_PRIOR_Vert_ExtP, oneP_TARGET_Vert_ExtP, oneP_Pred_Vert_ExtP)
    # # List_Vert_ExtP <- list(oneGT_Vert_ExtP_Gnd)
    # 
    # oneP_oneRoI_Dec_Vec <- as.vector(as.array(oneP_RoI_Dec[,Index_Best_Prior,]))[-1] # unlist(oneList_RoI_Dec)[-1]
    # Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec) # POLYGON OF ROI AREA
    # 
    # # INTERSECT PRED WITH JUST TREE VOXELS
    # oneGT_Intersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L3, as.matrix(oneGT_Vert_ExtP_Gnd), Tree_Vox[,1:3])
    # oneGT_insideVect <- which(oneGT_Intersect%%2 != 0)
    # LAS_oneP_Trees_ExtP@data$Pred[oneGT_insideVect] <- 1
    # 
    # LAS_oneP_Trees_ExtP_RoIArea <- filter_poi(LAS_oneP_Trees_ExtP, X > min(Vert_RoI$X) & X < max(Vert_RoI$X) &
    #                                             Y > min(Vert_RoI$Y) & Y < max(Vert_RoI$Y) &
    #                                             Z > min(Vert_RoI$Z) & Z < max(Vert_RoI$Z))
    # 
    # #LAS_oneP_Trees_ExtP_RoIArea@data <- rbind(LAS_oneP_Trees_ExtP_RoIArea@data, Test_LAS_Empty_N_Zero@data)
    # 
    # PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Trees_ExtP, List_Vert_ExtP, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "TID")
    # polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
    # polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
    # lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
    # lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3)
    # lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
    # lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3)
    # 
    # # TESTING OFFSETS
    # oneP_oneR_TARGET_XYZWLHR_ExtR_Goffset_DF <- as.data.frame(as.array(oneP_TARGET_XYZWLHR_ExtR_Goffset))[Index_Best_Prior,]
    # colnames(oneP_oneR_TARGET_XYZWLHR_ExtR_Goffset_DF) <- Colnames_XYZWLHR
    # 
    # oneP_oneR_Pred_XYZWLHR_Goffset_ExtR_DF <-  as.data.frame(as.array(oneP_Pred_XYZWLHR_Goffset_ExtR))[Index_Best_Prior,] # (Second one is added to prior to get predictions)
    # colnames(oneP_oneR_Pred_XYZWLHR_Goffset_ExtR_DF) <- Colnames_XYZWLHR
    # 
    # # PLOT THE OFFSET INFORMATION
    # PLOT_OFFSET_FUN(oneP_onePR_TARGET_XYZWLHR_ExtP_DF,
    #                 oneP_onePR_Pred_XYZWLHR_ExtP_DF,
    #                 oneP_oneR_PRIOR_XYZWLHR_ExtP_DF)
    # 
    # print(table(Output_IoU_allPred_allGT_onePlot$FScore))
    # print(Summary_FSc_IoU_onePredoneGT)

    # UPDATE ALL THE GT IoU
    Output_IoU_allPred_allGT_onePlot <- rbind(Output_IoU_allPred_allGT_onePlot, Output_IoU_allPred_oneGT_onePlot)
    
    } # GT LOOP

  Output_IoU_allPred_allGT_onePlot
  browser()
} # LOOP PLOT (ONE BATCH)



# Find the Jaccard overlaps between the 8732 priors and N ground truth objects. This will be a tensor of size 8732, N.

# Match each of the 8732 priors to the object with which it has the greatest overlap.


# If a prior is matched with an object with a Jaccard overlap of less than 0.5, then it cannot be said to "contain" the object, and is therefore a negative match. Considering we have thousands of priors, most priors will test negative for an object.

# On the other hand, a handful of priors will actually overlap significantly (greater than 0.5) with an object, and can be said to "contain" that object. These are positive matches.

# Now that we have matched each of the 8732 priors to a ground truth, we have, in effect, also matched the corresponding 8732 predictions to a ground truth.



#######################################################
# PREDICT XYZWLHR USING OFFSET (ExtR) AND PRIORS (ExtR)
#######################################################
### DOM DOM DOM !!! IS THE PRIOR IN THE CORRECT FORMAT
# ALSO ONCE YOU GET THE OFFSET DO YOU NEED OT SUBTRACT IT FROM PRIOR AS SHOWN BELOW ???? 
# OR IS THE ABOVE DECODER CORRECT???

#oneP_Pred_XYZWLHR_ExtR <-torch_subtract(oneP_PRIOR_XYZWLHR_ExtR, oneP_Pred_XYZWLHR_Goffset_ExtR)

# Test_Prior <- as.data.frame(as.array(oneP_PRIOR_XYZWLHR_ExtR))
# Test_Offset <- as.data.frame(as.array(oneP_Pred_XYZWLHR_Goffset_ExtR))
# Test_Pred <- Test_Prior -  Test_Offset



# INTERSECT_TRI_FUN
# TRIANG_ID_ALL_FUN
# NMS_FUN
# EMPTY_VOX_FUN
# EMPTY_VOX_FUN2
# EMPTY_RoI_VOX_FUN



# FOR EACH ROI YOU NEED TO WORK OUT THE LEVEL OF OVERLAP BETWEEN PREDICTED AND TARGET.
# JACCARD OVERLAP BETWEEN TARGET BOXES AND PRIORS_XY
# FOR EACH ROI PRIOR, FIND TARGET WITH MAX OVERLAP

# THEN IDENTIFY THE BEST OVERLAP AND DISCARD THE REST. 
# THEN PREDICT THE STOCKING AND USE IT IN THE LOSS FUNCTION ...

# We don't want a situation where an object is not represented in our positive (non-background) priors -
# 1. An object might not be the best object for all priors, and is therefore not in object_for_each_prior.
# 2. All priors with the object may be assigned as background based on the threshold (0.5).
# SO First, find the prior that has the maximum overlap for each object. 
# Then, assign each object to the corresponding maximum-overlap-prior. (This fixes 1.)
# To ensure these priors qualify, artificially give them an overlap of greater than 0.5. (This fixes 2.)
# Labels for each prior
# Set priors whose overlaps with objects are less than the threshold to be background (no object)

# Encode center-size object coordinates into the form we regressed predicted boxes to
# true_locs[i] = cxcy_to_gcxgcy(xy_to_cxcy(boxes[i][object_for_each_prior]), self.priors_cxcy)  # (8732, 4)
# first you get centre of each box (xy_to_cxcy) ... boundary coordinates (x_min, y_min, x_max, y_max) to center-size coordinates (c_x, c_y, w, h)
# then you encode it using (cxcy_to_gcxgcy)...  Encode bounding boxes (that are in center-size form) w.r.t. the corresponding prior boxes (that are in center-size form).
# For the center coordinates, find the offset with respect to the prior box, and scale by the size of the prior box.
# For the size coordinates, scale by the size of the prior box, and convert to the log-space.
# 
# torch.cat([(cxcy[:, :2] - priors_cxcy[:, :2]) / (priors_cxcy[:, 2:]),  # g_c_x, g_c_y
#            torch.log(cxcy[:, 2:] / priors_cxcy[:, 2:])], 1)  # g_w, g_h

### DOM DOM DOM !!! Note that for labelling using Jacard overlap you are already doing this in prep_data (i.e. between target and priors).
# THE TRUE LOCATIONS NEEDS TO BE NORMALISED WITH RESPECTS TO PRIOR. 
# NOTE THAT W AND H AND L HAVE FORMULA THAT APPLIES LOG PROCEDURE .... torch.log(cxcy[:, 2:] / priors_cxcy[:, 2:])], 1)  # g_w, g_

#########################################################################################################################################################
################
# LOCALISED LOSS
################
#########################################################################################################################################################

### DOM DOM DOM !!! https://github.com/sgrvinod/a-PyTorch-Tutorial-to-Object-Detection 
### NOTE THAT LINK ONLY USES NMS AFTER LOSS COMPUTATION... NOT A PART OF LOSS... 
# ALTHOUGH IT MAY BE USED TO COMPUTE STOCKING LOSS MAYBE???

### DOM DOM DOM !!! INPUTS ARE:

# TARGET_IoU_SUMMARY (12 64  1):  IDENTIFIES THE ROI THAT ARE GOOD ... (12 batch 64 RoI 1 Value (i.e. 0,1,2))
# USED TO INDEX RoI FOR FINAL ASSESSMENT

# pred_XYZWLHR_ExtR_Goffset (12 64 16):  PREDICTED 16 PARAMETER VALUES for each ROI (64) in each batch (12)...  

# TARGET_Goffset_allP_allR_ExtR (12 64 32  8  8 16): Batch, RoI, Channel, X, Y, Z


# https://github.com/sgrvinod/a-PyTorch-Tutorial-to-Object-Detection   (link has 8,732 priors which is much less than 786432 )
# BBox - For each voxel location do I evaluate 64 priors each with 16 parametres (i.e. 64*16 = 1024 channels)
# Predictions will be represented as 8*8*16*64*12 = 786432 * 16 Channels 
# Class - For each voxel locations do I evaluate 64 priors each with 2 parametres (i.e. 64*2 = 128 channels)
# Predictions will be represented as 8*8*16*64*12 = 786432 * 2 Channels
# DOM DOM DOM !!! AND IMPORTANT DIFFERENCE IS THAT YOU HAVE ADOPTED 64 PRIORS FOR THE WHOLE PLOT WHEREAS THEY COMPUTE OFFSET FOR 6 PRIORS IN EACH GRID. 
# YOU DO NOT INTEND TO PREDICT OFFSET OF EACH PRIOR WITHIN EACH VOXEL...
# THE LINK ABOVE USES SMOOTH LOSS AFTER IT COMPUTES IoU. 
# IoU IS TO DETERMINE WHICH PREDICTIONS ARE REASONABLY CLOSE TO TARGET (i.e. HYPERPARAMETER 0.5 MEANS 50% oVERLAP)
# "localization loss is computed only on how accurately we regress positively matched predicted boxes"
# "Since we predicted localization boxes in the form of offsets (g_c_x, g_c_y, g_w, g_h), 
#     we would also need to encode the ground truth coordinates accordingly before we calculate the loss"
# DOM DOM DOM !!! DOES THAT MEAN THAT THE PRIORS NEED TO BE REPRESENTED AS AN OFFSET FROM THE GROUND TRUTH? ...

# NOTES FROM ABOVE LINK
# "each offset is normalized by the corresponding dimension of the prior.
#     This makes sense because a certain offset would be less significant for a larger prior than it would be for a smaller prior."

# https://whatdhack.medium.com/a-deeper-look-at-how-faster-rcnn-works-84081284e1cd
# DOM DOM DOM !!! NOTE THAT HE BELOW PARAGRAPH SUGGESTS THAT THE FINAL DETECTION NETWORK SHOULD HAVE 64*16* Number of classes.....
# The features are cropped ( and scaled ) to 14x14 (eventually max-pooled to 7x7 before entering the Detection Network ) 
# according to the size of the ROIs (for this, ROI width and heights are scaled to the feature size). 
# Fig 4 shows examples of ROIs overlaid on the feature image. The set of cropped features for each image are 
# passed through the Detection Network as a batch.The final dense layers output for each cropped feature, 
# the score and bounding box for each class ( e.g. 256 x C, 256x4C in one-hot encoding form, where C is the number of classes) .







# Initialise model_Saved evaluation

############################################
# GET BATCH SPECIFIC CSV FILES AND LAS FILES
############################################

### DOM DOM DOM !!! REPLACE "Train_ds$DIR" WITH "Valid_ds$DIR" LATER
# Vox_N <- read.csv(paste(eval_ds$DIR[[1]][[1]][[p]], "/", eval_ds$DIR[[2]][[1]][[p]], sep="")) # "_LAS_Vox_N.csv"
# RoI_Vox <- read.csv(paste(eval_ds$DIR[[1]][[2]][[p]], "/", eval_ds$DIR[[2]][[2]][[p]], sep=""))
# RoI_Dec <- read.csv(paste(eval_ds$DIR[[1]][[3]][[p]], "/", eval_ds$DIR[[2]][[3]][[p]], sep=""))

# XYZWLHR_Prior <- read.csv(paste(eval_ds$DIR[[1]][[4]][[p]], "/", eval_ds$DIR[[2]][[4]][[p]], sep=""))
# Index_botBox <- grep("BotBox", colnames(XYZWLHR_Prior))
# if(length(Index_botBox)> 0){ XYZWLHR_Prior <- XYZWLHR_Prior[,-Index_botBox]}

# Stocking <- read.csv(paste(eval_ds$DIR[[1]][[4]][[p]], "/", eval_ds$DIR[[2]][[4]][[p]], sep=""))
# Index_botBox <- grep("BotBox", colnames(Stocking))
# if(length(Index_botBox)> 0){ Stocking <- Stocking[,-Index_botBox]}

# Summary_Prior <- read.csv(paste(eval_ds$DIR[[1]][[5]][[p]], "/", eval_ds$DIR[[2]][[5]][[p]], sep=""))

  # ##################################################################################################################################################################################
  # ##################################################################################################################################################################################
  # ##################################################################################################################################################################################
  # ##################################################################################################################################################################################
  # 
  # 
  # #######################################################################################################################
  # #######################################################################################################################
  # # NMS CALCULATION
  # #################
  # ### NMS: LOOP THROUGH EACH IoU USING INFERRED PRIOR AND TARGET_GT.. 
  # ## GET THE BEST INFERRED PRIORS 
  # # 
  # # Summary_Vox_GT <- data.frame(TID = names(table(LAS_Vox_N@data$TID)), 
  # #                               Vox_Count = as.vector(table(LAS_Vox_N@data$TID)))
  # 
  # browser()
  # Good <- c(9:16, 25:32, 41:48, 57:64) ### DOM DOM DOM !!! NOTE THAT SOME GOOD ARE ACTUALLY NOT GREAT... 
  # Bad <- c(1:8, 17:24, 33:40, 49:56) ### DOM DOM DOM !!! NOTE THAT SOME BAD ARE ACTUALLY QUITE GOOD... 
  # XYZWLHR_Prior_GOOD <- XYZWLHR_Prior[Good, ]
  # XYZWLHR_Prior_BAD<- XYZWLHR_Prior[Bad, ]
  # Pred_XYZWLHR_GOOD <- Pred_XYZWLHR_ExtR[Good, ]
  # Pred_XYZWLHR_BAD <- Pred_XYZWLHR_ExtR[Bad, ]
  # XYZ_plotVox <- data.frame(LAS_Vox_N@data[,1:3])
  # XYZ_plotVox$TID <- 0
  # XYZ_plotVox$InfPrior <- 0
  # # LOOP THROUGH EACH TARGET TREE AND EXTRACT LAS FILES FOR IT
  # for(NT in 1:nrow(oneP_TARGET_XYZWLHR_ExtP)){
  #   
  #   Output_IoU_allInfPrior_allGT_onePlot <- data.frame(Plot_ID = numeric(), 
  #                                                      InfPrior = as.numeric(),
  #                                                      TID = as.numeric(),
  #                                                      Count = as.numeric(),
  #                                                      Total_Vox = as.numeric() ,
  #                                                      Portion_IoU_CorrecIn = as.numeric(),
  #                                                      Portion_IoU_WrongIn = as.numeric(),
  #                                                      TP = as.numeric(),
  #                                                      FN = as.numeric(),
  #                                                      FP = as.numeric(),
  #                                                      Recall = as.numeric(),
  #                                                      Precision = as.numeric(),
  #                                                      FScore = as.numeric())
  #   
  #   oneP_oneT_TARGET_XYZWLHR <- oneP_TARGET_XYZWLHR_ExtP[NT,]
  #   
  #   oneV_TARGET_XYZWLHR <- XYZWHR_TO_VERT_FUN(oneP_oneT_TARGET_XYZWLHR, Base_WL = 0.5/15) # Base_WL IS NORMALISED
  #   
  #   
  #   nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L3, oneV_TARGET_XYZWLHR, XYZ_plotVox[,1:3])
  #   
  #   insideVect <- list(nbIntersect%%2 != 0)
  #   XYZ_plotVox$TID[insideVect[[1]]] <- oneP_oneT_TARGET_XYZWLHR$TID
  #   LAS_Vox_N@data$Prior[insideVect[[1]]] <- oneP_oneT_TARGET_XYZWLHR$TID
  #   #browser()
  #   # LOOP THROUGH EACH InfPrior AND SEE F_SCORE 
  #   for(NP in 1:nrow(Pred_XYZWLHR_ExtR)){
  #     XYZ_plotVox_GT <- XYZ_plotVox
  #     oneP_oneT_Pred_XYZWLHR <- Pred_XYZWLHR_ExtR[NP,]
  #     oneV_Pred_XYZWLHR<- XYZWHR_TO_VERT_FUN(oneP_oneT_Pred_XYZWLHR, Base_WL = 0.5/15) # Base_WL IS NORMALISED
  #     #browser()
  #     nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L3, (oneV_Pred_XYZWLHR), (XYZ_plotVox_GT[,1:3]))
  #     insideVect <- list(nbIntersect%%2 != 0)
  #     XYZ_plotVox_GT$InfPrior[insideVect[[1]]] <- oneP_oneT_Pred_XYZWLHR$PriorID         
  #     
  #     ####################
  #     # FSCORE CALCULATION
  #     ####################
  #     Summary_FSc_IoU_onePredoneGT <- as.data.frame(XYZ_plotVox_GT  %>%
  #                                              dplyr::group_by(TID, InfPrior) %>%
  #                                              dplyr::summarise(Count = length(InfPrior), .groups = 'drop'))
  #     
  #     Summary_FSc_IoU_onePredoneGT <- Summary_FSc_IoU_onePredoneGT[which(Summary_FSc_IoU_onePredoneGT$TID == oneP_oneT_TARGET_XYZWLHR$TID & 
  #                                                            Summary_FSc_IoU_onePredoneGT$InfPrior == oneP_oneT_Pred_XYZWLHR$PriorID ),]
  #     
  #     if(nrow(Summary_FSc_IoU_onePredoneGT) > 0){
  #       Summary_FSc_IoU_onePredoneGT$Total_Vox <- length(which(XYZ_plotVox_GT$InfPrior == oneP_oneT_Pred_XYZWLHR$PriorID))# Summary_Vox_GT$Vox_Count[match(Summary_FSc_IoU_onePredoneGT$TID,Summary_Vox_GT$TID)] ### DOM DOM DOM !!! CHANGES THIS 16/11/20  which(Summary_Vox_GT$TID %in% Summary_FSc_IoU_onePredoneGT$TID)
  #       
  #       # COMPUTING FSCORE AND INTERSECTION OF UNION (IoU)
  #       Summary_FSc_IoU_onePredoneGT$Portion_IoU_CorrecIn <- Summary_FSc_IoU_onePredoneGT$Count/ Summary_FSc_IoU_onePredoneGT$Total_Vox
  #       Summary_FSc_IoU_onePredoneGT$Portion_IoU_WrongIn <- (sum(Summary_FSc_IoU_onePredoneGT$Count) -Summary_FSc_IoU_onePredoneGT$Count)/ sum(Summary_FSc_IoU_onePredoneGT$Count)
  #       Summary_FSc_IoU_onePredoneGT$TP <- Summary_FSc_IoU_onePredoneGT$Count
  #       Summary_FSc_IoU_onePredoneGT$FN <- Summary_FSc_IoU_onePredoneGT$Total_Vox  - Summary_FSc_IoU_onePredoneGT$Count
  #       Summary_FSc_IoU_onePredoneGT$FP <- sum(Summary_FSc_IoU_onePredoneGT$Count) -Summary_FSc_IoU_onePredoneGT$Count
  #       Summary_FSc_IoU_onePredoneGT$Recall <- Summary_FSc_IoU_onePredoneGT$TP/(Summary_FSc_IoU_onePredoneGT$TP + Summary_FSc_IoU_onePredoneGT$FN)
  #       Summary_FSc_IoU_onePredoneGT$Precision <- Summary_FSc_IoU_onePredoneGT$TP/(Summary_FSc_IoU_onePredoneGT$TP + Summary_FSc_IoU_onePredoneGT$FP)
  #       Summary_FSc_IoU_onePredoneGT$FScore <- 2*((Summary_FSc_IoU_onePredoneGT$Recall* Summary_FSc_IoU_onePredoneGT$Precision)/(Summary_FSc_IoU_onePredoneGT$Recall + Summary_FSc_IoU_onePredoneGT$Precision))
  #       Output_IoU_allInfPrior_allGT_onePlot <- rbind(Output_IoU_allInfPrior_allGT_onePlot, Summary_FSc_IoU_onePredoneGT)
  #     } 
  #   } # LOOP InfPriors
  #   browser()
  #   # DOM DOM DOM !!! PERFORM THE LIST OF TASKS INVOLVED IN NMS
  #   # DOM DOM DOM !!! PERFORM DICE LOSS AND MAYBE PUT IT IN LOSS FUNCTION
  # } # LOOP TARGET
  # 
  # 
  # 
  # 
  # 
  # #######################################################################################################################
  # #######################################################################################################################
  # ##############
  # # PLOT XYZWLHR
  # ##############
  # 
  # # DOM DOM DOM!!! YOU NEED TO INVESTIGATE WHAT IS GOING ON WITH THE PRIORS IN YOUR ALGORITHM
  # 
  # 
  # 
  # Col_TID <- c("TID", "PriorID", "TID")
  # list_Colours <- list(c("grey100","Yellow", "orange", "red"),c("grey75","cyan", "cyan3", "blue"),c("grey50","chartreuse", "green", "chartreuse4"))
  # 
  # #DOM DOM DOM !!! USE Summary_Prior TO IDENTIFY THE GOOD AND THE BAD PRIORS AND PLOT THEM SEPERATELY
  # for(GB in 1:2){
  #   if(GB == 1){
  #     LIST_XYZWLHR_Types <- list(oneP_TARGET_XYZWLHR_ExtP, XYZWLHR_Prior_GOOD, Pred_XYZWLHR_GOOD) 
  #   }else{
  #     LIST_XYZWLHR_Types <- list(oneP_TARGET_XYZWLHR_ExtP, XYZWLHR_Prior_BAD, Pred_XYZWLHR_BAD) 
  #   }
  #   
  #   # PLOT THE LAS FILE
  #   LAS_PLOT@data$Color[index_Color_TID_1] <- as.character(Color_ID$ID_TID[index_Color_TID_2])
  #   plot(LAS_PLOT, color="Color", size=2)
  #   print(table(LAS_PLOT@data$TID))
  #   polygon3d(as.vector(c(0, 1, 1, 0)),
  #             as.vector(c(0, 0, 1, 1)),
  #             as.vector(c(0, 0, 0, 0)),
  #             fill = FALSE, col="white", lwd=2)
  #   polygon3d(as.vector(c(0, 1, 1, 0)),
  #             as.vector(c(0, 0, 1, 1)),
  #             as.vector(c(1, 1, 1, 1)),
  #             fill = FALSE, col="white", lwd=2)
  #   
  #   for(TT in 1:length(LIST_XYZWLHR_Types)){
  #     oneXYZWLHR_oneType <- LIST_XYZWLHR_Types[[TT]]
  #     Colours_oneType <- list_Colours[[TT]]
  #     
  #     for(TX in 1:nrow(oneXYZWLHR_oneType)){ ### DOM DOM DOM YOU CHANGED THE INDEX TO 2 (PUT BACK TO 1)
  #       oneVERTICES <- XYZWHR_TO_VERT_FUN(oneXYZWLHR_oneType[TX,], Base_WL = 0.5/15) # Base_WL IS NORMALISED
  #       
  #       points3d(as.vector(oneVERTICES[1:4,1]),
  #                as.vector(oneVERTICES[1:4,2]),
  #                as.vector(oneVERTICES[1:4,3]),
  #                col=Colours_oneType[1], size=10)
  #       polygon3d(as.vector(oneVERTICES[5:8,1]),
  #                 as.vector(oneVERTICES[5:8,2]),
  #                 as.vector(oneVERTICES[5:8,3]),
  #                 fill = FALSE, col=Colours_oneType[2], lwd=2)
  #       polygon3d(as.vector(oneVERTICES[9:12,1]),
  #                 as.vector(oneVERTICES[9:12,2]),
  #                 as.vector(oneVERTICES[9:12,3]),
  #                 fill = FALSE, col=Colours_oneType[3], lwd=2)
  #       polygon3d(as.vector(oneVERTICES[13:16,1]),
  #                 as.vector(oneVERTICES[13:16,2]),
  #                 as.vector(oneVERTICES[13:16,3]),
  #                 fill = FALSE, col=Colours_oneType[4], lwd=2)
  #       
  #     }
  #     #browser()
  #   } # LOOP DATA TYPE
  #   
  # }# LOOP GOOD BAD 
  # browser()
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################################################
  ##################################################################################################################################################################################
  ##################################################################################################################################################################################
  #
  
  # ### DOM DOM DOM !!! YOU NEED TO GET STOCKING AND NMS YOU CAN UNDERTAKE THE EVALUATION!!!
  # 
  # ### GET LOSS FOR THE oneB
  # bce <- nnf_binary_cross_entropy(Pred_XYZWLHR_ExtR, true_XYZWLHR)$to(device = "cpu") %>%
  #   as.numeric()
  # dc <- calc_dice_loss(Pred_XYZWLHR_ExtR, true_XYZWLHR)$to(device = "cpu") %>% as.numeric()
  # cat(sprintf("\nSample %d, bce: %3f, dice: %3f\n", p, bce, dc))
  # 
  # 
  # Pred_mask <- Pred_mask$to(device = "cpu") %>% as.array() %>% .[1, 1, , ]
  # Pred_mask <- ifelse(Pred_mask > 0.5, 1, 0)
  # 
  # img[1, 1, ,] %>% as.array() %>% as.raster() %>% plot()
  # true_mask$to(device = "cpu")[1, 1, ,] %>% as.array() %>% as.raster() %>% plot()
  # Pred_mask %>% as.raster() %>% plot()
  

  
  # #############################################
  # # SCALE ROIS TO PLOT LEVEL TO INVESTIGATE NMS
  # #############################################
  # 
  # #oneINPUT_Vox_Den_Array <- as.array(oneINPUT_Vox_Den$view(c(-1)))
  # 
  # # GET SHIFT INFORMATION
  # 
  # # ### DOM DOM DOM !!! REPLACE "Train_ds$DIR" WITH "Valid_ds$DIR" LATER
  # # Vox_N <- read.csv(paste(eval_ds$DIR[[1]][[1]][[p]], "/", eval_ds$DIR[[2]][[1]][[p]], sep="")) # "_LAS_Vox_N.csv"
  # # RoI_Vox <- read.csv(paste(eval_ds$DIR[[1]][[2]][[p]], "/", eval_ds$DIR[[2]][[2]][[p]], sep=""))
  # # RoI_Dec <- read.csv(paste(eval_ds$DIR[[1]][[3]][[p]], "/", eval_ds$DIR[[2]][[3]][[p]], sep=""))
  # # XYZWLHR_Prior <- read.csv(paste(eval_ds$DIR[[1]][[4]][[p]], "/", eval_ds$DIR[[2]][[4]][[p]], sep=""))
  # # Stocking <- read.csv(paste(eval_ds$DIR[[1]][[4]][[p]], "/", eval_ds$DIR[[2]][[4]][[p]], sep=""))
  # # Summary_Prior <- read.csv(paste(eval_ds$DIR[[1]][[5]][[p]], "/", eval_ds$DIR[[2]][[5]][[p]], sep=""))
  # # oneP_BestWorst_IoU<- read.csv(paste(eval_ds$DIR[[1]][[6]][[p]], "/", eval_ds$DIR[[2]][[6]][[p]], sep=""))
  # # oneP_BestWorst_IoU <- oneP_BestWorst_IoU[,-2] # REMOVE SAMPLE COLUMN
  # # 
  # # Flight <- numextract_all(eval_ds$DIR[[1]][[8]][1])
  # # Flight <- Flight[length(Flight)]
  # # Plot_ID <- oneP_BestWorst_IoU$Plot_ID[1]
  # # 
  # # Shift_Parameters <- read.csv(paste(eval_ds$DIR[[1]][[8]][1], "/CSV/F", as.numeric(Flight),"_Shifts_LAS_XYZWLHR.csv", sep=""))
  # # oneP_Shift_Parameters <- Shift_Parameters[which(Shift_Parameters$Plot_ID == Plot_ID),]
  # 
  # # LAS_Vox_oneP <- readLAS(paste(eval_ds$DIR[[1]][[8]][1], "/LAS/LAS_MP1/LAS_P/F", 1, "_MP1_P", Plot_ID, ".laz", sep=""))
  # 
  # # write.csv(LAS_Vox_N_DF, paste(FOLDER_CSV_VOX_MP_O,"/F",  FID,"_P",   Plot_ID, "_LAS_Vox_N.csv",sep=''), row.names=FALSE) 
  # 
  # 
  # LAS_Orig <- readLAS("//rcgdata/dj806/CNN/THINNED_SAMPLES_TORCH_V14/TRAIN_DATA/Flight_1/LAS/LAS_MP1/LAS_P/F1_MP1_P101.laz")
  # Vox_N <- read.csv(paste(eval_ds$DIR[[1]][[1]][[p]], "/", eval_ds$DIR[[2]][[1]][[p]], sep="")) # "_LAS_Vox_N.csv"
  # LAS_Vox_N <- LAS(Vox_N)
  # LAS_Vox_N_Tree <- filter_poi(LAS_Vox_N, Count > 0)
  # 
  
  
  ##################################################################################################################################################################################
  ##################################################################################################################################################################################
  ##################################################################################################################################################################################
  ##################################################################################################################################################################################
  



  # # ERROR/ DIFFERENCE CALCULATION fOR PRIORS THAT ARE TREE
  # Diff_Pred_target_XYZWLHR_A <- as.array(torch_squeeze(torch_subtract(oneP_TARGET_XYZWLHR_ExtR, oneP_Pred_XYZWLHR_ExtR)))
  # colnames(Diff_Pred_target_XYZWLHR_A) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
  # Good_Test <- Diff_Pred_target_XYZWLHR_A[which(oneP_BestWorst_IoU$Prior_Type == 1),]

 #  ###############################################################################
 #  # RESCALE RESULTS (oneP_Pred_XYZWLHR_ExtR) TO THE SIZE OF THE PLOT oneP_Pred_XYZWLHR_ExtP
 #  ################################################################################
 # 
 #  oneP_Pred_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, XYZWLHR = oneP_Pred_XYZWLHR_ExtR, batch=1, Shift=oneP_Shift_Parameters, ScaleRoI2Plot_First = "Yes")))
 #  colnames(oneP_Pred_XYZWLHR_ExtP_Coord) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  
 #  oneP_TARGET_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, XYZWLHR = oneP_TARGET_XYZWLHR_ExtR, batch=1, Shift=oneP_Shift_Parameters, ScaleRoI2Plot_First = "Yes")))
 #  colnames(oneP_TARGET_XYZWLHR_ExtP_Coord) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  
 #  oneP_PRIOR_XYZWLHR_ExtP_Coord <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
 #                                                                                XYZWLHR = oneP_PRIOR_XYZWLHR_ExtR, 
 #                                                                                batch=p, 
 #                                                                                Shift=oneP_Shift_Parameters, 
 #                                                                                ScaleRoI2Plot_First = "Yes")))
 # colnames(oneP_PRIOR_XYZWLHR_ExtP_Coord) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  
 #  ###################################################
 #  ## PLOT THE TARGET/INFERRED VOXEL, TID, and XYZWLHR
 #  ################################################### 
 #  
 #  # 62 63 64 59 60 61 44 47 57 52 46 48 36 37 38 15 16 43 45 33 34 
 #  # 35 41  42 13 14 39 40 11 12 56 51 55 31 32 23 24 25 26 21 22 19 20 
 #  # 17 18 50  9 10 29 30 27 28  7  8  5  6  3  4 54 58  1  2 49 53
 # 
 #  oneP_TARGET_XYZWLHR_ExtP <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
 #                                                                                XYZWLHR = oneP_TARGET_XYZWLHR_ExtP, 
 #                                                                                batch=p, 
 #                                                                                Shift=oneP_Shift_Parameters, 
 #                                                                                ScaleRoI2Plot_First = "No")))
 #  colnames(oneP_TARGET_XYZWLHR_ExtP) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  
 #  # <- data.frame(as.array(torch_squeeze(oneP_TARGET_XYZWLHR_ExtP)))
 #  # ScaleRoI2Plot_First = "Yes"
 #  
 # # oneP_PRIOR_XYZWLHR_ExtP <- data.frame(as.array(torch_squeeze(oneP_PRIOR_XYZWLHR_ExtP)))
 #  
 #  
 #  oneP_PRIOR_XYZWLHR_ExtP <- data.frame(as.array(SCALE_RoI2Plot_Coord_FUN(RoI_Dec = oneP_RoI_Dec, 
 #                                                                                XYZWLHR = oneP_PRIOR_XYZWLHR_ExtP, 
 #                                                                                batch=p, 
 #                                                                                Shift=oneP_Shift_Parameters, 
 #                                                                                ScaleRoI2Plot_First = "No")))
 #  colnames(oneP_PRIOR_XYZWLHR_ExtP) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  
 #  
 #  for(PR in 1:nrow(oneP_Pred_XYZWLHR_ExtP_Coord)){
 # 
 #    oneP_onePR_TARGET_XYZWLHR_ExtP <- oneP_TARGET_XYZWLHR_ExtP[Index_GT_PR,]
 #    oneP_onePR_PRIOR_XYZWLHR_ExtP <- oneP_PRIOR_XYZWLHR_ExtP[Index_GT_PR,]
 #    oneP_onePR_Pred_XYZWLHR_ExtP_Coord <- oneP_Pred_XYZWLHR_ExtP_Coord[Index_GT_PR,]
 #    oneP_onePR_TARGET_XYZWLHR_ExtP_Coord <- oneP_TARGET_XYZWLHR_ExtP_Coord[Index_GT_PR,]
 #    oneP_onePR_PRIOR_XYZWLHR_ExtP_Coord<- oneP_PRIOR_XYZWLHR_ExtP_Coord[Index_GT_PR,]
 #    
 #    ######################################################
 #    # CONVERT XYZWLHR (TARGET AND PREDICTED) INTO VERTICES
 #    ######################################################
 #    Vert_oneTarget_Orig <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_TARGET_XYZWLHR_ExtP, Base_WL = Para_Base_WL))
 #    Vert_onePRIOR_Orig <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_PRIOR_XYZWLHR_ExtP, Base_WL = Para_Base_WL))
 #    
 #    Vert_onePred <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_Pred_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL))
 #    Vert_oneTarget <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_TARGET_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL) )
 #    Vert_onePRIOR<- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_onePR_PRIOR_XYZWLHR_ExtP_Coord, Base_WL = Para_Base_WL) )
 #      
 #    Prior_Type <-  oneP_BestWorst_IoU$Prior_Type[Index_GT_PR]
 #    Title_Plot <- paste("F:", Flight,  "P:", Plot_ID, "PT:", Prior_Type, sep="")
 #    
 #    colours <- c( "white",  "green", "red", "purple", "blue")
 #    List_Vert <- list(Vert_onePred, Vert_oneTarget, Vert_onePRIOR, Vert_oneTarget_Orig, Vert_onePRIOR_Orig)
 #    PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_oneP, List_Vert, Title_Plot, colours)
 #    
 #    colours <- c( "purple", "blue")
 #    List_Vert <- list( Vert_oneTarget_Orig, Vert_onePRIOR_Orig)
 #    PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_oneP, List_Vert, Title_Plot, colours)
 #    
 #    browser()
 #  }
 #  
 #  ### DOM DOM DOM !!! YOU ARE GENERATING A MASK USING oneP_TID INFORMATION BUT YOU WILL NOT HAVE THIS INFORMATION WHEN EVALUATING YOUR MODEL
 #  ### THE MODEL WILL HAVE TO RUN WITHOUT "oneP_TID"
 # 
 #  
 # 
 # 
 #  
 #  
 #  ######################################################
 #  # CONVERT XYZWLHR (TARGET AND PREDICTED) INTO VERTICES
 #  ######################################################
 #  
 #  Pred_XYZWLHR_A <- as.data.frame(as.array(torch_squeeze(Pred_XYZWLHR_ExtR)))
 #  colnames(Pred_XYZWLHR_A) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #  oneP_TARGET_XYZWLHR_A <- as.data.frame(as.array(torch_squeeze(oneP_TARGET_XYZWLHR_ExtP)))
 #  colnames(oneP_TARGET_XYZWLHR_A) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
 #    
 #  for(PP in 1:nrow(Pred_XYZWLHR_A)){
 #    onePr_Pred_XYZWLHR_A <- Pred_XYZWLHR_A[PP,]
 #    Vert_onePred <- XYZWHR_TO_VERT_FUN(onePr_Pred_XYZWLHR_A, Base_WL = Para_Base_WL/Para_Target_Base)
 #    
 #    oneTarger_XYZWLHR <- oneP_TARGET_XYZWLHR_A[PP,]
 #    Vert_oneTarget <- as.data.frame(XYZWHR_TO_VERT_FUN(oneTarger_XYZWLHR, Base_WL = Para_Base_WL/Para_Target_Base) )
 #    
 #    PLOT_GT_PRIOR_VOX_ROI_FUN(LAS_Vox_N, Vert_onePred, Vert_oneTarget, oneP_RoI_Dec)
 #    
 #    browser()
 #  }
 #  
 #  
 # 
 #  
 # 
 #  
 #  browser()
 #  
 #
  # #################################
  # # CONVERT TO ARRAY AND DATA FRAME
  # #################################
  # 
  # Pred_XYZWLHR_OFFSET_ExtR_A <- as.array(torch_squeeze(Pred_XYZWLHR_OFFSET_ExtR))
  # colnames(Pred_XYZWLHR_OFFSET_ExtR_A) <- colnames(XYZWLHR_Prior)[2:ncol(XYZWLHR_Prior)]
  # oneP_TID_V <- as.array(torch_squeeze(oneP_TID))    #  TARGET_TID_T       1 64
  # Pred_XYZWLHR_OFFSET_ExtR_DF <- data.frame(TID = oneP_TID_V, Pred_XYZWLHR_OFFSET_ExtR_A)
  # 
  # oneP_PRIOR_XYZWLHR_DF <- as.data.frame(as.array(torch_squeeze(oneP_PRIOR_XYZWLHR_ExtP[p, .., drop = FALSE])))    #  TARGET_TID_T       1 64 16
  # oneP_PRIOR_XYZWLHR_DF <- data.frame(TID = oneP_TID_V, oneP_PRIOR_XYZWLHR_DF)
  # Pred_XYZWLHR_DF <- oneP_PRIOR_XYZWLHR_DF[2:ncol(oneP_PRIOR_XYZWLHR_DF)] - Pred_XYZWLHR_OFFSET_ExtR_DF[2:ncol(Pred_XYZWLHR_OFFSET_ExtR_DF)]
  
  
  
  #######################################
  # OUTPUT LIST (RUN "model_Initialised")
  #######################################
  
  # OUTPUT_LIST consists of list(out4, out5)
  # out4 <- list(torch_stack(allBatch_RoI), 
  #              torch_stack(allBatch_RoI_Dice),   ### DOM DOM DOM !!! HAVEN'T USED THIS YET
  #              torch_squeeze(torch_stack(allBatch_Mask), 3)) ### DOM DOM DOM !!! HAVEN'T USED THIS YET
  # out5 <- list(list(OUTPUT_STOCKING, Index_Stocking_NMS), OUTPUT_CLASS, OUTPUT_XYZWLHR)  # Stocking_Output,
  

 
  # out4 <-
      # INPUT ... self$RoI(out3, RoI, TARGET_V, TARGET_TID))
      # OUTPUT list(torch_stack(allBatch_RoI), 
               # torch_stack(allBatch_RoI_Dice),   ### DOM DOM DOM !!! HAVEN'T USED THIS YET
               # torch_squeeze(torch_stack(allBatch_Mask), 3)) ### DOM DOM DOM !!! HAVEN'T USED THIS YET
  # out5 <- 
      # INPUT ... self$out_tr(out4[[1]], Pr) # 64 10 
      # OUTPUT list(OUTPUT_CLASS, OUTPUT_XYZWLHR, OUTPUT_STOCKING)
  # return(list(out4, out5))
  
 
  
  
    
  # LAS_Vox_N <- LAS(Vox_N)
  # 
  # LAS_PLOT <- filter_poi(LAS_Vox_N, TID > 1)
  # n <- length(unique(LAS_PLOT@data$TID))
  # palette <- sample(distinctColorPalette(n))
  # 
  # # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
  # LAS_PLOT@data$Color <- as.character("")
  # Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TID),
  #                        ID_TID = palette)
  # index_Color_TID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_TID)
  # index_Color_TID_2 <- match(LAS_PLOT@data$TID[index_Color_TID_1],
  #                            Color_ID$Unique_TID)

 



# 
# 
# batch <- eval_dl %>% torch::dataloader_make_iter() %>% torch::dataloader_next()
# batch_1 <- batch[[1]]
# batch_1_1 <- batch_1[[1]]
# batch_1_2 <- batch_1[[2]] # 12 64  7
# batch_1_3 <- batch_1[[3]]
# batch_1_4 <- batch_1[[4]]
# 
# batch_2 <- batch[[2]]
# batch_2_1 <- batch_2[[1]]
# batch_2_2 <- batch_2[[2]]
# batch_2_3 <- batch_2[[3]]
# batch_2_4 <- batch_2[[4]] # 12 64 16
# batch_2_5 <- batch_2[[5]]
# 
# batch_3 <- batch[[3]]
# batch_3_1 <- batch_3[[1]]
# batch_3_2 <- batch_3[[2]]
# batch_3_3 <- batch_3[[3]]


# oneV_TARGET_XYZWLHR_Test <- oneV_TARGET_XYZWLHR
# oneV_TARGET_XYZWLHR_Test[which(oneV_TARGET_XYZWLHR_Test > 1)] <- 1
# which(oneV_TARGET_XYZWLHR < 0)

# Point_DF2 <- read.csv("//rcgdata/dj806/CNN/THINNED_SAMPLES_TORCH_V903/Point_DF.csv")
# Vert_oneT2 <- read.csv("//rcgdata/dj806/CNN/THINNED_SAMPLES_TORCH_V903/Vert_oneT.csv")
# nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L3, as.matrix(Vert_oneT2), Point_DF2)
# insideVect <- list(nbIntersect%%2 != 0)
# table(insideVect )

# oneV_TARGET_XYZWLHR <- XYZWHR_TO_VERT_FUN( XYZWLHR_GT[Index_GT_PR,], Base_WL = 0.5/15) # Base_WL IS NORMALISED
# XYZ_plotVox <- Vox_N 




# # LOOK AT PREDICTIONS! 
# # DOM DOM DOM !!! YOU GOT BELOW ERROR WHEN YOU JUST RAN "predict" ... The expected output shape is 1 when it should be 21 .... 
# # this may be a problem
# # ValueError: Mismatch between expected batch size and model output batch size. Output shape = (21, 16), expected output shape = shape (1, 16)
# ###############################################################################################################
# predictions <- predict_on_batch(Compiled_Model, x1) # PREDICTIONS NEED TO BE DONE ON THE LAS FILE TENSOR
# 
# predictions_Class <- predictions[[1]]
# predictions_16Para <- predictions[[2]]
# predictions_16Para_orig <- predictions_16Para
# 
# Input_Prior <- y3[1,,]
# predictions_16Para <- Input_Prior
# ### DOM DOM DOM !!! NOT SURE WHY predictions_16Para ONLY HAS 1 IN FIRST DIMENSION (i.e. not batchsize)
# #predictions_16Para <- k_squeeze(predictions_16Para,1)
# predictions_DF <- as.data.frame(as.matrix(predictions_16Para))
# colnames(predictions_DF) <-colnames(XYZWLHR_plotGT_N)[c(1,2,3,4,5,8,10,11,14,16,6,7,12,13,9,15)+3]
# colnames(predictions_DF)[1:3] <- c("X", "Y", "Z")
# # GROUND BOX
# LAS_PLOT<- lasfilter(LAS_Vox_N, TreeID > 1)
# 
# n <- length(unique(LAS_PLOT@data$TreeID))
# palette <- sample(distinctColorPalette(n))
# 
# # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
# LAS_PLOT@data$Color <- as.character("")
# Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TreeID),
#                        ID_TID = palette)
# index_Color_TID_1 <- which(LAS_PLOT@data$TreeID %in% Color_ID$Unique_TID)
# index_Color_TID_2 <- match(LAS_PLOT@data$TreeID[index_Color_TID_1],
#                            Color_ID$Unique_TID)
# LAS_PLOT@data$Color[index_Color_TID_1] <- as.character(Color_ID$ID_TID[index_Color_TID_2])
# 
# plot(LAS_PLOT, color="Color")
# text3d(min(LAS_PLOT@data$X),min(LAS_PLOT@data$Y),max(LAS_PLOT@data$Z), paste("P",Plot_ID, sep=""), col="white")
# 
# for(CNNP in 1:nrow(predictions_DF)){
#   oneXYZWLHR_CNN <- predictions_DF[CNNP,]
#   oneVeRTices <- XYZWHR_TO_VERT_FUN(oneXYZWLHR_CNN, Base_WL = 0.5/15)
#   
#   points3d(as.vector(oneXYZWLHR_CNN$X),
#            as.vector(oneXYZWLHR_CNN$Y),
#            as.vector(oneXYZWLHR_CNN$Z),
#            fill = FALSE, col="white", lwd=10)
#   
#   polygon3d(as.vector(oneVeRTices$X[5:8]),
#             as.vector(oneVeRTices$Y[5:8]),
#             as.vector(oneVeRTices$Z[5:8]),
#             col="yellow", fill = FALSE, size=2)
#   
#   polygon3d(as.vector(oneVeRTices$X[9:12]),
#             as.vector(oneVeRTices$Y[9:12]),
#             as.vector(oneVeRTices$Z[9:12]),
#             col="orange", fill = FALSE, size=2)
#   
#   polygon3d(as.vector(oneVeRTices$X[13:16]),
#             as.vector(oneVeRTices$Y[13:16]),
#             as.vector(oneVeRTices$Z[13:16]),
#             col="red", fill = FALSE, size=2)
# }
# 
# # PLOT GT OVER THE PREDICIONS
# XYZWLHR_allGT_N_Orig <- List_XYZWLHR_allGT_N_Orig[[1]]
# GT_TID_all <- XYZWLHR_allGT_N_Orig[,1]
# XYZWLHR_allGT_N_Plot <- XYZWLHR_allGT_N_Orig[,-c(1:3)]
# XYZWLHR_allGT_N_Plot <-XYZWLHR_allGT_N_Plot[,c(1,2,3,4,5,8,10,11,14,16,6,7,12,13,9,15)]
# colnames(XYZWLHR_allGT_N_Plot)[1:3] <- c("X", "Y", "Z")
# for(GVP in 1:nrow(XYZWLHR_allGT_N_Plot)){
#   oneXYZWLHR_G <- XYZWLHR_allGT_N_Plot[GVP,]
#   oneVeRTices <- XYZWHR_TO_VERT_FUN(oneXYZWLHR_G, Base_WL = 0.5/15)
#   
#   points3d(as.vector(oneXYZWLHR_G$X),
#            as.vector(oneXYZWLHR_G$Y),
#            as.vector(oneXYZWLHR_G$Z),
#            fill = FALSE, col="green", lwd=10)
#   
#   polygon3d(as.vector(oneVeRTices$X[5:8]),
#             as.vector(oneVeRTices$Y[5:8]),
#             as.vector(oneVeRTices$Z[5:8]),
#             col="green", fill = FALSE, size=4)
#   
#   polygon3d(as.vector(oneVeRTices$X[9:12]),
#             as.vector(oneVeRTices$Y[9:12]),
#             as.vector(oneVeRTices$Z[9:12]),
#             col="darkgreen", fill = FALSE, size=4)
# }
# 
# browser()
# 
# XYZWLHR_allGT_N_Orig <- XYZWLHR_plotGT_N
# XYZWLHR_plotGT_N <- XYZWLHR_allGT_N_Orig
# 
# XYZWLHR_allPriors_N_Orig <- XYZWLHR_plotPriors_N
# XYZWLHR_plotPriors_N <- XYZWLHR_allPriors_N_Orig
# 
# Prior_ID_all <- XYZWLHR_allGT_N_Orig[,1]
# # GENERATE XYZWLHR THAT ASSIGNS THE GT TO THE CORRECT PRIOR ROW (i.e. PRIOR WILL BE USED IN THE LOSS FUNCTION LIKE AN ANCHOR)
# XYZWLHR_allPriors_N_Plot <- XYZWLHR_allPriors_N_Orig[match(BestWorst_FSc_IoU$Prior, XYZWLHR_plotPriors_N$PriorID),]
# XYZWLHR_allPriors_N_Plot <- XYZWLHR_allPriors_N_Plot[,--c(1:3)]
# XYZWLHR_allPriors_N_Plot <-XYZWLHR_allPriors_N_Plot[,c(1,2,3,4,5,8,10,11,14,16,6,7,12,13,9,15)]
# 
# Base_Radius <- 0.5
# 
# plot(LAS_PLOT, color="Color")
# text3d(min(LAS_PLOT@data$X),min(LAS_PLOT@data$Y),max(LAS_PLOT@data$Z), paste("P",Plot_ID, sep=""), col="white")
# 
# for(PVP in 1:nrow(XYZWLHR_allPriors_N_Plot)){
#   
#   oneXYZWLHR_P <- XYZWLHR_allPriors_N_Plot[PVP,]
#   oneVeRTices <- XYZWHR_TO_VERT_FUN(oneXYZWLHR_P, Base_WL = 0.5/15)
#   
#   points3d(as.vector(oneXYZWLHR_P$X),
#            as.vector(oneXYZWLHR_P$Y),
#            as.vector(oneXYZWLHR_P$Z),
#            fill = FALSE, col="white", lwd=10)
#   
#   polygon3d(as.vector(oneVeRTices$X[5:8]),
#             as.vector(oneVeRTices$Y[5:8]),
#             as.vector(oneVeRTices$Z[5:8]),
#             col="yellow", fill = FALSE, size=2)
#   
#   polygon3d(as.vector(oneVeRTices$X[9:12]),
#             as.vector(oneVeRTices$Y[9:12]),
#             as.vector(oneVeRTices$Z[9:12]),
#             col="orange", fill = FALSE, size=2)
#   
#   polygon3d(as.vector(oneVeRTices$X[13:16]),
#             as.vector(oneVeRTices$Y[13:16]),
#             as.vector(oneVeRTices$Z[13:16]),
#             col="red", fill = FALSE, size=2)
# }
# 
# plot(LAS_PLOT, color="Color")
# text3d(min(LAS_PLOT@data$X),min(LAS_PLOT@data$Y),max(LAS_PLOT@data$Z), paste("P",Plot_ID, sep=""), col="white")
# 
# GT_TID_all <- XYZWLHR_allGT_N_Orig[,1]
# XYZWLHR_allGT_N_Plot <- XYZWLHR_allGT_N_Orig[,-c(1:3)]
# XYZWLHR_allGT_N_Plot <-XYZWLHR_allGT_N_Plot[,c(1,2,3,4,5,8,10,11,14,16,6,7,12,13,9,15)]
# colnames(XYZWLHR_allGT_N_Plot)[1:3] <- c("X", "Y", "Z")
# for(GVP in 1:nrow(XYZWLHR_allGT_N_Plot)){
#   oneXYZWLHR_G <- XYZWLHR_allGT_N_Plot[GVP,]
#   oneVeRTices <- XYZWHR_TO_VERT_FUN(oneXYZWLHR_G, Base_WL = 0.5/15)
#   
#   points3d(as.vector(oneXYZWLHR_G$X),
#            as.vector(oneXYZWLHR_G$Y),
#            as.vector(oneXYZWLHR_G$Z),
#            fill = FALSE, col="white", lwd=10)
#   
#   polygon3d(as.vector(oneVeRTices$X[5:8]),
#             as.vector(oneVeRTices$Y[5:8]),
#             as.vector(oneVeRTices$Z[5:8]),
#             col="yellow", fill = FALSE, size=2)
#   
#   polygon3d(as.vector(oneVeRTices$X[9:12]),
#             as.vector(oneVeRTices$Y[9:12]),
#             as.vector(oneVeRTices$Z[9:12]),
#             col="orange", fill = FALSE, size=2)
# }