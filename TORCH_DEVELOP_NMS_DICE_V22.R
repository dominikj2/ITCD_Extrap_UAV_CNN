#####################
# OPEN THE BEST MODEL
#####################
# DOM DOM DOM !!! PRESENTLY YOU ARE OPENING THE LAST MODEL RAN 
    # BUT YOU MaY WANT TO OPEN UP CSV AND SEE WHEN MODEL VALIDATION STARTS DETERIORATING AND OPEN MODEL JSUT BEFORE THAT POINT

FOLDER_SAVED_MODEL <- paste(FOLDER_MAIN_DATA, "/SAVED_MODELS/","RUN", Version_Data, "_" , Test_Run_Folder_Name, sep="")
Model_Epochs <- list.files(FOLDER_SAVED_MODEL)
LastEpoch <- Model_Epochs[which.max(as.numeric(numextract_all(Model_Epochs)))]

# LOAD SAVED MODEL
model_Saved <- torch_load(paste(FOLDER_SAVED_MODEL, "/",LastEpoch,  sep="")) 
model_Saved$eval()

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

# Test_XYZ_Empty_N <- EMPTY_RoI_VOX_FUN(Para_Target_Base, Para_Target_Z_Height)
# Test_LAS_Empty_N <- LAS(Test_XYZ_Empty_N)
# Test_LAS_Empty_N_all <- Test_LAS_Empty_N
# Test_LAS_Empty_N@data$Tensor_Count_Norm  <- 1; Test_LAS_Empty_N@data$Mask  <- 1; 
# Test_LAS_Empty_N_Zero <- filter_poi (Test_LAS_Empty_N, (X == 0 | X == 1) & (Y == 0 | Y == 1) & (Z == 0 | Z == 1))
# Test_LAS_Empty_N_Zero@data$TID <- 1

############################
# LOOPING THROUGH EACH PLOT
############################
for (PP in 1:batch_size){

  # GENERATE EMPTY VOX (LAS)
  Empty_Vox <- EMPTY_RoI_VOX_FUN(Para_Target_Base, Para_Target_Z_Height)
  LAS_Vox <- LAS(Empty_Vox)
  
  # PLOT SUMMARY    # 1"FlightID"    2 "PlotID"  3 "TID"  4 "Plot_ID"  5 "Prior" 6" Count_IoU"           
                    #[7] "Total_Vox" 8 "Portion_IoU_CorrecIn"  9 "Portion_IoU_WrongIn" 10 "TP" 11 "FN"  12  "FP"                  
                    #[13] "Recall"   14 "Precision"  15"FScore"  16 "PRIOR_Z_TopBox"  17"TID_Z_TopBox"  18 "Diff_ZTopBox"        
                    #[19] "Prior_Type"  20 "RoI_ID"  
  
  # PLOT INFORMATION
  oneP_BestWorst_IoU <- torch_clone(TARGET_LIST[[3]][PP, .., drop = TRUE]) 
  Flight <- as.array(oneP_BestWorst_IoU[1,1]) 
  Plot_ID <- as.array(oneP_BestWorst_IoU[1,2]) 
  TID_RoI <- as.array(oneP_BestWorst_IoU[,3])
  ID_allPriors <- as.array(oneP_BestWorst_IoU[,5])
  
  ############################
  # GET PLOT SPECIFIC TENSORS
  ############################
  
  # INPUT LISTS
  oneP_Vox_Den_ExtP <- torch_clone(INPUT_LIST[[1]][PP, .., drop = FALSE])         # INPUT_VOX_Den_T           1 1 16 16 40   1 CHANNEL PROVIDING DENSITY OF POINTS IN EACH VOXEL
  oneP_RoI_Vox <- torch_clone(INPUT_LIST[[2]][PP, .., drop = FALSE])             # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
  oneP_RoI_Dec <- torch_clone(INPUT_LIST[[3]][PP, .., drop = FALSE])            # INPUT_ROI_T               1 64  7         64 ROI LOCATOINS AND ID
  oneP_PRIOR_XYZWLHR_ExtP  <- torch_clone(INPUT_LIST[[4]][PP, .., drop = FALSE])  # INPUT_PRIOR_XYZWLHR_T     1 64 16         64 UNIQUE PRIORS WITH XYZWLHR LCOATION
  
  # TARGET LISTS
  oneP_Vox_TID_ExtP <- torch_clone(TARGET_LIST[[1]][PP, .., drop = FALSE])    #  TARGET_VOX_TID_T   1  1 16 16 40
  oneP_TARGET_XYZWLHR_ExtP <- torch_clone(TARGET_LIST[[4]][PP, .., drop = FALSE])
  oneP_TID <- torch_clone(TARGET_LIST[[5]][PP, .., drop = FALSE])        #  TARGET_TID_T       1 64
  
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

  #############################
  # GET THE SAVED MODEL OUTPUTS
  #############################
  
  OUTPUT_LIST <-model_Saved(oneP_Vox_Den_ExtP, oneP_RoI_Vox, oneP_RoI_Dec, oneP_PRIOR_XYZWLHR_ExtP, oneP_TARGET_XYZWLHR_ExtP, oneP_Vox_TID_ExtP, oneP_TID)
  
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

  Binary_Cross_Entropy_allPred_allGT <- data.frame(bce_Output = numeric(), 
                                                   bce_Target = numeric())
  
  # DOM DOM DOM !!! YOU NEED TO TRY AND GET THIS RESULTS IN DEVELOP_NMS_DICE
  Binary_Accuracy_Percent <- BINARY_ACCURACY_FUN(y_pred = BINARY_SCORE_View, y_test = TARGET_Prior_Type)
  Pred_Accuracy_Percent <- Binary_Accuracy_Percent[[1]]
  Pred_Binary <- Binary_Accuracy_Percent[[2]] # Predicted_Class <- torch_round(torch_sigmoid(BINARY_SCORE_View))
  
  ######################
  # LOOP THROUGH EACH GT
  ######################
  unique_TID <- unique(TID_RoI)

  #################################
  # LOOP THROUGH EACH PREDICTED RoI
  #################################

  # Index_oneGT_allPriors <- which(as.vector(as.array(oneP_TID)) %in% unique_TID[GT])
  # Missed_Priors <- c()
  for(PR in 1:nrow(oneP_Pred_XYZWLHR_ExtR_DF)[1]){
  
    NMS_Output <-  NMS_FUN(TIDs = TID_RoI,
                           Empty_Vox = Empty_Vox, 
                           Subj_XYZWLHR = oneP_TARGET_XYZWLHR_ExtP_DF,  
                           Rest_XYZWLHR = oneP_Pred_XYZWLHR_ExtP_DF,
                           Index_Rest = PR,
                           TriShp_Nodes = Triangles_ID_All_Mx_L4,
                           Para_Base_WL, Para_Target_Base, Para_TriShpParaCnt, Para_Target_Z_Height,
                           Plot_TriShp = "No")
    
    Summary_FSc_IoU_onePredoneGT <- NMS_Output[[1]]
    Binary_Cross_Entropy_onePredoneGT <- NMS_Output[[2]]
    
    #bce_loss <- nnf_binary_cross_entropy(output, target)
    
    colnames(Summary_FSc_IoU_onePredoneGT)[which(colnames(Summary_FSc_IoU_onePredoneGT) == "Subj")] <- "GT"
    colnames(Summary_FSc_IoU_onePredoneGT)[which(colnames(Summary_FSc_IoU_onePredoneGT) == "Rest")] <- "Pred"                                      
    
    Output_IoU_allPred_allGT_onePlot <- rbind(Output_IoU_allPred_allGT_onePlot, Summary_FSc_IoU_onePredoneGT)
    Binary_Cross_Entropy_allPred_allGT <-  rbind(Binary_Cross_Entropy_allPred_allGT, Binary_Cross_Entropy_onePredoneGT)

    browser()

    
  } # PRIOR LOOP FOR ONE GT

  ###################################################################################################################
  #######
  # NMS
  #######  
  # PRED_Goffset_allP_allR_ExtR, 
  # pred_Score, 
  # pred_Stock, 
  # pred_VoxDice, 
  # PRIOR_XYZWLHR_allP_allR_ExtR, 
  # ACTUAL_Goffset_allP_allR_ExtR, 
  # TARGET_IoU_SUMMARY, 
  # TARGET_MASK, 
  # TARGET_STOCK, 
  # INPUT_RoI_Dec
  
  # PRED_SCORE_View
  # conf_loss_all
  
  
  
  # NMS FUNCTION... IT WILL BE USED BETWEEN PRED AND GT AS WELL AS BETWEEN PRED AND PRED .....
  # SEE ... detect_objects ... in https://github.com/sgrvinod/a-PyTorch-Tutorial-to-Object-Detection/blob/43fd8be9e82b351619a467373d211ee5bf73cef8/model.py#L426
  
  
    ###################################################################################################################
  
  # Non Maximum Suppression 
  # Step 1: Select the Tri-Shape with highest objectiveness score (from Log Loss: Binary Cross Entropy)
  # Step 2: Then, compare the overlap (IoU) of this prior Tri-Shape with other prior Tri-Shape
  # Step 3: Remove the Tri-Shapes with overlap (IoU) >50%
  # Step 4: Then, move to Tri-Shape with next highest objectiveness score
  # Step 5: Finally, repeat steps 2-4 until all Tri-shapes have been assigned to GT tree or removed
  
  
  
    # FOR EACH GT FIND THE BEST PRIOR
    # ASSIGN CERTAIN PRIORS AS BACKGROUND IF POOR F-SCORE
  
  # PERFORM NMS WITH THE BELOW TABLE!!! FOR STOCKING ESTIMATE
  # ALSO PERFORM FSCORE ASSESSMENT (1- FSCORE)...LIKE DICE APPROACH
  
  #########################################################################################################################
  #########################################################################################################################
  
  # Find the Jaccard overlaps between the 8732 priors and N ground truth objects. This will be a tensor of size 8732, N.
  
  # Match each of the 8732 priors to the object with which it has the greatest overlap.
  
  
  # If a prior is matched with an object with a Jaccard overlap of less than 0.5, then it cannot be said to "contain" the object, 
      # and is therefore a negative match. Considering we have thousands of priors, most priors will test negative for an object.
  
  # On the other hand, a handful of priors will actually overlap significantly (greater than 0.5) with an object, and can be said to "contain" that object. 
      # These are positive matches.
  
  # Now that we have matched each of the 8732 priors to a ground truth, we have, in effect, also matched the corresponding 8732 predictions to a ground truth.
  
  
  
  
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
  
  

 # browser()
} # LOOP PLOT (ONE BATCH)


#########################################################################################################################
# IN PP LOOP
###########################################
# PLOT INPUTS TO MAKE SURE THEY ARE CORRECT
###########################################

# # INTERSECTION WITH JUST TREE VOXELS
# LAS_oneP_Trees_ExtP@data$GT <-0
# oneGT_Intersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneGT_Vert_ExtP_Gnd), Tree_Vox[,1:3])
# oneGT_insideVect <- which(oneGT_Intersect%%2 != 0)
# LAS_oneP_Trees_ExtP@data$GT[oneGT_insideVect] <- oneGT_Value #1
# 

# for(ggg in 1:dim(oneP_RoI_Dec)[2]){
#   oneP_TARGET_XYZWLHR_ExtP
#   # ExtP DF
#   oneP_PRIOR_XYZWLHR_ExtP_DF <- as.data.frame(as.array(torch_squeeze(oneP_PRIOR_XYZWLHR_ExtP)))[ggg,]
#   colnames(oneP_PRIOR_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
#   oneP_TARGET_XYZWLHR_ExtP_DF <-as.data.frame(as.array(torch_squeeze(oneP_TARGET_XYZWLHR_ExtP)))[ggg,]
#   colnames(oneP_TARGET_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
#   
#   oneGT_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_TARGET_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#   onePrior_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_PRIOR_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#   
#   # PLOT ExtR
#   colours <- c( "red","green", "white")
#   List_Vert_ExtR <- list(onePrior_Vert_ExtP, oneGT_Vert_ExtP)
#   PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Trees_ExtP, List_Vert_ExtR, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "Tensor_Count_Norm")
#   lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
#   lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
#   lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
#   lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue
#   
#   oneP_oneRoI_Dec_Vec <- as.array(oneP_RoI_Dec[1,ggg,])[-1]
#   Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec) # POLYGON OF ROI AREA
#   polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
#   polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
#   # browser()
#   }
################################################################################################################################
# IN GT LOOP start
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
################################################################################################################################
 # IN GT LOOP (END)
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
# Title_Plot <- paste("F", Flight,  ":PP", Plot_ID,":TID", TID,  ":PT", Prior_Type, sep="")
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
