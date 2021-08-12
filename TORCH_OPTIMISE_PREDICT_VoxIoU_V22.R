# FUNCTIONS FOR TRAINING AND VALIDATING

###########################################################################################################################################################
######################
# TRAINING OVER BATCHS
######################
train_batch <- function(b) {
  
  # RESET OPTIMISER
  optimizer$zero_grad()
  # browser()
  # DEFINE MODEL INPUTS
  INPUT_LIST <- b[[1]] 
  TARGET_LIST <- b[[2]]
  FLIGHT_PLOT_ID_LIST <- b[[3]]
  # browser()
  # RUN MODEL AND COMPUTE LOSS
  Loss_all <- RUN_MODEL_LOSS_FUN(INPUT_LIST, TARGET_LIST, FLIGHT_PLOT_ID_LIST, epoch_T, Batch_Count_T) # , Batch_Count
  # browser()
  # OUTPUTS
  OUTPUT_LIST <- Loss_all[[1]]  # Loc_Loss_Positive, conf_loss, loss_1min_VoxVOX_IoU,  multitaskloss, allP_VoxVOX_IoU_posPredGT_T
  Loc_Loss_Positive <- Loss_all[[2]][[1]]
  conf_loss <- Loss_all[[2]][[2]]
  VOX_IoU_loss <- Loss_all[[2]][[3]]
  # Stock_MSE_Loss <- Loss_all[[2]][[4]]
  multitaskloss <- Loss_all[[2]][[4]]
  #allP_VOX_IoU_T <- Loss_all[[2]][[5]]
  
  # BACKPROPAGATION AND OPTIMISE
  multitaskloss$backward()     #print("StartBackward!")  # backward pass calculates the gradients,  but does not update the parameters
  #browser()
  optimizer$step()    #print("StartOptimizer_Para_Update!")# Calling step() on the optimizer actually performs the parameters update
  #browser()
  
  if(!grepl( "ADAM", RUN_NAME, fixed = TRUE)){
    scheduler$step()
    }   #   # print("StartScheduler!")}

  
  
  # ONLY PLOT THE RESULTS EVERY 20 EPOCH
  if(epoch%%50 == 0){
    RUN_PLOT_TRAINED_MODEL <- "Yes"
  }else{
    RUN_PLOT_TRAINED_MODEL <- "No"
  }
  
  if(RUN_PLOT_TRAINED_MODEL == "Yes"){
    
    Empty_Vox <- EMPTY_RoI_VOX_FUN(Para_Target_Base, Para_Target_Z_Height)
    LAS_Vox <- LAS(Empty_Vox)
    ##################
    # ROI LEVEL OUTPUT
    ##################
    OUTPUT_out4 <- OUTPUT_LIST[[1]]
    TARGET_XYZWLHR_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[1]])#$to(device = device))$to(device = device)             # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
    ACTUAL_Goffset_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[2]])#$to(device = device))$to(device = device)     # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
    PRIOR_XYZWLHR_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[3]])#$to(device = device))$to(device = device)    # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
    OUTPUT_Vox_Den_RoI <- torch_clone(OUTPUT_out4[[4]])#$to(device = device))$to(device = device) 
    OUTPUT_RoI_32Ch <- torch_clone(OUTPUT_out4[[5]])#$to(device = device))$to(device = device)    # 16 64 32  8  8 16     # 64 ROIS, EACH WITH 16 CHANNELS FOR ALL VOXELS IN ROI
    pred_VoxDice <- torch_clone(OUTPUT_out4[[6]])#$to(device = device))$to(device = device)       # 16 64 1  8  8 16      # 64 ROIS, EACH WITH  1 "softmax" CHANNELS FOR ALL VOXELS IN ROI
    TARGET_MASK <- torch_clone(OUTPUT_out4[[7]])#$to(device = device))$to(device = device)        # 16 64 8  8 16         # 64 ROIS, BINARY MASK FOR ALL VOXELS IN ROI
    
    # if(RUN_Compute_VOX_IoU_EveryN == "Yes"){
    #   browser()
    #   source(paste(FOLDER_TORCH_CODE, "TORCH_VOX_IoU_Compute",Version_RCode,".R", sep=""))
    # }
   
    
    ##################
    # PLOT LEVEL OUTPUT
    ##################
    OUTPUT_out5 <- OUTPUT_LIST[[2]]
    Binary_Score <- torch_clone(OUTPUT_out5[[1]])#$to(device = device))$to(device = device)        # 16 64  2        # 64 ROIS, 2 Scores for background or tree
    PRED_Goffset_allP_allR_ExtR <- torch_clone(OUTPUT_out5[[2]])#$t
    
    # list(list(INPUT_VOX_Den_T, INPUT_ROI_Vox_T, INPUT_ROI_Dec_T, INPUT_PRIOR_XYZWLHR_T), 
    #      list(TARGET_VOX_TID_T,  IoU_SUMMARY_T, TARGET_XYZWLHR_T, TARGET_TID_T),  # TARGET_STOCK_T,
    #      list(Flight_T, Plot_T, MAIN_DIR))
    
    Number_Plots <- TARGET_LIST[[2]]$size(1)
    for(p in 1:length(Number_Plots)){
      oneP_BestWorst_IoU <- torch_clone(TARGET_LIST[[2]][p, .., drop = TRUE])$to(device ="cpu") 
      Flight <- as.array(oneP_BestWorst_IoU[1,1]) 
      Plot_ID <- as.array(oneP_BestWorst_IoU[1,2]) 
      TID_RoI <- as.array(oneP_BestWorst_IoU[,3])
      PRIOR_Type <- as.array(oneP_BestWorst_IoU[,19])
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
      
      ###########################
      # GET PLOT SPECIFIC OUTPUTS
      ###########################
      oneP_Pred_XYZWLHR_Goffset_ExtR <- torch_squeeze(torch_clone(PRED_Goffset_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu")) 
      oneP_TARGET_XYZWLHR_allR_ExtR <- torch_squeeze(torch_clone(TARGET_XYZWLHR_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))    # torch_clone(OUTPUT_out4[[1]])#$to(device = device))$to(device = device)             # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
      oneP_ACTUAL_Goffset_allR_ExtR <- torch_squeeze(torch_clone(ACTUAL_Goffset_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))   # torch_clone(OUTPUT_out4[[2]])#$to(device = device))$to(device = device)     # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
      oneP_PRIOR_XYZWLHR_allR_ExtR  <- torch_squeeze(torch_clone(PRIOR_XYZWLHR_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))    # torch_clone(OUTPUT_out4[[3]])#$t

      ######################################################################
      # CONVERT GoffSET TO PREDICT_XYZWLHR (FOR ROI) AND SCALE TO PLOT LEVEL       ### DOM DOM DOM !!! Loc_Loss (smooth_l1) uses TARGET THAT IS XYZWLHR_Goffset # decoded_locs = cxcy_to_xy( gcxgcy_to_cxcy(predicted_locs[i], self.priors_cxcy))
      ######################################################################
      
      oneP_Pred_XYZWLHR_ExtR  <- Goffset_To_XYZWLHR_FUN(oneP_Pred_XYZWLHR_Goffset_ExtR, oneP_PRIOR_XYZWLHR_allR_ExtR, 
                                                        Para_Cnt = Para_TriShpParaCnt, 
                                                        use_Tensor = "Yes")  # oneP_Pred_XYZWLHR_RevGoffset_ExtR

      oneP_Pred_XYZWLHR_ExtP <-  SCALE_RoI2PLOT_GPU_FUN(RoI_Dec = oneP_RoI_Dec, oneP_Pred_XYZWLHR_ExtR, 
                                                    batch=1, Para_Cnt = Para_TriShpParaCnt, Normalised = "Yes",
                                                    IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR")
      
      oneP_Pred_XYZWLHR_ExtP_DF <-as.data.frame(as.array(oneP_Pred_XYZWLHR_ExtP))
      colnames(oneP_Pred_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
      
      for(ggg in 1){ #:oneP_RoI_Dec$size(2))
        
        oneP_oneR_TARGET_XYZWLHR_ExtP_DF <- oneP_TARGET_XYZWLHR_ExtP_DF[ggg,]
        oneP_oneR_PRIOR_XYZWLHR_ExtP_DF <- oneP_PRIOR_XYZWLHR_ExtP_DF[ggg,] 
        oneP_oneR_Pred_XYZWLHR_ExtP_DF <- oneP_Pred_XYZWLHR_ExtP_DF[ggg,]
        
        oneGT_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_oneR_TARGET_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
        onePrior_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_oneR_PRIOR_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
        onPred_Vert_ExtP <- XYZWHR_TO_VERT_FUN(oneP_oneR_Pred_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
        
        # PLOT ExtR
        colours <- c( "red","green", "white")
        List_Vert_ExtR <- list(onePrior_Vert_ExtP, oneGT_Vert_ExtP, onPred_Vert_ExtP)
        Title_Plot <- paste("E:", epoch, " P:", Plot_ID, " R:", ggg, " PT:", PRIOR_Type[ggg], sep="")
        PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Trees_ExtP, List_Vert_ExtR, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "Tensor_Count_Norm")
        lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
        lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
        lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
        lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue
        
        oneP_oneRoI_Dec_Vec <- as.array(oneP_RoI_Dec[1,ggg,])[-1]
        Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec) # POLYGON OF ROI AREA
        polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
        polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
       
      }
      
      # WORK OUT HOW TO GET VOX_IoU EVERY 50 ITERATIONS... 
      # browser()
      
      # # CONVERT ExtR TENSOR TO DF
      # 
      # oneP_Pred_XYZWLHR_Goffset_ExtR_DF <- as.data.frame(as.array(oneP_Pred_XYZWLHR_Goffset_ExtR))
      # colnames(oneP_Pred_XYZWLHR_Goffset_ExtR_DF) <- Colnames_XYZWLHR
      # oneP_PRIOR_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_PRIOR_XYZWLHR_ExtR))
      # colnames(oneP_PRIOR_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      # oneP_TARGET_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_TARGET_XYZWLHR_ExtR))
      # colnames(oneP_TARGET_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      # oneP_Pred_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_Pred_XYZWLHR_ExtR))
      # colnames(oneP_Pred_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
      
      
    } # LOOP THROUGH PLOTS
  } # END OF PLOTTING SOURCE CODE
  
  
  
  return(list(OUTPUT_LIST, Loc_Loss_Positive, conf_loss, VOX_IoU_loss,  multitaskloss))  #, allP_VOX_IoU_T  , INPUT_TARGET_ID_LISTS      ............             Stock_MSE_Loss, 
} # END FUNCTION FOR TRAINING OVER BATCHES

###########################################################################################################################################################
########################
# VALIDATING OVER BATCHS
########################

validation_batch <- function(bb) {
  
  # INPUTS
  INPUT_LIST <- bb[[1]]
  TARGET_LIST <- bb[[2]]
  FLIGHT_PLOT_ID_LIST <- b[[3]]
  
  ### RUN MODEL AND COMPUTE LOSS
  Loss_all <- RUN_MODEL_LOSS_FUN(INPUT_LIST, TARGET_LIST, FLIGHT_PLOT_ID_LIST, epoch_T, Batch_Count_T)
  
  # OUTPUTS
  OUTPUT_LIST <- Loss_all[[1]]
  Loc_Loss_Positive <- Loss_all[[2]][[1]]
  conf_loss <- Loss_all[[2]][[2]]
  VOX_IoU_loss <- Loss_all[[2]][[3]]
  # Stock_MSE_Loss <- Loss_all[[2]][[4]]
  multitaskloss <- Loss_all[[2]][[4]]
  #allP_VOX_IoU_T <- Loss_all[[2]][[5]]
  
  return(list(OUTPUT_LIST, Loc_Loss_Positive, conf_loss, VOX_IoU_loss,  multitaskloss)) #  , allP_VOX_IoU_T Stock_MSE_Loss,       , loss_Class$item(), loss_Reg_BBox$item()
} # END FUNCTION FOR VALIDATING OVER BATCHES 

###########################################################################################################################################################

# FOLDER FOR SAVING MODEL RESULTS
dir.create(file.path(FOLDER_MAIN_DATA,  "SAVED_MODELS"), showWarnings = FALSE)
Folder_SAVED_MODELS <- paste(FOLDER_MAIN_DATA,  "/SAVED_MODELS", sep="") 

dir.create(file.path(Folder_SAVED_MODELS,  paste("RUN", Version_RCode, "_", RUN_NAME, sep="") ), showWarnings = FALSE) 
Folder_SAVED_MODELS_Version <- paste(Folder_SAVED_MODELS,  "/RUN", Version_RCode, "_",RUN_NAME, sep="") 

EpochMean_train_loss_XYZWLHR <- c()
EpochMean_train_loss_CONFIDENCE <- c()
EpochMean_train_loss_VOX_IoU <- c()
EpochMean_train_loss_MULTI_TASK <- c()

EpochMean_valid_loss_XYZWLHR <- c()
EpochMean_valid_loss_CONFIDENCE <- c()
EpochMean_valid_loss_VOX_IoU <- c()
EpochMean_valid_loss_MULTI_TASK <- c() 
Time_Taken_DF <- data.frame(Epoch = numeric(), Time_Taken_min =  numeric())

###########################################################################################################################################################
######################
# MODEL INSTANCE SETUP
######################

# LEARNING RATE EITHER ALREADY KNOWN OR 


if(RUN_CONTINUE_RUNNING_MODEL == "Yes"){
  ####################################
  # WORK OUT WHICH EPOCH TO START FROM (i.e. RUN_CONTINUE_RUNNING_MODEL or START FROM BEGINNING)
  ####################################
  Model_Epochs <- list.files(Folder_SAVED_MODELS_Version, pattern="model_Epoch")
  Order_Epoch <- Model_Epochs[order(as.numeric(numextract_all(Model_Epochs)))]
  Start_Epoch_FileName <- Order_Epoch[length(Order_Epoch)-1]                      # RUN FROM SECOND LAST
  Start_Epoch <- as.numeric(numextract_all(Start_Epoch_FileName))
  checkpoint_fpath <- paste(Folder_SAVED_MODELS_Version, "/model_Epoch", Start_Epoch, ".pt" , sep="")
  # INSTANCE WITH EPOCH = LOADED. USING STATE DICT PARAMETERS

  model_Loaded_Saved <- torch_load(checkpoint_fpath)
  model_Instance_Saved <- model_Loaded_Saved$model_state_dict
  Optim_Instance_Saved <- model_Loaded_Saved$optimizer_state_dict
  Scheduler_Instance_Saved <-model_Loaded_Saved$scheduler_state_dict
   
  # INITIALISING THE OPTIMISER
  if(grepl( "ADAM", RUN_NAME, fixed = TRUE)){ 
    optimizer = optim_adam(model_Instance$parameters, lr= OPTIMAL_LR, amsgrad = TRUE) # model_Instance_Para
  }else{  
    optimizer <- optim_sgd(model_Instance$parameters, lr= OPTIMAL_LR,   momentum=Para_momentum) # model_Instance_Para

  }

  # UPDATE OPTIMISER AND SCHEDULER
  optimizer$load_state_dict(Optim_Instance_Saved)
  
  scheduler <- optimizer %>%
    lr_one_cycle(max_lr = OPTIMAL_LR, epochs = Para_num_epochs, steps_per_epoch = Train_dl$.length())
  scheduler$load_state_dict( Scheduler_Instance_Saved)

  # UPDATE MODELPARAMETRES
  model_Instance$load_state_dict(model_Instance_Saved)


  List_Starting_Loss <- model_Loaded_Saved$loss
  train_loss_XYZWLHR <- List_Starting_Loss[[1]] 
  train_loss_CONFIDENCE <- List_Starting_Loss[[2]]
  train_loss_VOX_IoU <- List_Starting_Loss[[3]]
  # train_loss_STOCKING <- List_Starting_Loss[[4]]
  train_loss_MULTI_TASK <- List_Starting_Loss[[4]] 

}else{
  # INSTANCE WITH EPOCH = 1. USING MODEL INSTANCE PARAMETERS 
  Start_Epoch <-1
  
  if(grepl( "ADAM", RUN_NAME, fixed = TRUE)){ 
    optimizer = optim_adam(model_Instance$parameters, lr= OPTIMAL_LR, amsgrad = TRUE) # model_Instance_Para
  }else{  
    optimizer <- optim_sgd(model_Instance$parameters, lr= OPTIMAL_LR,   momentum=Para_momentum) # model_Instance_Para
    scheduler <- optimizer %>%
      lr_one_cycle(max_lr = OPTIMAL_LR, epochs = Para_num_epochs, steps_per_epoch = Train_dl$.length())
  }
  #model_Instance_Para <- model_Instance$parameters
}


###########################################################################################################################################################
###########
# CRITERION
###########

# # DEFINE CRITERIA/LOSS
# criterion_CLASS <- nn_cross_entropy_loss()    # nnf_binary_cross_entropy() #  # nn_nll_loss() #nn_cross_entropy_loss() #nll_loss
# # criterion_STOCKING <- nn_cross_entropy_loss() # DOM DOM DOM !!! YOU MAY WANT TO TRY A LOSS THAT PENALISES MORE IF FURTHER FROM VALUE (MAYBE REG)
# criterion_XYZWLHR <- nn_l1_loss()

##############################################################################################################################################################################################
##############################################################################################################################################################################################
# LOOP THROUGH EPOCHS
##############################################################################################################################################################################################
##############################################################################################################################################################################################


for (epoch in Start_Epoch:Para_num_epochs) {

  # TRACK TIME (EACH EPOCH)
  start_time <- Sys.time()

#  if(Start_Epoch == 1){
    # EMPTY DATA
  train_loss_XYZWLHR <- c()
  train_loss_CONFIDENCE <- c()
  train_loss_VOX_IoU <- c()
  # train_loss_STOCKING <- c()
  train_loss_MULTI_TASK <- c()
#  }

  valid_loss_XYZWLHR <- c()
  valid_loss_CONFIDENCE <- c()
  valid_loss_VOX_IoU <- c()
  valid_loss_MULTI_TASK <- c()
  
  #####################################
  # GENERATE FOLDERS FOR SAVING OUTPUTS
  #####################################

  dir.create(file.path(Folder_SAVED_MODELS_Version,  "TRAIN_OUTPUT"), showWarnings = FALSE)
  Folder_TRAIN_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/TRAIN_OUTPUT", sep="") 
  
  dir.create(file.path(Folder_SAVED_MODELS_Version,  "VALID_OUTPUT"), showWarnings = FALSE)
  Folder_VALID_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/VALID_OUTPUT", sep="") 
  
  dir.create(file.path(Folder_SAVED_MODELS_Version,  "BEST_OUTPUT"), showWarnings = FALSE)
  Folder_BEST_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/BEST_OUTPUT", sep="") 
  
  dir.create(file.path(Folder_SAVED_MODELS_Version,  "PDF_OUTPUT"), showWarnings = FALSE)
  Folder_PDF_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/PDF_OUTPUT", sep="") 
  
  dir.create(file.path(Folder_SAVED_MODELS_Version,  "TIME_TAKEN_OUTPUT"), showWarnings = FALSE)
  Folder_TIME_TAKEN_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/TIME_TAKEN_OUTPUT", sep="") 
  
  #############################################################################################################################################################################################
  ############
  # BATCH LOOP  # ITERATE OVER TRAINING BATCHES
  ############
  ##############################################################################################################################################################################################
  
  model_Instance$train()
  
  # browser()
  # if(epoch > Start_Epoch){ model_Instance$train()}
  
  Batch_Count <- 0 
  print(paste("..................................", RUN_NAME))
  coro::loop(for (b in Train_dl) { 
    # browser()
    start_time_subBatch <- Sys.time()
    # TENSOR OF EPOCH AND BATCH COUNT
    Batch_Count <- Batch_Count + 1 
    Batch_Count_T <- torch_tensor(Batch_Count)
    epoch_T <- torch_tensor(epoch)
    
    
    # OUTPUT TRAIN RESULTS (ONE BATCH)
    OUTPUT_TRAIN_oneB <- train_batch(b) 
    # browser()
    OUTPUT_loss_XYZWLHR <- as.array(OUTPUT_TRAIN_oneB[[2]]$to(device = "cpu"))   # Loc_Loss_Positive, conf_loss, IoU_DiceVox_Loss,  multitaskloss
    OUTPUT_loss_CONFIDENCE <- as.array(OUTPUT_TRAIN_oneB[[3]]$to(device = "cpu"))
    OUTPUT_loss_IoUVox <- as.array(OUTPUT_TRAIN_oneB[[4]]$to(device = "cpu"))
    # OUTPUT_loss_STOCKING <- as.array(OUTPUT_TRAIN_oneB[[5]]$to(device = "cpu"))
    OUTPUT_loss_MULTI_TASK <- as.array(OUTPUT_TRAIN_oneB[[5]]$to(device = "cpu"))
    #OUTPUT_allP_VOX_IoU <- as.array(OUTPUT_TRAIN_oneB[[6]]$to(device = "cpu"))
    
    end_time_subBatch <- Sys.time()
    time_taken_subBatch <- difftime(end_time_subBatch, start_time_subBatch, units='mins')
    
    # PRINT PROGRESS
    
    cat(sprintf("TRAIN E: %s ... B: %s ... loss_XYZWLHR: %s ... loss_CONFIDENCE: %s ...loss_VOX_IoU: %s ... loss_MULTI_TASK: %s... TIME_TAKEN: %s\n",
                round(epoch,1), paste(Batch_Count, "/",length(Train_dl), sep=""),  
                round(OUTPUT_loss_XYZWLHR,3),
                round(OUTPUT_loss_CONFIDENCE,3),
                round(OUTPUT_loss_IoUVox,3),
                # round(OUTPUT_loss_STOCKING,3),      Stocking: %s ...
                round(OUTPUT_loss_MULTI_TASK,3),
                round(time_taken_subBatch,2)))  

 
    # STORE LOSS FOR BATCH
    train_loss_XYZWLHR <- c(train_loss_XYZWLHR, OUTPUT_loss_XYZWLHR)
    train_loss_CONFIDENCE <- c(train_loss_CONFIDENCE, OUTPUT_loss_CONFIDENCE)
    train_loss_VOX_IoU <- c(train_loss_VOX_IoU, OUTPUT_loss_IoUVox)
    # train_loss_STOCKING <- c(train_loss_STOCKING, OUTPUT_loss_STOCKING)
    train_loss_MULTI_TASK <- c(train_loss_MULTI_TASK, OUTPUT_loss_MULTI_TASK)
    #()
    # # OUTPUT CSV FOR ONE BATCH
    # OUTPUT_allP_VOX_IoU_DF <- data.frame(OUTPUT_allP_VOX_IoU)
    # colnames(OUTPUT_allP_VOX_IoU_DF) <- c("Epoch", "Batch" , "Plot", "VOX_IoU")
    # write.csv(OUTPUT_allP_VOX_IoU_DF,  paste(Folder_TRAIN_OUTPUT, "/Train_allP_VOX_IoU_", epoch, "_", Batch_Count, ".csv", sep=""))

  }) # LOOP THROUGH ALL TRAINING BATCHES

  
  #################################
  # SAVE TRAINED MODEL (EACH EPOCH)
  #################################
  # browser()
  # torch_save(model_Instance, paste(Folder_SAVED_MODELS_Version, "/model_Epoch", epoch, ".pt" , sep=""))
  List_Loss_Output <- list(train_loss_XYZWLHR, train_loss_CONFIDENCE, train_loss_VOX_IoU,  train_loss_MULTI_TASK)  # train_loss_STOCKING,
  List_Saved <- list(model_Instance, epoch, model_Instance$state_dict(), optimizer$state_dict(),scheduler$state_dict, List_Loss_Output)
  names(List_Saved) <- c("model_Instance","epoch", 'model_state_dict', 'optimizer_state_dict', 'scheduler_state_dict' , 'loss')
  torch_save(List_Saved, paste(Folder_SAVED_MODELS_Version, "/model_Epoch", epoch, ".pt" , sep=""))
  #browser()
  # SAVE LOSS (ONE EPOCH)
  Train_Output <- data.frame(train_loss_XYZWLHR, train_loss_CONFIDENCE,  train_loss_VOX_IoU, train_loss_MULTI_TASK) # train_loss_STOCKING,
  write.csv(Train_Output, paste(Folder_TRAIN_OUTPUT, "/Train_Output_BatchLevel_Epoch", epoch,".csv", sep=""))
  # browser()
 
  
  ##############################################################################################################################################################################################
  #################
  # VALIDATION LOOP (i.e. EVALUATES THE MODEL USING $eval())
  #################
  ##############################################################################################################################################################################################
  
  # BEGIN VALIDATION
  model_Instance$eval()

  Batch_Count <- 0
  coro::loop(for (bb in Valid_dl) { 
    Batch_Count <- Batch_Count + 1 
    Batch_Count_T <- torch_tensor(Batch_Count)
    
    # OUTPUT VALID RESULTS (ONE BATCH)
    OUTPUT_valid_oneB <- validation_batch(bb)  
    OUTPUT_loss_XYZWLHR <- as.array(OUTPUT_valid_oneB[[2]]$to(device = "cpu"))
    OUTPUT_loss_CONFIDENCE <- as.array(OUTPUT_valid_oneB[[3]]$to(device = "cpu"))
    OUTPUT_loss_IoUVox <- as.array(OUTPUT_valid_oneB[[4]]$to(device = "cpu"))
    # OUTPUT_loss_STOCKING <- as.array(OUTPUT_valid_oneB[[5]]$to(device = "cpu"))
    OUTPUT_loss_MULTI_TASK <- as.array(OUTPUT_valid_oneB[[5]]$to(device = "cpu"))
    #OUTPUT_allP_VOX_IoU <- as.array(OUTPUT_valid_oneB[[6]]$to(device = "cpu"))
    
    # PRINT PROGRESS
    cat(sprintf("VALIDATE E: %s ... B: %s ... loss_XYZWLHR: %s ... loss_CONFIDENCE: %s ...loss_VOX_IoU: %s ... loss_MULTI_TASK: %s\n",
                round(epoch,1), paste(Batch_Count, "/",length(Valid_dl), sep=""),  
                round(OUTPUT_loss_XYZWLHR,3),
                round(OUTPUT_loss_CONFIDENCE,3),
                round(OUTPUT_loss_IoUVox,3),
                round(OUTPUT_loss_MULTI_TASK,3)))  

    # STORE AND SAVE LOSS FOR BATCH
    valid_loss_XYZWLHR <- c(valid_loss_XYZWLHR, OUTPUT_loss_XYZWLHR)
    valid_loss_CONFIDENCE <- c(valid_loss_CONFIDENCE, OUTPUT_loss_CONFIDENCE)
    valid_loss_VOX_IoU <- c(valid_loss_VOX_IoU, OUTPUT_loss_IoUVox)
    valid_loss_MULTI_TASK <- c(valid_loss_MULTI_TASK, OUTPUT_loss_MULTI_TASK)
    
    # OUTPUT_allP_VOX_IoU_DF <- data.frame(OUTPUT_allP_VOX_IoU)
    # colnames(OUTPUT_allP_VOX_IoU_DF) <- c("Epoch", "Batch" , "Plot", "VOX_IoU")
    # write.csv(OUTPUT_allP_VOX_IoU_DF,  paste(Folder_VALID_OUTPUT, "/Valid_allP_VOX_IoU_", epoch, "_", Batch_Count, ".csv", sep=""))
    
  }) # LOOP THROUGH ALL VALIDATION BATCHES

  # SAVE LOSS (ONE EPOCH)
  Valid_Output <- data.frame(valid_loss_XYZWLHR, valid_loss_CONFIDENCE, valid_loss_MULTI_TASK)
  write.csv(Valid_Output, paste(Folder_VALID_OUTPUT, "/Valid_Output_BatchLevel_Epoch", epoch,".csv", sep=""))

  ##############################################################################################################################################################################################
  ####################
  # GENERATING OUTPUTS
  ####################
  
  ###############
  # PRINT RESULTS
  ###############
  
  # train_loss_XYZWLHR train_loss_CONFIDENCE train_loss_MULTI_TASK
  cat(sprintf("\nEpoch %d, TRAIN: loss_MULTI_TASK:%3f, loss_VOX_IoU: %3f, loss_CONFIDENCE: %3f, loss_XYZWLHR: %3f\n",
              epoch, mean(train_loss_MULTI_TASK),  mean(train_loss_VOX_IoU), mean(train_loss_CONFIDENCE), mean(train_loss_XYZWLHR)))
  
  if(!is.null(valid_loss_MULTI_TASK) | !is.null(valid_loss_VOX_IoU) |!is.null(valid_loss_CONFIDENCE) |!is.null(valid_loss_XYZWLHR)){
    cat(sprintf("Epoch %d, VALIDATE: loss_MULTI_TASK:%3f, loss_VOX_IoU: %3f, loss_CONFIDENCE: %3f,  loss_XYZWLHR: %3f\n",
                epoch, mean(valid_loss_MULTI_TASK), mean(valid_loss_VOX_IoU), mean(valid_loss_CONFIDENCE), mean(valid_loss_XYZWLHR)))
  }
  browser()
  EpochMean_train_loss_XYZWLHR <- c(EpochMean_train_loss_XYZWLHR, mean(train_loss_XYZWLHR))
  EpochMean_train_loss_CONFIDENCE <- c(EpochMean_train_loss_CONFIDENCE, mean(train_loss_CONFIDENCE))
  EpochMean_train_loss_VOX_IoU <- c(EpochMean_train_loss_VOX_IoU, mean(train_loss_VOX_IoU))
  EpochMean_train_loss_MULTI_TASK <- c(EpochMean_train_loss_MULTI_TASK, mean(train_loss_MULTI_TASK))
  
  if(!is.null(valid_loss_XYZWLHR)){ EpochMean_valid_loss_XYZWLHR <- c(EpochMean_valid_loss_XYZWLHR, mean(valid_loss_XYZWLHR)) } 
  if(!is.null(valid_loss_CONFIDENCE)){EpochMean_valid_loss_CONFIDENCE <- c(EpochMean_valid_loss_CONFIDENCE, mean(valid_loss_CONFIDENCE)) } 
  if(!is.null(valid_loss_VOX_IoU)){ EpochMean_valid_loss_VOX_IoU <- c(EpochMean_valid_loss_VOX_IoU, mean(valid_loss_VOX_IoU)) } 
  if(!is.null(valid_loss_MULTI_TASK)){ EpochMean_valid_loss_MULTI_TASK <- c(EpochMean_valid_loss_MULTI_TASK, mean(valid_loss_MULTI_TASK))} 

  # TIME TAKEN FOR BATCH
  end_time <- Sys.time()
  time_taken <- difftime(end_time, start_time, units='mins')
  Time_Taken_DF <- rbind(Time_Taken_DF, data.frame(Epoch = epoch, Time_Taken_min = time_taken))
  write.csv(Time_Taken_DF, paste(Folder_TIME_TAKEN_OUTPUT, "/TIME_TAKEN", RUN_NAME, "_", epoch,".csv", sep=""))
  
  print(paste("EPOCH ", epoch, " TIME TAKEN:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ", round(time_taken,1), " min", sep=""))

  ############################
  # PLOTTING TRENDS FOR LOSSES
  ############################
  
  pdf(paste(Folder_PDF_OUTPUT, "/", "LOSS_TRENDS_", epoch, "_T2.pdf", sep=""))
  par(mfrow=c(2,2))
  
  Epoch_Seq <- seq(Start_Epoch,epoch,1)
  if(!is.na(EpochMean_train_loss_XYZWLHR)){plot(Epoch_Seq, EpochMean_train_loss_XYZWLHR, ylab = "Train_loss_XYZWLHR", xlab = "Epoch", pch=16, col="red", type="l",lwd=3, main = "TRAIN LOCATION", sub = RUN_NAME)}
  if(!is.na(EpochMean_train_loss_CONFIDENCE)){plot(Epoch_Seq, EpochMean_train_loss_CONFIDENCE, ylab = "Train_loss_CONFIDENCE", xlab = "Epoch", pch=16, col="green", type="l",lwd=3, main = "TRAIN BINARY", sub = RUN_NAME)}
  if(RUN_MULTI_TASK_LEVEL == 3){
    if(!is.na(EpochMean_train_loss_VOX_IoU)){plot(Epoch_Seq, EpochMean_train_loss_VOX_IoU, ylab = "Train_loss_VOX_IoU", xlab = "Epoch", pch=16, col="orange", type="l",lwd=3, main = "TRAIN VOX_IoU", sub = RUN_NAME)}
  }else{
    plot(1,1, ylab = "Train_loss_VOX_IoU", xlab = "Epoch", type="l",lwd=3, main = "TRAIN") # Start_Epoch
  }
  if(!is.na(EpochMean_train_loss_MULTI_TASK)){plot(Epoch_Seq, EpochMean_train_loss_MULTI_TASK, ylab = "Train_loss_MULTI-TASK", xlab = "Epoch", pch=16, col="blue", type="l",lwd=5, main = "TRAIN MULTI-TASK", sub = RUN_NAME)}
  
  
  if(!is.null(EpochMean_valid_loss_XYZWLHR)){ plot(Epoch_Seq, EpochMean_valid_loss_XYZWLHR,  ylab = "Validate_loss_XYZWLHR", xlab = "Epoch", pch=16, col="red", type="l",lwd=3, main = "VALIDATE LOCATION", sub = RUN_NAME) }
  if(!is.null(EpochMean_valid_loss_CONFIDENCE)){ plot(Epoch_Seq, EpochMean_valid_loss_CONFIDENCE,  ylab = "Validate_loss_CONFIDENCE", xlab = "Epoch", pch=16, col="green", type="l",lwd=3, main = "VALIDATE BINARY", sub = RUN_NAME)} 
  if(RUN_MULTI_TASK_LEVEL == 3){
    if(!is.null(EpochMean_valid_loss_VOX_IoU)){plot(Epoch_Seq, EpochMean_valid_loss_VOX_IoU,  ylab = "Validate_loss_VOX_IoU", xlab = "Epoch", pch=16, col="orange", type="l",lwd=3, main = "VALIDATE VOX_IoU", sub = RUN_NAME)  }  
  }else{
    plot(1,1, ylab = "Train_loss_XYZWLHR", xlab = "Epoch", type="l",lwd=3, main = "VALIDATE")
  }
  if(!is.null(EpochMean_valid_loss_MULTI_TASK)){ plot(Epoch_Seq, EpochMean_valid_loss_MULTI_TASK,  ylab = "Validate_loss_MULTI-TASK", xlab = "Epoch", pch=16, col="blue", type="l",lwd=5, main = "VALIDATE MULTI-TASK", sub = RUN_NAME)}  

  dev.off()
  
  ########################################
  # SAVING THE BEST TRAIN AND VALID RESULT
  ########################################  
  Index_Best_Train <- which(EpochMean_train_loss_MULTI_TASK == min(EpochMean_train_loss_MULTI_TASK)) 
  if(length(Index_Best_Train) > 0){
    if(Index_Best_Train == length(EpochMean_train_loss_MULTI_TASK)){
      Train_Output$Epoch  <- epoch
      write.csv(Train_Output, paste(Folder_BEST_OUTPUT, "/Train_Output_BEST_Epoch.csv", sep=""))
      }
    }
  
  Index_Best_Valid <- which(EpochMean_valid_loss_MULTI_TASK %in% min(EpochMean_valid_loss_MULTI_TASK))
  if(length(Index_Best_Valid) > 0){
    if(Index_Best_Valid == length(EpochMean_valid_loss_MULTI_TASK)){
      Valid_Output$Epoch  <- epoch
      write.csv(Valid_Output, paste(Folder_BEST_OUTPUT, "/Valid_Output_BEST_Epoch.csv", sep=""))
      }
    }
  
  ######################
  # OUTPUT EPOCH RESULTS
  ######################
  if(RUN_MULTI_TASK_LEVEL == 3){
    Epoch_Train_Output <- data.frame(EpochMean_train_loss_XYZWLHR, EpochMean_train_loss_CONFIDENCE, EpochMean_train_loss_VOX_IoU, EpochMean_train_loss_MULTI_TASK)
    Epoch_Valid_Output <- data.frame(EpochMean_valid_loss_XYZWLHR, EpochMean_valid_loss_CONFIDENCE, EpochMean_valid_loss_VOX_IoU, EpochMean_valid_loss_MULTI_TASK)
  }else{
    Epoch_Train_Output <- data.frame(EpochMean_train_loss_XYZWLHR, EpochMean_train_loss_CONFIDENCE,  EpochMean_train_loss_MULTI_TASK)
    Epoch_Valid_Output <- data.frame(EpochMean_valid_loss_XYZWLHR, EpochMean_valid_loss_CONFIDENCE,  EpochMean_valid_loss_MULTI_TASK)
  }
  write.csv(Epoch_Train_Output, paste(Folder_TRAIN_OUTPUT, "/Train_Output_EpochLevel.csv", sep=""))
  write.csv(Epoch_Valid_Output, paste(Folder_VALID_OUTPUT, "/Valid_Output_EpochLevel.csv", sep=""))
  } # LOOP THROUGH ALL THE EPOCHS


####################################################################################################################################
#  COMMENTS REMOVED FROM MAIN SECTION ###################################################################################################################################
####################################################################################################################################


# torch_save(list(epoch, model_Instance$state_dict(), list(optimizer$state, optimizer$param_groups)), paste(Folder_SAVED_MODELS_Version, "/model_Epoch", epoch, ".pt" , sep=""))
# names(Saved_Model) <- c('epoch', 'model_state_dict', 'optimizer_state_dict')
# 
# optimizer = optim_sgd(model_Initialised$parameters, lr= OPTIMAL_LR,   momentum=Para_momentum)
# # Print optimizer's state_dict
# print("Optimizer's state_dict:")
# Model_Optim <- model_Instance$stateModel_Optim
# for (var_name in 1: length(Model_Optim$state_dict())){
#   print(var_name, "\t", optimizer.state_dict()[var_name])
# }
# 
#   #   'epoch': epoch,
# #   'model_state_dict': Saved_Model$state_dict(),
# #   'optimizer_state_dict': ,
# #   'loss': loss,
# #   ...
# # }, PATH)
# # epoch
# 
# # https://pytorch.org/tutorials/beginner/saving_loading_models.html#what-is-a-state-dict
# # FOR LATER LOADING THE MODEL SEE: https://blogs.rstudio.com/ai/posts/2020-11-30-torch-brain-segmentation/


#browser()
# DEFINE OPTIMISER AND SCHEDULAR
# if(RUN_COMPUTE_LEARNING_RATE == "No"){ OPTIMAL_LR <- 0.005} # OTHERWISE THE OPTIMAL LEARNING RATE WAS COMPUTED USING TORCH_LEARNING_RATE_V7.R
# if(RUN_COMPUTE_LEARNING_RATE == "No"){ OPTIMAL_LR <- 0.0005}

# https://github.com/sgrvinod/a-PyTorch-Tutorial-to-Object-Detection/blob/master/train.py
### DOM DOM DOM !!! DOES THE SCHEDULER ADJUST THE DECAY RATE OR DO WE NEED TO DO SO WITH BELOW APPROACH... I THINK ITS ALREAD DYNAMIC 
# # Decay learning rate at particular epochs
# if (epoch in decay_lr_at{
#   adjust_learning_rate(optimizer, decay_lr_to)
# }



# loss <- TOTAL_LOSS
# browser()
#browser()
#is_torch_dtype(x)

# https://discuss.pytorch.org/t/how-to-learn-the-weights-between-two-losses/39681/36
# COMMENT 35 in above link  related to multi class loss....
# Do not want to complicate things? Accumulate gradients.
# 
# loss1.backward()
# loss2.backward()
# optimizer.step()
# 



####################################################################################################################################
####################################################################################################################################
####################################################################################################################################



##################################
 # ALTERNATIVE LOSSES AND CRITERIA
##################################

#optimizer <- optim_rmsprop(model_Instance$parameters, lr = 0.1)#, Para_momentum = 0.9  #optimizer <- optim_adam(model_Instance$parameters)

### DOM DOM DOM !!! DO YOU WANT TO USE BINARY CROSS ENTROPY INSTEAD OF nn_cross_entropy_loss() FOR TREE OR NOT TREE CLASS ??
# criterion_BCE_PRIOR_CLASS <- nnf_binary_cross_entropy()
# criterion_DICE_VOX <- DICE_LOSS_FUN()




############################################################################################################################################################################################################
############################################################################################################################################################################################################
## REMOVED BEFORE RE-WRITING PYTORCH SSD ##########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################

#



# OUTPUT_TRAIN_LOSS <- c(OUTPUT_TRAIN_LOSS, OUTPUT_LOSS_oneB)
#
# # OUTPUT_THREE_LOSSES <- OUTPUT_TRAIN_oneB[[3]]
# # loss_XYZWLHR <- as.array(OUTPUT_THREE_LOSSES[[3]])
# #
# # INPUT_DATA <- b[[1]] # THE FOURTH LIST IS SUMMARY DATA THAT MAY NO BE USEFUL
# # TARGET_DATA <- b[[2]]
# 
# 
# ### DOM DOM DOM !!! FOR EACH ROI YOU NEED A MASK OF THE TREE THAT YOU ARE TRYING TO PREDICT. YOU PREDICT THAT TREE (VOXEL LEVEL PREDICTIONS)
#   # AND USE THE DICE COEFFICIENT AS THE LOSS FUNCTION TO GET IT CORRECTLY...
#   # ... NOT SURE AT WHAT STAGE YOU DO THIS ...

# # DOM DOM DOM !!! BELOW IS AN ACCURACY ASSESSMENT AND HAS NOTHING TO DO WITH THE OPTIMISATION PROCEDURE....
# # YOU ARE JUST TRACKING THE ACCURACTY TO SEE IF THINGS A PROGRESSING BUT NOT DRIVING ANY RESULST WITH BELOW
# 
# 
# #############################
# # CALCULATING CORREST RESULTS
# #############################
# 
# # PRIOR CLASS
# PREDICT_CLASS <- torch_max(OUTPUT_CLASS_oneB$data(), dim = 3)[[2]] # Has dimension 2,21
# OBS_CLASS <- torch_squeeze(TARGET_DATA[[2]] - 1L) # USE NEGATIVE
# # OBS_CLASS <- torch_squeeze(INPUT_DATA[[4]])
# CORRECT_CLASS_oneB <- (PREDICT_CLASS == OBS_CLASS)$sum()$item()/(OBS_CLASS$size(1)*OBS_CLASS$size(2))
# TOTAL_CLASS <<- TOTAL_CLASS + (OBS_CLASS$size(1)*OBS_CLASS$size(2))
# CORRECT_CLASS_all <<- CORRECT_CLASS_all + (PREDICT_CLASS == OBS_CLASS)$sum()$item()
# 
# # STOCKING (F-SCORE)
# PREDICT_STOCK <- torch_max(OUTPUT_STOCKING_oneB$data(), dim = 2)[[2]] # Has dimension 2,21
# OBS_STOCK <- torch_squeeze(TARGET_DATA[[1]]) # Has dimension 2,21,1
# 
# ERROR_STOCK <- OBS_STOCK - PREDICT_STOCK
# tp =  dim(OBS_STOCK) -torch_count_nonzero(ERROR_STOCK)$to(torch_float32())
# fp = torch_sum(torch_ge(ERROR_STOCK, 1))$to(torch_float32())
# fn = torch_sum(torch_le(ERROR_STOCK, -1))$to(torch_float32())
# epsilon=1e-7
# precision = tp / (tp + fp + epsilon)
# recall = tp / (tp + fn + epsilon)
# VOX_IoU_STOCK = as.array(2* (precision*recall) / (precision + recall + epsilon))
# 


############################################################################################################################################################################################################
############################################################################################################################################################################################################
##for (b in enumerate(Valid_dl)) {} HERE IS THE CODE ##########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################

# ###################################################################################################
# model_Instance$eval()
# valid_losses <- c()
# # valid_ds <- dataset_FUN(test_x, test_y, num_classes = num_classes)
# # Valid_dl <-valid_ds %>% dataloader(batch_size = batch_size)
# TOTAL_CLASS <- 0
# correct <- 0
# for (b in enumerate(Valid_dl)) {
#   Valid_ouput <- validation_batch(b)
#   valid_losses <- c(valid_losses, Valid_ouput[[2]])
#   OUTPUT_DATA <- Valid_ouput[[1]]
#   predicted <- torch_max(OUTPUT_DATA$data(), dim = 2)[[2]]
#   
#   # add number of correct classifications in this batch to the aggregate
#   Observed <- b[[2]]
#   CORRECT_CLASS_oneB <- (predicted == Observed)$sum()$item()/Observed$size(1)
#   
#   TOTAL_CLASS <<- TOTAL_CLASS + Observed$size(1)
#   correct <<- correct + (predicted == Observed)$sum()$item()
#   cat(sprintf("Valid: %1f Loss: %3f Correct: %3f\n", epoch,  Valid_ouput[[2]],  CORRECT_CLASS_oneB))
#   
# }
# 
# #cat(sprintf("\nLoss at epoch %d: training: %3f, validation: %3f\n", epoch, mean(OUTPUT_TRAIN_LOSS), mean(valid_losses)))
# 
# 
# # EVALUATE model_Instance HERE (see model_Instance$eval() in https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/)
# #cat(sprintf("\nLoss at epoch %d: training: %3f\n", epoch, mean(OUTPUT_TRAIN_LOSS))) #, #, validation: %3f\n ... mean(valid_losses)
# 
# test_accuracy <-  correct/TOTAL_CLASS
# print(paste("test_accuracy:" , test_accuracy, "  Epoch: ", epoch))

# browser()

############################################################################################################################################################################################################
############################################################################################################################################################################################################
##NOTES ON VALIDATION ##########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################


# https://whatdhack.medium.com/a-deeper-look-at-how-faster-rcnn-works-84081284e1cd
# In the testing/validation phase the NMS boxes along with their scores 
# go straight to the Detection Network

#One_Hot_Format <- b[[2]] # NOTE THAT INPUT IS PYTHON ZERO BASED
#index_label = torch_argmax(One_Hot_Format, dim=2) + 1L

#loss <- criterion(output, b[[2]])

############################################################################################################################################################################################################
############################################################################################################################################################################################################
## COMPUTE LOSS FUNCTIONS AND NMS PROCESS FOR FINAL STOCKING DENSITY BELOW ###########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################


# https://whatdhack.medium.com/a-deeper-look-at-how-faster-rcnn-works-84081284e1cd
# In the training phase the 2000 boxes are further reduced through sampling to 
# about 256 before entering the Detection Network.
#browser()


# One_Hot_Format <- b[[2]] # NOTE THAT INPUT IS PYTHON ZERO BASED
# index_label = torch_argmax(One_Hot_Format, dim=2) + 1L
# loss <- criterion(output, index_label) # nnf_cross_entropy (1D target tensor expected, multi-target not supported)

# https://whatdhack.medium.com/a-deeper-look-at-how-faster-rcnn-works-84081284e1cd
# The labels (tree/not tree) from the IoU calcuations are used to calculate the cross-entropy loss, after first removing the ignored (-1) class boxes.

### DOM DOM DOM !!! YOU CAN FLATTEN BATCH AND ROI FOR BOTH MODEL AND TARGET AND THEN BRING IT BACK TO SAME DIMENSION AFTER LOSS CALC.

#loss_Class <- criterion_Class(OUTPUT_LIST[[1]], b[[2]][[1]]) # nnf_cross_entropy (1D target tensor expected, multi-target not supported)


# HERE YOU UNDERTAKE NMS TO WORK OUT FINAL PLOT STOCKING DENSITY. USE WIGHTS TO GIVEN FINAL STOCKING THE GREATEST WEIGHT IN LOSS CALCULATIONS

#loss = loss_Class + loss_Reg_BBox 

# LOSS ARE NEEDED FOR BACK PROP FOR FINE TUNING THE CLASs AND BBOX LOCATION

# b[[2]][[3]] IS THE STOCKING DENSITY THAT YOU WANT TO PREDICT HERE USING NMS

##############################################
# NMS PROCESS FOR FINAL STOCKING DENSITY BELOW
##############################################

# Below is how the end of the algorithm will behave to get plot-level stocking densities.
# Prior_Prob want to first use Softmax to work out whether each prior is a tree or non-tree.
# loss_Stocking_Class <- criterion_Class(OUTPUT_LIST[[2]]$view(c(-1,2)), torch_squeeze(b[[2]][[1]]$view(c(-1,1))))
# 
# loss_Binary_Class <- criterion_Class(OUTPUT_LIST[[1]]$view(c(-1,2)), torch_squeeze(b[[2]][[2]]$view(c(-1,1))))



# If a prior is a tree then Prior_Prob want to get the fine-tuned BBox Reg of that prior.
# loss_Reg_BBox <-  criterion_Reg_BBox(OUTPUT_LIST[[3]], b[[2]][[3]])

# Non-max suppression (NMS)
# Start with the tree priors with highest objectiveness score and remove all priors that intersect it (IoU) by a threshold.
## CODE NEEDS TO DO IoU FOR ALL PRIORS AGAINST THE BEST PRIOR (FOR COMP EFFICIENCY US 2D COMP OF TOP BBOX FIRST, THEN SECOND BBOX ETC...)

# Then move to the tree prior with the next highest objectiveness score and repeat.
## CODE REMOVES THE IoU PRIORS AND ID NEXT HEIGHEST IN AN ITTERATIVE MANNER.
# biggest IoU of priors with each target tree ends up being the predicted tree.
#     Some target trees may not have a good prior with high objectiveness score and therefore missed (stocking will be smaller than observed). 
#     if two priors represent the same tree (Prior_Prob.e. both do not overlap with each other and represent different parts of the tree will lead to higher stocking than observed
#                                          All final predicted trees are counted to work out the stocking.
#                                          This stocking can be the final model's plot-level predicted stocking (using softmax suppress with a set number of stems).
#   ## CODE COMPUTES THE STOCKING DENSITY AND INTEGRATE THIS INTO THE LOSS (PROBABLY USING SOFTMAX WITH MANY CLASSES ONE FOR EACH STOCKING 0-10 TREES LETS SAY)                                        
# OR USE F-SCORE BASED ON HOW MANY TREES ARE OMITTED, COMMISSION, OR TP.
#   ## CODE: FINAL LOSS IS A COMBINATION TREE LEVEL loss_Class AND loss_Reg_BBox AND PLOT LEVEL STOCKING (WEIGHTS 0.25, 0.25, 0.5)



#regression model performance evaluate using Root Mean Square(RMS) or Mean Average Percentage Error(MAPE)

# Classification models are evaluated using Accuracy, Precision, Recall or an F1- Score.

#browser()
# hist(as.array(output))
# browser()
#loss <- criterion(output, b[[2]]) # nnf_cross_entropy (1D target tensor expected, multi-target not supported)
# ...  nn_cross_entropy_loss() and nn_nll_loss() gives Error in train_batch(b) : attempt to apply non-function


############################################################################################################################################################################################################
############################################################################################################################################################################################################
##PYTHON CODE FOR NMS ##########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################





# bboxes = [box for box in bboxes if box[1] > threshold]
# bboxes = sorted(bboxes, key=lambda x: x[1], reverse=True)
# bboxes_after_nms = []
# 
# while bboxes:
#   chosen_box = bboxes.pop(0)
# 
# bboxes = [
#   box
#   for box in bboxes
#   if box[0] != chosen_box[0]
#   or intersection_over_union(
#     torch.tensor(chosen_box[2:]),
#     torch.tensor(box[2:]),
#     box_format=box_format,
#   )
#   < Para_Threshold_IoU
# ]
# 
# bboxes_after_nms.append(chosen_box)
# 
# return bboxes_after_nms


############################################################################################################################################################################################################
############################################################################################################################################################################################################
## NOTES OUTLINING BROAD OBJECTIVES ##########################################################################################################################################################################################################
############################################################################################################################################################################################################
############################################################################################################################################################################################################


#browser()
# DOM DOM DOM !!! SHOULD THERE BE A FUNCTION HERE THAT COMPUTES THE Non-max suppression (NMS)
#Stocking_Count <- NMS_FUNCTION()
# OUTPUT_CLASS <- as.array(OUTPUT_CLASS)
# # LOGICAL PROCEDURE
#   # FIND THE PRIORS (64) FROM EACH OF THE PLOTS (16) THAT HAVE THE PROBABILITY ABOVE Para_Threshold_Prob (<0.3). SORT THOSE PRIORS USING PROBABILITY
#   # PICK THE PRIOR WITH THE LARGEST PROBABILITY AND DISCARD ALL THE PRIORS THAT OVERLAP BY Para_Threshold_IoU (>0.5). PUT INTO FINAL OUTPUT
#       # PICK NEXT LARGEST PRIOR AND REPEAT UNTIL YOU HAVE NO FURTHER PRIORS.
# 
# 
# 
# Stocking_NMS <- x %>% 
#   torch_flatten(start_dim = 3L) %>% 
#   self$den_StockClass() %>% 
#   self$softmax2()

