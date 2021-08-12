################################
################################
################################
################################
# TORCH MODULES AND LOSS MODULES
################################
################################
################################
################################

############################################################################################ SEQUENTIAL MODULE
############################################################################################ SEQUENTIAL MODULE
make_nConv  <- function(nChan, nConv, kernel_size_Seq, padding_Seq){
  seq_module <- nn_module(
    "make_nConv",
    initialize = function(nChan, nConv, kernel_size_Seq, padding_Seq) { # p
      self$make_nConv1 <- nn_module_list(lapply(1:nConv, function(x) nn_conv3d(nChan, nChan, kernel_size= kernel_size_Seq, padding = padding_Seq)))
      self$bn1 = nn_batch_norm3d(nChan)
      # self.DrO_p = p
    },
    forward = function(x, i) {
      for (i in 1:length(self$make_nConv1)){
        x <- self$make_nConv1[[i]](x) %>%
          self$bn1() %>%
          nnf_relu()  ## CONSIDER DROPOUT HERE nn_dropout3d(p = self.DrO_p)
      } # i LOOP
      x
    } # FORWARD
  ) # NN_MODULE
  Output <- seq_module(nChan, nConv, kernel_size_Seq, padding_Seq)
} # FUNCTION: MAKE_NCONV

############################################################################################ INPUT MODULE
############################################################################################ INPUT MODULE
InitTransition <- nn_module(
  "Initial_VNet",
  initialize = function(nChan, outChan, kernel_size_Conv, padding_Conv, stride_Conv, kernel_size_Seq, padding_Seq) {
    self$Conv3D1 = nn_conv3d(nChan, outChan, kernel_size = kernel_size_Seq, padding = padding_Seq)
    self$bn1 = nn_batch_norm3d(outChan)
    self$conv3D2 = nn_conv3d(outChan, outChan*2, kernel_size = kernel_size_Conv, padding = padding_Conv, stride = stride_Conv)
  },
  forward = function(x) {
    # browser()
    inputs_shortcut <- x
    # browser()
    Output <- x %>% self$Conv3D1() %>% 
      self$bn1() %>%
      nnf_relu() # CONSIDER DROPOUT WITH LOW P i.e. 0.2 ...
    Input1Res = torch_add(Output, inputs_shortcut) 
    Output <- Input1Res %>%  self$conv3D2()
    return(list(Output, Input1Res))
  } # FORWARD
) # NN_MODULE

############################################################################################ DOWN MODULE
############################################################################################ DOWN MODULE
DownTransition <- nn_module(
  "Down_VNet",
  initialize = function(nChan, OutChan, nConv, kernel_size_Conv, kernel_size_Seq, padding_Conv, padding_Seq) {
    self$ops = make_nConv(nChan, nConv, kernel_size_Seq, padding_Seq)
    self$Conv3D1 = nn_conv3d(nChan, OutChan, kernel_size=kernel_size_Conv, padding = padding_Conv, stride=2)
    self$bn1 = nn_batch_norm3d(nChan)
    self$bn2 = nn_batch_norm3d(OutChan)
  },
  forward = function(x) {
    inputs_shortcut <- x
    Output <- x %>% self$ops() %>% 
      self$bn1() %>%
      nnf_relu() # CONSIDER DROPOUT 
    Input1Res = torch_add(Output, inputs_shortcut) %>%
      self$Conv3D1()%>% 
      self$bn2() %>%
      nnf_relu() # CONSIDER DROPOUT 
  } # FORWARD
) # NN_MODULE

############################################################################################ UP MODULE
############################################################################################ UP MODULE
UpTransition <- nn_module(
  "Up_VNet",
  initialize = function(nChan, OutChan, nConv, kernel_size_Conv, kernel_size_Seq, padding_Conv, padding_Seq) {
    self$Conv_T1 = nn_conv_transpose3d(nChan, OutChan, kernel_size=kernel_size_Conv, padding = padding_Conv, stride=2)
    self$ops = make_nConv(nChan,  nConv, kernel_size_Seq, padding_Seq)
  },
  forward = function(x) {
    inputs_shortcut <- x[[1]] 
    OutRes <-  x[[1]] %>% 
      self$ops()
    OutRes = torch_add(OutRes, inputs_shortcut) %>%
      self$Conv_T1() 
    OutRes = torch_add(OutRes, x[[2]])
  } # FORWARD
) # NN_MODULE

############################################################################################ RoI_MaxPool_3D MODULE
############################################################################################ RoI_MaxPool_3D MODULE
RoI_Transition <- nn_module( 
  "RoI_Module",
  initialize = function(pool_size=pool_size, nChan, outChan1, outChan2, outChan3, kernel_size_Seq, padding) {
    self$Pool3D <- nn_adaptive_max_pool3d(pool_size)
    self$conv1 = nn_conv3d(nChan, outChan1, kernel_size = kernel_size_Seq, padding = padding)
    self$conv2 = nn_conv3d(outChan1, outChan2, kernel_size = kernel_size_Seq, padding = padding)
    self$RoIconv = nn_conv3d(outChan2, outChan3, kernel_size = 1, padding = 0)
    self$bn1 = nn_batch_norm3d(outChan1)
    self$bn2 = nn_batch_norm3d(outChan2)
    self$bn3 = nn_batch_norm3d(outChan3)
  },

  forward = function(out3, Vox_Den_ExtP, RoI_Vox, RoI_Dec, PRIOR_XYZWLHR_ExtP, TARGET_XYZWLHR_ExtP, TARGET_Vox_TID, TARGET_TID, TARGET_IoU_SUMMARY) { 

    ##############################
    # PERFORM CONVOLUTION ON INPUT
    ##############################
    x_Conv <-  out3 %>% self$conv1() %>% 
      self$bn1() %>% # # CONSIDER DROPOUT  AFTER BN1
      nnf_relu() %>%
      self$conv2() %>%
      self$bn2() %>% # # CONSIDER DROPOUT  AFTER BN2
      nnf_relu()

    
    # EMPTY LISTS FOR p LOOP and r LOOP

    ACTUAL_Goffset_allP_allR_ExtR <- list()
    PRIOR_XYZWLHR_allP_allR_ExtR <- list()
    TARGET_XYZWLHR_allP_allR_ExtR <- list()
    
    Conv_allP_allR_ExtR = list()
    VoxDen_allP_allR_ExtR  <- list()
    IoU_allP_allR_ExtR = list()
    Mask_allP_allR_ExtR = list()
    
    plot_num = out3$size()[1]
    rois_num = RoI_Vox$size()[2]

    # LOOP PLOT
    for(p in 1:plot_num){   
      
      ####################################################################################################################################################
      ####################################################################################################################################################
      ####################################################################################################################################################
      
      #with_no_grad({
      
      ##################################################################
      # SCALING ExtP (TARGET_XYZWLH and PRIOR_XYZWLH) to ExtR (i.e. 0-1)
      ##################################################################
      # browser()
      TARGET_XYZWLHR_oneP_allR_ExtP <- TARGET_XYZWLHR_ExtP[p,,]
      # TARGET_XYZWLHR_oneP_allR_ExtP_DF <- as.data.frame(as.array(TARGET_XYZWLHR_oneP_allR_ExtP))  # GPU
      # colnames(TARGET_XYZWLHR_oneP_allR_ExtP_DF) <- Colnames_XYZWLHR  # GPU

      TARGET_XYZWLHR_oneP_allR_ExtR <- SCALE_PLOT2RoI_VERT_GPU_FUN(RoI_Dec = RoI_Dec, 
                                                               XYZWLHR = TARGET_XYZWLHR_oneP_allR_ExtP, 
                                                               OUT_VERT_or_XYZWLHR = "XYZWLHR", 
                                                               batch=p, 
                                                               Para_Cnt = Para_TriShpParaCnt, 
                                                               use_Tensor = "Yes",
                                                               Col_Name= Colnames_XYZWLHR)   # GPU
      
      PRIOR_XYZWLHR_oneP_allR_ExtP <- PRIOR_XYZWLHR_ExtP[p,,]
      # PRIOR_XYZWLHR_oneP_allR_ExtP_DF <- as.data.frame(as.array(PRIOR_XYZWLHR_oneP_allR_ExtP))   # GPU
      # colnames(PRIOR_XYZWLHR_oneP_allR_ExtP_DF) <- Colnames_XYZWLHR   # GPU
      PRIOR_XYZWLHR_oneP_allR_ExtR <- SCALE_PLOT2RoI_VERT_GPU_FUN(RoI_Dec = RoI_Dec, 
                                                              XYZWLHR = PRIOR_XYZWLHR_oneP_allR_ExtP, 
                                                              OUT_VERT_or_XYZWLHR = "XYZWLHR", 
                                                              batch=p,
                                                              Para_Cnt = Para_TriShpParaCnt, 
                                                              use_Tensor = "Yes",
                                                              Col_Name= Colnames_XYZWLHR)   # GPU
      
      #print("DONE: SCALE_PLOT2RoI_VERT_GPU_FUN")
     
      if(p == 1){
        TARGET_XYZWLHR_allP_allR_ExtR <- torch_unsqueeze(TARGET_XYZWLHR_oneP_allR_ExtR, dim=1)
        PRIOR_XYZWLHR_allP_allR_ExtR <- torch_unsqueeze(PRIOR_XYZWLHR_oneP_allR_ExtR, dim=1)
       
      }else{
        
        TARGET_XYZWLHR_oneP_allR_ExtR_T <- torch_unsqueeze(TARGET_XYZWLHR_oneP_allR_ExtR, dim=1)
        PRIOR_XYZWLHR_oneP_allR_ExtR_T <- torch_unsqueeze(PRIOR_XYZWLHR_oneP_allR_ExtR, dim=1)
        
        TARGET_XYZWLHR_allP_allR_ExtR <- torch_cat(c(TARGET_XYZWLHR_allP_allR_ExtR, TARGET_XYZWLHR_oneP_allR_ExtR_T))
        PRIOR_XYZWLHR_allP_allR_ExtR <- torch_cat(c(PRIOR_XYZWLHR_allP_allR_ExtR, PRIOR_XYZWLHR_oneP_allR_ExtR_T)) 

      }
      # TARGET_XYZWLHR_allP_allR_ExtR <- list.append(TARGET_XYZWLHR_allP_allR_ExtR, TARGET_XYZWLHR_oneP_allR_ExtR)   # GPU
      # PRIOR_XYZWLHR_allP_allR_ExtR <- list.append(PRIOR_XYZWLHR_allP_allR_ExtR, PRIOR_XYZWLHR_oneP_allR_ExtR)
      # 
      # # TESTING THE ALGORITHM
      # Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
      #                       "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
      #                       "Z_TopTree")
     
      
      # Test_Goffset_ExtR <- XYZWLHR_To_Goffset_FUN(Test_XYZWLHR, Test_PRIOR, Plot_Output = "No", 
      #                                             Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, use_Tensor = "Yes")
      
      ##############################################################
      # DECODING TARGET_XYZWLHR_oneP_allR_ExtR INTO OFFSET REPRESENTATION     # NOTE: PREDICTIONS ARE OFFSETS (RELATIVE TO RESPECTIVE PRIORS), SO TARGETS NEED TO BE CONVERTED TO OFFSETS (FOR LAS PROCEDURE).... WORKING WITH RoI EXTENT MEASURES...
      ##############################################################
      # browser()

      ACTUAL_Goffset_oneP_allR_ExtR <- XYZWLHR_To_Goffset_GPU_FUN(TARGET_XYZWLHR_oneP_allR_ExtR, PRIOR_XYZWLHR_oneP_allR_ExtR, Plot_Output = "No", 
                                                                 Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, use_Tensor = "Yes") #, Batch_Count   # GPU
      #print("DONE: XYZWLHR_To_Goffset_GPU_FUN")
      #browser()
      ACTUAL_Goffset_allP_allR_ExtR <- list.append(ACTUAL_Goffset_allP_allR_ExtR, ACTUAL_Goffset_oneP_allR_ExtR)
    
      #}) # END with_no_grad

      ####################################################################################################################################################
      
      # # SANITY CHECK FOR ERRORS # GPU
      # FINITE_Count <- length(which(is.finite(as.vector(as.array(PRIOR_XYZWLHR_oneP_allR_ExtR)))))
      # if(FINITE_Count != length(as.vector(as.array(PRIOR_XYZWLHR_oneP_allR_ExtR)))){browser()} # not one of the values NA, NaN, Inf or -Inf
      # FINITE_Count <- length(which(is.finite(as.vector(as.array(TARGET_XYZWLHR_oneP_allR_ExtR)))))
      # if(FINITE_Count != length(as.vector(as.array(TARGET_XYZWLHR_oneP_allR_ExtR)))){browser()} # not one of the values is NA, NaN, Inf or -Inf
      # FINITE_Count <- length(which(is.finite(as.vector(as.array(ACTUAL_Goffset_oneP_allR_ExtR)))))
      # if(FINITE_Count != length(as.vector(as.array(ACTUAL_Goffset_oneP_allR_ExtR)))){browser()} # not one of the values NA, NaN, Inf or -Inf
      
      # EMPTY LISTS FOR r LOOP
      Conv_allR_ExtR = list()
      VoxDen_allR_ExtR = list()
      Mask_allR_ExtR = list()
      # LOOP ROI
      
      for (r in 1:rois_num){

        oneTID = TARGET_TID[p,r]   ### THE MODEL WILL HAVE TO RUN WITHOUT "TARGET_TID"
        
        # GET Vox ID FOR BOT AND TOP (XYZ)
        Vox_oneR = RoI_Vox[p,r,] # $to(device="cpu")

        xxB <- as.array(Vox_oneR[2])
        xxT <- as.array(Vox_oneR[3])
        yyB <- as.array(Vox_oneR[4])
        yyT <- as.array(Vox_oneR[5])
        zzB <- as.array(Vox_oneR[6])
        zzT <- as.array(Vox_oneR[7])
        
        ##############################################################################
        # RoI MaxPOOL OF TENSOR (16*16*40 WITH CHANNELS IN 2nd AXIS) INTO RoI (8*8*16)   
        ##############################################################################
        # browser()
        #x_Conv$gather(2, index)
        
        Conv_oneR_ExtR <- torch_squeeze(self$Pool3D(x_Conv[p,, (xxB:xxT), (yyB:yyT), (zzB:zzT)]))   # CHANGED FOR TORCH_0.4
        Conv_allR_ExtR <- list.append(Conv_allR_ExtR, Conv_oneR_ExtR)

        # CLIP VoxDen USING RoI REGION
        VoxDen_oneR_ExtR <- torch_squeeze(self$Pool3D(Vox_Den_ExtP[p,, (xxB:xxT), (yyB:yyT), (zzB:zzT)]))  # CHANGED FOR TORCH_0.4
        VoxDen_allR_ExtR <- list.append(VoxDen_allR_ExtR, VoxDen_oneR_ExtR)
  
        # BINARY MASKS (TARGET_TID = 1 else 0)   ### DOM DOM DOM !!! YOU ARE GENERATING A MASK USING TARGET_TID BUT YOU WILL NOT HAVE THIS INFORMATION WHEN EVALUATING YOUR MODEL
        VMask_oneR_ExtP <- torch_clone(TARGET_Vox_TID)
        VMask_oneR_ExtP = VMask_oneR_ExtP[p,,,,] 
        VMask_oneR_ExtP[VMask_oneR_ExtP != oneTID] <- 0
        VMask_oneR_ExtP[VMask_oneR_ExtP == oneTID] <- 1

        Mask_oneR_ExtR <- torch_squeeze(self$Pool3D(VMask_oneR_ExtP[,(xxB:xxT),  (yyB:yyT),  (zzB:zzT)]) )   # CHANGED FOR TORCH_0.4
        Mask_allR_ExtR <- list.append(Mask_allR_ExtR, Mask_oneR_ExtR)
      } # RoI LOOP

      ############################################
      # STACK ALL PLOTS ExtR (VoxDen, Conv & Mask)
      ############################################
      VoxDen_oneP_allR_ExtR <- torch_stack(VoxDen_allR_ExtR) # x <- torch_randn(c(2, 3, 4))
      VoxDen_allP_allR_ExtR <- list.append(VoxDen_allP_allR_ExtR, VoxDen_oneP_allR_ExtR)

      Conv_oneP_allR_ExtR <- torch_stack(Conv_allR_ExtR) 
      Conv_allP_allR_ExtR <- list.append(Conv_allP_allR_ExtR, Conv_oneP_allR_ExtR)
      
      Mask_oneP_allR_ExtR <- torch_stack(Mask_allR_ExtR) 
      Mask_allP_allR_ExtR <- list.append(Mask_allP_allR_ExtR, Mask_oneP_allR_ExtR)
    
      
      browser()
      
      # x.gather(2, index)
      # 
      # t = torch_tensor(c(1, 2, 3, 4))$view(c(2,2))
      # u = torch_tensor(c(1, 1, 2, 1), dtype=torch_int())$view(c(2,2))
      # torch_gather(t, 1, u)
      # 
      # torch_gather(t, 2, u)
      # Input and index have same dimensions (eac dimension needs representation)
      # ouput has same shape as index
      
      ####################################################################################################################################################
      ######################################
      # OUTPUT FOR MASK AND IoU COMPUTATION (not using yet)
      ######################################
      
      # BELOW CONVOLUTION GENERATES BINARY VOXEL OUTPUT FOR IoU COMPUTATION
      IoU_oneP_allR_ExtR <- Conv_oneP_allR_ExtR %>% 
                             self$RoIconv() %>%
                             torch_sigmoid()
      IoU_allP_allR_ExtR <- list.append(IoU_allP_allR_ExtR,IoU_oneP_allR_ExtR)
      ####################################################################################################################################################
      
    } # BATCH LOOP
    
    # STACKING THE BATCHES AND OUTPUTING RESULTS
    
    # TASKS... MAKE SURE ALL THE FUNCTION IN THE BATCH/RoI LOOP ARE CORRECT. 
    # MAKE SURE ALL THE PLOTS MAKE SENSE....
    # browser()
    TARGET_XYZWLHR_allB_allP_allR_ExtR <- torch_stack(TARGET_XYZWLHR_allP_allR_ExtR)$squeeze()    # 12 64 16
    ACTUAL_Goffset_allB_allP_allR_ExtR <- torch_stack(ACTUAL_Goffset_allP_allR_ExtR)
    PRIOR_XYZWLHR_allB_allP_allR_ExtR <- torch_stack(PRIOR_XYZWLHR_allP_allR_ExtR)$squeeze()      # THIS GETS USED IN CONVERT PREDICTED XYZWLHR INTO OFFSET USING "XYZWLHR_To_Goffset_FUN"  
    VoxDen_allB_allP_allR_ExtR <- torch_stack(VoxDen_allP_allR_ExtR)
    Conv_allB_allP_allR_ExtR <- torch_stack(Conv_allP_allR_ExtR)                        # 12 64 32  8  8 16  ### DOM DOM DOM SHOULD THE 32 CHANNELS BE IN THE LAST AXIS???
    IoU_allB_allP_allR_ExtR <- torch_squeeze(torch_stack(IoU_allP_allR_ExtR))                        # 12 64  1  8  8 16                      ### DOM DOM DOM !!! HAVEN'T USED THIS YET
    Mask_allB_allP_allR_ExtR <- torch_stack(Mask_allP_allR_ExtR)
    # browser()
    out4 <- list(TARGET_XYZWLHR_allB_allP_allR_ExtR,
                 ACTUAL_Goffset_allB_allP_allR_ExtR,  #### USED IN LOSS CALCULATION  !!!! IMPORTANT
                 PRIOR_XYZWLHR_allB_allP_allR_ExtR,
                 VoxDen_allB_allP_allR_ExtR,  # 12 64  8  8 16 versus...  12 64  1  8  8 16 in V20     
                 Conv_allB_allP_allR_ExtR,
                 IoU_allB_allP_allR_ExtR,
                 Mask_allB_allP_allR_ExtR)  # 12 64  8  8 16 versus...  12 64  1  8  8 16 in V20                   # torch_squeeze(torch_stack(Mask_allP_allR_ExtR), 3))  # 12 64  8  8 16           ### DOM DOM DOM !!! I MAY WANT TO KEEP CHANNEL = 1 FOR VOX BINARY PROBABILITY CALC

    # # ###################################################################################################################
    # # ###################################################################################################################
    # # # TESTING: PLOTTING RoI AND MASKS AGAINST BBoxes
    # # ###################################################################################################################
    # # ###################################################################################################################
    # #
    # 
    # # [1] "FlightID"             "PlotID"               "TID"                  "Plot_ID"              "Prior"                "Count_IoU"            "Total_Vox"            "Portion_IoU_CorrecIn"
    # # [9] "Portion_IoU_WrongIn"  "TP"                   "FN"                   "FP"                   "Recall"               "Precision"            "FScore"               "PRIOR_Z_TopBox"
    # # [17] "TID_Z_TopBox"         "Diff_ZTopBox"         "Prior_Type"           "RoI_ID"
    # 
    # 
    # # LOOP PLOT
    # for(PP in 1:dim(PRIOR_XYZWLHR_allB_allP_allR_ExtR)[1]){
    # 
    #   #####################
    #   # PLOT THE PLOT LEVEL
    #   #####################
    #   oneP_TARGET_XYZWLHR_ExtP_DF <- as.data.frame(as.array(TARGET_XYZWLHR_ExtP[PP,,]))
    #   colnames(oneP_TARGET_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
    #   oneP_TARGET_XYZWLHR_ExtP_DF <- dplyr::distinct(oneP_TARGET_XYZWLHR_ExtP_DF)
    # 
    #   oneP_PRIOR_XYZWLHR_ExtP_DF <- as.data.frame(as.array(PRIOR_XYZWLHR_ExtP[PP,,]))
    #   colnames(oneP_PRIOR_XYZWLHR_ExtP_DF) <- Colnames_XYZWLHR
    #   # oneP_PRIOR_XYZWLHR_ExtP_DF <- dplyr::distinct(oneP_PRIOR_XYZWLHR_ExtP_DF)
    # 
    #   # GENERATE LAS WITH Vox_Den_ExtP and Mask
    #   oneP_Vox_Den <- torch_squeeze(Vox_Den_ExtP[PP,,,,])
    #   oneP_TARGET_Vox_TID <- torch_squeeze(TARGET_Vox_TID[PP,,,,])
    # 
    #   LAS_Tensor_Vox_Den_ExtP <- TENSOR_LAS_RoI_FUN(oneP_Vox_Den,
    #                                            oneP_TARGET_Vox_TID,
    #                                            Para_Target_Base,
    #                                            Para_Target_Z_Height)
    # 
    #   LAS_Tensor_Vox_Den_Trees <- filter_poi(LAS_Tensor_Vox_Den_ExtP, Tensor_Count_Norm > 0)
    #   plot(LAS_Tensor_Vox_Den_Trees, color="Mask")
    # 
    #   oneP_TARGET_TID <- as.vector(as.array(torch_squeeze(TARGET_TID[PP,])))
    # 
    #   # TARGET_Vox_TID, TARGET_TID
    # 
    #   ################
    #   # LOOP EACH TREE
    #   ################
    # 
    #   Unique_TID <- unique(oneP_TARGET_TID)
    #   for (TT in 1:length(Unique_TID)){
    #     
    #     Index_TT <- which(oneP_TARGET_TID == Unique_TID[TT])
    #     oneP_oneT_TARGET_XYZWLHR_ExtP_DF <- oneP_TARGET_XYZWLHR_ExtP_DF[TT,]
    #     oneP_oneT_TARGET_Vert <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_oneT_TARGET_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base))
    # 
    #     colours <- c("green")
    #     #List_Vert <- #list(oneP_oneT_TARGET_Vert[1:4,], oneP_oneT_TARGET_Vert[5:8,], oneP_oneT_TARGET_Vert[9:12,])
    #     LAS_Tensor_Vox_Den_Trees@data$TID <-LAS_Tensor_Vox_Den_Trees@data$Mask
    #     # PLOT_Ver1_Vert2_VOX_FUN(LAS_Tensor_Vox_Den_Trees, list(oneP_oneT_TARGET_Vert), Title_Plot= paste("GT:", Unique_TID[TT]), colours, Normalised = "Yes", Para_Cnt = 10, Plot_Colour_Att = "TID")
    # 
    #     
    #     oneP_oneT_PRIOR_XYZWLHR_ExtP_DF <- oneP_PRIOR_XYZWLHR_ExtP_DF[Index_TT,]
    #     List_Prior <- list()
    # 
    #     Do_All <- nrow(oneP_oneT_PRIOR_XYZWLHR_ExtP_DF)
    #     Do_1 <- 1
    #     for(PTPT in 1:Do_1){
    #       oneP_oneT_onePRIOR_XYZWLHR_ExtP_DF <- oneP_oneT_PRIOR_XYZWLHR_ExtP_DF[PTPT,]
    #       oneP_oneT_onePRIOR_Vert <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_oneT_onePRIOR_XYZWLHR_ExtP_DF, Base_WL = Para_Base_WL/Para_Target_Base))
    #       #List_Prior <- list(List_Prior, oneP_oneT_onePRIOR_Vert)
    #       List_Prior[[PTPT]] <- oneP_oneT_onePRIOR_Vert
    # 
    #     }
    #     List_Prior[[PTPT + 1]] <- oneP_oneT_TARGET_Vert
    #     colours <- c(rep("orange", length(List_Prior)-1), "green")
    #     PLOT_Ver1_Vert2_VOX_FUN(LAS_Tensor_Vox_Den_Trees, List_Prior, Title_Plot= paste("PriorTT:", Unique_TID[TT]), colours, 
    #                             Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, Plot_Colour_Att = "TID")
    #     oneP_oneRoI_Dec_Vec <- as.vector(as.array(RoI_Dec[PP,Index_TT[1],]))[-1] # unlist(oneList_RoI_Dec)[-1]
    #     Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec)
    #     polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
    #     polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
    #     
    #   }
    # 
    #   #####################
    #   # PLOT THE ROI LEVEL
    #   #####################
    # 
    #   # GET ROI INFO
    #   oneP_allR_TARGET_Prior_Type <- TARGET_IoU_SUMMARY[PP,,19]
    # 
    #   oneP_allR_PRIOR_XYZWLHR_ExtR <- PRIOR_XYZWLHR_allB_allP_allR_ExtR[PP,,]
    #   oneP_allR_PRIOR_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_allR_PRIOR_XYZWLHR_ExtR))
    #   colnames(oneP_allR_PRIOR_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
    # 
    # 
    #   oneP_allR_TARGET_XYZWLHR_ExtR <- TARGET_XYZWLHR_allB_allP_allR_ExtR[PP,,]
    #   oneP_allR_TARGET_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_allR_TARGET_XYZWLHR_ExtR))
    #   colnames(oneP_allR_TARGET_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
    # 
    # 
    #   oneP_allR_Vox_Den <- VoxDen_allB_allP_allR_ExtR[PP,,,,,]
    #   oneP_allR_MASK_ExtR <- Mask_allB_allP_allR_ExtR[PP,,,,,]
    # 
    #   # LOOP THROUGH EACH ROI
    #   Do_All <- 1:rois_num
    #   Index_1st_EachT <- match(Unique_TID, oneP_TARGET_TID)
    # 
    #   for(RR in 1:length(Index_1st_EachT)){
    #     oneTID <- as.array(TARGET_TID[PP,Index_1st_EachT[RR]])
    #     Prior_Type <- as.array(oneP_allR_TARGET_Prior_Type[Index_1st_EachT[RR]])
    # 
    #     ######################################################
    #     # CONVERT XYZWLHR (TARGET AND PREDICTED) INTO VERTICES
    #     ######################################################
    #     oneP_PRIOR_Vert <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_allR_PRIOR_XYZWLHR_ExtR_DF[Index_1st_EachT[RR],], Base_WL = Para_Base_WL/Para_Target_Base))
    #     oneP_TARGET_Vert <- as.data.frame(XYZWHR_TO_VERT_FUN(oneP_allR_TARGET_XYZWLHR_ExtR_DF[Index_1st_EachT[RR],], Base_WL = Para_Base_WL/Para_Target_Base))
    # 
    #     Title_Plot <- paste("P", PP, ":R", Index_1st_EachT[RR], ":TID", oneTID, ":Type", Prior_Type ,sep="")
    # 
    #     colours <- c( "red","green")
    #     List_Vert <- list(oneP_PRIOR_Vert, oneP_TARGET_Vert)
    # 
    #     ###################################
    #     # CONVERT TENSOR OF VoxDen INTO LAS
    #     ###################################
    # 
    #     LAS_Tensor_Vox_Den_ExtR <- TENSOR_LAS_RoI_FUN(torch_squeeze(oneP_allR_Vox_Den[Index_1st_EachT[RR],,,,]),
    #                                              torch_squeeze(oneP_allR_MASK_ExtR[Index_1st_EachT[RR],,,,]),
    #                                              para_RoI_Pool_Dim_XY,
    #                                              para_RoI_Pool_Dim_Z)
    #     LAS_Tensor_Vox_Den_ExtR@data$TID <- LAS_Tensor_Vox_Den_ExtR@data$Mask
    # 
    #     if(length(unique(LAS_Tensor_Vox_Den_ExtR@data$Mask))> 1){
    #       # PLOT_Ver1_Vert2_VOX_FUN(LAS_Tensor_Vox_Den_ExtR, List_Vert, Title_Plot, colours, Plot_Colour_Att = "Tensor_Count_Norm")
    #       
    #       LAS_Tensor_Vox_Den_Tree <- filter_poi(LAS_Tensor_Vox_Den_ExtR, Tensor_Count_Norm > 0)
    #       # LAS_Tensor_Vox_Den_Tree@data$TID <- LAS_Tensor_Vox_Den_Tree@data$Tensor_Count_Norm
    #       PLOT_Ver1_Vert2_VOX_FUN(LAS_Tensor_Vox_Den_Tree, List_Vert, Title_Plot, colours, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, Plot_Colour_Att = "Tensor_Count_Norm")
    # 
    #     }else{
    #       LAS_Tensor_Vox_Den_oneTree <- filter_poi(LAS_Tensor_Vox_Den_Trees, TID == oneTID)
    #       plot(LAS_Tensor_Vox_Den_oneTree)
    #     }
    #   } # RR LOOP
    #   # browser()
    # } # PP LOOP
    # 
    # ###################################################################################################################
    # ###################################################################################################################
    # # END TEST PLOT
    # ##################################################################################################################
    # ##################################################################################################################
    return(out4)
  }# FORWARD
) # NN_MODULE


###################################
# RoI ALLIGNMENTS NEEDS INTEGRATING
###################################

### DOM DOM DOM !!! BASED ON BELOW COMMENT ON RoiAlign, RoiAlign WOULD YOU USE TRILINEAR INTERPOLATION HERE 
# THE x_Conv WOULD BE USED IN THIS INTERPOLATION TO GET A MORE ACCURATE REPRESENTATION OF EACH VOXEL VALUE WITHIN RoI

# https://blog.athelas.com/a-brief-history-of-cnns-in-image-segmentation-from-r-cnn-to-mask-r-cnn-34ea83205de4
# DOM DOM DOM !!! SEE RoiAlign (THIS IS ONLY USED FOR THE SEGMENTATION (MASk OUTPUT Voxel-voxel procedure and not applied in BBox regression))

# Imagine we have an image (i.e.Prior) of size 128x128 and a feature map (i.e. Layers in VNet) of size 25x25. 
# Let's imagine we want features of the region (i.e. RoI) corresponding to the top-left 15x15 pixels in the original image (see above). 
# How might we select these pixels from the feature map?
# We know each pixel in the original image corresponds to ~ 25/128 pixels in the feature map. To select 15 pixels from the original image, we just select 15 * 25/128 ~= 2.93 pixels.
# In RoIPool, we would round this down and select 2 pixels causing a slight misalignment. However, in RoIAlign, we avoid such rounding. Instead, 
# we use bilinear interpolation to get a precise idea of what would be at pixel 2.93. 
# This, at a high level, is what allows us to avoid the misalignments caused by RoIPool.
# DOM DOM DOM !!! USE approx3d from library(oce) for Interpolate within a 3D array, using the trilinear approximation.

# HOW RoI ALIGN WORKS (FROM PDF IN D:\DOCUMENTS\instance_Mask_RoI_Align.pdf)
# Simply put, if an N XN output is desired, the proposed region (black rectangle in the upper-right image)
# is divided into an N X N grid. Unlike RoI Pooling, these regions will contain the exact same number of
# pixels, so we will often have fractional pixels. From each grid cell, we sample four regions as shown by
# the red x marks in the third image. We then subdivide each grid cell into four subcells, each centered on
# an x. We perform bilinear interpolation to get a single value for each subregion, or four values for each
#cell. These values are shown in the fourth image. Finally, we perform a simple max pooling on the bilinear
#interpolated values, taking the maximum value per cell to reach an N X N output. This output is then
#passed through the fully connected layers for bounding-box regression and classification, and through the
#small Fully Convolutional Network (FCN) that makes up our masking head.

# ## set up a grid
# library(oce)
# n_xy <- 16
# n_z <- 40
# x <- seq(0, 1, length.out=n_xy)
# y <- seq(0, 1, length.out=n_xy)
# z <- seq(0, 1, length.out=n_z)
# f <- array(1:(n_xy^2*n_z), dim=c(length(x), length(y), length(z)))
# ## interpolate along a diagonal line
# m_xy <- 8
# m_z <- 16
# xout <- seq(0, 1, length.out=m_xy)
# yout <- seq(0, 1, length.out=m_xy)
# zout <- seq(0, 1, length.out=m_z)
# 
# # DOM DOM DOM !!! YOU NEED TO GET ALL Xs etc AND NOT JUST THE DIAGNAL ONES AS SHOWN IN ABOVE EXAMPLE...
# approx <- approx3d(x, y, z, f, xout, yout, zout)
# ## graph the results
# plot(xout, approx, type='l')
# points(xout[1], f[1, 1, 1])
# points(xout[m], f[n,n,n])

############################################################################################ Output MODULE
############################################################################################ Output MODULE

OutputTransition <- nn_module(
  "Output_VNet",  
  initialize = function(nChan, Smax_dim_1, Smax_dim_2, nRoI, RoIX, RoIY,RoIZ, Binary_Chan, BBox_Chan) { #Stock_Chan, Smax_dim_2, 

    #self$flat = torch_flatten( start_dim = flat_start_dim) #3L
    self$den_BinaryClass = nn_linear(nChan*RoIX*RoIY*RoIZ, Binary_Chan)
    #self$den_Regress_Stock = nn_linear(nChan*RoIX*RoIY*RoIZ, Stock_Chan)
    self$den_Regress_XYZWLHR = nn_linear(nChan*RoIX*RoIY*RoIZ, BBox_Chan)
    # self$softmax1 = nn_softmax(dim= Smax_dim_1)
    # self$softmax2 = nn_softmax(dim= Smax_dim_2)
    # self$den_Regress_Stock  = nn_linear(nChan*RoIX*RoIY*RoIZ, Stock_Chan)  # nRoI*
    self$softmax2 = nn_softmax(dim= Smax_dim_2)  ### DOM DOM DOM V4 !!! YOU CHANGEDS THIS TO dim=1
    
  },
  forward = function(out4, PRIOR_XYZWLHR_ExtP) {
    #inputs_shortcut <- x # SHOULD DIMENSION BE (2 16 21  7  7  7) or (2 21 16 7  7  7)
    Conv_allP_allR_ExtR <- torch_clone(out4[[5]])
    ### DOM DOM DOM !!! JUST LIKE FAST_RCNN sHOULDN'T THE OUTPUT FROM RoI (i.e. out4[[1]] == Conv_allP_allR_ExtR) BE PUT THROUGH SOME CONVOLUTIONS HERE FIRST BEFORE FLATTENING
    ### 
    
    OUTPUT_BINARY_SCORE <- Conv_allP_allR_ExtR %>% 
      torch_flatten(start_dim = 3L) %>% 
      self$den_BinaryClass() # %>%   # 12 64  1
    

      # %>% self$softmax1()   #### DOM DOM DOM THIS IS NOT WORKING!!!

    # WHAT HAS BEEN TRIED: 
      # USING SOFTMAX WITH DIM=2 DOESNT WORK
      # USING SOFTMAX (DIM=3) RESULTS WITH BCE DOESN'T WORK... STALLS ON 0.97
      # TRYING WITHOUT Softmax1 GIVES VERY RANDOM RESULTS INITIALLY BUT RUNNING 
   # WHAT I SHOULD TRY:
       # TRY CONVERTING THE MIN AND MAX INTO INTEGER TO PREDICT MAYBE
    
    # out4[[3]] ... 12 64 16  8  8 16
    # PRED_Goffset_allP_allR_ExtR ... 12 64 16
    PRED_Goffset_allP_allR_ExtR <- Conv_allP_allR_ExtR %>%  
      torch_flatten(start_dim = 3L) %>%   ### DOM DOM DOM !!! FASTER RCNN DOES NOT DO THIS BECAUSE EACH PIXEL HAS A PREDICTION (NO FLATTENING....)
      self$den_Regress_XYZWLHR()

    # browser()
    
    # DOM DOM DOM !!! THIS IS POINT OF CONTENTION THAT YOU WILL NEED TO INVESTIGATE. FLATTEN BEFORE PREDICTING THE 10 PARAMETERS ???
    #browser() 
    
    # # PERFORM NMS BEFORE STOCKING COMPUTATION
    # Index_Stocking_NMS <- NMS_ALL_PRIORS_FUN (Empty_Vox, Triangles_ID_All_Mx, INPUT_PRIOR_XYZWLHR_ExtP=PRIOR_XYZWLHR_ExtP, Para_Threshold_IoU, Para_Threshold_Prob) # OUTPUT_BINARY_SCORE, 
    # Stocking_Batch <- rep(0, dim(PRIOR_XYZWLHR_ExtP)[1])
    # Stocking_Summary <- table(Index_Stocking_NMS[[1]]$Batch)
    # Stocking_Batch[as.numeric(names(Stocking_Summary ))] <- as.vector(Stocking_Summary)
    # #Stocking_Calc <- torch_tensor(Stocking_Batch) # OUTPUT_STOCKING <- torch_unsqueeze(torch_tensor(Stocking_Batch),2)
    # Stocking_Calc <- torch_unsqueeze(torch_tensor(Stocking_Batch),2)
    
    # OUTPUT_STOCKING <- Conv_allP_allR_ExtR %>% 
    #   torch_flatten(start_dim = 3L) %>% 
    #   self$den_Regress_Stock() 

    # OUTPUT_RoI_IoU <- out4[[2]] %>% 
    #   torch_flatten(start_dim = 3L)

    # GENERATE AN OUTPUT RoI BINARY 
    out5 <- list(OUTPUT_BINARY_SCORE, PRED_Goffset_allP_allR_ExtR)  #, OUTPUT_STOCKING    ...... Stocking_Output,
    
    return(out5)
    
  }
)

#############################################################################################################################################################################################
#############################################################################################################################################################################################
# MAIN MODEL MODULE
#############################################################################################################################################################################################
#############################################################################################################################################################################################

VNet_ROI_model <- nn_module(
  "VNet_ROI_model",
  initialize = function() { 
    # cat("Calling intialise!")
    self$in_tr8 = InitTransition(nChan=1, outChan = 4,  kernel_size_Conv=2, padding_Conv = 0, stride_Conv = 2, kernel_size_Seq =3 , padding_Seq = 1) 
    
    self$down_tr16 = DownTransition(nChan=8, OutChan = 16,nConv=2, kernel_size_Conv=2, kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    self$down_tr32 = DownTransition(nChan=16, OutChan = 32, nConv=3, kernel_size_Conv=2, kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    self$down_tr64 = DownTransition(nChan=32, OutChan = 64, nConv=3, kernel_size_Conv=2, kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    #self$cent_tr256 = DownTransition(nChan=32, OutChan = 64, nConv=3, kernel_size_Conv=2, kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    
    #self$up_tr256 = UpTransition(nChan=64, OutChan = 32, nConv=3, kernel_size_Conv=2, kernel_size_Seq=3, padding_Conv=0, padding_Seq=1) # NEED TO SPECIFY HOW MANY CHANNELS IN RESPECTIVE OutRes IN SECOND ARG
    self$up_tr128 = UpTransition(nChan=32, OutChan = 16, nConv=3, kernel_size_Conv=2,  kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    self$up_tr64 = UpTransition(nChan=16, OutChan = 8, nConv=3, kernel_size_Conv=2,  kernel_size_Seq=3, padding_Conv=0, padding_Seq=1)
    self$up_tr32 = UpTransition(nChan=8, OutChan = 4, nConv=2, kernel_size_Conv=2,  kernel_size_Seq=3, padding_Conv=0, padding_Seq=1) # THIS OUTPUT NEEDS ATTENTION...
    self$RoI = RoI_Transition(pool_size=pool_size, nChan = 4, outChan1 = 8, outChan2 = 32, outChan3 = 1,kernel_size_Seq = 3, padding = 1) 
    #self$out_tr = OutputTransition(nChan=16,  Smax_dim_1 = 2, Smax_dim_2 = 13,  nRoI=64,RoIX=8, RoIY=8, RoIZ=16, Binary_Chan = 2, Stock_Chan = 12, BBox_Chan = 16)# flat_start_dim = 3L,
    self$out_tr = OutputTransition(nChan=32,  Smax_dim_1 = 3, Smax_dim_2 = 2,  nRoI=Para_min_Plot_Priors, RoIX=para_RoI_Pool_Dim_XY, 
                                   RoIY=para_RoI_Pool_Dim_XY, RoIZ=para_RoI_Pool_Dim_Z, 
                                   Binary_Chan = 1,  BBox_Chan = Para_TriShpParaCnt)#  Stock_Chan = 1, flat_start_dim = 3L,
    ### DOM DOM DOM !!! CHANGE BBox_Chan WHEN NO BotBox
    },
 
  forward = function(Vox_Den_ExtP, RoI_Vox, RoI_Dec, PRIOR_XYZWLHR_ExtP, TARGET_XYZWLHR_ExtP, TARGET_Vox_TID, TARGET_TID, TARGET_IoU_SUMMARY, epoch, Batch_Count ){
    #cat("Calling forward!") 
    # browser()
    out_in_tr8 <- self$in_tr8(Vox_Den_ExtP)                               # 64 16 16 16 16  (4)  NEW: 64  8  8  8  8
    # browser()
    #print("out_in_tr8")
    out_down_tr16 <- self$down_tr16(out_in_tr8[[1]])            # 64 32  8  8  8 (8)  NEW: 64 16  4  4  4
    #print("out_down_tr16")
    out_down_tr32 <- self$down_tr32(out_down_tr16)              # 64 64  4  4  4   (16  NEW: 64 32  2  2  2
    #print("out_down_tr32")
    # out_down_tr64 <- self$down_tr64(out_down_tr32)            # 64 128   2   2   2 (32)  NEW:  64 64  1  1  1
    out1 <-  self$up_tr128(list(out_down_tr32, out_down_tr16)) # 64 128   2   2   2   (32) NEW: 64 32  2  2  2
    #print("out1")
    out2 <- self$up_tr64(list(out1, out_in_tr8[[1]]))             # 64 64  4  4  4  (16)  NEW: 64 16  4  4  4
    #print("out2")
    out3 <- self$up_tr32(list(out2, out_in_tr8[[2]]))              # 64 32  8  8  8  (8)  NEW: 64  8  8  8  8
    #print("out3")
    #out4 <- self$up_tr32(list(out3, out_in_tr8[[2]]))                # 64 16 16 16 16  (4) NEW: 64  4 16 16 16
    out4 <-self$RoI(out3, Vox_Den_ExtP, RoI_Vox, RoI_Dec, PRIOR_XYZWLHR_ExtP, TARGET_XYZWLHR_ExtP, TARGET_Vox_TID, TARGET_TID, TARGET_IoU_SUMMARY)
    # print("out4_Roi")
    out5 <- self$out_tr(out4, PRIOR_XYZWLHR_ExtP) # 64 10 
 
    return(list(out4, out5))
  }
)

# TO PRINT OUT HOW MANY PARAMETERS THE MODEL HAS ..... VNet_ROI_model()

# device <- torch_device(if(cuda_is_available()) "cuda" else "cpu")
# 
model_Instance <- VNet_ROI_model()$to(device = device) # THIS INSTANTIATES THE MODEL

##############################################################################################################################################################################################
##############################################################################################################################################################################################
# LOSS MODULES
##############################################################################################################################################################################################
##############################################################################################################################################################################################

MultiTaskLoss <- nn_module(
  "MultiTaskLoss_Module",
  
  initialize = function(is_regression, reduction='none') {  # tasks
    # cat("MultiTaskLoss_Module!")
    self$is_regression = is_regression
    self$n_tasks = length(is_regression)
    self$log_vars = nn_parameter(torch_zeros(self$n_tasks, device=device), requires_grad = TRUE)     #torch.nn.Parameter(torch.zeros(self$n_tasks))
    self$reduction = reduction
  }, 
  
  forward = function(losses){ 
    dtype = torch_float32()
    # browser()
    stds = (torch_exp(self$log_vars)**(1/2))$to(dtype = dtype)  # SQUARE ROOT(EXP())
    self$is_regression = self$is_regression$to(dtype = dtype) 
    coeffs = 1 / ( (self$is_regression+1)*(stds**2) )
    multi_task_losses = coeffs*torch_stack(losses) + torch_log(stds)
    # browser()
    if (self$reduction == 'sum'){multi_task_losses = torch_sum(multi_task_losses)}
    if (self$reduction == 'mean'){multi_task_losses = torch_mean(multi_task_losses)}
    return(multi_task_losses)
  }
)
# browser()
# FUNCTION FOR WEIGHTED MSE LOSS
weighted_mse_loss <- function(input, target, weight){
  mse_loss_W  <-  torch_mean(weight * (input - target) ** 2)
  return (mse_loss_W)
}

mse_loss <- function(input, target){
  mse_loss <-  torch_mean((input - target) ** 2)
  return (mse_loss)
}

# GENERATE MSE WEIGHTS 

MSE_WEIGHTS_Temp <- torch_repeat_interleave(torch_unsqueeze(torch_tensor(as.array(Para_MSE_WEIGHTS), device=device), 1)
                                            , torch_tensor(as.integer(Para_min_Plot_Priors/2), device=device), dim = 1)
MSE_WEIGHTS_T <- torch_repeat_interleave(torch_unsqueeze(MSE_WEIGHTS_Temp, 1), 
                                         torch_tensor(as.integer(batch_size), device=device), dim = 1)

Loss_Module <- nn_module(
  "Loss_Module",
  initialize = function() { 
    self$para_N_hn = Para_N_hn
    self$MSE_Weighted = weighted_mse_loss # nn_mse_loss() #nn_l1_loss()
    self$bce_loss = nn_bce_with_logits_loss(reduction = "none") # combines a Sigmoid layer and the BCELoss in one single class
    self$MSE = nn_mse_loss() # squared L2 norm 
  }, 
  
  forward = function( TARGET_XYZWLHR_allP_allR_ExtR, PRIOR_XYZWLHR_allP_allR_ExtR, 
                      ACTUAL_Goffset_allP_allR_ExtR,  PRED_Goffset_allP_allR_ExtR, 
                      Binary_Score, 
                      IoUVox_Con_Prob, TARGET_MASK, 
                      TARGET_IoU_SUMMARY, INPUT_RoI_Dec,   # TARGET_STOCK,
                      epoch_T, Batch_Count_T, RUN_MULTI_TASK_LEVEL, MSE_WEIGHTS_T){ # pred_Stock, # PRED_Goffset_allP_allR_ExtR, Binary_Score, pred_Stock, IoUVox_Con_Prob, PRIOR_XYZWLHR_allP_allR_ExtR, ACTUAL_Goffset_allP_allR_ExtR, TARGET_IoU_SUMMARY, TARGET_MASK, TARGET_STOCK, INPUT_RoI_Dec

    ##########################################
    # INDEX POSITIVE PRIORS FOR self$smooth_l1
    ##########################################
 
    TARGET_Prior_Type <- torch_unsqueeze(TARGET_IoU_SUMMARY[,,19], 3)

    #Index_PosPriors_Array <- which(as.array(TARGET_Prior_Type$view(c(-1))$to(device="cpu")) ==1)  # (THIS HAS NON TENSOR PROCEDURES ... NEEDS ADDRESSING!!!!!)
    Index_PosPriors_Array <-(TARGET_Prior_Type$view(c(-1)) == 1)$nonzero()$squeeze()  # (as_tuple=False)
    PRED_Goffset_allP_allR_ExtR_View <- PRED_Goffset_allP_allR_ExtR$view(c(-1,Para_TriShpParaCnt ))
    PRED_Goffset_allP_allR_ExtR_View_Pos <- PRED_Goffset_allP_allR_ExtR_View[Index_PosPriors_Array,]
    ACTUAL_Goffset_allP_allR_ExtR_View <- ACTUAL_Goffset_allP_allR_ExtR$view(c(-1,Para_TriShpParaCnt ))
    ACTUAL_Goffset_allP_allR_ExtR_View_Pos <- ACTUAL_Goffset_allP_allR_ExtR_View[Index_PosPriors_Array,]

    if(RUN_CIoU_BINARY_LEVEL2 == "Yes"){ # RUN CIoU LOSS


      # TARGET_XYZWLHR_allP_allR_ExtR 12 64 10
      # PRIOR_XYZWLHR_allP_allR_ExtR  12 64 10
      # PRED_Goffset_allP_allR_ExtR   12 64 10
      # TARGET_IoU_SUMMARY            12 64 20
      # INPUT_RoI_Dec                 12 64  7
      # TARGET_Prior_Type             12 64  1
      # Binary_Score                  12 64  1
      
      # Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",
      #                       "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox",
      #                       "Z_TopTree")
      # Colnames_XYZWLHR[c(4,5,8,6,7,10,9)]
      # (x,y,z,w,l,h,alpha)
      # browser()  # x.add_(y)
      
      TARGET_xyzwlhr_ciou <-  torch_clone(TARGET_XYZWLHR_allP_allR_ExtR[,,c(4,5,8,6,7,10,9)])
      #TARGET_Test <- TARGET_XYZWLHR_allP_allR_ExtR[,,c(4,5,8,6,7,10,9)]
      TARGET_xyzwlhr_h <- TARGET_xyzwlhr_ciou[,,6]- TARGET_xyzwlhr_ciou[,,3]
      TARGET_xyzwlhr_ciou[,,6] <- TARGET_xyzwlhr_h
      
      PRED_xyzwlhr_ciou <-  torch_clone(PRED_Goffset_allP_allR_ExtR[,,c(4,5,8,6,7,10,9)])
      #TARGET_Test <- TARGET_XYZWLHR_allP_allR_ExtR[,,c(4,5,8,6,7,10,9)]
      PRED_xyzwlhr_h <- PRED_xyzwlhr_ciou[,,6]- PRED_xyzwlhr_ciou[,,3]
      PRED_xyzwlhr_ciou[,,6] <- PRED_xyzwlhr_h



      ### DOM DOM DOM THIS IS WHERE YOU GENERATE THE CIoU LOSS
      Output_IoU = cal_complete_iou_3d(PRED_xyzwlhr_ciou, TARGET_xyzwlhr_ciou, enclosing_type ="smallest")
      iou_loss <- Output_IoU[[1]] 
      iou <- Output_IoU[[2]]
      CIoU_Loss <- torch_mean(iou_loss)
      Loc_Loss_Positive <- CIoU_Loss
      # CIoU <- CIoU_GPU_FUN(TARGET_XYZWLHR_allP_allR_ExtR, PRIOR_XYZWLHR_allP_allR_ExtR, PRED_Goffset_allP_allR_ExtR,  
      #                                TARGET_IoU_SUMMARY,  INPUT_RoI_Dec, TARGET_Prior_Type, Binary_Score,
      #                                Para_Target_Base, Para_Target_Z_Height, Para_TriShpParaCnt, Para_Base_WL,
      #                                Batch_Count_T, epoch_T)
      # browser()
     
      # allP_VoxFScore_posPredGT_T <- allP_FSCORE_Output[[1]]
      # loss_1min_VoxFScore <- allP_FSCORE_Output[[2]]
      # Loc_Loss_Positive <- 0
      
      
    }else{ # ELSE RUN REG LOS
      
      # PREPARING THE WEIGHTED TENSOR FOR MSE COMPUTATION (NOTE IF ALL MSE_WEIGHTS_T VALUES ARE 1 THEN NOT WEIGHTED)
      Index_Size <- PRED_Goffset_allP_allR_ExtR$size(1)
      MSE_WEIGHTS_T <- MSE_WEIGHTS_T[1:Index_Size,,]
      MSE_WEIGHTS_T_view <- MSE_WEIGHTS_T$view(c(-1,Para_TriShpParaCnt))
      Loc_Loss_Positive <- self$MSE_Weighted(PRED_Goffset_allP_allR_ExtR_View_Pos, ACTUAL_Goffset_allP_allR_ExtR_View_Pos, MSE_WEIGHTS_T_view)
    }

   #################################################################################################################################################################
    #################
    # CONFIDENCE LOSS
    #################
    #################################################################################################################################################################
    ##############################################################################################################################################################################################################
    
    TARGET_Prior_Type <-  TARGET_IoU_SUMMARY[,,19]$view(c(-1))
    BINARY_SCORE_View <- Binary_Score$view(c(-1))
    conf_loss_all = self$bce_loss(BINARY_SCORE_View, TARGET_Prior_Type)
    conf_loss = torch_mean(conf_loss_all)

    #################################################################################################################################################################
    ################
    # IoU VOXEL-WISE
    ################
    #################################################################################################################################################################
    ##############################################################################################################################################################################################################
    #browser()
    #IoUVox_Con_Prob, TARGET_MASK,
    
    # Int_IoUVoxConProb <- torch_sum(IoUVox_Con_Prob*TARGET_MASK)
    # Union_IoUVoxConProb <- torch_sum(IoUVox_Con_Prob + TARGET_MASK -(IoUVox_Con_Prob*TARGET_MASK))
    # IoU_IoUVox_Loss <- torch_squeeze(1 - (Int_IoUVoxConProb/Union_IoUVoxConProb))
    #browser()

    IoUVox_Con_Prob_View <- IoUVox_Con_Prob$view(c(-1,pool_size))
    IoUVox_Con_Prob_View_Pos <- IoUVox_Con_Prob_View[Index_PosPriors_Array,,,]
    
    TARGET_MASK_View <- TARGET_MASK$view(c(-1,pool_size))
    TARGET_MASK_View_Pos <- TARGET_MASK_View[Index_PosPriors_Array,,,]
    
    IoU_IoUVox_Loss <- DICE_LOSS_FUN(IoUVox_Con_Prob_View_Pos, TARGET_MASK_View_Pos)
    
    #################################################################################################################################################################
    ##############################################################################################################################################################################################################
    
    
    # browser()
    # if(RUN_MULTI_TASK_LEVEL > 2 ){ # | RUN_FSCORE_BINARY_LEVEL2 == "Yes"
    #   
    #   source(paste(FOLDER_TORCH_CODE, "TORCH_FScore_Compute",Version_RCode,".R", sep=""))
    #   allP_FSCORE_Output <- FSCORE_COMPUTE_FUN(TARGET_XYZWLHR_ExtP, TARGET_IoU_SUMMARY, PRED_Goffset_allP_allR_ExtR, PRIOR_XYZWLHR_allP_allR_ExtR,
    #                                            INPUT_RoI_Dec, TARGET_Prior_Type, Binary_Score,
    #                                            Para_Target_Base, Para_Target_Z_Height, Para_TriShpParaCnt, Para_Base_WL,
    #                                            Batch_Count_T, epoch_T)
    #   allP_VoxFScore_posPredGT_T <- allP_FSCORE_Output[[1]]
    #   loss_1min_VoxFScore <- allP_FSCORE_Output[[2]]
    # 
    # }
    
    # else{ # RUNNING MODEL WITHOUT VoxFScore
    #   if(RUN_CIoU_BINARY_LEVEL2 == "No"){ ### DOM DOM DOM !!! THIS MAY BE CONFUSING... NEED TO CLEAN THIS IF STATEMENT OUT
    #     allP_VoxFScore_posPredGT_T <- torch_tensor(matrix(rep(0,8), ncol=4, nrow=2))
    #     loss_1min_VoxFScore <- torch_tensor(NaN)
    #     }
    # }
   

    # #########################################################################################
    # # WHEN NOT RUNNING FSCORE EVERY EPOCH BUT RUNNING EVERY N ITERATION FOR CHECKING RESULTS.
    # #########################################################################################
    # #browser() # ... TARGET_XYZWLHR_ExtP
    # if(epoch%%30 == 0){
    #   RUN_Compute_Fscore_EveryN <- "Yes"
    # }else{
    #   RUN_Compute_Fscore_EveryN <- "No"
    # }
    # 
    # if(RUN_Compute_Fscore_EveryN == "Yes"){
    #   source(paste(FOLDER_TORCH_CODE, "TORCH_FScore_Compute",Version_RCode,".R", sep=""))
    #   allP_FSCORE_Output <- FSCORE_COMPUTE_FUN(TARGET_XYZWLHR_ExtP, TARGET_IoU_SUMMARY, PRED_Goffset_allP_allR_ExtR, PRIOR_XYZWLHR_allP_allR_ExtR, 
    #                                                    INPUT_RoI_Dec, TARGET_Prior_Type, Binary_Score,
    #                                                    Para_Target_Base, Para_Target_Z_Height, Para_TriShpParaCnt, Para_Base_WL,
    #                                                    Batch_Count_T, epoch_T)
    #   allP_VoxFScore_posPredGT_T <- allP_FSCORE_Output[[1]]
    #   loss_1min_VoxFScore <- allP_FSCORE_Output[[2]]
    # }

    # #################################################################################################################################################################
    # #############################
    # # MULTI-TASK LOSS COMPUTATION
    # #############################
    # #################################################################################################################################################################
    # # print("MULTI-TASK COMP")
    # 
    # # DOM DOM DOM !!! DOES THIS MULTI-TASK LOSS STILL WORK WHEN RUNNING CIoU
    #        ### DO I STILL RUN THE CIoU VALUE FOR EACH RoI THROUGH MSE... NO YOU DO NOT!!! 
    # 
    # 
    # if(RUN_MULTI_TASK_LEVEL == 2){
    #  
    #   if(RUN_CIoU_BINARY_LEVEL2 == "Yes"){
    #     losses <- torch_stack(list(CIoU_Loss, conf_loss)) # Add FScore
    #     multitaskloss <- torch_sum(losses)
    #     allP_VoxFScore_posPredGT_T <- torch_tensor(NaN)
    #     loss_1min_VoxFScore <- torch_tensor(NaN)
    #     Loc_Loss_Positive <- torch_clone(CIoU_Loss) # JUST FOR PLOTTING
    #     
    #   }else{
    #     is_regression <- torch_tensor(c(TRUE, FALSE), device=device) # True: Regression/nn_l1_loss, False: Binary Classification/nn_cross_entropy_loss   
    #     losses <- list(Loc_Loss_Positive, conf_loss) # Add FScore
    #   }
    # }else{ # RUN THREE TASKS
    #   is_regression <- torch_tensor(c(TRUE, FALSE, TRUE), device=device) # True: Regression/nn_l1_loss, False: Binary Classification/nn_cross_entropy_loss   
    #   losses <- list(Loc_Loss_Positive, conf_loss, loss_1min_VoxFScore) # Add FScore
    # }
    # 
    # # RUN MULTI-TASK WITH BAYSIAN HYPERPARAMETER OPTIMISATION
    # if(RUN_OPTIM_HYPERPARAMETER_MULTITASK == "Yes"){
    #   multitaskloss_instance <- MultiTaskLoss(is_regression, reduction = 'sum') #DOM DOM DOM !!! NOTE HOW INSTANCE ARG ARE PRESENTED
    #   multitaskloss <- multitaskloss_instance(losses)
    # }else{
    #   multitaskloss <- torch_sum(losses)
    # }
    # # browser()
    # FINAL_LOSS_Output <- list(Loc_Loss_Positive, conf_loss, loss_1min_VoxFScore,  multitaskloss, allP_VoxFScore_posPredGT_T) # Stock_MSE_Loss,
    
    losses <- torch_stack(list(Loc_Loss_Positive, conf_loss, IoU_IoUVox_Loss))
    multitaskloss <- torch_sum(losses)
    FINAL_LOSS_Output <- list(Loc_Loss_Positive, conf_loss, IoU_IoUVox_Loss,  multitaskloss)
    
    return(FINAL_LOSS_Output) 
    }
  ) # Loss Module

# TO PRINT OUT HOW MANY PARAMETERS THE LOSS HAS..... Loss_Module()
Loss_Module_Instance <- Loss_Module() 

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
# RUN MODEL AND LOSS CALCULATION
##############################################################################################################################################################################################################
##############################################################################################################################################################################################################

RUN_MODEL_LOSS_FUN <- function(INPUT_LIST, TARGET_LIST, FLIGHT_PLOT_ID_LIST, epoch_T, Batch_Count_T){

  #################################
  # GENERATE INPUT AND TARGET LISTS 
  #################################

  INPUT_Vox_Den <- torch_clone(INPUT_LIST[[1]])#$to(device = device))$to(device = device)     # 16  1 16 16 40   1 CHANNEL PROVIDING DENSITY OF POINTS IN EACH VOXEL
  INPUT_RoI_Vox <- torch_clone(INPUT_LIST[[2]])#$to(device = device))$to(device = device)            # 16 64  7         64 ROI LOCATOINS AND ID
  INPUT_RoI_Dec <- torch_clone(INPUT_LIST[[3]])#$to(device = device))$to(device = device)             # 16 64  7         64 ROI LOCATOINS AND ID
  INPUT_PRIOR_XYZWLHR_ExtP  <- torch_clone(INPUT_LIST[[4]])#$to(device = device))$to(device = device)  # 16 64 10         64 UNIQUE PRIORS WITH XYZWLHR LCOATION

  TARGET_Vox_TID <- torch_clone(TARGET_LIST[[1]])#$to(device = device))$to(device = device)         # 16  1 16 16 40   1 CHANNEL PROVIDING TARGET_TID OF EACH VOXEL (TARGET FOR IoU COEF)
  TARGET_IoU_SUMMARY <- torch_clone(TARGET_LIST[[2]])#$to(device = device))$to(device = device)         # 16 64  20         # 64 SUMMARY OF IoU AND BINARY BACKGROUND:TREE (0:1) FOR EACH PRIOR
  TARGET_XYZWLHR_ExtP <- torch_clone(TARGET_LIST[[3]])#$to(device = device))$to(device = device)       # 16 64 10         # 64 GT XYZWLHR LOCATION (BEST ONE FOR EACH PRIOR???)
  TARGET_TID <- torch_clone(torch_squeeze(TARGET_LIST[[4]], 3))#$to(device = device))$to(device = device)  # 16 64      # 64 TARGET_TID REPRESENTING 64 GT XYZWLHR LOCATION (i.e. TARGET_XYZWLHR_ExtP)

  FlightID_LIST <- torch_clone(FLIGHT_PLOT_ID_LIST[[1]]) #as.array(FLIGHT_PLOT_ID_LIST[[1]])#$to(device = device)) 
  PlotID_LIST <- torch_clone(FLIGHT_PLOT_ID_LIST[[2]])  #as.array(FLIGHT_PLOT_ID_LIST[[2]])#$to(device = device)) 
  DIR_LIST <- FLIGHT_PLOT_ID_LIST[[3]] #as.array(FLIGHT_PLOT_ID_LIST[[3]]) 

  ###########
  # RUN MODEL 
  ###########
  #browser()
  OUTPUT_LIST <- model_Instance(INPUT_Vox_Den, INPUT_RoI_Vox, INPUT_RoI_Dec, INPUT_PRIOR_XYZWLHR_ExtP, TARGET_XYZWLHR_ExtP, TARGET_Vox_TID, TARGET_TID, TARGET_IoU_SUMMARY, epoch_T, Batch_Count_T) # p[[1]][[1]]  16 1 16 16 40    p[[1]][[2]]  16 64  7 p[[1]][[3]]  16 64 16 # OLD list(list(INPUT_VOX_DF, INPUT_GT, INPUT_ROI), INPUT_TARGET_ROI_AND_CLASS)

  ##################
  # ROI LEVEL OUTPUT
  ##################

  OUTPUT_out4 <-OUTPUT_LIST[[1]] 
  TARGET_XYZWLHR_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[1]])#$to(device = device))$to(device = device)             # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  ACTUAL_Goffset_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[2]])#$to(device = device))$to(device = device)     # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  PRIOR_XYZWLHR_allP_allR_ExtR <- torch_clone(OUTPUT_out4[[3]])#$to(device = device))$to(device = device)    # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  OUTPUT_Vox_Den_RoI <- torch_clone(OUTPUT_out4[[4]])#$to(device = device))$to(device = device) 
  OUTPUT_RoI_32Ch <- torch_clone(OUTPUT_out4[[5]])#$to(device = device))$to(device = device)    # 16 64 32  8  8 16     # 64 ROIS, EACH WITH 16 CHANNELS FOR ALL VOXELS IN ROI
  IoUVox_Con_Prob <- torch_clone(OUTPUT_out4[[6]])#$to(device = device))$to(device = device)       # 16 64 1  8  8 16      # 64 ROIS, EACH WITH  1 "softmax" CHANNELS FOR ALL VOXELS IN ROI
  TARGET_MASK <- torch_clone(OUTPUT_out4[[7]])#$to(device = device))$to(device = device)        # 16 64 8  8 16         # 64 ROIS, BINARY MASK FOR ALL VOXELS IN ROI

  ###################
  # PLOT LEVEL OUTPUT
  ###################
  OUTPUT_out5 <- OUTPUT_LIST[[2]]
  Binary_Score <- torch_clone(OUTPUT_out5[[1]])#$to(device = device))$to(device = device)        # 16 64  2        # 64 ROIS, 2 Scores for background or tree
  PRED_Goffset_allP_allR_ExtR <- torch_clone(OUTPUT_out5[[2]])#$to(device = device))$to(device = device)       # 16 64  16        # 64 ROIS, 16 offsets for locations

  # TO PRINT OUT HOW MANY PARAMETERS THE MODEL HAS..... Loss_Module_Instance()
  Loss_all <- Loss_Module_Instance(  TARGET_XYZWLHR_allP_allR_ExtR, PRIOR_XYZWLHR_allP_allR_ExtR, 
                                     ACTUAL_Goffset_allP_allR_ExtR,  PRED_Goffset_allP_allR_ExtR, 
                                     Binary_Score, 
                                     IoUVox_Con_Prob, TARGET_MASK,  
                                     TARGET_IoU_SUMMARY,  INPUT_RoI_Dec,   # TARGET_STOCK,
                                     epoch_T, Batch_Count_T, RUN_MULTI_TASK_LEVEL, MSE_WEIGHTS_T)  # TARGET_XYZWLHR_ExtP, INPUT_PRIOR_XYZWLHR_ExtP, 
  # browser()
  return(list(OUTPUT_LIST, Loss_all))
}
 
