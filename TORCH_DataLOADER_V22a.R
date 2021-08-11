# initialize() creates an inventory of scan and mask file names, 
# to be used by .getitem() when it actually reads those files.

TREES_DS_FUN <- dataset(
  name = "TREES_DS_FUN",
  
  initialize = function(DATA_Type) {
    self$DIR <- self$POINT_TO_DATA(DATA_Type)
  },

  .getitem = function(index) {

    ### DOM DOM DOM !!! BELOW RETURNS INPUT-TARGET PAIRS IN ORDER. 
    # IS IT WORTH USING "parameter random_sampling is true" TO PERFORM WIGHTED SAMPLING AND FAVOUR PLOTS WITH MANY WELL DEFINED TREES.... (NEED TO CREATE DATA TO FAVOUR SOME PLOTS)

    ##############################################################################################################################################################################################
    ######################
    # INPUTS FOR MODEL RUN
    ######################
    
    ############################
    # INPUT_VOX:    "_LAS_Vox_N"
    ############################
    INPUT_VOX <- read.csv(paste(self$DIR[[1]][[1]][index], "/", self$DIR[[2]][[1]][index], sep=""))
    
    ### DOM DOM DOM !!! HERE I REMOVE RESIDUAL POINTS (i.e. TID=1 above ground IS CONVERTED TO TID=0)... 
          # YOU COULD CLEAN THIS UP IN TORCH_PreMASTER_PLOT_SAMPLING_V1 SO THAT ITS REFLECTIVE OF REAL DATA IN FUTURE
    Summary_oneVox <- as.data.frame(INPUT_VOX %>%
                                      dplyr::group_by(TID) %>%
                                      dplyr::summarise(Zmin = min(Z),
                                                       ZQuant = quantile(Z, probs = c(0.1)),  .groups = 'drop'))
    oneVox_CutOff <- min(Summary_oneVox$Zmin[which(Summary_oneVox$TID > 1)])
    Index_Remove_Residual_Noise <- which(INPUT_VOX$Z > oneVox_CutOff & INPUT_VOX$TID == 1) 
    INPUT_VOX$TID[Index_Remove_Residual_Noise] <- 0
    INPUT_VOX$Count_Norm[Index_Remove_Residual_Noise] <- 0
    # browser()
    INPUT_VOX_Den_T <- torch_tensor(array(INPUT_VOX$Count_Norm,
                                          dim = c(1, Para_Target_Base, Para_Target_Base, Para_Target_Z_Height)), device = device) #$to() 
    
    # INPUT_VOX_TID_T <- torch_tensor(array(INPUT_VOX$TID,
    #                                       dim = c(1, Para_Target_Base, Para_Target_Base, Para_Target_Z_Height)))
    # 
    # INPUT_VOX_Den_TID_T <- torch_squeeze(torch_stack(list(INPUT_VOX_Den_T,INPUT_VOX_TID_T), dim = 1L))
    
    
    # Test <- hist(as.vector(as.array(torch_squeeze(INPUT_VOX_Den_TID_T[1,,,]))))
    # browser()
    # THE INPUT VOXEL HAS BOTH TID VALUE AND THE COUNT_Norm (DENSITY OF POINTS WITHIN EACH VOXEL)
    
    # CHECKING MY 
    # Test <- readLAS(paste(Comp, "/CNN/THINNED_SAMPLES_TORCH_V18/TRAIN_DATA/Flight_1/LAS/LAS_MP1/Vox_P/LAS_F1_MP1_P30_Vox.laz", sep=""), select = "xyzp0")
    # 

    # 
    # Test <- torch_tensor(array(INPUT_VOX$X),
    #                                       dim = c(1, Para_Target_Base, Para_Target_Base, Para_Target_Z_Height)))
    # table(Test$Y)
    # as.array(Test)[1:(16*16)]
    # browser()
    
    ### DOM THOUGHT !!! ... THE SEGMENTED LAS FILE IS LIKE A MASK, YOU WANT INPUT TO BE PLAIN DATA... THINK ABOUT HOW YOU WANT LOSS TO WORK WITH INPUT:TARGET (DICE??) 
    ### MAKE GROUND A CLASS TOO!!! IN BINARY TREE BACKGROUND, AND GROUND!!!
    #     ### BUT YOU ARE USING THE PRIORS TO CLASSIFY GOOD AND BAD ... SO HOW DO YOU CLASSIFY GROUND WITH PRIOR XYZWLHR

    ###################################################
    # INPUT_ROI         _List_RoI_Vox_DF_S64_XYZWLH.csv
    ###################################################
    INPUT_ROI_Vox <- read.csv(paste(self$DIR[[1]][[2]][index], "/",  self$DIR[[2]][[2]][index],  sep=""))
    INPUT_ROI_Vox_T <- torch_tensor(as.matrix(INPUT_ROI_Vox, dim = c(nrow(INPUT_ROI_Vox),ncol(INPUT_ROI_Vox))), device = device) #$to() 
    
    INPUT_ROI_Dec <- read.csv(paste(self$DIR[[1]][[3]][index], "/",  self$DIR[[2]][[3]][index],  sep=""))
    INPUT_ROI_Dec_T <- torch_tensor(as.matrix(INPUT_ROI_Dec, dim = c(nrow(INPUT_ROI_Dec),ncol(INPUT_ROI_Dec))), device = device) # $to() 
    # browser()
    ###################################################
    # INPUT_PRIORS      _XYZWLHR_BestWorst_plotPriors_S
    ###################################################
    INPUT_PRIOR_XYZWLHR <- read.csv(paste(self$DIR[[1]][[4]][index], "/",  self$DIR[[2]][[4]][index],  sep=""))
    
    # REMOVING BotBox COLUMNS
    if(Para_TriShpParaCnt == 10) {
      INPUT_PRIOR_XYZWLHR <- INPUT_PRIOR_XYZWLHR[,-grep("BotBox", colnames(INPUT_PRIOR_XYZWLHR))]
      }
    
    # browser()
    # DOM DOM DOM !!! YOU CAN REMOVE THIS ONCE THE ANGLE IS NORMALISED IN Pre_Master
    # if(max(INPUT_PRIOR_XYZWLHR$R_BotBox) > 2){
    #   INPUT_PRIOR_XYZWLHR$R_BotBox <-  INPUT_PRIOR_XYZWLHR$R_BotBox/90
    #   INPUT_PRIOR_XYZWLHR$R_TopBox <-  INPUT_PRIOR_XYZWLHR$R_TopBox/90
    # }
           
    Index_ColNames <- which(colnames(INPUT_PRIOR_XYZWLHR) == "PriorID")
    if(length(Index_ColNames) == 1){INPUT_PRIOR_XYZWLHR <- INPUT_PRIOR_XYZWLHR[, -Index_ColNames]}
    INPUT_PRIOR_XYZWLHR_T <- torch_tensor(as.matrix(INPUT_PRIOR_XYZWLHR, dim = c(nrow(INPUT_PRIOR_XYZWLHR),ncol(INPUT_PRIOR_XYZWLHR))), device = device)#$to() 

    #####################
    # TARGETS FOR VOX TID
    #####################

    TARGET_VOX_TID_T <- torch_tensor(array(INPUT_VOX$TID,
                                           dim = c(1, Para_Target_Base, Para_Target_Base, Para_Target_Z_Height)), device = device)#$to() 
    
    # ####################################
    # # TARGET_STOCKING   _Stocking_VoxCnt
    # ####################################
    # TARGET_STOCK <- read.csv(paste(self$DIR[[1]][[5]][index], "/", self$DIR[[2]][[5]][index], sep=""))
    # TARGET_STOCK <- length(which(TARGET_STOCK$TID > 1))
    # # STOCKING ONLY GOES UPTO 12 AND ANY MORE THEN IT BECOMES 12
    # # TARGET_STOCK <- ifelse(TARGET_STOCK > 12, 12, TARGET_STOCK) 
    # TARGET_STOCK_T <- torch_tensor(array(TARGET_STOCK, dim = c(1)), device = device) #$to()  # + 1L # SO ZERO STOCKING IS 1 ....... AND 13 STOCKING IS 12 (OTHERWISE TARGET "ZERO" IS OUT OF BOUNDS)
    
    ##################################################
    # PRIORS      _BestWorst_Summary_Prior_TID_S
    ##################################################
    # [1] "FlightID"             "PlotID"               "TID"                  "Plot_ID"              "Prior"                "Count_IoU"            "Total_Vox"           
    # [8] "Portion_IoU_CorrecIn" "Portion_IoU_WrongIn"  "TP"                   "FN"                   "FP"                   "Recall"               "Precision"           
    # [15] "FScore"               "PRIOR_Z_TopBox"       "TID_Z_TopBox"         "Diff_ZTopBox"         "Prior_Type"           "RoI_ID"    
    
    IoU_SUMMARY <- read.csv(paste(self$DIR[[1]][[5]][index], "/",  self$DIR[[2]][[5]][index],  sep=""))
    IoU_SUMMARY_T <- torch_tensor(as.matrix(IoU_SUMMARY,  dim = c(nrow(IoU_SUMMARY),ncol(IoU_SUMMARY))), device = device) #$to() 
    
    # JUST THE PRIOR IS USED HERE
    # IoU_SUMMARY$Prior_Type <- IoU_SUMMARY$Prior_Type # + 1L # DOM DOM DOM !!! Making Target class and index and not class ID
    # IoU_SUMMARY_T <- torch_tensor(as.matrix(IoU_SUMMARY$Prior_Type,  dim = c(nrow(IoU_SUMMARY),1)))
    
    # browser()
    
    ###################################################
    # TARGET_XYZWLHR    _Ext15X15_allT_LocBox_XYZWLHR_N 
    ###################################################
    TARGET_XYZWLHR <- read.csv(paste(self$DIR[[1]][[6]][index], "/", self$DIR[[2]][[6]][index], sep="")) # TARGET_XYZWLHR_T <- torch_tensor(as.matrix(TARGET_XYZWLHR, dim = c(nrow(TARGET_XYZWLHR),ncol(TARGET_XYZWLHR))))
    
    # REMOVING BotBox COLUMNS
    if(Para_TriShpParaCnt == 10) {
      TARGET_XYZWLHR <- TARGET_XYZWLHR[,-grep("BotBox", colnames(TARGET_XYZWLHR))]
    }
    
    # TEMPORARY WORK AROUND UNTIL INTEGRATED INTO MAIN CODE
    Index_ZeroA <- which(TARGET_XYZWLHR$W_TopBox == 0)
    if(length(Index_ZeroA) > 0) {
      TARGET_XYZWLHR$W_TopBox[Index_ZeroA] <- 1/15
    }
    Index_ZeroB <- which(TARGET_XYZWLHR$L_TopBox == 0)
    if(length(Index_ZeroB) > 0) {
      TARGET_XYZWLHR$L_TopBox[Index_ZeroB] <- 1/15
    }
    
    # TARGET_XYZWLHR$W_TopBox(TARGET_XYZWLHR$W_TopBox)
    # Index_ZeroB <- which(TARGET_XYZWLHR$L_TopBox == 0)
    # if(length(Index_ZeroA) > 0 | length(Index_ZeroB) > 0) {browser()}
    
    # DOM DOM DOM !!! YOU CAN REMOVE THIS ONCE THE ANGLE IS NORMALISED IN Pre_Master
    # browser()
    if(max(TARGET_XYZWLHR$R_TopBox) > 2){
      if(Para_TriShpParaCnt == 16){ # DEPENDING ON HOW MANY PARAMETERS USED IN MODEL
        TARGET_XYZWLHR$R_BotBox <-  TARGET_XYZWLHR$R_BotBox/90
        TARGET_XYZWLHR$R_TopBox <-  TARGET_XYZWLHR$R_TopBox/90
      }else{
        TARGET_XYZWLHR$R_TopBox <-  TARGET_XYZWLHR$R_TopBox/90
      }

    }

    # browser()
    TARGET_XYZWLHR <- TARGET_XYZWLHR[match(IoU_SUMMARY$TID, TARGET_XYZWLHR$TID),]
    TARGET_XYZWLHR_Reduced <- TARGET_XYZWLHR[,-which(colnames(TARGET_XYZWLHR) %in% c("TID", "Sample"))]
    TARGET_XYZWLHR_T <- torch_tensor(as.matrix(TARGET_XYZWLHR_Reduced, dim = c(nrow(TARGET_XYZWLHR_Reduced),ncol(TARGET_XYZWLHR_Reduced))), device = device) #$to() 
    
    TARGET_TID <- TARGET_XYZWLHR$TID
    TARGET_TID_T <- torch_tensor(as.matrix(TARGET_TID, dim = length(TARGET_XYZWLHR$TID)), device = device) # $to(device = device) 

    MAIN_DIR <- self$DIR[[1]][[7]][index]
    Flight_T <- torch_tensor(as.numeric(numextract_all(self$DIR[[2]][[1]][index])[1]), device = device) #$to() 
    Plot_T <- torch_tensor(as.numeric(numextract_all(self$DIR[[2]][[1]][index])[2]), device = device) #$to() 
    # browser()
    return(list(list(INPUT_VOX_Den_T, INPUT_ROI_Vox_T, INPUT_ROI_Dec_T, INPUT_PRIOR_XYZWLHR_T), 
                list(TARGET_VOX_TID_T,  IoU_SUMMARY_T, TARGET_XYZWLHR_T, TARGET_TID_T),  # TARGET_STOCK_T,
                list(Flight_T, Plot_T, MAIN_DIR))) # TARGET_STOCKING Output[[2]][[3]]
  },

  ##############################################################################################################################################################################
   .length = function() {
     length(self$DIR[[1]][[1]])
   },

  ##############################################################################################################################################################################
  POINT_TO_DATA = function(DATA_Type) {

    # TEST DATA USES ALL THE SAME FILES FOR TRAIN AND VALIDATE EXCEPT FOR THE VOXEL LAYER (INPUT VOX) WHICH IS DIFFERENT
    Orig_Data_Type <- DATA_Type
    if(DATA_Type == "TEST_DATA"){
      # TEST INFORMATION: ALL IS SAME BUT VOX IS ALS (_Vox_ALS) # Y:\CNN\THINNED_SAMPLES_TORCH_V900\TRAIN_DATA\Flight_1\LAS\LAS_MP1\LAS_ALS
      DATA_Type <- c("TRAIN_DATA", "VALIDATE_DATA")
    }
    
    Files_INPUT_VOX <- c()
    Files_INPUT_ROI_Vox <- c()
    Files_INPUT_ROI_Dec <- c()
    Files_INPUT_PRIOR_XYZWLHR <- c()
    
    # Files_TARGET_STOCKING <- c()
    Files_IoU_SUMMARY <- c()
    Files_TARGET_XYZWLHR <- c()
    
    Folders_INPUT_VOX <- c()
    Folders_INPUT_ROI_Vox <- c()
    Folders_INPUT_ROI_Dec <- c()
    Folders_INPUT_PRIOR_XYZWLHR <- c()
    
   # Folders_TARGET_STOCKING <- c()
    Folders_IoU_SUMMARY <- c()
    Folders_TARGET_XYZWLHR <- c()
    Folder_FLIGHT <- c()
    
    for(D in 1:length(DATA_Type)){
      Folders_oneDATA_TYPE <- paste(FOLDER_MAIN_DATA, "/", DATA_Type[D], sep="")
      flights <- list.files(Folders_oneDATA_TYPE, pattern = "Flight_")# [1] # DOM DOM DOM !!! YOU ARE ONLY DOING FLIGHT 1
      
      for(ff in 1:length(flights)){
        ##############################################################################################################################################################################
        ##################################
        # LIST ALL THE FILES IN THE FLIGHT
        ##################################

        #browser()
        if(length(DATA_Type) == 1){  
          # VALIDATE OR TRAIN DATA 
          oneFolder_INPUT_VOX <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/CSV_MP1/VOX_DF", sep="") 
          Listed_VOX <- list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N") 
        }else{
          # TEST DATA  # WHEN length(DATA_Type) != 1 THEN ITS A TEST_DS AND THEREFORE GET VOXEL FROM ALS
          oneFolder_INPUT_VOX <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/CSV_MP1/VOX_DF_ALS", sep="")
          Listed_VOX <- list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N_ALS")
        }
        
        oneFolder_INPUT_ROI_Vox <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/INPUT_ROI", sep="") 
        Listed_INPUT_ROI_Vox <- list.files(oneFolder_INPUT_ROI_Vox , pattern = paste("_List_RoI_Vox_DF_S",Para_min_Plot_Priors,".csv", sep="")) 
        #
        oneFolder_INPUT_ROI_Dec <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/INPUT_ROI", sep="") 
        Listed_INPUT_ROI_Dec <- list.files(oneFolder_INPUT_ROI_Dec , pattern = paste("_List_RoI_Dec_DF_S",Para_min_Plot_Priors,".csv", sep="")) 
#
        oneFolder_INPUT_PRIORS <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/PRIORS", sep="")
        Listed_INPUT_PRIOR_XYZWLHR <- list.files(oneFolder_INPUT_PRIORS , pattern = "_XYZWLHR_BestWorst_plotPriors_S")
#        
        # oneFolder_TARGET_STOCKING <- paste(Folders_oneDATA_TYPE, "/", flights[ff],"/CSV/CSV_MP1/Stocking_VoxCnt_GT", sep="")
        # Listed_TARGET_STOCKING <- list.files(oneFolder_TARGET_STOCKING , pattern = "_Stocking_VoxCnt")
 #       
        oneFolder_IoU_SUMMARY <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/PRIORS", sep="") 
        Listed_IoU_SUMMARY <- list.files(oneFolder_IoU_SUMMARY , pattern = "_BestWorst_Summary_Prior_TID_S") 
  #      
        oneFolder_TARGET_XYZWLHR <- paste(Folders_oneDATA_TYPE, "/", flights[ff],"/CSV/CSV_MP1/LocBox_allP15X15", sep="")
        Listed_TARGET_XYZWLHR <- list.files(oneFolder_TARGET_XYZWLHR , pattern = "_Ext15X15_allT_LocBox_XYZWLHR_N")
 #       
        oneFolder_FLIGHT <- paste(Folders_oneDATA_TYPE, "/", flights[ff], sep="")
        
        # browser()
        ##############################################################################################################################################################################
        ##############################################################
        # IDENTIFY MISSING FILES (CHECK ALL FILES REPRESENT SAME PLOT)
        ##############################################################
        
        # "VOX", "ROI_Vox", "ROI_Dec", "PRIOR_XYZWLHR", "TARGET_STOCKING", "IoU_SUMMARY", "TARGET_XYZWLHR", "MAIN_FOLDERS"
        
        allList_Folders <-list( oneFolder_INPUT_VOX, oneFolder_INPUT_ROI_Vox, oneFolder_INPUT_ROI_Dec , oneFolder_INPUT_PRIORS, 
                                 oneFolder_IoU_SUMMARY, oneFolder_TARGET_XYZWLHR, oneFolder_FLIGHT)  # oneFolder_TARGET_STOCKING,
         
        allList_Files <-list(Listed_VOX, Listed_INPUT_ROI_Vox, Listed_INPUT_ROI_Dec,  Listed_INPUT_PRIOR_XYZWLHR, 
                              Listed_IoU_SUMMARY, Listed_TARGET_XYZWLHR)  # Listed_TARGET_STOCKING, 
        
        # browser()
   
        Files_in_Lists <- unlist(lapply(allList_Files, function(x) length(x)))
        if(min(Files_in_Lists) > 0){
          File_Lengths <- lengths(allList_Files)
          Min_File_Length <- min(File_Lengths)
          Max_File_Length <- max(File_Lengths)
          # IF THERE ARE MISSING DATA
          if(Min_File_Length != Max_File_Length){
            # IDENTIFY COMMON PLOTS IN ALL LISTS
            List_P <- list()
            for(LL in 1:length(allList_Files)){
              oneCol <- substr(allList_Files[[LL]],1, 9)
              oneList <- numextract_all(oneCol)
              oneList_P <- oneList[seq(2,length(oneList),2)]
              List_P[[LL]] <- sort(as.numeric(oneList_P))
              
            }
            Common_Plots <- sort(intersect(intersect(intersect(intersect(intersect(List_P[[1]],List_P[[2]]),List_P[[3]]),List_P[[4]]),List_P[[5]]),List_P[[6]]))
            
            # FIX LIST WITH ONLY COMMON PLOTS
            allList_Files_Common <- list()
            for(LL in 1:length(allList_Files)){
              oneCol <- substr(allList_Files[[LL]],1, 9)
              oneList <- numextract_all(oneCol)
              oneList_P <- as.numeric(oneList[seq(2,length(oneList),2)])
              allList_Files_Common[[LL]] <- allList_Files[[LL]][match(Common_Plots, oneList_P)]
            }
            
            File_Lengths <- lengths(allList_Files_Common)
            if(length(unique(File_Lengths)) != 1){browser()} ## THIS CATCHES ANY INCONSISTENCY IN NUMBER OF FILES FOR A GIVEN FLIGHT
            
            Listed_VOX = allList_Files_Common[[1]]
            Listed_INPUT_ROI_Vox = allList_Files_Common[[2]]
            Listed_INPUT_ROI_Dec = allList_Files_Common[[3]]
            Listed_INPUT_PRIOR_XYZWLHR = allList_Files_Common[[4]]
            #Listed_TARGET_STOCKING = allList_Files_Common[[5]]
            Listed_IoU_SUMMARY = allList_Files_Common[[5]]
            Listed_TARGET_XYZWLHR = allList_Files_Common[[6]]
            
            # SANITY CHECK
            Output_Data_Files <- data.frame(VOX = allList_Files_Common[[1]],
                                            ROI_Vox = allList_Files_Common[[2]],
                                            ROI_Dec = allList_Files_Common[[3]],
                                            PRIOR_XYZWLHR = allList_Files_Common[[4]],
                                            #STOCKING = allList_Files_Common[[5]],
                                            IoU_SUMMARY = allList_Files_Common[[5]],
                                            TARGET_XYZWLHR = allList_Files_Common[[6]])
            # browser()
            # write.csv(Output_Data_Files, paste(FINAL_FILES_USED, "/",Orig_Data_Type,"_List_of_Files_Torch.csv", sep=""))
            
            for(RR in 1:ncol(Output_Data_Files)){
              oneCol <- substr(Output_Data_Files[,RR],1, 9)
              oneCol <- numextract_all(oneCol)
              Flight_oneCol <- oneCol[seq(1,length(oneCol),2)]
              Plot_oneCol <- oneCol[seq(2,length(oneCol),2)]
              Col_Name <- colnames(Output_Data_Files)[RR]
              Output_Data_Files <- data.frame(Output_Data_Files, Flight_oneCol, Plot_oneCol)
              colnames(Output_Data_Files)[ncol(Output_Data_Files)-1] <- paste(Col_Name, "_FF", sep="")
              colnames(Output_Data_Files)[ncol(Output_Data_Files)] <- paste(Col_Name, "_PP", sep="")
            }
            # SANITY CHECK TO MAKE SURE ALL PLOTS AND FLIGHTS ARE SAME FOR EACH FILE
            FF_Col <- grep("_FF", colnames(Output_Data_Files))
            PP_Col <- grep("_PP", colnames(Output_Data_Files))
            FF_Same <- transform(Output_Data_Files[,FF_Col], same = apply(Output_Data_Files[,FF_Col], 1, function(x) length(unique(x)) == 1))
            PP_Same <- transform(Output_Data_Files[,PP_Col], same = apply(Output_Data_Files[,PP_Col], 1, function(x) length(unique(x)) == 1))
            # if(!all(c(FF_Same$same, PP_Same$same))){browser()}
            
            
          } # IF THERE ARE MISSING DATA
          
          
          ##############################################################################################################################################################################
          ########################################
          # CREATE FINAL LIST OF FILES AND FOLDERS
          ########################################
          
          Files_INPUT_VOX <- c(Files_INPUT_VOX, Listed_VOX)
          Folders_INPUT_VOX <- c(Folders_INPUT_VOX, rep(oneFolder_INPUT_VOX, length(Listed_VOX)))
          
          Files_INPUT_ROI_Vox <- c(Files_INPUT_ROI_Vox , Listed_INPUT_ROI_Vox) 
          Folders_INPUT_ROI_Vox <- c(Folders_INPUT_ROI_Vox, rep(oneFolder_INPUT_ROI_Vox, length(Listed_INPUT_ROI_Vox)))
          
          Files_INPUT_ROI_Dec <- c(Files_INPUT_ROI_Dec , Listed_INPUT_ROI_Dec) 
          Folders_INPUT_ROI_Dec <- c(Folders_INPUT_ROI_Dec, rep(oneFolder_INPUT_ROI_Dec, length(Listed_INPUT_ROI_Dec)))
          
          Files_INPUT_PRIOR_XYZWLHR <- c(Files_INPUT_PRIOR_XYZWLHR, Listed_INPUT_PRIOR_XYZWLHR)
          Folders_INPUT_PRIOR_XYZWLHR  <- c(Folders_INPUT_PRIOR_XYZWLHR, rep(oneFolder_INPUT_PRIORS, length(Listed_INPUT_PRIOR_XYZWLHR)))
          
          #Files_TARGET_STOCKING <- c(Files_TARGET_STOCKING, Listed_TARGET_STOCKING)
          #Folders_TARGET_STOCKING <- c(Folders_TARGET_STOCKING, rep(oneFolder_TARGET_STOCKING, length(Listed_TARGET_STOCKING)))
          
          Files_IoU_SUMMARY <- c(Files_IoU_SUMMARY, Listed_IoU_SUMMARY)
          Folders_IoU_SUMMARY <- c(Folders_IoU_SUMMARY, rep(oneFolder_IoU_SUMMARY, length(Listed_IoU_SUMMARY)))
          
          Files_TARGET_XYZWLHR <- c(Files_TARGET_XYZWLHR,  Listed_TARGET_XYZWLHR)
          Folders_TARGET_XYZWLHR <- c(Folders_TARGET_XYZWLHR, rep(oneFolder_TARGET_XYZWLHR, length(Listed_TARGET_XYZWLHR)))
          
          Folder_FLIGHT <- c(Folder_FLIGHT, rep(oneFolder_FLIGHT, length(Listed_TARGET_XYZWLHR)))

          
        } # IF THERE ARE AT LEAST SOME FILES IN EACH LIST ELEMENT

      } # FLIGHT LOOP 
    } # DATA TYPE LOOP 
    #browser()
    if(RUN_SUBSAMPLE_OF_PLOTS  == "Yes"){  # unlist(lapply(OUTPUT_LIST[[1]], function(x) length(x))) [1]
 
      if(is.numeric(Para_Sample_Size)){
        # ONLY SELECTING A VERY SMALL SUBSET FOR TESTING
        Index_Sample <- 1:Para_Sample_Size
        
        if(length(Plot_Selection) > 0){
          Index_Sample <- which(numextract_all(Files_INPUT_VOX)[c(FALSE, TRUE)] %in% Plot_Selection)[1:Para_Sample_Size]
        }
        
      }else{
        # ONLY SELECTING THE FLIGHTS OF INTEREST
        Subj_Flight_ID <- numextract_all(Para_Sample_Size)
        Avail_Flight_ID <-numextract_all(Files_INPUT_VOX)
        Avail_Flight_ID <-Avail_Flight_ID[c(TRUE, FALSE)]
        Index_Sample <- which(Avail_Flight_ID %in% Subj_Flight_ID)
        
        }

      OUTPUT_LIST <- list(list( Folders_INPUT_VOX[Index_Sample], Folders_INPUT_ROI_Vox[Index_Sample], Folders_INPUT_ROI_Dec[Index_Sample], Folders_INPUT_PRIOR_XYZWLHR[Index_Sample], 
                                Folders_IoU_SUMMARY[Index_Sample], Folders_TARGET_XYZWLHR[Index_Sample], Folder_FLIGHT[Index_Sample]),  # Folders_TARGET_STOCKING[Index_Sample], 
                          list( Files_INPUT_VOX[Index_Sample], Files_INPUT_ROI_Vox[Index_Sample],  Files_INPUT_ROI_Dec[Index_Sample], Files_INPUT_PRIOR_XYZWLHR[Index_Sample],
                                Files_IoU_SUMMARY[Index_Sample], Files_TARGET_XYZWLHR[Index_Sample])) # Files_TARGET_STOCKING[Index_Sample],  
    }else{ # RUN ALL THE DATA
      OUTPUT_LIST <- list(list( Folders_INPUT_VOX, Folders_INPUT_ROI_Vox, Folders_INPUT_ROI_Dec, Folders_INPUT_PRIOR_XYZWLHR, 
                                 Folders_IoU_SUMMARY, Folders_TARGET_XYZWLHR, Folder_FLIGHT),  #  Folders_TARGET_STOCKING,
                          list( Files_INPUT_VOX, Files_INPUT_ROI_Vox,  Files_INPUT_ROI_Dec, Files_INPUT_PRIOR_XYZWLHR,
                                Files_IoU_SUMMARY, Files_TARGET_XYZWLHR))  # Files_TARGET_STOCKING,  
      } 
    
    


    # OUTPUT LIST OF FILES AND FOLDERS
    return(OUTPUT_LIST)

  } # POINT_TO_DATA FUNCTION
)

##############################################################################################################################################################################
##############################################################################################################################################################################
##############################################################################################################################################################################

############################
# LOAD THE DATA WITH BATCHES
############################

# IF ONLY EVALUATING YOU DON'T NEED TO TRAIN
if(RUN_ONLY_EVALUATE_MODE == "No"){
  DATA_Type <- "TRAIN_DATA"
  Train_ds <- TREES_DS_FUN(DATA_Type)
  num_GPU <- 1
  number_workers = para_Number_Workers * num_GPU
  Train_dl <-Train_ds %>% dataloader(batch_size = batch_size, num_workers = number_workers, pin_memory = TRUE)#, number_workers = number_workers)  # DOM DOM DOM !!! TO SPEED UP PROCESS USE num_workers = 4 (or 4 times available GPUs), pin_memory = TRUE # Note that increasing num_workerswill increase your CPU memory consumption
  
  print(paste("TRAIN_DS: ", Train_ds$.length()))
  print(paste("TRAIN_DL: ", Train_dl$.length()))
  # browser()
  # iter <- Train_dl$.iter()
  # b <- iter$.next()
}

DATA_Type <- "VALIDATE_DATA"
Valid_ds <- TREES_DS_FUN(DATA_Type)
Valid_dl <-Valid_ds %>% dataloader(batch_size = batch_size, num_workers  = number_workers, pin_memory = TRUE)

print(paste("VALIDATE_DS: ", Valid_ds$.length()))
print(paste("VALIDATE_DL: ", Valid_dl$.length()))


### DOM DOM DOM !!! YOU WILL NEED TO SORT OUT THE TEST DATA AT SOME POINT
# DATA_Type <- "TEST_DATA"
# Test_ds <- TREES_DS_FUN(DATA_Type)
# Test_dl <-Test_ds %>% dataloader(batch_size = batch_size)
# 
# print(paste("TEST_DS: ", Test_ds$.length()))
# print(paste("TEST_DL: ", Test_dl$.length()))

# Trees_ds$.getitem(1)
# Trees_ds$.length()
# Train_dl$.length()
# iter <- Train_dl$.iter()
# b <- iter$.next()
#
#
# #########
# # FOLDERS 
# #########
# dir.create(file.path(paste(FOLDER_MAIN_DATA, sep=""), "FINAL_FILES_USED"), showWarnings = FALSE)
# FINAL_FILES_USED <-paste(FOLDER_MAIN_DATA, "/FINAL_FILES_USED", sep="") 



#########################################
#BELOW HAS BEEN PUT IN TORCH_PrepDATA_V7
#########################################

# ##############################################################
# # IDENTIFY MISSING FILES (CHECK ALL FILES REPRESENT SAME PLOT)
# ##############################################################
# allList_Folders <-list( oneFolder_INPUT_VOX, oneFolder_INPUT_ROI, oneFolder_INPUT_IoU_SUMMARY,
#                         oneFolder_TARGET_STOCKING, oneFolder_IoU_SUMMARY, oneFolder_TARGET_XYZWLHR)
# 
# allList_Files <-list(Listed_VOX, Listed_INPUT_ROI,  Listed_INPUT_PRIOR_XYZWLHR,
#                      Listed_TARGET_STOCKING,  Listed_IoU_SUMMARY, Listed_TARGET_XYZWLHR)
# 
# File_Type <- c("VOX", "ROI", "PRIOR_XYZWLHR", "TARGET_STOCKING", "IoU_SUMMARY", "TARGET_XYZWLHR")
# ##############################################
# # REMOVE FILES WITH INCORRECT COLUMNS AND ROWS
# ##############################################
# 
# ### DOM DOM DOM !!! NOTE THAT IF YOU CHANGE THE FILE DIMENSIONS IN PreMASTER THEN YOU NEED TO CHANGE HERE!!!
# allList_Dim <- list(c(-1,9), c(Para_min_Plot_Priors,7), c(Para_min_Plot_Priors,17), c(-1,2), c(Para_min_Plot_Priors,17), c(-1,18))
# 
# allListRowCol <- list()
# allList_Files <- list()
# for(RC in 1:length(allList_Files)){
#   oneList_Files <- allList_Files[[RC]]
#   oneList_Folders <-allList_Folders[[RC]]
#   oneList_Dim <- allList_Dim[[RC]]
#   oneList_Files_Clean <- c()
#   Files_Col_Row <- data.frame(File = character(), RowCnt = numeric(), ColCnt = numeric())
#   for(RC2 in 1:length(oneList_Files)){
#     oneFiles <- read.csv(paste(oneList_Folders, "/", oneList_Files[[RC2]], sep=""))
#     oneFilesRow <- nrow(oneFiles)
#     oneFilesCol <-ncol(oneFiles)
#     oneFile_Col_Row <- data.frame(File = oneList_Files[[RC2]], RowCnt = oneFilesRow, ColCnt = oneFilesCol)
#     Files_Col_Row <- rbind(Files_Col_Row, oneFile_Col_Row)
#     if((oneList_Dim[1] == -1 | oneList_Dim[1] == oneFilesRow) & oneList_Dim[2] == oneFilesCol){
#       oneList_Files_Clean <- c(oneList_Files_Clean, oneList_Files[[RC2]])
#     } else {
#       # REMOVE THE FILE AND PUT IT IN
# 
#     }
#   }
#   allListRowCol[[RC]] <- Files_Col_Row
#   allList_Files[[RC]] <- oneList_Files_Clean
#   write.csv(Files_Col_Row, paste(FINAL_FILES_USED, "/",Orig_Data_Type,"_DIM_",File_Type[RC],".csv", sep=""))
# }





###########################################################################################
# OLD 
###########################################################################################

# ###########################
# # FINDING MISSING FILES ...
# ###########################
# NA_Count <- length(which(is.na(Files_INPUT_VOX))) + 
#   length(which(is.na(Files_INPUT_ROI))) +
#          length(which(is.na(Files_INPUT_PRIOR_XYZWLHR))) +
#                 length(which(is.na(Files_TARGET_STOCKING))) +
#                        length(which(is.na(Files_PRIORS))) +
#                               length(which(is.na(Files_TARGET_XYZWLHR)))
# if(NA_Count > 0){browser()} ### DOM DOM DOM !!! THERE ARE NA FOR FILES... FIX!!!
# 
# List_Folders <- list( Folders_INPUT_VOX, Folders_INPUT_ROI,Folders_INPUT_PRIOR_XYZWLHR, 
#       Folders_TARGET_STOCKING, Folders_PRIORS, Folders_TARGET_XYZWLHR)
# List_Files <-list( Files_INPUT_VOX, Files_INPUT_ROI,  Files_INPUT_PRIOR_XYZWLHR,
#       Files_TARGET_STOCKING,  Files_PRIORS, Files_TARGET_XYZWLHR)
# 
# File_Lengths <- lengths(List_Files)
# Min_File_Length <- min(File_Lengths)
# Extra_Files <- which(File_Lengths > Min_File_Length)
# if(length(Extra_Files) > 0){
#   all_Files_Extra <- c()
#   for(EE in 1:length(Extra_Files)){
#     Files_Extra <- List_Files[[Extra_Files[EE]]]
#     Files_Extra <- Files_Extra[(Min_File_Length+1):length(Files_Extra)]
#     all_Files_Extra <- c(all_Files_Extra, Files_Extra)
#   }
# }
# all_Files_Extra_Substr <- substr(all_Files_Extra,1, 9)
# Extra <- numextract_all(all_Files_Extra_Substr)
# Flight_Extra <- Extra[seq(1,length(Extra),2)]
# Plot_Extra <- Extra[seq(2,length(Extra),2)]
# Output_Extra_Files <- data.frame(Extra_File=all_Files_Extra, Flight_Extra, Plot_Extra)
# write.csv(Output_Extra_Files, paste(FOLDER_MAIN_DATA, "/",Orig_Data_Type,"_List_of_EXTRA_Files_Torch.csv", sep=""))
# 
# Output_Data_Files <- data.frame(VOX = Files_INPUT_VOX, 
#                                 ROI = Files_INPUT_ROI, 
#                                 PRIOR_XYZWLHR = Files_INPUT_PRIOR_XYZWLHR, 
#                                 STOCKING = Files_TARGET_STOCKING, 
#                                 CLASS = Files_PRIORS, 
#                                 TARGET_XYZWLHR = Files_TARGET_XYZWLHR)
# 
# for(RR in 1:ncol(Output_Data_Files)){
#   oneCol <- substr(Output_Data_Files[,RR],1, 9)
#   # print(oneCol[1])
#   # 
#   oneCol <- numextract_all(oneCol)
#   Flight_oneCol <- oneCol[seq(1,length(oneCol),2)]
#   Plot_oneCol <- oneCol[seq(2,length(oneCol),2)]
#   Col_Name <- colnames(Output_Data_Files)[RR]
#   Output_Data_Files <- data.frame(Output_Data_Files, Flight_oneCol, Plot_oneCol)
#   colnames(Output_Data_Files)[ncol(Output_Data_Files)-1] <- paste(Col_Name, "_FF", sep="")
#   colnames(Output_Data_Files)[ncol(Output_Data_Files)] <- paste(Col_Name, "_PP", sep="")
# }
# 
# # SANITY CHECK TO MAKE SURE ALL PLOTS AND FLIGHTS ARE SAME FOR EACH FILE
# FF_Col <- grep("_FF", colnames(Output_Data_Files))
# PP_Col <- grep("_PP", colnames(Output_Data_Files))
# FF_Same <- transform(Output_Data_Files[,FF_Col], same = apply(Output_Data_Files[,FF_Col], 1, function(x) length(unique(x)) == 1))
# PP_Same <- transform(Output_Data_Files[,PP_Col], same = apply(Output_Data_Files[,PP_Col], 1, function(x) length(unique(x)) == 1))
# if(!all(c(FF_Same$same, PP_Same$same))){browser()}
# write.csv(Output_Data_Files, paste(FOLDER_MAIN_DATA, "/",Orig_Data_Type,"_List_of_Files_Torch.csv", sep=""))
# 