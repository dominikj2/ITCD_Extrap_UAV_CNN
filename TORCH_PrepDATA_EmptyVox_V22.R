############################################################################################################################################ 1
############################################################################################################################################ 1
# PREPARE DATA
############################################################################################################################################ 1
############################################################################################################################################ 1

#### DOM DOM DOM !!! HAs Non-Maximum Suppression ( NMS) been undertaken/used.... NMS removes boxes that overlaps with other boxes that has higher scores ( scores are unnormalized probabilities , e.g. before softmax is applied to normalize).
      # OUTPUT SHOULD GO TO THE "DETECTION PHASE"

### DOM DOM DOM !!! NEED TO THINK ABOUT HOW TO DEAL WITH TEST DATA IN THE PrepDATA
### DOM DOM DOM !!! THIS NEEDS CHANGING TO RECOGNISE THE TRAIN, VALIDATE AND TEST FOLDERS

dir.create(file.path(paste(FOLDER_MAIN_DATA, sep=""), "REMOVE_PLOTS_NO_PRIOR"), showWarnings = FALSE)
FOLDER_REMOVE_PLOTS <- paste(FOLDER_MAIN_DATA,"/REMOVE_PLOTS_NO_PRIOR" , sep="")

DATA_Type<- list.files(FOLDER_MAIN_DATA, pattern = "_DATA")

RUN_Train_Flights <- c() ### PUT IN FLIGHT NUMBERS THAT YOU WANT TO PROCESS
RUN_Validate_Flights <- c()


for(D in 1:length(DATA_Type)){
  DATA_TYPE_FOLDER <- paste(FOLDER_MAIN_DATA, "/", DATA_Type[D], sep="")
  Flight_Folders<- list.files(DATA_TYPE_FOLDER, pattern = "Flight_")  
  Flight_ID <- as.numeric(numextract_all(Flight_Folders))
 
  Max_RoI_ID <- 0 # DOM DOM DOM !!! NOT SURE WHEN TO ZERO THE ID (i.e. should it be unique within all flights or only within plots)
  
  # GETTING THE FLIGHT LIST THAT NEEDS TO BE PROCESSED
  if(DATA_Type[D] =="TRAIN_DATA"){ 
    # IF THERE IS NOT A SPECIFIC FLIGHT TO PrepDATA THEN USE ALL FLIGHTS (CONTROL WITH NO RUN USING MASTER ...RUN_PREPARE_DATA <- "No")
    if(length(RUN_Train_Flights) == 0){RUN_Train_Flights <- Flight_ID}
    Run_Flights <- RUN_Train_Flights}
  else{ 
    if(length(RUN_Validate_Flights) == 0){RUN_Validate_Flights <- Flight_ID}
    Run_Flights <- RUN_Validate_Flights}
    
  # LOOP FLIGHTS
  for(ff in 1:length(Flight_ID)){
    Flight_NeedsProcess <- which(Flight_ID[ff] %in% Run_Flights)
    if(length(Flight_NeedsProcess) == 1){
      print(paste(" ................... Starting Flight", Flight_ID[ff], "........................", ff, " out of ", length(Flight_ID)))
      FOLDER_oneF <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], sep="")
    
      # NAME FIRECTORIES WITH DATA
      FOLDER_CSV <-  paste(FOLDER_oneF, "/CSV", sep="") 
      FOLDER_INPUT_VOX_DF <- paste(FOLDER_oneF, "/CSV/CSV_MP1/VOX_DF", sep="")             
      FOLDER_INPUT_GT <- paste(FOLDER_oneF, "/CSV/CSV_MP1/LocBox_allP15X15", sep="")   
      FOLDER_INPUT_PRIORS <- paste(FOLDER_oneF, "/CSV/CSV_MP1/Prior_LocBox", sep="")  
      FOLDER_INPUT_VOX <- paste(FOLDER_oneF, "/LAS/LAS_MP1/Vox_P", sep="") # DONE 
      FOLDER_EXTRA_PRIORS <- paste(FOLDER_oneF, "/CSV/CSV_MP1/IoU_Prior_GT", sep="")
      
      # CREAT DIRECTORIES THAT WILL BE USED FOR OUTPUT
      dir.create(file.path(FOLDER_CSV, "INPUT_ROI"), showWarnings = FALSE)
      FOLDER_INPUT_ROI <- paste(FOLDER_oneF, "/CSV/INPUT_ROI", sep="")
      
      dir.create(file.path(FOLDER_CSV, "PRIORS"), showWarnings = FALSE)
      FOLDER_TARGET_ROI_AND_CLASS <- paste(FOLDER_oneF, "/CSV/PRIORS", sep="")
      
      dir.create(file.path(FOLDER_CSV, "EXTRA_ROI"), showWarnings = FALSE)
      FOLDER_EXTRA_ROI <- paste(FOLDER_oneF, "/CSV/EXTRA_ROI", sep="")
      
      # dir.create(file.path(FOLDER_CSV, "SHIFTS_NORMALISING"), showWarnings = FALSE)
      # FOLDER_SHIFTS_NORMALISING <- paste(FOLDER_oneF, "/CSV/SHIFTS_NORMALISING", sep="")
  
      #Fsc_FOLDER <- paste(Comp, "/CNN/THINNED_SAMPLES_V6/Flight_1/CSV/CSV_MP1/Intersect_Union_Prior_GroundT", sep="")
  
      # LOOP PLOTS
      Plot_ID <- list.files(FOLDER_INPUT_VOX_DF, pattern="_LAS_Vox_N") # c(102,103, 104, 105)
      
      if(length(Plot_ID) > 0){
        Plot_ID <- numextract_all(Plot_ID)[seq(2,length(numextract_all(Plot_ID)),2)]
        
        for(pp in 1:length(Plot_ID)){
          
          #######################################
          # IMPORT DATA RATHER THAN GENERATING IT
          #######################################
          
          # LAS DF
          Vox_N_DF <- read.csv( paste(FOLDER_INPUT_VOX_DF,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')) 
          
          #### DOM DOM DOM !!! THIS IS A TEMPORARY WORK AROUND (BOTTOM LINE NEEDS DELETING ONCE I HAVE DATA)
          #if(length(which(Vox_N_DF$Z == 0)) > 0){browser()}
          Vox_N_DF <- Vox_N_DF[-which(Vox_N_DF$Z == 0),]
          
          XYZWLHR_plotGT_N <- read.csv( paste(FOLDER_INPUT_GT,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep=''))  
          
          XYZWLHR_plotPriors_N <- read.csv( paste(FOLDER_INPUT_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_AllPRIORS_N.csv",sep='')) 
          
          # OUTPUT OF IOU BETWEEN EACH PRIOR AND ALL GT
          Output_Fsc_Prior_GT <- read.csv(paste(FOLDER_EXTRA_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_All_IoU_Prior_GroundTruth.csv",sep='')) 
          Output_Fsc_Prior_GT$Diff_ZTopBox <- abs(Output_Fsc_Prior_GT$TID_Z_TopBox - Output_Fsc_Prior_GT$PRIOR_Z_TopBox)
          # ADD HEIGHT OF BOUNDING BOX IN THE IoU/FSC TABLE (USED TO SELECT THE BEST PRIORS BASED ON THE LOCATION OF THE MAIN BOUNDING BOX)
          #Output_Fsc_Prior_GT$TID_Z_TopBox <- XYZWLHR_plotGT_N$Z_TopBox[match(Output_Fsc_Prior_GT$TID, XYZWLHR_plotGT_N$TID)]
          
          
          if(nrow(Output_Fsc_Prior_GT) > 0){
            #########################################################################################################################################################
            ### DOM DOM DOM !!! YOU REMOVE ALL THE _All_Fsc_Prior_GroundTruth WITH TID THAT ARE NOT IN XYZWLHR_plotGT_N.
            # THIS IS A TEMPORARY FIX BUT YOU SHOULD REVISIT CODE THE GENERATES THESE FILES ("TORCH_PreMASTER_PLOT_SAMPLING_V1.r") TO MAKE SURE YOU ARE NOT REMOVING TREE INFORMATION 
            Output_Fsc_Prior_GT <- Output_Fsc_Prior_GT[which(Output_Fsc_Prior_GT$TID %in%  XYZWLHR_plotGT_N$TID), ]
            
            
            
            # DOM DOM DOM !!! YOU ARE REMOVING TREES THAT DON'T HAVE A PRIOR.... YOU SHOULDN'T DO THIS AND PRIOR PROCEDURE IS WHAT NEEDS FIXING!!!! 
            # TEMPORARY PROCEDURE WHILST FOCUSING ON TESTING CNN
            
            # if(length(which(XYZWLHR_plotGT_N$TID %in%  unique(Output_Fsc_Prior_GT$TID))) != nrow(XYZWLHR_plotGT_N)){browser()}
            
            XYZWLHR_plotGT_N <- XYZWLHR_plotGT_N[which(XYZWLHR_plotGT_N$TID %in%  unique(Output_Fsc_Prior_GT$TID)),]
            
            #########################################################################################################################################################
            # OPEN LAS DATA
            LAS_Vox_N <- readLAS(paste(FOLDER_INPUT_VOX, "/LAS_F",Flight_ID[ff], "_MP", MovPos, "_P", Plot_ID[pp],  "_Vox_N.laz", sep=""), select = "xyzp0") 
            LAS_Vox_N <- add_lasattribute(LAS_Vox_N, x= LAS_Vox_N@data$PointSourceID, name="TID", desc = "TID")
            
            # EXTENT (XYZ) OF EACH PRIOR
            
            Ext_plotPrior_N <- read.csv(paste(FOLDER_EXTRA_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_All_Prior_XYZ_Extent_N.csv",sep='')) 
            
            
            # Shift_DATA <- read.csv(paste(FOLDER_CSV , "/F",  Flight_ID[ff], "_Shifts_LAS_XYZWLHR.csv",sep=''))
            # Shift_DATA_oneP <-Shift_DATA[which(Shift_DATA$Plot_ID == Plot_ID[pp]),]  # DOM DOM DOM !!! REMOVED AS I DON'T USE ANYWHERE
            
            # ####################################
            # # GETTTING THE PRIOR AND GT VERTICES (DOM DOM DOM !!! SHOULD THIS BE IN THE PRE_MASTER CODE???)
            # ####################################
            # 
            # Extent_XYZWLHR_Priors  <- XYZ_Extent_XYZWLHR_FUN(XYZWLHR_plotPriors_N, ID_Col = "PriorID", Normalise = "Yes")
            # Priors_Vert <- Extent_XYZWLHR_Priors[[1]]
            # GT_Extent_Summary <- Extent_XYZWLHR_Priors[[2]]
            # 
            # XYZWLHR_plotGT_N <- XYZWLHR_plotGT_N[,-2] # REMOVE SAMPLE COLUMN
            # Extent_XYZWLHR_GT  <- XYZ_Extent_XYZWLHR_FUN(XYZWLHR_plotGT_N, ID_Col = "TID", Normalise = "Yes")
            # GT_Vert <- Extent_XYZWLHR_GT[[1]]
            # GT_Extent_Summary <- Extent_XYZWLHR_GT[[2]]
            
            
            
            
            ### DOM DOM DOM !!!! YOU REMOVED THIS BUT NEED TO MAKE SURE IT DOESN'T EFFECT WHAT FOLLOWS BELOW ..
            
            
            # # MAKE SURE EXTENT IS BETWEEN 0 and 1 (DOM DOM DOM !!! WHY WOULD EXTENT EVERY BE LESS THAN 0 OR MORE THAN 1 ???)
            # Ext_plotPrior_N$X <- ifelse(Ext_plotPrior_N$X < 0, 0, Ext_plotPrior_N$X)
            # Ext_plotPrior_N$Y <- ifelse(Ext_plotPrior_N$Y < 0, 0, Ext_plotPrior_N$Y)
            # Ext_plotPrior_N$Z <- ifelse(Ext_plotPrior_N$Z < 0, 0, Ext_plotPrior_N$Z)
            # Ext_plotPrior_N$X <- ifelse(Ext_plotPrior_N$X > 1, 1, Ext_plotPrior_N$X)
            # Ext_plotPrior_N$Y <- ifelse(Ext_plotPrior_N$Y > 1, 1, Ext_plotPrior_N$Y)
            # Ext_plotPrior_N$Z <- ifelse(Ext_plotPrior_N$Z > 1, 1, Ext_plotPrior_N$Z)
            # 
            # #SANITY CHECK
            # if(length(which(Ext_plotPrior_N$X < 0 | Ext_plotPrior_N$X > 1 |
            #                 Ext_plotPrior_N$Y < 0 | Ext_plotPrior_N$Y > 1 |
            #                 Ext_plotPrior_N$Z < 0 | Ext_plotPrior_N$Z > 1 )) > 0){browser()}
            
            ### DOM DOM DOM !!! MAKE SURE THAT THE NA FILLS WITHIN EACH PSID TRISHAPE ARE USING THE CORRECT INPUT HERE OUTPUT IN SAMPLE_PREP FOLDER
            
            # # GET "GROUND TRUTH" DATA FOR PLOT (i.e. SEGMENTED POINT CLOUD)... GENERATED IN "CNN_PLOT_LAS_VOX_GENERATOR_ResidTriShp_V44_T2_V6.r (renamed: CNN_PLOT_SAMPLE_PREP_V1.r)"
            # XYZWLHR_plotGT <- read.csv(paste(Test_Run_Folder, "/F",Flight_ID[ff], "_P", Plot_ID[pp],  "_Ext15X15_AllPSID_Loc_BBox_XYZWLHR_N.csv", sep=""))
            # XYZWLHR_plotGT <- XYZWLHR_plotGT[,-which(colnames(XYZWLHR_plotGT) %in% c("Flight", "Plot_ID", "Bot_TreeHeight"))]
            
            #################
            ### DOM DOM DOM !!! NOTE THAT PRIORS WERE CALCULATED USING THE ACTUAL LIDAR POINT CLOUD (NOT VOXELS) BUT ARE THEN USED TO CALCULATE THE LEVEL
            # OF IoU USING VOX LAS FILE. MAYBE IoU WOULD BE BETTER IF PRIORS WERE COMPUTED USING THE VoX FILE.
            ### DOM DOM DOM !!! REGARDING ABOVE COMMENT. NOTE THAT PRIORS WERE COMPUTED USING WS PROCEDURE WHICH MAY REQUIRE THE POINT CLOUD INFO...
            # THINK ABOUT
            #################
            
            # GET "PRIORS" DATA FOR PLOT (### DOM DOM DOM !!! YOU NEED TO FIND THE VEG STRATIFICATION CODE THAT DERIVES THIS)
            #XYZWLHR_plotPriors <- read.csv(paste(Test_Run_Folder, "/F",Flight_ID[ff], "_P", Plot_ID[pp],  "_Ext15X15_CNN_AllPRIORS_N.csv", sep=""))
            
            ### DOM DOM DOM !!! I NEED TO READ IN THE GTT LAZ LAYER TOO WHEN I CONSTRUCT IN OTHER CODE...
            
            ############################################################################################################################################ 3
            ############################################################################################################################################ 3
            # IDENTIFY N BEST AND WORST PRIORS FOR EACH GT TID (USING IoU and FScore)
            ############################################################################################################################################ 3
            ############################################################################################################################################ 3
            
            Nested_by_TID <- Output_Fsc_Prior_GT %>% 
              group_by(TID) %>%
              nest() %>%
              rowwise() %>%
              mutate(data = list(data[with(data, order(-FScore, Diff_ZTopBox, -Portion_IoU_CorrecIn, Portion_IoU_WrongIn )),])) %>% ### DOM DOM DOM !!! YOU FLIPPED THIS (Portion_IoU_CorrecIn, -FScore) AROUND BUT SHOULD CHECK 
              mutate(Unique_FScore = list(sort(unique(round(data$FScore,2))))) %>%
              mutate(Count_Unique_FScore = list(table(sort(round(data$FScore,2))))) %>%
              
              mutate(Unique_CorrecIn_All = list(sort(unique(round(data$Portion_IoU_CorrecIn,2))))) %>%
              mutate(Count_Unique_CorrecIn_All = list(table(sort(round(data$Portion_IoU_CorrecIn,2))))) %>%
              
              mutate(Unique_CorrecIn_Bad = list(Unique_CorrecIn_All[which(Unique_CorrecIn_All < 0.3)])) %>%
              # 
              # mutate(Count_Unique_CorrecIn_Bad = list(table(sort(which(Unique_CorrecIn_All < 0.3))))) %>%
              # 
              # mutate(Unique_CorrecIn_Good = list(Unique_CorrecIn_All[which(Unique_CorrecIn_All > 0.7)])) %>%
              mutate(Unique_CorrecIn_Avg = list(Unique_CorrecIn_All[which(Unique_CorrecIn_All >= 0.3 & Unique_CorrecIn_All <= 0.7)])) %>%
              # mutate(Length_All = length(Unique_CorrecIn_All))  %>%
              # mutate(Length_Bad = length(which(Unique_CorrecIn_All < 0.3)))  %>%
              # mutate(Length_Good = length(which(Unique_CorrecIn_All > 0.7))) %>%
              # mutate(Length_Avg = length(which(Unique_CorrecIn_All >= 0.3 & Unique_CorrecIn_All <= 0.7))) %>%
              ungroup() 
            
            # TIBBLE (EACH TIDs MAX IoU and MAX FScore... DATA IS TID's Output_Fsc_Prior_GT)
            
            # Nested_by_TID_Portion_Correct <- Output_Fsc_Prior_GT %>% 
            #   group_by(TID, round(Portion_IoU_CorrecIn,2)) %>%
            #   nest() %>%
            #   rowwise() %>%
            #   mutate(data = list(data[with(data, order(FScore, Portion_IoU_WrongIn )),])) %>% ### DOM DOM DOM !!! YOU FLIPPED THIS (Portion_IoU_CorrecIn, -FScore) AROUND BUT SHOULD CHECK 
            #   mutate(Unq_Fscore_All = list(sort(unique(round(data$FScore,2))))) %>%
            #   mutate(Cnt_Unq_Fscore_All = list(table(sort(round(data$FScore,2))))) %>%
            #   mutate(Unq_Fscore_Bad = list(Unq_Fscore_All[which(Unq_Fscore_All < 0.3)])) %>%
            #   mutate(Unq_Fscore_Good = list(Unq_Fscore_All[which(Unq_Fscore_All > 0.7)])) %>%
            #   mutate(Unq_Fscore_Avg = list(Unq_Fscore_All[which(Unq_Fscore_All >= 0.3 & Unq_Fscore_All <= 0.7)])) %>%
            #   mutate(Lgth_FSc_All = length(Unq_Fscore_All))  %>%
            #   mutate(Lgth_FSc_Bad = length(which(Unq_Fscore_All < 0.3)))  %>%
            #   mutate(Lgth_FSc_Good = length(which(Unq_Fscore_All > 0.7))) %>%
            #   mutate(Lgth_FSc_Avg = length(which(Unq_Fscore_All >= 0.3 & Unq_Fscore_All <= 0.7))) %>%
            #   ungroup() 
            
            
            ################################################################################# 3a
            # FOR EACH COMB GT:PRIOR GET Para_Learning_Sample OF BEST AND WORST (AND AVERAGE)
            ################################################################################# 3a
            Para_Learning_Sample <- round(Para_min_Plot_Priors/(2*nrow(Nested_by_TID)),0)
            if(Para_Learning_Sample < 1) {Para_Learning_Sample == 1}
            
            # DOM DOM DOM !!! FOR EACH Unique_Portion_IoU_CorrecIn LESS THAN 0.3 GET PRIORS WITH THE SMALLEST FSCORE
            # FOR EACH Unique_Portion_IoU_CorrecIn GREATER THAN 0.7 GET PRIORS WITH THE LARGEST FSCORE
            
            #######################################
            # LOOP THROUGH EACH GT (i.e. NESTED GT)
            #######################################
            for(NT in 1:nrow(Nested_by_TID)){
              # FScore and IoU CALC FOR one GT
              oneGT_Nested <- Nested_by_TID[NT,]
              TID <- oneGT_Nested$TID
              Fsc_oneGT <- as.data.frame(oneGT_Nested$data[[1]])
              Fsc_oneGT <- Fsc_oneGT[rev(order(Fsc_oneGT$FScore)),]

              # Portion_IoU_CorrecIn_GOOD <- oneGT_Nested$Unique_CorrecIn_Good[[1]][seq(1,oneGT_Nested$Length_Good,  length.out = min(oneGT_Nested$Length_Good, Para_Learning_Sample))]
              # Portion_IoU_CorrecIn_BAD <- oneGT_Nested$Unique_CorrecIn_Bad[[1]][seq(1,oneGT_Nested$Length_Bad,  length.out = min(oneGT_Nested$Length_Bad, Para_Learning_Sample))]
              # Portion_IoU_CorrecIn_Avg <- oneGT_Nested$Unique_CorrecIn_Avg[[1]][seq(1,oneGT_Nested$Length_Avg,  length.out = min(oneGT_Nested$Length_Avg, Para_Learning_Sample))]
              
              # LIST_FScore_G_B <- list(Portion_IoU_CorrecIn_GOOD, Portion_IoU_CorrecIn_BAD,  Portion_IoU_CorrecIn_Avg )
              
              Prior_Type <- c(1, 0)
              
              # GOOD PRIORS ARE THE TOP Para_Learning_Sample FSCORES (WITH UNIQUE IN/OUTS)
              Length_Unique_FScore <- length(oneGT_Nested$Unique_FScore[[1]])
              Index <- max((Length_Unique_FScore - Para_Learning_Sample + 1), 1)
              FScore_GOOD <- oneGT_Nested$Unique_FScore[[1]][Index:Length_Unique_FScore]
              FScore_GOOD_UnqCnt <- oneGT_Nested$Count_Unique_FScore[[1]][Index:Length_Unique_FScore]
              #browser()
              
              # # DOM DOM DOM !!! BAD PRIORS SHOULD HAVE NO LAS DATA IN THEM 
              # 
              # # BAD PRIORS ARE HIGHEST F-SCORES WITH PORTION IN AS CLOSE AS POSSIBLE TO 0.3 BUT NOT OVER
              # Length_Unique_CorrectIn_Bad <- length(oneGT_Nested$Unique_CorrecIn_Bad[[1]])
              # Index <- max((Length_Unique_CorrectIn_Bad - Para_Learning_Sample + 1), 1)
              # FScore_BAD <- oneGT_Nested$Unique_CorrecIn_Bad[[1]][Index:Length_Unique_CorrectIn_Bad]
              # FScore_BAD_UnqCnt <- oneGT_Nested$Count_Unique_CorrecIn_All[[1]][which(names(oneGT_Nested$Count_Unique_CorrecIn_All[[1]]) %in% FScore_BAD)]
              
              # # AVERAGE PRIORS ARE LOWEST F-SCORES WITH PORTIONS > 0.3
              # Length_Unique_Average <- length(oneGT_Nested$Unique_CorrecIn_Avg[[1]])
              # CorrecIn_Avg <- oneGT_Nested$Unique_CorrecIn_Avg[[1]][1:Para_Learning_Sample]
              # CorrecIn_Avg_UnqCnt <- oneGT_Nested$Count_Unique_CorrecIn_All[[1]][which(names(oneGT_Nested$Count_Unique_CorrecIn_All[[1]]) %in% CorrecIn_Avg)]
              # FScore_AVG <- c()
              
              ### DOM DOM DOM !!! YOU HAVEN'T ASSESSED THE F_SCORE FOR ANY OF THESE YET
              
              LIST_FScore_G_B <- list(FScore_GOOD) # , FScore_BAD,  FScore_AVG 
              
              
              # CREATE EMPTY OUTPUT
              Fsc_oneGT$Prior_Type <- 0
              
              
              ###################################
              # LOOP THROUGH GOOD                   # BAD AND AVERAGE
              ###################################
              Fsc_Best_oneGT <- Fsc_oneGT[0,]
           
              for(LL in 1:length(LIST_FScore_G_B)){
                
                # if(LL == 3 & nrow(Fsc_Best_oneGT) == (Para_Learning_Sample*2)){break()}
                # 
                # if(LL == 3){browser()}
                
                Prior_Type_LL <- Prior_Type[LL]
                oneLIST_FSc_G_B <- LIST_FScore_G_B[[LL]]
                
                if(all(!is.na(oneLIST_FSc_G_B))){
                  ############################################################################
                  # GROUPING BY FSC AND CorrIn
                  if(LL == 1){
                    LL_oneGT <- Fsc_oneGT[which(round(Fsc_oneGT$FScore,2) %in% oneLIST_FSc_G_B),]
                  }
                  if(LL == 2){
                    LL_oneGT <- Fsc_oneGT[which(round(Fsc_oneGT$Portion_IoU_CorrecIn ,2) %in% oneLIST_FSc_G_B),]
                  }
                  
                  Group_oneGT <- LL_oneGT %>% 
                    group_by(Gp_FSc = round(LL_oneGT$FScore , 2),
                             Gp_CorrIn = round(LL_oneGT$Portion_IoU_CorrecIn, 2) 
                    ) %>%
                    nest() %>%
                    rowwise() 
                  
                  ############################################################################
                  # ORDERING SO GOOD WITH HIGHEST FSC AND BAD WITH 
                  if(LL == 1){Group_oneGT <- Group_oneGT[order(-Group_oneGT$Gp_FSc, -Group_oneGT$Gp_CorrIn),]}
                  # if(LL == 2){Group_oneGT <- Group_oneGT[order(-Group_oneGT$Gp_FSc, Group_oneGT$Gp_CorrIn),]} # YOU ALWAYS WANT THE HIGHEST FSc AT TOP BUT GP_CORR SHOULD BE SMALLEST IN POOR Prior_Type 
                  #if(LL == 1) {browser()}
                  
                  ######################################################
                  # COMPUTING HOW MANY PRIORS TO SELECT FROM EACH Gp_Fsc
                  ######################################################
                  
                  Unique_Select <- rev(table(Group_oneGT$Gp_FSc))
                  
                  # IF CORRECT NUMBER, GET ONE PRIOR FROM EACH UNIQUE FSC
                  if(length(Unique_Select) == Para_Learning_Sample){
                    Unique_Select[1:Para_Learning_Sample] <- 1
                  }
                  
                  # IF TOO MANY, REDUCE THE NUMBER UNTIL COUNT IS Para_Learning_Sample
                  if(sum(Unique_Select) > Para_Learning_Sample){
                    Remaining <- Para_Learning_Sample- sum(Unique_Select)
                    while(Remaining != 0){
                      Unique_Select[which.max(Unique_Select)] <- Unique_Select[which.max(Unique_Select)] - 1
                      Remaining <- Para_Learning_Sample - sum(Unique_Select)
                    }
                  }
                  
                  ##########################
                  # PRODUCE OUTPUT OF PRIORS (Evenly distributed)
                  ##########################
                  Cnt_Each_USelect <- ceiling(Para_Learning_Sample/sum(Unique_Select))
                  for(US in 1:length(Unique_Select)){
                    Index_oneFSc <- which(Group_oneGT$Gp_FSc == names(Unique_Select)[US])#[1:Unique_Select[US]]
                    
                    # REDUCE INDEX TO THE NUMBER OF REQUIRED SAMPLES FOR GIVEN oneCorrIn
                    Cnt_FSc_inOneCorrIn <- length(Index_oneFSc)
                    Cnt_Required_inOneCorrIn <- as.vector(Unique_Select[US])
                    if(Cnt_FSc_inOneCorrIn > Cnt_Required_inOneCorrIn){
                      Index_oneFSc <- Index_oneFSc[1:Cnt_Required_inOneCorrIn]
                    }
                    
                    Cnt_FSc_inOneCorrIn <- length(Index_oneFSc)
                    Cnt_Select_Each_oneCorrIn <- (Cnt_FSc_inOneCorrIn/Cnt_Required_inOneCorrIn) * Cnt_Each_USelect
                    
                    Group_oneGT_oneCorrIn <- Group_oneGT[Index_oneFSc,]
                    Cnt_Select <- as.vector(Unique_Select[US])
                    while (Cnt_Select > 0){
                      for(UPP in 1:nrow(Group_oneGT_oneCorrIn)){
                        Add_Best <- as.data.frame(Group_oneGT_oneCorrIn$data[[UPP]])[1:Cnt_Select_Each_oneCorrIn,]
                        Add_Best$Prior_Type <- Prior_Type[[LL]]
                        # Add_Best$Prior_Type[which(Add_Best$FScore > Para_minFSc)] <- 1 # Prior_Type <- c(1 (good), 0 (bad))
                        Fsc_Best_oneGT <- rbind(Fsc_Best_oneGT, Add_Best)
                        Cnt_Select <- Cnt_Select - Cnt_Select_Each_oneCorrIn 
                        
                        if(Cnt_Select <= 0){break()}
                      }
                      # print(paste("NROW OneGT:", nrow(Fsc_Best_oneGT)))
                    }
                  } # LOOP USE
                  
                  ############################################################################
                  # IF THERE ARE NO GOOD OR BAD THEN USE AVERAGE OR SEE WHERE YOU CAN GET PRIORS FROM ....
                  if(length(oneLIST_FSc_G_B) == 0){
                    browser()
                    if(LL == 2){browser()} # WORK OUT HOW AVERAGE WORKS
                    oneLIST_FSc_G_B <- LIST_FScore_G_B[[3]]
                    if(length(oneLIST_FSc_G_B) == 0){
                      Index_Has_Data <- which(lapply(LIST_FScore_G_B, length) != 0)
                      oneLIST_FSc_G_B <- LIST_FScore_G_B[[Index_Has_Data ]]
                      Prior_Type_LL <- Prior_Type[Index_Has_Data]
                    }
                    Prior_Type_LL <- 2
                  }
                  #browser()
                  Fsc_Best_oneGT <- na.omit(Fsc_Best_oneGT)
                  
                }# !is.na(oneLIST_FSc_G_B)
              } # LOOP LL
           
              #################################################################################################################################################################################
              #################################################################################################################################################################################
              # PROCEDURE FOR IDENTIFYING THE BAD PRIORS
              
              # browser()
              oneGT_XYZWLHR_N <- XYZWLHR_plotGT_N[which(XYZWLHR_plotGT_N$TID == TID),] 
              
              # PREPARING THE EMPTY VOXELS FOR TRISHAPE ASSESSMENT
              LAS_Vox_ZRange <- filter_poi(LAS_Vox_N, Z <= oneGT_XYZWLHR_N$Z_TopTree &  Z >= oneGT_XYZWLHR_N$Z_Base )
              LAS_Vox_ZRange_Empty <-  filter_poi(LAS_Vox_ZRange, TID == 0)
              Vox_ZRange_Empty <- LAS_Vox_ZRange_Empty@data[,1:3]
              
              # plot(LAS_Vox_ZRange_Empty, color="TID")
              # plot(LAS_Vox_ZRange, color="TID")
              
              ############################################################
              # GET THE NUMBER OF VOXELS THAT FALL IN ORIGINAL GT TRISHAPE
              ############################################################
              
              oneGT_XYZWLHR_N_Redcuced <- torch_squeeze(torch_tensor(as.matrix(oneGT_XYZWLHR_N[,-c(1,2,6:11),])))
              oneGT_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneGT_XYZWLHR_N_Redcuced, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
              oneGT_Vert_ExtP_oneGnd <- torch_clone(oneGT_Vert_ExtP)
              oneGT_Vert_ExtP_oneGnd <- oneGT_Vert_ExtP_oneGnd[1:4,]
              oneGT_Vert_ExtP_oneGnd[,3] <- 0
              oneGTVert_ExtP_Gnd <- as.array(torch_cat(c(oneGT_Vert_ExtP_oneGnd, oneGT_Vert_ExtP)))
              
              oneGTVert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneGTVert_ExtP_Gnd, Box_Levels = 4)
              oneGT_Intersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneGTVert_ExtP_Gnd), Empty_Vox)         # GIVE VOXELS IN Subj TRISHAPE TID VALUE
              oneGT_VoxInTrishp <- which(oneGT_Intersect%%2 != 0)
              
              oneGT_VoxCnt <- length(oneGT_VoxInTrishp)
              
              TreeVox <- Empty_Vox[oneGT_VoxInTrishp,]
              # LAS_TreeVox <- LAS(TreeVox)
              
              ############################################################
              # WORK OUT HOW FAR THE FURTHEST VOXEL IS FROM THE XY CENTRE (in X and Y DIRETION)
              ############################################################
              # THIS THROWS WARNING IF TreeVox HAS NO VOXELS
              Summary_GT_Ranges <- TreeVox %>% 
                summarise(HalfRangeX = (range(X)[2] - range(X)[1])/2,
                          HalfRangeY = (range(Y)[2] - range(Y)[1])/2)
                    

              # REMOVE OUT VOXELS AND THEN GET EVERY SECOND XY LOCATION 
              # GET UNIQUE X AND Y THAT WILL BE EVALUATED BY REMOVING EDGE XY FIRST
              unique_X <- unique(LAS_Vox_N@data$X)
              unique_Y <- unique(LAS_Vox_N@data$Y)
              
              unique_X_reduced <- unique_X[which(unique_X > Summary_GT_Ranges$HalfRangeX & unique_X < (1-Summary_GT_Ranges$HalfRangeX))]
              if(length(unique_X_reduced) > 6){
                  unique_X_reduced <-  unique_X_reduced[c(TRUE, FALSE)] 
                  }
              if(length(unique_X_reduced) < 3){
                unique_X_reduced <- c(unique_X_reduced,  unique_X[1], unique_X[length(unique_X)])
              }
              
              
              unique_Y_reduced <- unique_Y[which(unique_Y > Summary_GT_Ranges$HalfRangeY & unique_Y < (1-Summary_GT_Ranges$HalfRangeY))]
              if(length(unique_Y_reduced) > 6){
                unique_Y_reduced <-  unique_Y_reduced[c(TRUE, FALSE)]
              }
              if(length(unique_Y_reduced) < 3){
                unique_Y_reduced <- rbind(unique_Y_reduced,  unique_Y[1], unique_Y[length(unique_Y)])
              }
              
              # if(NT == 6){browser()}
              

              # MOVE THE GT AROUND AND IDENTIFY THE POSITIONS THAT HAVE THE SMALLEST NUMBER OF VOXELS THAT FALL IN THE TREE VOXEL SPACE.
                # CHANGE THE XY OF X_TopBox AND INCREMENTALLY CHANGE IT, MOVING TWO VOXELS ACROSS ,  UNTIL AND WORK THROUGH ALL THE
              
              # loOP THROUGH X
              
              Offset_BaseX <- oneGT_XYZWLHR_N$X_Base - oneGT_XYZWLHR_N$X_TopBox
              Offset_BaseY <- oneGT_XYZWLHR_N$X_Base - oneGT_XYZWLHR_N$X_TopBox
              oneGT_XYZWLHR_N$Y_Base - oneGT_XYZWLHR_N$Y_TopBox

              for(XX in 1:length(unique_X_reduced)){
                # loOP THROUGH X
                for(YY in 1:length(unique_Y_reduced)){
                  #print(oneGT_XYZWLHR_N)
                  unique_X_reduced[XX]
                  unique_Y_reduced[YY]
                  # CHANGE GT CENTROID AND COMPUTE VOX_In_Tri_Shp.
                  oneVoxPos_XYZWLHR_N_ChangedCentroid <- oneGT_XYZWLHR_N
                  oneVoxPos_XYZWLHR_N_ChangedCentroid$X_TopBox <- unique_X_reduced[XX]
                  oneVoxPos_XYZWLHR_N_ChangedCentroid$Y_TopBox <- unique_Y_reduced[YY]
                  oneVoxPos_XYZWLHR_N_ChangedCentroid$X_Base <- unique_X_reduced[XX] + Offset_BaseX
                  oneVoxPos_XYZWLHR_N_ChangedCentroid$Y_Base  <- unique_Y_reduced[YY] + Offset_BaseY
                  
                  # print(oneVoxPos_XYZWLHR_N_ChangedCentroid)
                  # browser()
                  oneVoxPos_XYZWLHR_N_ChangedCentroid_T <- torch_squeeze(torch_tensor(as.matrix(oneVoxPos_XYZWLHR_N_ChangedCentroid[,-c(1,2,6:11),])))
                  oneVoxPos_Vert_ExtP_ChangedCentroid <- XYZWHR_TO_VERT_GPU_FUN(oneVoxPos_XYZWLHR_N_ChangedCentroid_T, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
                  oneVoxPos_Vert_ExtP_oneGnd_ChangedCentroid <- torch_clone(oneVoxPos_Vert_ExtP_ChangedCentroid)
                  oneVoxPos_Vert_ExtP_oneGnd_ChangedCentroid <- oneVoxPos_Vert_ExtP_oneGnd_ChangedCentroid[1:4,]
                  oneVoxPos_Vert_ExtP_oneGnd_ChangedCentroid[,3] <- 0
                  oneGTVert_ExtP_Gnd_ChangedCentroid <- as.array(torch_cat(c(oneVoxPos_Vert_ExtP_oneGnd_ChangedCentroid, oneVoxPos_Vert_ExtP_ChangedCentroid)))
                  oneGTVert_ExtP_Gnd_ChangedCentroid <- VERTICIES_ORDER_FUN(oneGTVert_ExtP_Gnd_ChangedCentroid, Box_Levels = 4)
                  oneVoxPos_Intersect_ChangedCentroid = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneGTVert_ExtP_Gnd_ChangedCentroid), Vox_ZRange_Empty)         # GIVE VOXELS IN Subj TRISHAPE TID VALUE
                  oneVoxPos_VoxInTrishp_ChangedCentroid <- which(oneVoxPos_Intersect_ChangedCentroid%%2 != 0)

                  #####################################################
                  # STORE INFORMATION ABOUT HOW MANY VOXELS IN TRISHAPE
                  #####################################################
                  
                  oneEmpVox_VoxCnt <- length(oneVoxPos_VoxInTrishp_ChangedCentroid)
                  oneVoxPos_XYZWLHR_N_ChangedCentroid <- data.frame(oneGT_VoxCnt, oneEmpVox_VoxCnt, oneVoxPos_XYZWLHR_N_ChangedCentroid)
                  
                  if(XX == 1 & YY == 1){
                    allVoxPos_XYZWLHR_N_ChangedCentroid <- oneVoxPos_XYZWLHR_N_ChangedCentroid
                  }else{
                    allVoxPos_XYZWLHR_N_ChangedCentroid <- rbind(allVoxPos_XYZWLHR_N_ChangedCentroid, oneVoxPos_XYZWLHR_N_ChangedCentroid)
                  }
                } # LOOP XX
              } # LOOP YY
              # browser()
              if(nrow(allVoxPos_XYZWLHR_N_ChangedCentroid) < Para_Learning_Sample){
                allVoxPos_XYZWLHR_N_ChangedCentroid <- rbind(allVoxPos_XYZWLHR_N_ChangedCentroid, allVoxPos_XYZWLHR_N_ChangedCentroid, allVoxPos_XYZWLHR_N_ChangedCentroid, allVoxPos_XYZWLHR_N_ChangedCentroid)
                
                allVoxPos_XYZWLHR_N_ChangedCentroid <- allVoxPos_XYZWLHR_N_ChangedCentroid[1:Para_Learning_Sample,]
                
                # browser()
                }
              
              allVoxPos_XYZWLHR_N_ChangedCentroid <- allVoxPos_XYZWLHR_N_ChangedCentroid[rev(order(allVoxPos_XYZWLHR_N_ChangedCentroid$oneEmpVox_VoxCnt))[1:Para_Learning_Sample],]
              

              # PERFORM DISTANCE CALCULATION TO SELECT THE Para_Learning_Sample POSITIONS WITH THE MOST EMPTY VOXELS AND THE FURTHEST APART
                ### DOM DOM DOM !!! DIDN'T DO THIS AT THIS STAGE....
              
              
              #################################################################################################################################################################################
              #################################################################################################################################################################################
              
              ### THINK ABOUT HOW YOU WILL FINALISE THE FINAL PRIORS....
              
              # REDUCING SIZE IF MORE THAN MIN
              # if(nrow(Fsc_Best_oneGT) < Para_Learning_Sample){browser()}
              
              Fsc_Best_oneGT <- Fsc_Best_oneGT[1:(Para_Learning_Sample),] # Fsc_Best_oneGT[1:(Para_Learning_Sample*2),]
              
              # IF NOT ENOUGH PRIORS THEN BROWSER
              if(nrow(Fsc_Best_oneGT) < Para_Learning_Sample) {browser()} # Para_Learning_Sample*2) {browser()}
              
              # ADD TID TO DATA.FRAME
              Fsc_Best_oneGT <- data.frame(TID, Fsc_Best_oneGT)
              #PLOT_GT_PRIOR_VOX_FUN(Fsc_Best_oneGT, XYZWLHR_plotGT_N, XYZWLHR_plotPriors_N, LAS_Vox_N)
              
              # UPDATE LIST FOR ALL GT
              if(NT == 1){
                Fsc_Best_allGT <- Fsc_Best_oneGT
                allGT_allVoxPos_XYZWLHR_EmptyVox_N <- allVoxPos_XYZWLHR_N_ChangedCentroid
                # print(paste("NROW AllGT:", nrow(Fsc_Best_allGT)))
              }else{
                Fsc_Best_allGT <- rbind(Fsc_Best_allGT, Fsc_Best_oneGT)
                allGT_allVoxPos_XYZWLHR_EmptyVox_N <- rbind(allGT_allVoxPos_XYZWLHR_EmptyVox_N, allVoxPos_XYZWLHR_N_ChangedCentroid)
                # print(paste("NROW AllGT:", nrow(Fsc_Best_allGT)))
              }
              
               
            }# LOOP NT

            # IF NOT ENOUGH PRIORS THEN DUPLICATE SOME OF THEM
            if(nrow(allGT_allVoxPos_XYZWLHR_EmptyVox_N) < Para_min_Plot_Priors/2){
              allGT_allVoxPos_XYZWLHR_EmptyVox_N <- rbind(allGT_allVoxPos_XYZWLHR_EmptyVox_N, allGT_allVoxPos_XYZWLHR_EmptyVox_N)
            }
            allGT_allVoxPos_XYZWLHR_EmptyVox_N <- allGT_allVoxPos_XYZWLHR_EmptyVox_N[1:(Para_min_Plot_Priors/2),]
   
            
            Fsc_Best_allGT <- na.omit(Fsc_Best_allGT)
            allGT_allVoxPos_XYZWLHR_EmptyVox_N$PriorID <- seq((max(Fsc_Best_allGT$Prior)+1),(max(Fsc_Best_allGT$Prior)+nrow(allGT_allVoxPos_XYZWLHR_EmptyVox_N)) , 1)
        
          #  
            
            
            ########################
            # REMOVING ANY NA PRIORS
            ########################
            
            
            while(nrow(Fsc_Best_allGT) != (Para_min_Plot_Priors/2)){
              #browser()
              Diff_Best <- nrow(Fsc_Best_allGT) - (Para_min_Plot_Priors/2)
              
              # IF THERE ARE TOO MANY IN PLOT EVENLY REMOVE
              if(Diff_Best > 0){
                Fsc_Best_allGT <-Fsc_Best_allGT[order(Fsc_Best_allGT$TID, Fsc_Best_allGT$Prior_Type),]
                # browser()
                # GET WORST ROW OF EACH TID
                Fsc_Best_allGT_Last <- Fsc_Best_allGT %>% 
                  group_by(TID) %>% 
                  arrange(-FScore) %>%  
                  slice(n())
                # REMOVE WORST ROW OF EACH TID
                Fsc_Best_allGT <- Fsc_Best_allGT %>% 
                  group_by(TID) %>% 
                  arrange(-FScore) %>%  
                  slice(1:(n()-1))
                # GET THE CORRECT NUMBER OF ROWS (RE-INTRODUCE THE WORST OF SOME TID)
                Fsc_Best_allGT <- rbind(Fsc_Best_allGT, Fsc_Best_allGT_Last[1:(nrow(Nested_by_TID)-Diff_Best),])
                
                if(nrow(Fsc_Best_allGT) != (Para_min_Plot_Priors/2)){
                  # browser()
                  repeat_times <- ceiling((Para_Learning_Sample)/nrow(Fsc_Best_allGT))
                  Fsc_Best_allGT <-do.call("rbind", replicate(repeat_times, Fsc_Best_allGT, simplify = FALSE))
                  Fsc_Best_allGT <- Fsc_Best_allGT[1:(Para_min_Plot_Priors/2),]
                  # Fsc_Best_oneGT <- na.omit(Fsc_Best_oneGT)
                  # Fsc_Best_oneGT <- Fsc_Best_oneGT[1:Para_min_Plot_Priors,]
                  ### DOM DOM DOM !!! THINK ABOUT GIVING THESE AVERAGE ID (i.e. 2 and not 1 or 0)
                }       
                
                # IF NOT ENOUGH THEN EVENLY DUPLICATE
              }else{
                
                Diff_Best <- (Para_min_Plot_Priors/2) - nrow(Fsc_Best_allGT) 
                Fsc_Best_allGT_First <- Fsc_Best_allGT %>% 
                  group_by(TID) %>% 
                  arrange(-FScore) %>%  
                  slice(1)
                while(nrow(Fsc_Best_allGT_First) < Diff_Best){
                  Fsc_Best_allGT_First <- rbind(Fsc_Best_allGT_First, Fsc_Best_allGT_First)
                }
                # GET THE CORRECT NUMBER OF ROWS
                Fsc_Best_allGT <- rbind(Fsc_Best_allGT, Fsc_Best_allGT_First[1:Diff_Best,])
                
                # SANITY CHECK
                if(nrow(Fsc_Best_allGT) != (Para_min_Plot_Priors/2)){
                  browser()
                } 
              }
              Fsc_Best_allGT <- na.omit(Fsc_Best_allGT)
              
            } #WHILE LOOP
            
            ################################
            # PLOT ONE GT AND ALL ITS PRIORS
            ################################
            #PLOT_GT_PRIOR_VOX_FUN(Fsc_Best_allGT, XYZWLHR_plotGT_N, XYZWLHR_plotPriors_N, LAS_Vox_N, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt)
            
            ############################################################################################################################################ 4
            ############################################################################################################################################ 4
            # GENERATE TABLE FOR INTERSECTION OF UNION POOLING ALGORITHM (IN BASE OF PLOT LOOP)
            ############################################################################################################################################ 4
            ############################################################################################################################################ 4
            
            
            # GET EXTENT OF EACH TID
            LAS_Vox_TID <- filter_poi(LAS_Vox_N, TID > 1)
            Summary_TID_Ext <- as.data.frame(as.data.frame(LAS_Vox_TID@data)  %>%
                                               dplyr::group_by(TID) %>%
                                               dplyr::summarise(MinX = min(X),
                                                                MaxX = max(X),
                                                                MinY = min(Y),
                                                                MaxY = max(Y),
                                                                MinZ = min(Z),
                                                                MaxZ = max(Z),
                                                                RangeX = MaxX-MinX,
                                                                RangeY = MaxY-MinY,
                                                                RangeZ = MaxZ-MinZ, .groups = 'drop'))
            
            #Decimal_Vox <- sort(unique(LAS_Vox_N@data$X)) #[2] # WHEN NOT NORMALISED ... Summary_TID_Ext$MinX[1]%%1 
            
            para_Fsc_XYRes <- para_RoI_Pool_Dim_XY/Para_Target_Base 
            para_Fsc_ZRes <- para_RoI_Pool_Dim_Z/Para_Target_Z_Height 
            
            Fsc_Best_allGT$RoI_ID <- 1:nrow(Fsc_Best_allGT) 
            Max_RoI_ID <- max(Fsc_Best_allGT$RoI_ID)
            # DOM DOM DOM !!! THINK ABOUT HOW THE FINAL CNN RESULTS ARE USED TO IDENTIFY THE BEST ADJUCTED PRIORS FOR EACH TREE... THE RoI_ID PROVIDES THIS INFO
            
            ########################### 4a
            # SAVING THE RoI LIST AS DF
            ########################### 4a
            
            # GENERATES RoI WITH ORIGINAL FORMAT (i.e. XYZ area/voxels that needs to be cut of from each end) AND CORRECTED FORMAT (i.e. XYZ Bot left, XYZWLH ...)
            #browser()
            
            ################################
            # PLOTTING THE BBOXES TO COMPARE
            ################################
            # PLOT BEST AND WORST TIU

            
            # GET EXTENT OF EACH EMPTY PRIOR 
            allGT_XYZWLHR_Extent <-  allGT_allVoxPos_XYZWLHR_EmptyVox_N[,-c(1:4, 8:13)]
            Extent_EMPTYPriors  <- XYZ_Extent_XYZWLHR_FUN(allGT_XYZWLHR_Extent, ID_Col = "PriorID", Normalise = "Yes")
            Extent_EMPTYPriors_Vert <- Extent_EMPTYPriors[[1]]
            Extent_EMPTYPriors_Summary <- Extent_EMPTYPriors[[2]]
            Extent_EMPTYPriors_Summary_FORMAT <- Extent_EMPTYPriors[[3]]
            Extent_EMPTYPriors <- data.frame(TID = allGT_allVoxPos_XYZWLHR_EmptyVox_N$TID, Extent_EMPTYPriors)

            # INTEGRATE EMPTY PRIORS INTO RoI CALCULATIONS
            
            # MERGE ALL PAIRS OF PRIOR (Empty and Best) AND GT
            Pair_PRIOR_GT_ID <- Fsc_Best_allGT[,which(colnames(Fsc_Best_allGT) %in% c("TID", "Prior"))]
            Pair_EmptyPRIOR_GT_ID <-allGT_allVoxPos_XYZWLHR_EmptyVox_N[,which(colnames(allGT_allVoxPos_XYZWLHR_EmptyVox_N) %in% c("TID", "PriorID"))]
            colnames(Pair_EmptyPRIOR_GT_ID) <- colnames(Pair_PRIOR_GT_ID)
            Pair_PRIOR_GT_ID <- rbind(Pair_PRIOR_GT_ID, Pair_EmptyPRIOR_GT_ID)
            
            # MERGE (Empty and Best) Prior XYZWLHR
            XYZWLHR_emptyPriors_N <- allGT_allVoxPos_XYZWLHR_EmptyVox_N[,match(colnames(XYZWLHR_plotPriors_N), colnames(allGT_allVoxPos_XYZWLHR_EmptyVox_N))]
            XYZWLHR_allPriors_N <- rbind(XYZWLHR_plotPriors_N, XYZWLHR_emptyPriors_N)

            
            # MERGE (Empty and Best) Prior EXTENT
            Ext_allPrior_N <- rbind(Ext_plotPrior_N, Extent_EMPTYPriors_Summary_FORMAT)

            List_RoI_Both <- LIST_ROI_NORM_FUN(Pair_PRIOR_GT_ID, # 64, 18
                                               Summary_TID_Ext, XYZWLHR_plotGT_N,  # N_Trees, 
                                               XYZWLHR_allPriors_N, Ext_allPrior_N, # N_Priors
                                               LAS_Vox_N,
                                               para_Fsc_XYRes, para_Fsc_ZRes, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt)
           
            
            List_RoI_Dec <- List_RoI_Both[[1]]
            List_RoI_Dec_DF <- as.data.frame(t(matrix(unlist(List_RoI_Dec), 7, Para_min_Plot_Priors)))
            colnames(List_RoI_Dec_DF) <- c("RoI_ID", "X_Bot", "X_Top", "Y_Bot", "Y_Top", "Z_Bot", "Z_Top")
            
            List_RoI_Vox <- List_RoI_Both[[2]]
            List_RoI_Vox_DF <- as.data.frame(t(matrix(unlist(List_RoI_Vox), 7, Para_min_Plot_Priors)))
            colnames(List_RoI_Vox_DF) <- c("RoI_ID", "X_Bot", "X_Top", "Y_Bot", "Y_Top", "Z_Bot", "Z_Top")
            
            # if(dim(List_RoI_Dec_DF) != 64){browser()}
            # if(dim(List_RoI_Vox_DF) != 64){browser()}
            
            # # TESTING THE ROI PROCESS
            # 
            # for(RRRR in 1:nrow(Fsc_Best_allGT)){
            #   Test_IoU <- Fsc_Best_allGT[RRRR,]
            #   Test_GT <-XYZWLHR_plotGT_N[which(XYZWLHR_plotGT_N$TID == Test_IoU$TID),]
            #   Test_Prior <-XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$Prior == Test_IoU$Prior),]
            #   Test_RoI <-List_RoI_Dec_DF[RRRR,]
            #   #
            #   Test_GT_Vert_ExtP <- XYZWHR_TO_VERT_FUN(Test_GT, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
            #   Test_Prior_Vert_ExtP <- XYZWHR_TO_VERT_FUN(Test_Prior, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
            #   #
            #   #oneP_oneRoI_Dec_Vec <- as.array(oneP_RoI_Dec[1,ggg,])[-1]
            #   Test_Vert_RoI <- GEN_ROI_VERTICES_FUN(as.matrix(Test_RoI)[-1]) # POLYGON OF ROI AREA
            #   polygon3d(Test_Vert_RoI[1:4,], fill=FALSE, col="yellow")
            #   polygon3d(Test_Vert_RoI[5:8,], fill=FALSE, col="yellow")
            #   #
            #   # PLOT ExtR
            #   colours <- c( "green","red")
            #   List_Vert_ExtR <- list(Test_GT_Vert_ExtP, Test_Prior_Vert_ExtP)
            #   PLOT_Ver1_Vert2_VOX_FUN(LAS_Vox_TID, List_Vert_ExtR, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "TID")
            #   lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
            #   lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
            #   lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
            #   lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue
            #   #
            #   polygon3d(Test_Vert_RoI[1:4,], fill=FALSE, col="yellow")
            #   polygon3d(Test_Vert_RoI[5:8,], fill=FALSE, col="yellow")
            #   #
            #   browser()
            # }
            
            
            #############################################################################################################################################
            #############################################################################################################################################
            #############################################################################################################################################
            ######################################
            #PLOT RESULTS TO VISUALISE FINAL PRIOR
            ######################################
            
            if(RUN_PLOTS_PREPARE_DATA_1 == "Yes"){
              Index_Check <- seq(1, 64, Para_Learning_Sample*2)
              
              unique_TID <- unique(Fsc_Best_allGT$TID)
              
              for(ppp in 1:length(unique_TID)){
                
                PLOT_oneFsc_Best_Summary_Prior_TID <- Fsc_Best_allGT[which(Fsc_Best_allGT$TID %in% unique_TID[ppp]),]
                
                PLOT_oneGT_XYZWLHR_N <- XYZWLHR_plotGT_N[which(XYZWLHR_plotGT_N$TID == unique_TID[ppp]),]
                PLOT_Vert_oneGT <- XYZWHR_TO_VERT_FUN(PLOT_oneGT_XYZWLHR_N, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt)
                
                PLOT_BestPrior_Fsc <- PLOT_oneFsc_Best_Summary_Prior_TID[which.max(PLOT_oneFsc_Best_Summary_Prior_TID$FScore)[1],]
                PLOT_onePriors_XYZWLHR_N <- XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$PriorID == PLOT_BestPrior_Fsc$Prior),]
                PLOT_Vert_onePriors <- as.data.frame(XYZWHR_TO_VERT_FUN(PLOT_onePriors_XYZWLHR_N, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) )
                
                Index_Prior <- which(Fsc_Best_allGT$Prior == PLOT_BestPrior_Fsc$Prior)[1]
                PLOT_RoI_Dec <- unlist(List_RoI_Dec[[Index_Prior]]) # unlist(oneList_RoI_Dec)[-1]
                PLOT_Vert_RoI <- GEN_ROI_VERTICES_FUN(PLOT_RoI_Dec[-1])
                #
                PLOT_unique_GT <- PLOT_oneFsc_Best_Summary_Prior_TID$TID[1]
                
                # PLOT PRIOR GT AND BBOX OF BEST
                
                PLOT_GT_PRIOR_VOX_RoI_FUN(LAS_Vox_N, PLOT_Vert_oneGT, PLOT_Vert_onePriors, PLOT_Vert_RoI, PLOT_unique_GT, Para_Cnt = Para_TriShpParaCnt)
                
                # PLOT ALL PRIOR BOXES FOR PLOT
                LAS_oneP_Vox_N <- filter_poi(LAS_Vox_N, TID > 1)
                plot(LAS_oneP_Vox_N, color="TID")
                for(rrr in 1:nrow(XYZWLHR_plotPriors_N)){
                  PLOT3_onePriors_XYZWLHR_N <- XYZWLHR_plotPriors_N[rrr,]
                  PLOT3_Vert_onePriors <- as.data.frame(XYZWHR_TO_VERT_FUN(PLOT3_onePriors_XYZWLHR_N, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) )
                  PLOT3_Vert_oneBBox <- PLOT3_Vert_onePriors[5:8,]
                  PLOT3_Vert_oneBBox <- rbind(PLOT3_Vert_oneBBox,PLOT3_Vert_oneBBox[1,])
                  polygon3d(PLOT3_Vert_oneBBox, fill=FALSE, col="white", lwd=1)
                }
                
                # PLOT CHOSEN BBOXS FOR GT
                for(qqq in 1:nrow(PLOT_oneFsc_Best_Summary_Prior_TID)){
                  PLOT2_oneFsc_Best_Summary_Prior_TID <- PLOT_oneFsc_Best_Summary_Prior_TID[qqq,]
                  PLOT2_onePriors_XYZWLHR_N <- XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$PriorID == PLOT2_oneFsc_Best_Summary_Prior_TID$Prior),]
                  PLOT2_Vert_onePriors <- as.data.frame(XYZWHR_TO_VERT_FUN(PLOT2_onePriors_XYZWLHR_N, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt) )
                  PLOT2_Vert_oneBBox <- PLOT2_Vert_onePriors[5:8,]
                  PLOT2_Vert_oneBBox <- rbind(PLOT2_Vert_oneBBox,PLOT2_Vert_oneBBox[1,])
                  if(PLOT2_oneFsc_Best_Summary_Prior_TID$Prior_Type == 1){
                    Color = "white"}else{Color = "purple"}
                  polygon3d(PLOT2_Vert_oneBBox, fill=FALSE, col=Color, lwd=1)
                }
                browser()
                
              }
            }
            
            #############################################################################################################################################
            #############################################################################################################################################
            #############################################################################################################################################          
            
            # #### DOM DOM DOM !!! BELOW IS INCORRECT.... THEY ARE RANGES AND NOT WLH
            # List_RoI_Dec_XYZWLH <- List_RoI_Both[[3]]
            # List_RoI_Dec_XYZWLH_DF <- as.data.frame(t(matrix(unlist(List_RoI_Dec_XYZWLH), 7, Para_min_Plot_Priors)))
            # colnames(List_RoI_Dec_XYZWLH_DF) <- c("RoI_ID", "X_Bot", "Y_Bot", "Z_Bot", "Width_X", "Length_Y", "Height_Z")
            # 
            # List_RoI_Vox_XYZWLH <- List_RoI_Both[[4]]
            # List_RoI_Vox_XYZWLH_DF <- as.data.frame(t(matrix(unlist(List_RoI_Vox_XYZWLH), 7, Para_min_Plot_Priors)))
            # colnames(List_RoI_Vox_XYZWLH_DF) <- c("RoI_ID", "X_Bot", "Y_Bot", "Z_Bot", "Width_X", "Length_Y", "Height_Z")
            
            # SANITY CHECK
            
            if((length(which(List_RoI_Dec_DF$X_Bot < 0)) > 0) | 
               (length(which(List_RoI_Dec_DF$X_Bot > 1)) > 0) |
               (length(which(List_RoI_Dec_DF$X_Top < 0)) > 0) |
               (length(which(List_RoI_Dec_DF$X_Top > 1)) > 0) |
               
               (length(which(List_RoI_Dec_DF$Y_Bot < 0)) > 0) | 
               (length(which(List_RoI_Dec_DF$Y_Bot > 1)) > 0) |
               (length(which(List_RoI_Dec_DF$Y_Top < 0)) > 0) |
               (length(which(List_RoI_Dec_DF$Y_Top > 1)) > 0) |
               
               (length(which(List_RoI_Dec_DF$Z_Bot < 0)) > 0) | 
               (length(which(List_RoI_Dec_DF$Z_Bot > 1)) > 0) |
               (length(which(List_RoI_Dec_DF$Z_Top < 0)) > 0) |
               (length(which(List_RoI_Dec_DF$Z_Top > 1)) > 0)) {browser()} else { print("ROI Good Range")}
            
            ##################################  4b
            # Tracking PlotID and and FlightID  
            ##################################  4b
            
            ###############################################
            # OUTPUT ALL THE FILES THAT WILL BE USED IN CNN
            ###############################################
            
            Fsc_Best_allGT <- data.frame(FlightID = Flight_ID[ff], PlotID = Plot_ID[pp], Fsc_Best_allGT)
            write.csv(Fsc_Best_allGT, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Best_Summary_Prior_TID_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE) 
            
            # GENERATING SUMMARY TABLE FOR PRIORS WITH PRIOR ID
            Fsc_Empty_Vox_allGT <- Fsc_Best_allGT
            Fsc_Empty_Vox_allGT$TID <- allGT_allVoxPos_XYZWLHR_EmptyVox_N$TID
            Fsc_Empty_Vox_allGT$Prior <- allGT_allVoxPos_XYZWLHR_EmptyVox_N$PriorID
            Fsc_Empty_Vox_allGT[,6:ncol(Fsc_Empty_Vox_allGT) ] <- NA
            Fsc_Empty_Vox_allGT$Prior_Type  <- 0
            Fsc_Empty_Vox_allGT$RoI_ID  <- seq((max(Fsc_Best_allGT$RoI_ID)+1), (nrow(Fsc_Empty_Vox_allGT)+ nrow(Fsc_Empty_Vox_allGT)), 1)
            
            Fsc_BestWorst_allGT <- rbind(Fsc_Best_allGT, Fsc_Empty_Vox_allGT)
            write.csv(Fsc_BestWorst_allGT, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_BestWorst_Summary_Prior_TID_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE) 
            
            # browser()
            
            allGT_allVoxPos_XYZWLHR_EmptyVox_N <- data.frame(FlightID = Flight_ID[ff], PlotID = Plot_ID[pp], allGT_allVoxPos_XYZWLHR_EmptyVox_N)
            write.csv(allGT_allVoxPos_XYZWLHR_EmptyVox_N, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "XYZWLHR_Best_EmptyVox_Priors",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE)
            
            Extent_EMPTYPriors_Vert <- data.frame(FlightID = Flight_ID[ff], PlotID = Plot_ID[pp], Extent_EMPTYPriors_Vert)
            write.csv(Extent_EMPTYPriors_Vert, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "Extent_EMPTYPriors_Vert",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE)
            
            Extent_EMPTYPriors_Summary <- data.frame(FlightID = Flight_ID[ff], PlotID = Plot_ID[pp], Extent_EMPTYPriors_Summary)
            write.csv(Extent_EMPTYPriors_Summary, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "Extent_EMPTYPriors_Summary",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE)
            
            Extent_EMPTYPriors_Summary_FORMAT <- data.frame(FlightID = Flight_ID[ff], PlotID = Plot_ID[pp], Extent_EMPTYPriors_Summary_FORMAT)
            write.csv(Extent_EMPTYPriors_Summary_FORMAT, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "Extent_EMPTYPriors_Summary_FORMAT",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE)

            # browser()
            # max(Fsc_oneGT$FScore)
            # max(Fsc_Best_allGT$FScore)
            XYZWLHR_Best_plotPriors <- XYZWLHR_plotPriors_N[match(Fsc_Best_allGT$Prior, XYZWLHR_plotPriors_N$PriorID),]
            write.csv(XYZWLHR_Best_plotPriors, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_XYZWLHR_Best_plotPriors_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE) 
            
            # MERGE BEST AND WORST (EMPTY VOX) PRIORS
            XYZWLHR_EmptyVox_plotPriors <- allGT_allVoxPos_XYZWLHR_EmptyVox_N[, match(colnames(XYZWLHR_Best_plotPriors), colnames(allGT_allVoxPos_XYZWLHR_EmptyVox_N))]
            XYZWLHR_BestWorst_plotPriors <- rbind(XYZWLHR_Best_plotPriors, XYZWLHR_EmptyVox_plotPriors)
            write.csv(XYZWLHR_BestWorst_plotPriors, paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_XYZWLHR_BestWorst_plotPriors_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE) 
            
            
            write.csv(List_RoI_Vox_DF, paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Vox_DF_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE)
            write.csv(List_RoI_Dec_DF, paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Dec_DF_S",Para_min_Plot_Priors,".csv",sep=''), row.names=FALSE) 
            # write.csv(List_RoI_Vox_XYZWLH_DF, paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Vox_DF_S",Para_min_Plot_Priors,"_XYZWLH.csv",sep=''), row.names=FALSE)
            # write.csv(List_RoI_Dec_XYZWLH_DF, paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Dec_DF_S",Para_min_Plot_Priors,"_XYZWLH.csv",sep=''), row.names=FALSE) 
            
            print(paste("pp : ", pp, " out of ", length(Plot_ID) , sep=""))
            
            # # INVESTIGATING SOME GT DROPPING OFF  ### DOM DOM DOM !!! YOU MAY WaNT TO INVESTIGATE THIS ONE DAY ... IF YOU HAVE TIME!!!
            # if(!all(sort(as.numeric(names(table(Output_Fsc_Prior_GT$TID)))) == sort(XYZWLHR_plotGT_N$TID))){browser()}
            # if(!all(sort(as.numeric(names(table(Output_Fsc_Prior_GT$TID)))) == sort(as.numeric(names(table(Fsc_Best_allGT$TID)))))){browser()}
            
            ####################################################################
            # CHECK THAT ALL FILES HAVE CORRECT DIMENSIONS OTHERWISE REMOVE THEM 
            ####################################################################
            
            allList_DIM <- list(c(-1,9), round(c(Para_min_Plot_Priors,7),0), c(Para_min_Plot_Priors,17),  c(Para_min_Plot_Priors,20), c(-1,18))
            File_Type <- c("_LAS_Vox_N AND _LAS_Vox_N_ALS", paste("_List_RoI_Vox_DF_S",Para_min_Plot_Priors, sep=""), "_XYZWLHR_Best_plotPriors_S",
                           "_Best_Summary_Prior_TID_S", "_Ext15X15_allT_LocBox_XYZWLHR_N")
            CHECK_DIM <- list(c(-1,dim(Vox_N_DF)[2]), round(dim(List_RoI_Vox_DF), 0), dim(XYZWLHR_BestWorst_plotPriors), dim(Fsc_BestWorst_allGT), c(-1,dim(XYZWLHR_plotGT_N)[2]))
            DIFFERENCES_DIM <- setdiff(allList_DIM,CHECK_DIM)

            ##############################################################################
            # IF THERE IS ONE DIMENSION THAT IS NOT CORRECT THEN REMOVE ALL FILES FOR PLOT
            ##############################################################################
            if(length(DIFFERENCES_DIM) > 0){ 
              browser()
              dir.create(file.path(paste(FOLDER_REMOVE_PLOTS, sep=""), paste("Flight_", Flight_ID[ff], sep="")), showWarnings = FALSE)
              FOLDER_FLIGHT_REMOVE_PLOTS <- paste(FOLDER_REMOVE_PLOTS,"/Flight_",Flight_ID[ff] , sep="")
              
              MoveFrom_LAS_Vox_N <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/VOX_DF", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')
              MoveTo_LAS_Vox_N <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')        
              file.rename(from = MoveFrom_LAS_Vox_N,  to = MoveTo_LAS_Vox_N)
              
              MoveFrom_LAS_Vox_N_ALS <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/VOX_DF_ALS", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N_ALS.csv",sep='')
              MoveTo_LAS_Vox_N_ALS <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N_ALS.csv",sep='')
              file.rename(from = MoveFrom_LAS_Vox_N_ALS,  to = MoveTo_LAS_Vox_N_ALS)
              
              MoveFrom_Stocking_VoxCnt <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/Stocking_VoxCnt_GT", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Stocking_VoxCnt.csv",sep='')
              MoveTo_Stocking_VoxCnt <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Stocking_VoxCnt.csv",sep='')
              file.rename(from = MoveFrom_Stocking_VoxCnt,  to = MoveTo_Stocking_VoxCnt)
              
              MoveFrom_Ext15X15_allT_LocBox_XYZWLHR_N <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/LocBox_allP15X15", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep='')
              MoveTo_Ext15X15_allT_LocBox_XYZWLHR_N <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep='')
              file.rename(from = MoveFrom_Ext15X15_allT_LocBox_XYZWLHR_N,  to = MoveTo_Ext15X15_allT_LocBox_XYZWLHR_N)
              
              ###
              MoveFrom_List_RoI_Vox_DF <- paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Vox_DF_S",Para_min_Plot_Priors,".csv",sep='')
              MoveTo_List_RoI_Vox_DF <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Vox_DF_S",Para_min_Plot_Priors,".csv",sep='')
              file.rename(from = MoveFrom_List_RoI_Vox_DF,  to = MoveTo_List_RoI_Vox_DF)
              
              MoveFrom_List_RoI_Dec_DF <- paste(FOLDER_INPUT_ROI,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Dec_DF_S",Para_min_Plot_Priors,".csv",sep='')
              MoveTo_List_RoI_Dec_DF <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_List_RoI_Dec_DF_S",Para_min_Plot_Priors,".csv",sep='')
              file.rename(from = MoveFrom_List_RoI_Dec_DF,  to = MoveTo_List_RoI_Dec_DF)
              
              
              MoveFrom_Fsc_Best_allGT <- paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Best_Summary_Prior_TID_S",Para_min_Plot_Priors,".csv",sep='')
              MoveTo_Fsc_Best_allGT <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Best_Summary_Prior_TID_S",Para_min_Plot_Priors,".csv",sep='')
              file.rename(from = MoveFrom_Fsc_Best_allGT,  to = MoveTo_Fsc_Best_allGT)
              
              MoveFrom_XYZWLHR_Best_plotPriors <- paste(FOLDER_TARGET_ROI_AND_CLASS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_XYZWLHR_Best_plotPriors_S",Para_min_Plot_Priors,".csv",sep='')
              MoveTo_XYZWLHR_Best_plotPriors <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_XYZWLHR_Best_plotPriors_S",Para_min_Plot_Priors,".csv",sep='')
              file.rename(from = MoveFrom_XYZWLHR_Best_plotPriors,  to = MoveTo_XYZWLHR_Best_plotPriors)
              
              print(paste("REMOVING:   ", "/",  Flight_ID[ff],"_P",  Plot_ID[pp]))
            }
            
            #####################################################################
            # IF THERE ARE NO PRIORS, MOVE ALL THE PLOTS DATA OUT OF THE ANALYSIS
            #####################################################################  
          }else{
            dir.create(file.path(paste(FOLDER_REMOVE_PLOTS, sep=""), paste("Flight_", Flight_ID[ff], sep="")), showWarnings = FALSE)
            FOLDER_FLIGHT_REMOVE_PLOTS <- paste(FOLDER_REMOVE_PLOTS,"/Flight_",Flight_ID[ff] , sep="")
            
            MoveFrom_LAS_Vox_N <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/VOX_DF", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')
            MoveTo_LAS_Vox_N <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')        
            file.rename(from = MoveFrom_LAS_Vox_N,  to = MoveTo_LAS_Vox_N)
            
            MoveFrom_LAS_Vox_N_ALS <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/VOX_DF_ALS", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N_ALS.csv",sep='')
            MoveTo_LAS_Vox_N_ALS <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N_ALS.csv",sep='')
            file.rename(from = MoveFrom_LAS_Vox_N_ALS,  to = MoveTo_LAS_Vox_N_ALS)
            
            MoveFrom_Stocking_VoxCnt <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/Stocking_VoxCnt_GT", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Stocking_VoxCnt.csv",sep='')
            MoveTo_Stocking_VoxCnt <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Stocking_VoxCnt.csv",sep='')
            file.rename(from = MoveFrom_Stocking_VoxCnt,  to = MoveTo_Stocking_VoxCnt)
            
            MoveFrom_Ext15X15_allT_LocBox_XYZWLHR_N <- paste(DATA_TYPE_FOLDER, "/Flight_", Flight_ID[ff], "/CSV/CSV_MP1/LocBox_allP15X15", "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep='')
            MoveTo_Ext15X15_allT_LocBox_XYZWLHR_N <- paste(FOLDER_FLIGHT_REMOVE_PLOTS, "/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep='')
            file.rename(from = MoveFrom_Ext15X15_allT_LocBox_XYZWLHR_N,  to = MoveTo_Ext15X15_allT_LocBox_XYZWLHR_N)
          }
          # browser() 
        } # PLOT LOOP
      } # IF THERE ARE PROCESSED PLOTS IN FLIGHT
    }
  } # FLIGHT LOOP
}

########################################################################################################################################################################
########################################################################################################################################################################

#plot(LAS_oneP_Vox_N_Trees_oneT, color="TID")

#  # GET ALL PRIORS REPRESENTING THE GT
#  PLOT_Fsc_oneGT_allPriors <- Output_Fsc_Prior_GT[which(Output_Fsc_Prior_GT$TID == PLOT_unique_GT),]
#  PLOT_ID_allPriors_oneGT <- unique(PLOT_Fsc_oneGT_allPriors$Prior)
#  PLOT_XYZWLHR_allPriors_oneGT <- XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$PriorID %in% PLOT_ID_allPriors_oneGT),]
# # browser()
#  for(PL in 1:nrow(PLOT_XYZWLHR_allPriors_oneGT)){
#    PLOT_Vert_allPriors <- XYZWHR_TO_VERT_FUN(PLOT_XYZWLHR_allPriors_oneGT[PL,], Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt)
#    PLOT_Vert_oneBBox <- PLOT_Vert_allPriors[5:8,]
#    #PLOT_Vert_oneBBox <- rbind(PLOT_Vert_oneBBox,PLOT_Vert_oneBBox[1,])
#    LAS_oneP_Vox_N_Trees_oneT <- filter_poi(LAS_Vox_N, TID > 1)
#    LAS_oneP_Vox_N_Trees_oneT@data$TID[which(LAS_oneP_Vox_N_Tcrees_oneT@data$TID != PLOT_unique_GT)] <- 1
#    plot(LAS_oneP_Vox_N_Trees_oneT, color="TID")
#    polygon3d(PLOT_Vert_oneBBox, fill=FALSE, col="white", lwd=1)
#    browser()
#    Index_Good <- which(Output_Fsc_Prior_GT$TID == PLOT_unique_GT & Output_Fsc_Prior_GT$Prior == 4)
#    Output_Fsc_Prior_GT[Index_Good,]
#  }
## PLOT_GT_PRIOR_VOX_ROI_FUN(LAS_Vox_N, XYZWLHR_plotGT_N, XYZWLHR_plotPriors_N, oneList_RoI_Dec)

########################################################################################################################################################################
########################################################################################################################################################################

# FOR DELETING PLOT DATA THAT DOES NOT HAVE ANY PRIORS

# DATA_TYPE_FOLDER
# FOLDER_INPUT_VOX_DF
# FOLDER_INPUT_GT
# FOLDER_INPUT_PRIORS
# FOLDER_INPUT_VOX
# FOLDER_EXTRA_PRIORS
# FOLDER_INPUT_ROI
# FOLDER_TARGET_ROI_AND_CLASS
# FOLDER_EXTRA_ROI
# paste(FOLDER_INPUT_VOX_DF,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_LAS_Vox_N.csv",sep='')
# paste(FOLDER_INPUT_GT,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_allT_LocBox_XYZWLHR_N.csv",sep='')
# paste(FOLDER_INPUT_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_Ext15X15_AllPRIORS_N.csv",sep='')
# paste(FOLDER_EXTRA_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_All_Fsc_Prior_GroundTruth.csv",sep='')
# paste(FOLDER_EXTRA_PRIORS,"/F",  Flight_ID[ff],"_P",  Plot_ID[pp], "_All_Prior_XYZ_Extent.csv",sep='')
# paste(FOLDER_INPUT_VOX, "/LAS_F",Flight_ID[ff], "_MP", MovPos, "_P", Plot_ID[pp],  "_Vox.laz", sep="")



# # WHEN length(DATA_Type) != 1 THEN ITS A TEST_DS AND THEREFORE GET VOXEL FROM ALS
# if(length(DATA_Type) == 1){  
#   oneFolder_INPUT_VOX <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/CSV_MP1/VOX_DF", sep="") # Flight_1\LAS\LAS_MP1\LAS_ALS
#   Files_INPUT_VOX <- c(Files_INPUT_VOX, list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N"))
#   Folders_INPUT_VOX <- c(Folders_INPUT_VOX, rep(oneFolder_INPUT_VOX, length(list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N"))))
# }else{
#   oneFolder_INPUT_VOX <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/CSV_MP1/VOX_DF_ALS", sep="")
#   Files_INPUT_VOX <- c(Files_INPUT_VOX, list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N_ALS"))
#   Folders_INPUT_VOX <- c(Folders_INPUT_VOX, rep(oneFolder_INPUT_VOX, length(list.files(oneFolder_INPUT_VOX , pattern = "_LAS_Vox_N_ALS"))))
# }

# THIS ONE DOESN'T GET PRODUCED
# oneFolder_INPUT_ROI <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/INPUT_ROI", sep="")
# Files_INPUT_ROI <- c(Files_INPUT_ROI , list.files(oneFolder_INPUT_ROI , pattern = "_List_RoI_Vox_DF_S64_XYZWLH.csv")) ### DOM DOM DOM !!! SHOULD THIS BE List_RoI_Vox_DF_S21_XYZWLH
# Folders_INPUT_ROI <- c(Folders_INPUT_ROI, rep(oneFolder_INPUT_ROI, length(list.files(oneFolder_INPUT_ROI , pattern = "_List_RoI_Vox_DF_S64_XYZWLH.csv"))))

# THIS ONE DOESN'T GET PRODUCED
# oneFolder_INPUT_PRIORS <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/PRIORS", sep="")
# Files_INPUT_PRIOR_XYZWLHR <- c(Files_INPUT_PRIOR_XYZWLHR, list.files(oneFolder_INPUT_PRIORS , pattern = "_XYZWLHR_Best_plotPriors_S"))
# Folders_INPUT_PRIOR_XYZWLHR  <- c(Folders_INPUT_PRIOR_XYZWLHR, rep(oneFolder_INPUT_PRIORS, length(list.files(oneFolder_INPUT_PRIORS , pattern = "_Best_Summary_Prior_TID_S"))))
# 
# # USE SAME FOLDER AS oneFolder_INPUT_PRIORS
# # SAME AS TARGET CLASS
# Files_INPUT_ROI_CLASS <- c(Files_INPUT_ROI_CLASS, list.files(oneFolder_INPUT_PRIORS , pattern = "_Best_Summary_Prior_TID_S"))
# Folders_INPUT_ROI_CLASS <- c(Folders_INPUT_ROI_CLASS, rep(oneFolder_INPUT_PRIORS, length(list.files(oneFolder_INPUT_PRIORS , pattern = "_Best_Summary_Prior_TID_S"))))

# oneFolder_TARGET_STOCKING <- paste(Folders_oneDATA_TYPE, "/", flights[ff],"/CSV/CSV_MP1/Stocking_VoxCnt_GT", sep="")
# Files_TARGET_STOCKING <- c(Files_TARGET_STOCKING, list.files(oneFolder_TARGET_STOCKING , pattern = "_Stocking_VoxCnt"))
# Folders_TARGET_STOCKING <- c(Folders_TARGET_STOCKING, rep(oneFolder_TARGET_STOCKING, length(list.files(oneFolder_TARGET_STOCKING , pattern = "_Stocking_VoxCnt"))))
# oneFolder_PRIORS <- paste(Folders_oneDATA_TYPE, "/", flights[ff], "/CSV/PRIORS", sep="") ### DOM DOM DOM !!! THIS NEEDS FIXING WHEN YOU HAVE MANY MPs
# Files_PRIORS <- c(Files_PRIORS, list.files(oneFolder_PRIORS , pattern = "_Best_Summary_Prior_TID_S") )
# Folders_PRIORS <- c(Folders_PRIORS, rep(oneFolder_PRIORS, length(list.files(oneFolder_PRIORS , pattern = "_Best_Summary_Prior_TID_S"))))
# 
# oneFolder_TARGET_XYZWLHR <- paste(Folders_oneDATA_TYPE, "/", flights[ff],"/CSV/CSV_MP1/LocBox_allP15X15", sep="")
# Files_TARGET_XYZWLHR <- c(Files_TARGET_XYZWLHR, list.files(oneFolder_TARGET_XYZWLHR , pattern = "_Ext15X15_allT_LocBox_XYZWLHR_N") )
# Folders_TARGET_XYZWLHR <- c(Folders_TARGET_XYZWLHR, rep(oneFolder_TARGET_XYZWLHR, length(list.files(oneFolder_TARGET_XYZWLHR , pattern = "_Ext15X15_allT_LocBox_XYZWLHR_N"))))
# 
# 


    
    
    
    
    
    
    
    
    
    
    
    
    
    
 
