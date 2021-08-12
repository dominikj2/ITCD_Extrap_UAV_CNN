rm(list=ls())
options(digits = 12)
options(warn=1, error = NULL) # options(warn=2, error=recover)

library(torch)
library(stats)
library(tidyr)
library(lidR)
library(dplyr)
library(rlist)
library(alphashape3d)
library(randomcoloR)
library(spdep) 
library(pastecs)
library(ggplot2)
library(oce)
library(stplanr)
library(geosphere)
library(fields)
library(geometry)
library(tibble)
library(splitstackshape)


##############################
# PARAMETERS USED IN THE MODEL
##############################

Para_Vox_Res <- 1

MovPos <- 1

Para_Target_Base <- 16 # torch_tensor(16L, device=device)     # FOR V-NET OF EACH POTENTIAL TREE
Para_Target_Z_Height <- 40  # FOR V-NET OF EACH POTENTIAL TREE
Para_MaxZ_Shrink <- 38.99
Para_Base_WL <- 0.5 # BASE LENGTH AND WIDTH ... THIS IS NORMALISED WHEN EVERYTHING ELSE GETS NORMALISED

Para_min_Plot_Priors <- 64   # THIS SHOULD BE MUCH LARGER 256 maybe... # 200 # 30 # THIS IS THE TARGET AIM OF SAMPLES FOR EACH PLOT
#Para_TriShp_Parameters <- 10 
Para_TriShpParaCnt <- 10
# DOM DOM DOM !!! Para_Learning_Sample will now vary depending on the number of trees 
#Para_Learning_Sample <- 4 # 2   # Para_min_Plot_Priors/2 #4 #THIS IS THE MINIMUM NUMBER OF 0 and 1 SAMPLES FOR EACH TREE    Para_min_Plot_Priors/2 # NUMBER OF POOR AND NUMBER OF GOOD PRIORS USED FOR EACH TREE

# Para_BBox_ParaCnt <- 16
# Para_RoI_ParaCnt <- 6 # # xyzwlh (where xyz is bottom left corner of the RoI, w is width in x, l is length in y, and h is height in z direction)

# PARAMETERS TO BE MORE SELECTIVE WITH TYPE OF PRIORS uSED AT THE END
Para_min_TID_GoodPrior <- c(3, 5) #c(5, 10)
Para_max_IoU_Bad <- 0.3
Para_min_IoU_Good <- 0.7
Para_min_IoU_Good_Extra <- c(0.6, 0.5)

# THE STANDARD RoI AREA FOR EACH PRIOR USED IN THE FINAL CLASSIFICATION.... MAKE SURE THE RoIs SCALING FACTOR AND LOCATION RELATIVE TO WHOLE PLOTS IS CONSIDERED 
para_RoI_Pool_Dim_XY <- 8
para_RoI_Pool_Dim_Z <- 16
pool_size = c(para_RoI_Pool_Dim_XY,para_RoI_Pool_Dim_XY,para_RoI_Pool_Dim_Z) 

# PARAMETERS FOR NON-MAXIMUM SUPPRESSION PROCEDURE
### DOM DOM DOM !!! INITIALISE CODE HAS Para_Threshold_Prob <- 0
Para_Threshold_Prob <- 0.5 # THIS NEEDS TO BE GIVEN A REASONABLE VALUE AT SOME STAGE ... 
Para_Threshold_IoU <- 0.5

Para_Faces_Used = 4

# OPTIMISE PARAMETERS 
Para_learning_rate <-  0.01
Para_momentum <- 0.5
Para_num_epochs <- 800

para_dice_weight <- 0.3
Para_N_hn <- 0.5 # NUMBER OF HARD NEGATIVES IS FIXED MULTIPLE OF HOW MANY POSITIVE MATCHES THERE ARE FOR PLOT
  
Para_minFSc <- 0.15

Para_Sample_Size <- "Flight1"
# Para_Sample_Size <- 2
#Plot_Selection <- c("102", "103")  # IF SAMPLE SIZE IS SMALL YOU CAN SELECT CERTAIN PLOTS

EPSILON = 1e-8

shuffle <- TRUE

if(Para_TriShpParaCnt == 16){
  Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
                        "X_BotBox", "Y_BotBox", "L_BotBox", "W_BotBox", "Z_BotBox", "R_BotBox", 
                        "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
                        "Z_TopTree")
}else{
  Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
                        "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
                        "Z_TopTree")
}
############################################################################################################################################################
############################################################################################################################################################
#########
# FOLDERS
######### 


#############
# TYPE OF RUN
############# RUN_V22__MT3_sub2_GPU_CUDA_FixedBinary_V3

RUN_NAME <- "_MT2_F1_GPU_CUDA_FxBinary_MSEnoWght_LR005_DICE_TreeOnly_V1" # "_MT2_allF_GPU_CUDA_ReqGrad_V2inRoI" # "_MULTI_TASK_TEST_BATCH_SIZE_62" # RUN_V21__MULTI_TASK_TEST_ONLY_FLIGHT_1
RUN_GPU <- "Yes"
RUN_DESKTOP <- "Yes"
RUN_SUBSAMPLE_OF_PLOTS <- "Yes"              # Para_Sample_Size <- 3 OR DEPENDS ON SPECIFIED FLIGHTS
RUN_CONTINUE_RUNNING_MODEL <- "No"           # THIS IS IN THE "TORCH_OPTIMISE_PREDICT_V22.R"
RUN_OPEN_EXISTING_MODEL <- "No"
RUN_MULTI_TASK_LEVEL <- 2                    # OPTION 2 or 3
RUN_CIoU_BINARY_LEVEL2 <- "Yes"
# RUN_OPEN_SAVED_MODEL <- "Yes"
RUN_PREPARE_DATA <- "No"                     # "Yes"
RUN_COMPUTE_LEARNING_RATE <- "No"
RUN_ONLY_EVALUATE_MODE <- "No"               # Yes if NOT training
RUN_REVERSE_ENG_TENSORS_VISUALISE <- "No"     # "Yes"
RUN_PLOT_TRAINED_MODEL <- "Yes"
RUN_PLOT_LIDAR_TRISHAPE <- "No"
RUN_PLOTS_1 <- "No"
RUN_OPTIM_HYPERPARAMETER_MULTITASK <- "No" 
RUN_Compute_Fscore_EveryN <- "Yes"
RUN_MSE_WEIGHTS <- "No"
RUN_DEBUGGING <- "No" # USE traceback() or Rerun with Debug

if(RUN_COMPUTE_LEARNING_RATE == "No"){ OPTIMAL_LR <- 0.005 } # 10^-2.4 ... 0.00398107170553 0.005 (best 0.19)... 0.001 (jumped around)

if(RUN_MSE_WEIGHTS == "Yes"){
  Para_MSE_WEIGHTS <-c(1,1,1, 3, 3, 1.5,1.5, 2, 1, 1) # SEE Colnames_XYZWLHR BELOW
}else{
  Para_MSE_WEIGHTS <-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1) # SEE Colnames_XYZWLHR BELOW
}


if(RUN_GPU == "Yes"){
  device <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"
  print(paste("CUDA AVAILABLE.............................................................................:", cuda_is_available()))
  GPU <- "_GPU" # 
}else{
  device <- if (FALSE) torch_device("cuda:0") else "cpu"
  GPU <- "" # 
}

# ### DOM DOM DOM !!! PRESENTLY ONLY USING CPU  
# device <- if (FALSE) torch_device("cuda:0") else "cpu"


#Comp <-"D:/"
if(RUN_DESKTOP == "Yes"){
  Comp <- "D:/Y_Drive/" # "//rcgdata/dj806/" 
}else{
  Comp <- "/home/dj806/" 
}

if(RUN_DEBUGGING == "Yes"){
  options(error = browser())
}

Version_RCode <- "_V22"
Version_Data <- "_DATA" # THIS NEEDS CHANGING and then THINNED_SAMPLES_TORCH_DATA below

FOLDER_MAIN_DATA <- paste(Comp, "THINNED_SAMPLES_TORCH", Version_Data, sep="") # CNN/  
FOLDER_TORCH_CODE <- paste(Comp, "CNN/R_Code/TORCH_ITCD_EXTRAP", Version_RCode, "/ITCD_Extrap_UAV_CNN/", sep="")

para_Number_Workers <- 0
batch_size = 12 # 64 #64 #62 # THIS SHOULD BE AS LARGE AS POSSIBLE WITHOUT EXCEEDING MEMORY # https://stackoverflow.com/questions/35050753/how-big-should-batch-size-and-number-of-epochs-be-when-fitting-a-model-in-keras


#browser()
############################################################################################################################################################
############################################################################################################################################################
##################
# SOURCE FUNCTIONS
##################
source(paste(FOLDER_TORCH_CODE, "TORCH_FUNCTIONS_CNN_MASTER", Version_RCode, ".R", sep="")) 
source(paste(FOLDER_TORCH_CODE, "TORCH_CIoU",Version_RCode,".R", sep="")) 

Triangles_ID_All_L4 <-TRIANG_ID_ALL_FUN(Para_Faces_Used = 4, Box_Levels = 4)
Triangles_ID_All_Mx_L4 <- matrix(Triangles_ID_All_L4, ncol=3, byrow= TRUE)

Triangles_ID_All_L3 <-TRIANG_ID_ALL_FUN(Para_Faces_Used = 4, Box_Levels = 3)
Triangles_ID_All_Mx_L3 <- matrix(Triangles_ID_All_L3, ncol=3, byrow= TRUE)

############################
# INPUTS FOR NMS CALCULATION
############################

# CREATE AN EMPTY VOXEL SPACE 
Vox_Dim <- c(Para_Target_Base, Para_Target_Base, Para_Target_Z_Height)
X_Coord <- seq(0,1, length.out =Vox_Dim[1])
Y_Coord <- seq(0,1, length.out =Vox_Dim[2]) 
Z_Coord <- seq(0,1, length.out =Vox_Dim[3]) 
XY_Coord <- as.data.frame(crossing(X=X_Coord, Y=Y_Coord))
Empty_Vox <- XY_Coord[rep(seq_len(nrow(XY_Coord)), each = length(Z_Coord)), ] 
Empty_Vox$Z <- as.double(rep(Z_Coord, nrow(XY_Coord)))

############################################################################################################################################################
############################################################################################################################################################
if(RUN_OPEN_EXISTING_MODEL == "Yes"){ # NO LONGER USING THIS APPRAOCH ... FOR RUN_CONTINUE_RUNNING_MODEL
  source(paste(FOLDER_TORCH_CODE, "TORCH_DataLOADER",Version_RCode,"a.R", sep=""))
  source(paste(FOLDER_TORCH_CODE, "TORCH_INITIALISE", GPU, "_VOXIoU", Version_RCode,"e.R", sep=""))

  #source(paste(FOLDER_TORCH_CODE, "TORCH_DEVELOP_NMS_DICE",Version_RCode,"b.R", sep=""))
  source(paste(FOLDER_TORCH_CODE, "TORCH_OPEN_EXISTING_MODEL",Version_RCode,".R", sep="")) # b
  #source(paste(FOLDER_TORCH_CODE, "TORCH_CONTINUE_RUNNING_MODEL",Version_RCode,".R", sep=""))
}else{
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  #######################################
  # PRODUCE THE BEST/WORST PRIORS AND ROI
  #######################################
  if(RUN_PREPARE_DATA == "Yes"){
    source(paste(FOLDER_TORCH_CODE, "TORCH_PrepDATA_EmptyVox",Version_RCode,".R", sep=""))  # NO NEED TO RUN EVERY TIME (ONLY RUN IF YOU CHANGE PARAMETERS)
    }
  #browser()
  ############################################################################################################################################################
  ############################################################################################################################################################
  ####################
  # GENERATE CNN INPUT
  ####################

  source(paste(FOLDER_TORCH_CODE, "TORCH_DataLOADER",Version_RCode,"a.R", sep="")) 
  # browser()
  ############################################################################################################################################################
  ############################################################################################################################################################
  ##########################
  # PLOT INPUT LAS TRISHAPES
  ##########################
  if(RUN_PLOT_LIDAR_TRISHAPE == "Yes"){
    # source(paste(FOLDER_TORCH_CODE, "TORCH_DataLOADER",Version_RCode,"a.R", sep="")) 
    source(paste(FOLDER_TORCH_CODE, "TORCH_INITIALISE", GPU, "_VOXIoU", Version_RCode, "e.R", sep="")) 
    source(paste(FOLDER_TORCH_CODE, "TORCH_PLOT_LIDAR_TARGET_BEST_PRIOR",Version_RCode,".R", sep="")) 
  }
  ############################################################################################################################################################
  ############################################################################################################################################################
  ############
  # INITIALISE
  ############
  source(paste(FOLDER_TORCH_CODE, "TORCH_INITIALISE", GPU, "_VOXIoU", Version_RCode, "e.R", sep="")) 
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  #########################
  # LEARNING RATE PROCEDURE
  #########################
  if(RUN_COMPUTE_LEARNING_RATE == "Yes"){
    source(paste(FOLDER_TORCH_CODE, "TORCH_LEARNING_RATE",Version_RCode,".R", sep="")) 
  }
  # browser()
  ############################################################################################################################################################
  ############################################################################################################################################################
  ######################
  # OPTIMISE AND PREDICT
  ######################
  #browser()
  source(paste(FOLDER_TORCH_CODE, "TORCH_OPTIMISE_PREDICT","_VoxIoU",Version_RCode,".R", sep="")) 
  #browser()
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ######################
  # TEST THE FINAL MODEL
  ######################
  source(paste("D:/CODE/CNN/R_Code/TORCH_TEST_ALS_DATA", Version_RCode, ".R", sep="")) 

} # IFELSE FOR DEVELOPING DICE


