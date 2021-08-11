
################################
# FIRST BOUNDING BOX CALCULATION (THIS BECOMES THE LOCATION FOR PARA10 )
################################

LAS_St <- filter_poi(LAS_UAS_Thin_oneT, Z < Z_C_Base)
Coord_St <- matrix(c(LAS_St$X,  LAS_St$Y), ncol=2)

if(nrow(Coord_St) > 2){
  BBox1_oneT_oneS <- as.data.frame(BBOX_PNTS_FUN(Coord_St))
  BBox1_oneT_oneS <- cbind( Unique_TID[TT], S, 2 , BBox1_oneT_oneS, Z = Z_C_Base)
  colnames(BBox1_oneT_oneS) <- c("TID", "Sample", "Quantile", "X", "Y", "Z")
  BBox1_oneT_oneS <- as.data.frame(BBox1_oneT_oneS)
  BBox1_oneT_oneS = apply(BBox1_oneT_oneS, 2, function(x) as.numeric(as.character(x)))
  
}else{
  Cent_X <- mean(LAS_UAS_Thin_oneT$X[which.min(LAS_UAS_Thin_oneT$Z)])
  Cent_Y <- mean(LAS_UAS_Thin_oneT$Y[which.min(LAS_UAS_Thin_oneT$Z)])
  Cent_Z <- mean(LAS_UAS_Thin_oneT$Z[which.min(LAS_UAS_Thin_oneT$Z)])
  
  BBox1_oneT_oneS <- BBox_Sqr_FUN(Cent_X, Cent_Y, Cent_Z, Sqr_Offset = Para_Base_WL/2)
  BBox1_oneT_oneS <- cbind( Unique_TID[TT], S, 2 , BBox1_oneT_oneS)
  colnames(BBox1_oneT_oneS) <- c("TID", "Sample", "Quantile", "X", "Y", "Z")
  BBox1_oneT_oneS <- as.data.frame(BBox1_oneT_oneS)
  BBox1_oneT_oneS = apply(BBox1_oneT_oneS, 2, function(x) as.numeric(as.character(x)))
}

################################
# SECOND BOUNDING BOX CALCULATION
################################
LAS_Can <- filter_poi(LAS_UAS_Thin_oneT, Z >= Z_C_Base)
pdens <- density(LAS_Can$Z, bw=Para_C_Base_BW)
Peak_Dip_Summary <- Peak_Dip_FUN(pdens)

Coord_Can <- matrix(c(LAS_Can$X,  LAS_Can$Y), ncol=2)
if(nrow(Coord_Can) > 2){
  BBox2_oneT_oneS <- as.data.frame(BBOX_PNTS_FUN(Coord_Can))
  BBox2_oneT_oneS <- cbind(Unique_TID[TT], S, 3 , BBox2_oneT_oneS, Z = Peak_Dip_Summary$maxPeak_DF$Z)
  colnames(BBox2_oneT_oneS) <- c("TID", "Sample", "Quantile", "X", "Y", "Z")
  BBox2_oneT_oneS <- as.data.frame(BBox2_oneT_oneS)
  BBox2_oneT_oneS = apply(BBox2_oneT_oneS, 2, function(x) as.numeric(as.character(x)))
}else{
  Cent_X <- mean(LAS_UAS_Thin_oneT$X[which.max(LAS_UAS_Thin_oneT$Z)])
  Cent_Y <- mean(LAS_UAS_Thin_oneT$Y[which.max(LAS_UAS_Thin_oneT$Z)])
  Cent_Z <- mean(LAS_UAS_Thin_oneT$Z[which.max(LAS_UAS_Thin_oneT$Z)])
  
  BBox2_oneT_oneS <- BBox_Sqr_FUN(Cent_X, Cent_Y, Cent_Z, Sqr_Offset = Para_Base_WL/2)
  BBox2_oneT_oneS <- cbind( Unique_TID[TT], S, 3 , BBox2_oneT_oneS)
  colnames(BBox2_oneT_oneS) <- c("TID", "Sample", "Quantile", "X", "Y", "Z")
  BBox2_oneT_oneS <- as.data.frame(BBox2_oneT_oneS)
  BBox2_oneT_oneS = apply(BBox2_oneT_oneS, 2, function(x) as.numeric(as.character(x)))
}

#####################
# TID LOCATION (base)
#####################
X_oneT_oneS <- mean(as.data.frame(BBox1_oneT_oneS)$X) #mean(LAS_UAS_Thin_oneT$X[which.min(LAS_UAS_Thin_oneT$Z)])
Y_oneT_oneS <- mean(as.data.frame(BBox1_oneT_oneS)$Y)  #mean(LAS_UAS_Thin_oneT$Y[which.min(LAS_UAS_Thin_oneT$Z)])
Z_oneT_oneS <- mean(as.data.frame(BBox1_oneT_oneS)$Z)   #mean(LAS_UAS_Thin_oneT$Z[which.min(LAS_UAS_Thin_oneT$Z)])

###############################
# MERGING TID LOC AND BBOX DATA 
###############################

XYZ_oneT_oneS <- data.frame( TID =Unique_TID[TT], Sample =S, Quantile = 0 , X = X_oneT_oneS, Y =Y_oneT_oneS, Z =Z_oneT_oneS )
DF_LocBox_oneF_allT <- rbind(DF_LocBox_oneF_allT, XYZ_oneT_oneS)
DF_LocBox_oneT <- rbind(DF_LocBox_oneT, XYZ_oneT_oneS)

#browser()
# NULLAFY BBox1
BBox1_oneT_oneS[,4] <- NA
BBox1_oneT_oneS[,5] <- NA
BBox1_oneT_oneS[,6] <- NA
DF_LocBox_oneF_allT <- rbind(DF_LocBox_oneF_allT, BBox1_oneT_oneS)
DF_LocBox_oneT <- rbind(DF_LocBox_oneT, BBox1_oneT_oneS)

DF_LocBox_oneF_allT <- rbind(DF_LocBox_oneF_allT, BBox2_oneT_oneS)
DF_LocBox_oneT <- rbind(DF_LocBox_oneT, BBox2_oneT_oneS)

if(nrow(DF_LocBox_oneT) > 3){ # IF LESS THAN THREE THEN BOTH BBox ARE MISSING
  
  # TID LOCATIONS
  Index_Zero <- which(DF_LocBox_oneT$Quantile == 0)
  if(length(Index_Zero) > 0 ){
    Loc_T <- DF_LocBox_oneT[Index_Zero,]
  }else{
    Loc_T <- DF_LocBox_oneT
  }
  Loc_T <- Loc_T[,-which(colnames(Loc_T) == "Quantile")]
  colnames(Loc_T)[which(colnames(Loc_T) %in% c("X", "Y", "Z"))] <- c("X_Base","Y_Base", "Z_Base")
  XYZWLHR_oneF_oneT <- Loc_T
  
  BBox_Bot <- data.frame(X_BotBox = NA,
                         Y_BotBox = NA,
                         L_BotBox = NA,
                         W_BotBox = NA,
                         Z_BotBox = NA,
                         R_BotBox = NA  )
  XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, BBox_Bot)
  
  # # BOTTOM BOUNDING BOX
  # oneT_Bot_BoundBox <- DF_LocBox_oneT[which(DF_LocBox_oneT$Quantile == 2),]
  # if(nrow(oneT_Bot_BoundBox) == 5){
  #   BBox_Bot <- XYZWLHR_FUN(oneT_Bot_BoundBox, Prefix = "Bot", Height_Strata = 2, Sample_Att = "Yes")
  #   XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, BBox_Bot[,4:ncol(BBox_Bot)])
  # }else{
  #   # IF BOTTOM BOUND BOX IS NA GIVE IT TOP MINUS 1 m 
  #   oneT_Bot_BoundBox_ChangeZ <- DF_LocBox_oneT[which(DF_LocBox_oneT$Quantile == 3),]
  #   if(nrow(oneT_Bot_BoundBox_ChangeZ) == 5){
  #     oneT_Bot_BoundBox_ChangeZ$Z <- oneT_Bot_BoundBox_ChangeZ$Z  - 1
  #     BBox_Bot <- XYZWLHR_FUN(oneT_Bot_BoundBox_ChangeZ, Prefix = "Bot", Height_Strata = 2, Sample_Att = "Yes")
  #     XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, BBox_Bot[,4:ncol(BBox_Bot)])
  #     # IF TOP AND BOTTOM DONT EXIST GIVE NA
  #   }else{
  #     flag <- 6
  #     browser()
  #     XYZWLHR_oneT_NA <- data.frame(X_BotBox = NA,
  #                                   Y_BotBox = NA,
  #                                   L_BotBox = NA,
  #                                   W_BotBox = NA,
  #                                   Z_BotBox = NA,
  #                                   R_BotBox = NA  ) #, Bot_TreeHeight = NA
  #     XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, XYZWLHR_oneT_NA)
  #   }
  # }
  
  # TOP BOUNDING BOX
  oneT_Top_BoundBox <- DF_LocBox_oneT[which(DF_LocBox_oneT$Quantile == 3),]
  
  if(nrow(oneT_Top_BoundBox) == 5){
    BBox_Top <- XYZWLHR_FUN(oneT_Top_BoundBox, Prefix = "Top", Height_Strata = 3, Sample_Att="Yes")
    XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, BBox_Top[,4:ncol(BBox_Top)])
    XYZWLHR_oneF_oneT$Z_TopTree <- max(LAS_UAS_Thin_oneT$Z) # DOM DOM DOM !!! JUST PUT THIS IN 11/12/20
  }else{
    # IF TOP BOUND BOX IS NA GIVE IT BOTTOM + 1
    oneT_Top_BoundBox_ChangeZ <- DF_LocBox_oneT[which(DF_LocBox_oneT$Quantile == 2),]
    if(nrow(oneT_Top_BoundBox_ChangeZ) == 5){
      oneT_Top_BoundBox_ChangeZ$Z <- oneT_Top_BoundBox_ChangeZ$Z  + 1
      BBox_Top <- XYZWLHR_FUN(oneT_Top_BoundBox_ChangeZ, Prefix = "Top", Height_Strata = 3, Sample_Att = "Yes")
      XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, BBox_Top[,4:ncol(BBox_Top)])
      # IF TOP AND BOTTOM DONT EXIST GIVE NA
    }else{
      flag <- 7
      browser()
      XYZWLHR_oneT_NA <- data.frame(X_BotBox = NA,
                                    Y_BotBox = NA,
                                    L_BotBox = NA,
                                    W_BotBox = NA,
                                    Z_BotBox = NA,
                                    R_BotBox = NA  )#, Bot_TreeHeight = NA
      XYZWLHR_oneF_oneT <- cbind(XYZWLHR_oneF_oneT, XYZWLHR_oneT_NA)
    }
  }
  
  if(!exists("XYZWLHR_oneF_allT")){
    XYZWLHR_oneF_allT <- XYZWLHR_oneF_oneT
  }else{
    XYZWLHR_oneF_allT <- rbind(XYZWLHR_oneF_allT,XYZWLHR_oneF_oneT)
  }  
}else{# IF TIC HAS ONE OF TWO BOUNDING BOXES ARE PRESENT.
  browser() # BOTH BBOX MISSING
}

# browser()