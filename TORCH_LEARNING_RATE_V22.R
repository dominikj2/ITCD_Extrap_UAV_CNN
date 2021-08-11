#FIND OPTIMALLY EFFICIENT LEARNING RATE

# spend some time upfront to determine an efficient learning rate. While out-of-the-box, torch does not provide a tool like fast.ai's learning rate finder, the logic is straightforward to implement. Here's how to find a good learning rate, as translated to R from Sylvain Gugger's post:
optimizer = optim_sgd(model_Instance$parameters, lr=Para_learning_rate,
                      momentum=Para_momentum)  
losses <- c()
log_lrs <- c()
print("In LEARNING_RATE")

find_lr <- function(init_value = 1e-8, final_value = 10, beta = 0.98) {
  
  num <- Train_dl$.length()
  mult = (final_value/init_value)^(1/num)
  lr <- init_value
  # browser()
  optimizer$param_groups[[1]]$lr <- lr
  avg_loss <- 0
  best_loss <- 0
  batch_num <- 0
  
  coro::loop(for (LL in Train_dl) {    # for (LL in enumerate(Train_dl)) {
    #browser()
    batch_num <- batch_num + 1
    optimizer$zero_grad()
    
    # DOM DOM DOM !!! output AND loss NEED TO BE DETERMINED USING MY MODEL 
    
    # #output <- model(LL[[1]]$to(device = device))
    # output <- model_Instance(INPUT_Vox_Den, INPUT_Vox_TID, INPUT_RoI, INPUT_PRIOR_XYZWLHR, TARGET_TID)
    # 
    # # loss <- criterion(output, LL[[2]]$to(device = device))
    
    ### RUN MODEL AND COMPUTE LOSS
    INPUT_LIST <- LL[[1]]
    TARGET_LIST <- LL[[2]]
    FLIGHT_PLOT_ID_LIST <- LL[[3]]
    Loss_all <- RUN_MODEL_LOSS_FUN(INPUT_LIST, TARGET_LIST, FLIGHT_PLOT_ID_LIST, epoch_T, Batch_Count_T) #INPUT_LIST, TARGET_LIST, FLIGHT_PLOT_ID_LIST, epoch_T, Batch_Count_T
    
    # OUTPUTS
    OUTPUT_LIST <- Loss_all[[1]]
    
    # Loc_Loss_Positive <- Loss_all[[2]][[1]]
    # conf_loss <- Loss_all[[2]][[2]]
    # TOTAL_LOSS <- Loss_all[[2]][[3]]
    # loss <- Loc_Loss_Positive
    
    TOTAL_LOSS <- unlist(Loss_all[[2]][[1]]) 
   
    #Compute the smoothed loss  #
    avg_loss <- as.array(beta * avg_loss + (1-beta) * TOTAL_LOSS$item())
    smoothed_loss <- as.array(avg_loss / (1 - beta^batch_num))
    #Stop if the loss is exploding
    if (batch_num > 1 && smoothed_loss > 4 * best_loss) break
    #Record the best loss
    if (smoothed_loss < best_loss || batch_num == 1) best_loss <- smoothed_loss
    
    #Store the values
    losses <<- c(losses, smoothed_loss)
    log_lrs <<- c(log_lrs, (log(lr, 10)))
    # browser()
    # TOTAL_LOSS_T <-unlist(Loss_all[[2]][[3]])
    TOTAL_LOSS$backward()
    optimizer$step()
    # browser()
    #Update the lr for the next step
    lr <- lr * mult
    optimizer$param_groups[[1]]$lr <- lr
    print(paste( "LEARNING RATE: ", lr))
    print(paste( "RUN: ", batch_num, " out of ", num))
    #print(paste( "LEARNING RATE: ", batch_num, " out of ", num))
  })
}

find_lr()
#browser()
# PLOT THE LEARNING RATE OUTPUT TO DETERMINE THE BEST PARAMETER
Learning_Rate_Results <- data.frame(log_lrs = log_lrs, losses = losses)

# FOLDER FOR SAVING LEARNING RATE DATA
dir.create(file.path(FOLDER_MAIN_DATA,  "SAVED_MODELS"), showWarnings = FALSE)
Folder_SAVED_MODELS <- paste(FOLDER_MAIN_DATA,  "/SAVED_MODELS", sep="") 

dir.create(file.path(Folder_SAVED_MODELS,  paste("RUN", Version_RCode, "_", RUN_NAME, sep="") ), showWarnings = FALSE) 
Folder_SAVED_MODELS_Version <- paste(Folder_SAVED_MODELS,  "/RUN", Version_RCode, "_",RUN_NAME, sep="") 

dir.create(file.path(Folder_SAVED_MODELS_Version,  "PDF_OUTPUT"), showWarnings = FALSE)
Folder_PDF_OUTPUT <- paste(Folder_SAVED_MODELS_Version,  "/PDF_OUTPUT", sep="") 

write.csv(Learning_Rate_Results, paste(Folder_SAVED_MODELS_Version, "/Learning_Rate_Results_B64_AllFlights.csv", sep = ""))
pdf(paste(Folder_PDF_OUTPUT, "/", "LEARNING_RATE.pdf", sep=""))
ggplot(Learning_Rate_Results, aes(log_lrs, losses)) + geom_point(size = 1) + theme_classic()
dev.off()
# browser()

#OPTIMAL_LR <- 10^-2.3 # 0.00501187233627
# OPTIMAL_LR <- 10^-2.3 = 0.00501187233627