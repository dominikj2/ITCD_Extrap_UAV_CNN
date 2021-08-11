# ############################################################################################################
# ############################################################################################################
# 
# Rotate_Deg <- 30
# Length_Y <- 0.4
# Width_X <- 0.2
# Cent_X <- 0.5 
# Cent_Y <- 0.5
# # translate point to origin
# # cx, cy - center of square coordinates
# # x, y - coordinates of a corner point of the square
# # theta is the angle of rotation
# 
# rad2deg <- function(rad) {(rad * 180) / (pi)}
# deg2rad <- function(deg) {(deg * pi) / (180)}
# Rotate_Deg <- deg2rad(40)
# 
# # translate point to origin
# tempX <- c(Cent_X-(0.5*Width_X), Cent_X-(0.5*Width_X), Cent_X+(0.5*Width_X),  Cent_X+(0.5*Width_X))
# tempY <- c(Cent_Y-(0.5*Length_Y), Cent_Y+ (0.5*Length_Y), Cent_Y- (0.5*Length_Y), Cent_Y+(0.5*Length_Y))
# tempX = tempX - Cent_X #c(tempX,Cent_X) #- Cent_X;
# tempY = tempY - Cent_Y #c(tempY,Cent_Y) #- Cent_Y;
# 
# # now apply rotation
# rotatedX = tempX*cos(Rotate_Deg) - tempY*sin(Rotate_Deg);
# rotatedY = tempX*sin(Rotate_Deg) + tempY*cos(Rotate_Deg);
# 
# # translate back
# xNew <- rotatedX + Cent_X
# YNew <- rotatedY + Cent_Y
# 
# # make SpatialPoints
# points <- sp::SpatialPoints(cbind(c(xNew, Cent_X),c(YNew, Cent_X)))
# #points2 <- sp::SpatialPoints(cbind(X,Y))
# plot(points)
# 
# 
# ############################################################################################################
# ############################################################################################################
