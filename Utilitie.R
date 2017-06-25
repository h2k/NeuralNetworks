#-----------------------------------------------------------------
# Initializes a vector with n counts
#
# Args:
#   z: multiplication result list of the Weights and Inputs  
#                      
#
# Returns:
#   Sigmoid: 
#-----------------------------------------------------------------

Sigmoid <- function(z){
  return(1 / (1 + exp(-z)))
}

#-----------------------------------------------------------------
# Initializes a vector with n counts
#
# Args:
#   z: multiplication result list of the Weights and Inputs
#                      
#
# Returns:
#   SigmoidPrime: 
#-----------------------------------------------------------------

SigmoidPrime <- function(z){
  return(exp(-z) / ((1 + exp(-z))^2))
}

#-----------------------------------------------------------------
# Updating the Wieghts of the neural network 
#
# Args:
#   Wieghts: original neural network Wieghts                         
#
#   dJdW: calculated neural network Wieghts
#
# Returns:
#   UpdatedWieghts: 
#-----------------------------------------------------------------

updatingWieghts <- function(Wieghts, dJdW ){
  scalar = 0.3
  print("---------------------Wieghts-----------------------")
  print(Wieghts)
  print("---------------------dJdW-----------------------")
  print(dJdW)

  for(i in 1:length(forwardList[["zList"]]))
  {
    Wieghts[[i]] = Wieghts[[i]] - (scalar*dJdW[[i]])
  }

  UpdatedWieghts = Wieghts

  #print("---------------------Updated Wieghts-----------------------")
  #print(UpdatedWieghts)
  return(UpdatedWieghts)
}


#-----------------------------------------------------------------
# Output sum margin of error
#
# Args:
#   y: output of the target training/ test set 
#                      
#   activationList: output of the calculated training/ test set 
#
# Returns:
#   OutputSumMarginError: Output sum margin of error
#-----------------------------------------------------------------

OutputSumMarginError <- function( y ,   activationList){
  return(y -   activationList)
}