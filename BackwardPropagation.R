

#-----------------------------------------------------------------
# Create the errorList (Delta) for all output Y
#
# Args:
#   Y: List of output for the input(observations) X 
#
#   forwardList: List of output for the input(observations) X                     
#
# Returns:
#   backwardPropagationList: list that contain the error result and 
#-----------------------------------------------------------------

backwardPropagation <- function(InputParrameters , Wieghts, forwardList , OutputSumMarginError  ){

  backwardList<- list()
  dalta<- list()
  dJdW<- list()

  for(i in length(forwardList[["zList"]]):1)
  {
    if(i == length(forwardList[["zList"]])) #Last level in the array
    {
      print("--------------------------------------------")
      dalta[[i]] <-  OutputSumMarginError * SigmoidPrime(forwardList[["zList"]][[i]][[1]]) 
      print(sprintf("Delta output sum %f " ,dalta[[i]] , i))
      dJdW[[i]] <- t(forwardList[["aList"]][[i-1]][[1]]) %*% dalta[[i]]
      print(sprintf("Delta output weight %f " ,dJdW[[i]]))
    }
    else 
    {
      print("--------------------------------------------")

      dalta[[i]] <- OutputSumMarginError%*% t(Wieghts[[i+1]])*SigmoidPrime(forwardList[["zList"]][[i]][[1]])

      print(sprintf("Delta hidden sum %f for Level %s" ,dalta[[i]] , i))

      dJdW[[i]] <- InputParrameters %*% dalta[[i]]

      #dJdW[[i]] <- InputParrameters[1,] %*% dalta[[i]]

      print(sprintf("Delta hidden weight %f" ,dJdW[[i]]))
    }
  }

    #########################################

    #adding more hidden layers here will make a deep network

    #########################################

    return(list(dJdW=dJdW,dalta=dalta))
}