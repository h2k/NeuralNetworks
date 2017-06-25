#-----------------------------------------------------------------
# Create the activationList and  for all input X
#
# Args:
#   InputParrameters: List of input (observations) X 
#
#   Wieghts: List of all the wieghts for each input of X
#
# Returns:
#   activationList: 
#-----------------------------------------------------------------
forwardPropagation <- function(InputParrameters , Wieghts){
  forwardList<- list()
  zList<- list()
  aList<- list()

  for(i in 1:(hiddenLayerLevels+1))
  {
    if(i == 1)
    {
      z2<- InputParrameters[i,] %*% Wieghts[[i]]
      a2<- Sigmoid(z2)
      zList[[i]]<- z2
      aList[[i]]<- a2
    }
    else
    {
      Zs = aList[[i-1]] %*% Wieghts[[i]]
      As = Sigmoid(Zs)

      zList<-list(zList, list(Zs))
      aList<-list(aList, list(As))
    }
  }

  print("---------------------Wieghts-----------------------")
  print(aList[[2]][[1]])

  forwardList <- list( zList=zList, aList=aList)
  activationList <- forwardList
  return(activationList)
}
