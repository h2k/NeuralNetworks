#-----------------------------------------------------------------
# Create the activationList and  for all input X
#
# Args:
#   InputParrameters: List of input (observations) X 
#
#   Wieghts: List of all the wieghts for each input of X
#
# Returns:
#   activationList: List that contain the result of the Sigmoid function 
#-----------------------------------------------------------------

source("./Utilitie.R")
source("./BackwardPropagation.R")
source("./ForwardPropagation.R")


InputParrameters=cbind(c(3, 5, 10),c(5, 1, 2) )
y=c( 75, 82 , 93)

#InputParrameters=cbind(c(1, 0, 1, 0),c(1, 1, 0,0) )
#y=c( 0, 1 , 1 , 0)



#Definig the network
inputLayerSize <- 2
outputLayerSize <- 1
hiddenLayerSize <- 3
hiddenLayerLevels <- 1

#Wieghts (Theta)
W1 <- matrix( rnorm(inputLayerSize*hiddenLayerSize,mean=0,sd=1), inputLayerSize,hiddenLayerSize)
W2 <- matrix( rnorm(hiddenLayerSize*outputLayerSize,mean=0,sd=1), hiddenLayerSize, outputLayerSize)
Wieghts <- list(W1=W1 , W2=W2)

#debug(forwardPropagation)
#debug(OutputSumMarginError)
#debug(backwardPropagation)
#debug(updatingWieghts)
for(i in 1:length(y))
{
    print("----------------Forward Propagation Output--------------------")
    forwardList = forwardPropagation(InputParrameters , Wieghts)
    forwardPropagationOutput = forwardList[["aList"]][length(forwardList)]
    #print(forwardList[["aList"]])
    print(forwardPropagationOutput)

    print("----------------Output Sum Margin Error--------------------")
    OutputSumMarginErrorVar = OutputSumMarginError( y[1] ,   forwardPropagationOutput[[1]][[1]])
    print(sprintf("Output Sum Margin Error: %s", OutputSumMarginErrorVar))

    print("----------------Backward Propagation--------------------")
    backwardList = backwardPropagation( InputParrameters[i,] , Wieghts ,forwardList , OutputSumMarginErrorVar)
    print(backwardList)

    print("----------------Updating Wieghts--------------------")
    Wieghts  = updatingWieghts( Wieghts, backwardList[["dJdW"]] )
    print(Wieghts)
}

