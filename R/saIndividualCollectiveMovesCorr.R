#simulated annealing algorithm according to Guimera & Amaral. Corrected Version.
saIndividualCollectiveMovesCorr <- function(adjacency,numRandom=0,initial=c("general","own"),beta=length(adjacency[1,])/2,alpha=1.005,fixed=25,numIter=1.0){

  initial <- match.arg(initial)

  if(initial=="own"){
    C_initial <- adjacency[,length(adjacency[1,])]
    adjacency <- adjacency[,-length(adjacency[1,])]
  }
  else{
    C_initial <- seq(0,0,length.out=length(adjacency[,1]))
  }

  network <- graph.adjacency(adjacency,mode="undirected",weighted=TRUE)
  res <- callSaIndividualCollectiveMoves(network,initialC=C_initial,beta=beta,alpha=alpha,fixed=fixed,numIter=numIter)
  res[vcount(network)+1] <- round(res[vcount(network)+1],2)

  randomResults <- NULL
  if(numRandom!=0){
    for(i in 1:numRandom){
      rndNetwork <- calculateRandomGraph(network)
      rndResult <- callSaIndividualCollectiveMoves(rndNetwork,
                                                   initialC=seq(0,0,length.out=vcount(rndNetwork)),
                                                   beta=beta,alpha=alpha,fixed=fixed,numIter=numIter)
      rndResult[vcount(rndNetwork)+1] <- round(rndResult[vcount(rndNetwork)+1],2)
      randomResults <- rbind(randomResults,rndResult[vcount(rndNetwork)+1])
    }
    result <- generateOutput(res,randomResults,random=TRUE)
  }
  else{
    result <- generateOutput(res)
  }

  return(result)
}
