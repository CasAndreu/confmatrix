confmatrix <- function(docsum = analytics@document_summary){
  labels <- c(as.character(names(table(docsum$MANUAL_CODE))))
  topicn <- length(table(docsum$MANUAL_CODE))
  mat <- as.data.frame(matrix(nrow= 2 + topicn, ncol= 2 + topicn))
  colnames(mat) <- c(labels,'n','Pct Right')
  rownames(mat) <- colnames(mat)
  mat$n <- c(table(docsum$MANUAL_CODE),'','')
  machineT <- NULL
  man <- sort(unique(docsum$MANUAL_CODE))
  mach <- sort(unique(docsum$CONSENSUS_CODE))
  counter <- 1
  for (i in 1:length(man)) {
    if (man[i] != mach[counter]) {
      machineT <- c(machineT,0)
    } else if (counter < length(mach)){
      machineT <- c(machineT,table(docsum$CONSENSUS_CODE)[counter])
      counter <- counter + 1
    } else {
      machineT <- c(machineT,table(docsum$CONSENSUS_CODE)[counter])
    }
  }
  mat[length(mat[,1])-1,] <- c(machineT,'','')

  mat[length(mat[,1]),] <- c(rep('',length(mat[1,])))
  for (i in 1:length(labels)){
    for (j in 1:length(labels)){
      mat[j,i] <- length(docsum$CONSENSUS_CODE[docsum$CONSENSUS_CODE==as.integer(labels[i]) 
                                               & docsum$MANUAL_CODE==as.integer(labels[j])])
    }
    mat[i,length(mat[1,])] <- round(as.integer(mat[i,i])/as.integer(mat$n[i]),2)
    mat[length(mat[,1]),i] <- round(as.integer(mat[i,i])/sum(as.integer(mat[1:(length(mat[,1])-2),i]),na.rm=T),2)
  }
  mat[length(mat[,1]),which(mat[length(mat[,1]),]==NaN)] <- 0
  return(mat)
}
