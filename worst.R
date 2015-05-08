worst <- function(state, outcome){
    
data  <- read.csv("outcome-of-care-measures.csv", colClasses="character", 
                     na.strings="Not Available", stringsAsFactors = FALSE )    
    
    
    
state01 <- data$State
state_sub <- state01[state01==state]
if   (length(state_sub) == 0){ 
    stop("invalid state")  }

match <- state01 == state


if (outcome=="heart attack"){
    data1 <- data[,c(2,7,11)]
    data_by_state <- data1[match,]
    
    maxVal <- max(as.numeric(data_by_state[,3]),na.rm=TRUE)
    maxIndex <- data_by_state[,3] == format(maxVal, nsmall=1)
    bestH <- data_by_state[maxIndex,1]
    bestH <- bestH[!is.na(bestH)]
   
}

else if (outcome == "heart failure"){
    data1 <- data[,c(2,7,17)]
    data_by_state <- data1[match,]
    
    maxVal <- max(as.numeric(data_by_state[,3]),na.rm=TRUE)
    maxIndex <- data_by_state[,3] == format(maxVal, nsmall=1)
    bestH <- data_by_state[maxIndex,1]
    bestH <- bestH[!is.na(bestH)]
}

else if (outcome == "pneumonia"){
    data1 <- data[,c(2,7,23)]
    data_by_state <- data1[match,]
    
    maxVal <- max(as.numeric(data_by_state[,3]),na.rm=TRUE)
    maxIndex <- data_by_state[,3] == format(maxVal, nsmall=1)
    bestH <- data_by_state[maxIndex,1]
    bestH <- bestH[!is.na(bestH)]
}
else{
    stop("invalid outcome")
}

Alphabetical <- sort(bestH)
first <- Alphabetical[1]

first



    
    
}