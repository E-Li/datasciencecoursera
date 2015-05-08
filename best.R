best <- function(state, outcome){

#This function takes two inputs: state and outcome. The state is a two letter 
#character of state that you want to look at. The outcome is the three types of 
#diseases that are included in the data. The function will find the best hospital
#in the state based on the lowest 30 day mortality rates for an outcome.
#If a tie takes place, the function will alphabetize it and print out the first 
#result.
    
data  <- read.csv("outcome-of-care-measures.csv", colClasses="character", 
                  na.strings="Not Available", stringsAsFactors = FALSE )
##The data variable just represents the raw data that is read. colClasses is defined
#to speed up the read. The other arguments just turn the NA into Not Available and 
#sets the strings as factors to false.

state01 <- data$State
state_sub <- state01[state01==state]
#This part subsets the states column of the dataset. state_sub represents the 
#number of entries that match the state given.

 if   (length(state_sub) == 0){ 
    stop("invalid state")  }
#This if statement is a check for valid state entries. If a state is not in the
#dataset, then the length of state_sub will be 0, and thus the error message
#is delivered.
match <- state01 == state

#match is the index of the dataset that match the state provided.
  if (outcome=="heart attack"){
    data1 <- data[,c(2,7,11)]
    data_by_state <- data1[match,]
    
    minVal <- min(as.numeric(data_by_state[,3]),na.rm=TRUE)
    minIndex <- data_by_state[,3] == format(minVal, nsmall=1)
    bestH <- data_by_state[minIndex,1]
    bestH <- bestH[!is.na(bestH)]
#This is the script for when the outcome is heart attack. All the scripts are same
#except for a few variable changes, otherwise they work exactly the same

#The function will subset the name column, the state column and the 30 day mortality
#rate. The mortality rate subset number changes depending on the outcome.
#The minVal is the minimum in the mortality rate. This has to be changed to a numerical
#vector because you can't find the min of a character vector.
#minIndex is the index of data1 which corresponds to the minimum. Need to use
#format because if the number is an integer, it will leave off the decimal point
#which breaks the code. bestH is a vector with all the best hopsitals.
}

  else if (outcome == "heart failure"){
    data1 <- data[,c(2,7,17)]
    data_by_state <- data1[match,]
    
    minVal <- min(as.numeric(data_by_state[,3]),na.rm=TRUE)
    minIndex <- data_by_state[,3] == format(minVal, nsmall=1)
    bestH <- data_by_state[minIndex,1]
    bestH <- bestH[!is.na(bestH)]
  }

  else if (outcome == "pneumonia"){
    data1 <- data[,c(2,7,23)]
    data_by_state <- data1[match,]
    
    minVal <- min(as.numeric(data_by_state[,3]),na.rm=TRUE)
    minIndex <- data_by_state[,3] == format(minVal, nsmall=1)
    bestH <- data_by_state[minIndex,1]
    bestH <- bestH[!is.na(bestH)]
  }
else{
  stop("invalid outcome")
}
#This else statement will occur if the outcome does not match any that are found
#in the dataset.

Alphabetical <- sort(bestH)
first <- Alphabetical[1]
#This part sorts and alphabeticatizes the hospitals in the case where there are
#multiple hospitals with the same mortality rate.
#first will print out the first element which should be the first one that comes
#up in the alphabet.
first


}