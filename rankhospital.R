rankhospital=function(state, outcome, num = "best"){
#This function takes three inputs. The state is which state you would like to
#find the rank in, the outcome is one of three different causes of mortality that
#were studied. num is the rank that is looked for. It can take on the values of
#"best", which will find the best hospital in the state for the given outcome, 
#"worst", which will find the worst hospital, or a numeric value which will
#give you the hospital for a certain rank.
    
data  <- read.csv("outcome-of-care-measures.csv", colClasses="character", 
                      na.strings="Not Available", stringsAsFactors = FALSE )
state01 <- data$State
#state01 is a subset of the total data by state
state_sub <- state01[state01==state]
#state_sub will give the subset of state01 that has states that match the one
#given in the function call. fortunately, the length of this vector also corresponds
#to the number of hospitals in that state. so an if statement easily test to see if 
#the rank is too high.


    if   (length(state_sub) == 0){ 
        stop("invalid state")  }
    #This part checks to see if the state entered was valid or not.
testout <- c("heart attack", "heart failure", "pneumonia")
testout2 <- grep(outcome,testout)
# this part compares the outcome to a character vector that contains all the possible
#strings for the outcome input. If there is a match then output testout2 will return
#the index of where the match occured, if there was no match, then the vector will
#be empty.
    if (length(testout2) == 0) {
        stop("invalid outcome")
#Thus we can test whether there was a valid outcome by comparing the length of the
#vector. 0 being an invalid outcome.
    }
    
#If a valid outcome was inputed, then we go ahead to this else statement which
#will run the function depending on input, num.
    else{
        if (num == "best"){
           first <-  best(state,outcome)
           return(first) 
#If the input was the string "best", then it will run the best.R function which
#finds the best hospital.
        }

        else if (num == "worst"){
           first <-  worst(state,outcome)
           return(first) 
# If the input was the string "worst", then it will run worst.R, which finds
#the worst hospital, aka the hospital with the highest mortality for a given outcome.
        }
        
        else if(length(state_sub) < num){
            output <- NA}
#This conditional occurs if the rank is greater than the number of hospitals in the
#state, which of course makes the result meaningless and prints out NA.
        else{
            match <- state01 == state
#This conditional will occur if the num imput is a numeric instead of a string.
#it will run the bulk of the function, giving a rank based on the outcome provided.
                if (outcome == "heart attack"){
                    data1 <- data[,c(2,7,11)]
                    data_by_state <- data1[match,]
                    numV <- as.numeric(data_by_state[,3])
                    #we must change column 3, the mortality rates, to numeric
                    #because since it is a character vector, it won't sort by
                    #numeric logic.
                    idx <- order(numV,data_by_state$Hospital.Name, decreasing=FALSE)
                    #This gives a sorted index of the data by the mortality rate
                    #also, it breaks ties by alphabetical order. That is what the
                    #argument data_by_state$Hospital.Name does.
                    data_sorted <- data_by_state[idx,]
                    #The index is used to sort the data by rank.
                    output <- data_sorted[num,1]
                    #The data can now be subsetted by rank which is determined by
                    #the user input, num.
                    }
            
            if (outcome == "heart failure"){
                    data1 <- data[,c(2,7,17)]
                    data_by_state <- data1[match,]
                    numV <- as.numeric(data_by_state[,3])
                    idx <- order(numV,data_by_state$Hospital.Name, decreasing=FALSE)
                    #This gives a sorted index of the data by the mortality rate
                    #also, it breaks ties by alphabetical order. That is what the
                    #argument data_by_state$Hospital.Name does.
                    data_sorted <- data_by_state[idx,]
                    #The index is used to sort the data by rank.
                    output <- data_sorted[num,1]
                }
                if (outcome == "pneumonia"){
                    data1 <- data[,c(2,7,23)]
                    data_by_state <- data1[match,]
                    numV <- as.numeric(data_by_state[,3])
                    idx <- order(numV,data_by_state$Hospital.Name, decreasing=FALSE)
                    #This gives a sorted index of the data by the mortality rate
                    #also, it breaks ties by alphabetical order. That is what the
                    #argument data_by_state$Hospital.Name does.
                    data_sorted <- data_by_state[idx,]
                    #The index is used to sort the data by rank.
                    output <- data_sorted[num,1]
                }
                }
         output
        }

        
            
            
            
        }
