rankall <- function(outcome, num="best"){ #This function reads a data table with
#a bunch of data about hospitals and ranks them based on three outcomes. The mortality
#rate of heart attack, heart failure, and pneumonia. It will give the rank of hospital
#in each state that there is data for.

data  <- read.csv("outcome-of-care-measures.csv", colClasses="character", 
                     na.strings="Not Available", stringsAsFactors = FALSE )
    
#This part below checks that the outcome is valid
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

else{
    if(outcome=="heart attack"){
        data_sub <- data[,c(2,7,11)] #This subsets the data.frame with all the values important
#to this function, which is Hospital.Name, the state, and the outcome.
    }
    else if(outcome=="heart failure"){
        data_sub <- data[,c(2,7,17)]}#This is the data subset for heart failure.
    else{
        data_sub <- data[,c(2,7,23)] #This is the data subset for pneumonia.
    }

numer <- as.numeric(data_sub[,3])#Have to convert characters to numeric in order to sort

        if(num =="best"){
idx <- order(numer,data_sub$Hospital.Name) #orders the data based on lowest number
#for the outcome therefore best hospital is at the top. To break ties, it uses 
#alphabetical order for hospital names.
num <- 1 #have to set num = 1, so that the function can extract the top item
#in each list.
        }
        
        else if(class(num)=="numeric"){
        idx <- order(numer,data_sub$Hospital.Name)            
            
        }

        else{
            
idx <- order(numer,data_sub$Hospital.Name, decreasing=TRUE)
#have to set decreasing=TRUE
#for the "worst" case.
num <- 1  #have to set num = 1 so that the function can extract the top item
#in each list.
        }

data_sorted <- data_sub[idx,] #This uses the index from above, and sorts the 
#actually data.
data_split <- split(data_sorted, data_sorted$State) #This splits the data by state
namestate <- lapply(data_split, "[", c(1,2)) #This operates on all data.frames in
#the list. It will take only the hospital name and state columns since outcome is
#unimportant now that it has already been sorted.
Hnames <- lapply(namestate, "[", num, 1) #This extracts the nth row of the first 
#column which is the hospital name, this giving the name provided a rank. 
state1  <- names(namestate) #This will get the second part of the output column
#the names of the states. This works because the names of the data.frame are also
#the states.
output <- cbind(Hnames, state1) #This binds the two columns together
output <- as.data.frame(output) #This converts the data back into a data.frame
colnames(output) <- c("hospital", "state") #This changes the column names to the
#correct ones.
    
output    
    
        


}  
    
    
    
}
    