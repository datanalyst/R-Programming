# Write a function that reads a directory full of files and reports 
# the number of completely observed cases in each data file. The function 
# should return a data frame where the first column is the name of the file 
# and the second column is the number of complete cases. 

# Set your working directory
#setwd("")

complete <- function(directory, id = 1:332) {
        
        #set the directory
        path <- getwd()
        
        ## 'directory' is a character vector of length 1 indicating the location of
        ## the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers to be used
        
        ## Return a data frame of the form: id nobs 1 117 2 1041 ...  where 'id' is
        ## the monitor ID number and 'nobs' is the number of complete cases
        
        #set variable noobs
        nobs = numeric(0)
        for(i in id)  {
                
                csv <- read.csv(paste(path,"/", formatC(i, width = 3, flag = "0"),".csv",sep=""))                                        
                nobs = c(nobs, sum(!is.na(csv$sulfate) & !is.na(csv$nitrate)))    
        }
        
        data.frame(id=id, nobs=nobs) 

}

# Run tests

complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

complete("specdata", 3)
##   id nobs
## 1  3  243
