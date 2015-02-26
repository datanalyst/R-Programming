complete <- function(directory, id = 1:332) {
        #set the directory
        path <- getwd()
        
        #set variable noobs
        nobs = numeric(0)
        for(i in id)  {
                
                csv <- read.csv(paste(path,"/", formatC(i, width = 3, flag = "0"),".csv",sep=""))                                        
                nobs = c(nobs, sum(!is.na(csv$sulfate) & !is.na(csv$nitrate)))    
        }
        
        data.frame(id=id, nobs=nobs) 

}
#source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
#submit()


