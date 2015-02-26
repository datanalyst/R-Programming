## Part 1

pollutantmean <- function(directory, pollutant ="nitrate", id = 1:332) {

#set the directory
path <- "C:/Users/ISABELLA/Documents/Coursera/Problem Sets/specdata"

#get the file List in that directory
pmean <-numeric(0)
     
for (i in id)  {
  
  csv <- read.csv(paste(path,"/", formatC(i, width = 3, flag = "0"),".csv",sep=""))                                        
  pmean <- c(pmean, mean(csv[,pollutant],na.rm=TRUE))
}      

#convert into data frame
data.frame(id=id,pmean=pmean)      
}

