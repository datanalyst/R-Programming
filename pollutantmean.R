## Part 1

# Set your working directory
#setwd("")

pollutantmean <- function(directory, pollutant, id = 1:332) {

            #set the directory
            path <- getwd()
             
            #get the file List in that directory
            fileList <- list.files(path)

            #extract the file names and store as numeric for comparison
	    file.names <- as.numeric(sub("\\.csv$","",fileList))

            #select files to be imported based on the user input or default
            selected.files <- na.omit(fileList[match(id, file.names)])
           
            #import data (it is a list)
            data <- lapply(file.path(path,selected.files),read.csv,header=TRUE)

            #convert into data frame
            data <- do.call(rbind.data.frame,data)

            #create a subset by ID =id
            subid <- subset(data, ID = id)
                       
            #compute the mean
            mean(subid[, pollutant],na.rm = TRUE) 
  	 
}

# Run tests
pollutantmean("specdata", "nitrate", 23)
## [1] 1.280833

pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047

pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064128

