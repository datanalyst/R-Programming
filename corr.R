corr <- function(directory, threshold = 0) {
        
        path <- getwd()
        source("complete.R")
        
        com <- complete(path)
        data <- com[com$nobs > threshold, ]
        result <- numeric(0)
        
        for (id in data$id) {
                csv <- read.csv(paste(path,"/", sprintf("%03d", id), ".csv",sep=""))
                tf <- !is.na(csv$sulfate) & !is.na(csv$nitrate)
                x <- csv[tf, ]
                result <- c(result, cor(x$sulfate, x$nitrate))        
        }
           
        result
}


