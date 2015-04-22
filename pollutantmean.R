pollutantmean <- function(directory, pollutant, id = 1:332) {
        
                dr <- directory
                
                poll <- c()
                
                for(num in id) {
                        if(num < 10) {
                                table <- read.csv(paste(dr, "/00", num, ".csv", sep=""))
                        } else if(num < 100) {
                                table <- read.csv(paste(dr, "/0", num, ".csv", sep=""))
                        } else {
                                table <- read.csv(paste(dr, "/", num, ".csv", sep=""))
                        }
                        
                        poll <- c(poll, table[[pollutant]])
                }
                
                pm <- mean(poll, na.rm=TRUE)
                
                pm
}