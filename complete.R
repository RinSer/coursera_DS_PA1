complete <- function(directory, id = 1:332) {
        
        dr <- directory
        
        idn <- c()
        cc <- c()
        
        for(num in id) {
                
                if(num < 10) {
                        table <- read.csv(paste(dr, "/00", num, ".csv", sep=""))
                        idn <- c(idn, num)
                        cc <- c(cc, sum(complete.cases(table)))
                } else if(num < 100) {
                        table <- read.csv(paste(dr, "/0", num, ".csv", sep=""))
                        idn <- c(idn, num)
                        cc <- c(cc, sum(complete.cases(table)))
                } else {
                        table <- read.csv(paste(dr, "/", num, ".csv", sep=""))
                        idn <- c(idn, num)
                        cc <- c(cc, sum(complete.cases(table)))
                }
        }
        
        df <- data.frame(id = idn, nobs = cc)
        
        df
}