corr <- function(directory, threshold = 0) {
        
        dr <- directory
        
        tb <- complete(dr)
        
        gids <- c()
        
        for(n in tb[["id"]]) {
                if(tb$nobs[n] > threshold) {
                        gids <- c(gids, n)
                }
        }
        
        cv <- c()
        
        if(length(gids) > 0) {
                for(num in gids) {
                        if(num < 10) {
                                table <- read.csv(paste(dr, "/00", num, ".csv", sep=""))
                                cr <- cor(table$sulfate, table$nitrate, use="pairwise.complete.obs")
                                cv <- c(cv, cr)
                        } else if(num < 100) {
                                table <- read.csv(paste(dr, "/0", num, ".csv", sep=""))
                                cr <- cor(table$sulfate, table$nitrate, use="pairwise.complete.obs")
                                cv <- c(cv, cr)
                        } else {
                                table <- read.csv(paste(dr, "/", num, ".csv", sep=""))
                                cr <- cor(table$sulfate, table$nitrate, use="pairwise.complete.obs")
                                cv <- c(cv, cr)
                        }
                        
                }
        } else {
                cv <- vector('numeric')
        }
        
        cv
}