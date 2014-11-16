corr <- function(directory, threshold = 0) {
        path <- paste(getwd(),"/", directory, sep="")
        fileList <- list.files(path=path,pattern="\\.csv$",full.names=F)
        fileCount <- length(fileList)

        all.files.data = lapply(file.path(directory,fileList),
                                read.csv,header=TRUE)
        DATA <- do.call("rbind",all.files.data)
        goodDATA <- DATA[complete.cases(DATA), ]

        result <- as.numeric(NULL)
        
        for(i in 1:fileCount)
                {
                fileLen <- nrow(subset(goodDATA, goodDATA$ID==i))
                if (fileLen > threshold)
                        {
                        sset <- subset(goodDATA, goodDATA$ID==i)
                        presult <- cor(sset$sulfate, sset$nitrate)
                        result <- c(result, presult)
                        }         
                }
        
        result

}
