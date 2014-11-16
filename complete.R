complete <- function(directory, id = 1:332) {
        path <- paste(getwd(),"/", directory, sep="")
        fileList <- list.files(path=path,pattern="\\.csv$",full.names=F)
        fileNumbers <- as.numeric(sub('\\.csv$','', fileList))
        selectedFiles <- na.omit(fileList[match(id, fileNumbers)])
        
        all.files.data = lapply(file.path(directory,selectedFiles),
                                read.csv,header=TRUE)
        DATA <- do.call("rbind",all.files.data)
        
        goodDATA <- DATA[complete.cases(DATA), ]
        
        result <- NULL
        result <- data.frame(id = numeric(), nobs = numeric())
        for(i in id) {
                fileLen <- nrow(subset(goodDATA, goodDATA$ID==i))
                result <- rbind(result, data.frame(id = i, nobs = fileLen))
                }
        
        result
     

        
        
}
