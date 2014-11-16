pollutantmean <- function(directory, pollutant, id = 1:332) {
        path <- paste(getwd(),"/", directory, sep="")
        fileList <- list.files(path=path,pattern="\\.csv$",full.names=F)
        fileNumbers <- as.numeric(sub('\\.csv$','', fileList))
        selectedFiles <- na.omit(fileList[match(id, fileNumbers)])
        selected.dfs <- lapply(file.path(directory,selectedFiles), read.csv)
        all.files.data = lapply(file.path(directory,selectedFiles),
                                read.csv,header=TRUE)
        DATA = do.call("rbind",all.files.data)
     
        pol <- DATA[ ,pollutant]
        bad <- is.na(pol)
        result <- mean(pol[!bad])
        result


}