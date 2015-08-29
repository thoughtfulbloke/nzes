if(!require(haven)){
  install.packages("haven")
  require(haven)
}

fileLocation <- file.choose()
ds <- read_spss(fileLocation)
##this function is needed as there are missing value labels so can't as_factor
applyValLabels <- function(column){
  knownvalues <- attributes(column)$labels
  valuelabels <- names(knownvalues)
  lookup <- data.frame(column = as.character(knownvalues), valuelabels, stringsAsFactors = FALSE)
  target <- data.frame(column = as.character(column), index=1:length(column), stringsAsFactors = FALSE)
  combined <- merge(target, lookup, all.x=TRUE)
  combined <- combined[order(combined$index),]
  output <- as.character(column)
  output[!(is.na(combined$valuelabels))] <- combined$valuelabels[!(is.na(combined$valuelabels))]
  return(output)
}
newfile <- paste(fileLocation,".csv", sep="")
for (eachName in names(ds)){
  ds[[eachName]] <- applyValLabels(ds[[eachName]])
}

write.csv(ds, file=newfile, row.names = FALSE)
