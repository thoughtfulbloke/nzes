setwd("~/Desktop/NZES")

library(haven)
ds <- read_spss("NZES2011_Jun_18_rel.sav")
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
intery <- ds[,c(235,140:147)]
intery$jpartyvote <- applyValLabels(intery$jpartyvote)
for (i in names(intery[2:ncol(intery)])){
  intery[[i]] <- as.integer(intery[[i]])
}

library(tidyr)
longForm <- gather(intery, party, liking, -jpartyvote)
longForm$party <- as.factor(longForm$party)
levels(longForm$party) <- c("Labour", "National", "Green", "NZ First", "Act", "United Future", "Maori Party", "Mana Party")
library(dplyr)
longForm %>% filter(liking != 99) %>% group_by(jpartyvote, party) %>% summarise(meanliking = mean(liking), likingsd = sd(liking)) -> agLong


aggregate(meanliking ~ jpartyvote, data=agLong[c(37, 59, 65, 79, 84, 90, 102),], mean)

aggregate(meanliking ~ jpartyvote, data=agLong[-1 * c(37, 59, 65, 79, 84, 90, 102),], mean)
