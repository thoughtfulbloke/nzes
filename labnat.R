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
intery <- ds[,c(235,269,162,163, 170)]
intery$jpartyvote <- applyValLabels(intery$jpartyvote)
intery$jlastpvote <- applyValLabels(intery$jlastpvote)
intery$jlablr <- as.integer(intery$jlablr)
intery$jnatlr <- as.integer(intery$jnatlr)
intery$jslflr <- as.integer(intery$jslflr)
nrow(intery) #3101
intery <- intery[(intery$jpartyvote =="Labour" | intery$jpartyvote =="National") & (intery$jlastpvote =="Labour" | intery$jlastpvote =="National") & !(is.na(intery$jpartyvote)) & !(is.na(intery$jlastpvote)),]
nrow(intery) #1587
intery <- intery[(intery$jlablr < 99 & !(is.na(intery$jlablr))) & (intery$jnatlr < 99 & !(is.na(intery$jnatlr))) & (intery$jslflr < 99 & !(is.na(intery$jslflr))),]
nrow(intery) #1114
intery <- intery[intery$jlablr < intery$jnatlr,]
nrow(intery) #986
intery$affil <- "M"
intery$affil[intery$jslflr <= intery$jlablr] <- "L"
intery$affil[intery$jslflr >= intery$jnatlr] <- "N"
prop.table(table(intery$affil))

#once more with greens

gintery <- ds[,c(235,269,163,164, 170)]
gintery$jpartyvote <- applyValLabels(gintery$jpartyvote)
gintery$jlastpvote <- applyValLabels(gintery$jlastpvote)
gintery$jlablr <- as.integer(gintery$jgrnlr)
gintery$jnatlr <- as.integer(gintery$jnatlr)
gintery$jslflr <- as.integer(gintery$jslflr)
nrow(gintery) #3101
gintery <- gintery[(gintery$jpartyvote =="Green" | gintery$jpartyvote =="National") & (gintery$jlastpvote =="Green" | gintery$jlastpvote =="National") & !(is.na(gintery$jpartyvote)) & !(is.na(gintery$jlastpvote)),]
nrow(gintery) #1587
gintery <- gintery[(gintery$jgrnlr < 99 & !(is.na(gintery$jgrnlr))) & (gintery$jnatlr < 99 & !(is.na(gintery$jnatlr))) & (gintery$jslflr < 99 & !(is.na(gintery$jslflr))),]
nrow(gintery) #1114
gintery <- gintery[gintery$jgrnlr < gintery$jnatlr,]
nrow(gintery) #986
gintery$affil <- "teal"
gintery$affil[gintery$jslflr <= gintery$jgrnlr] <- "green"
gintery$affil[gintery$jslflr >= gintery$jnatlr] <- "blue"
prop.table(table(gintery$affil))

barplot(table(intery$jnatlr - intery$jlablr), main="distance from Lab to Nat,\n everyone on labnat axis")
scl <- 10 * 9 * 8 * 7 * 6 #30240
xlow <- -40317.666
xhigh <- 107517.406
intery$self <- (intery$jslflr - intery$jlablr) * (scl / (intery$jnatlr - intery$jlablr))
cutborders <- seq(from= xlow , to=xhigh, by=3359.888)
par(mfrow=c(3,1))
hist(intery$self[intery$jpartyvote == "Labour"], xlim=c(xlow,xhigh), xlab="", main="Labour Voters", col="darkred", ylim=c(0,300), breaks=cutborders)
abline(v=0, col="darkred")
abline(v=30240, col="lightblue")
abline(v=median(intery$self), col="purple")
hist(intery$self[intery$jpartyvote == "National"], xlim=c(xlow,xhigh), xlab="", main="National Voters", col="lightblue", ylim=c(0,300), breaks=cutborders)
abline(v=0, col="darkred")
abline(v=30240, col="lightblue")
abline(v=median(intery$self), col="purple")
hist(intery$self, xlim=c(xlow,xhigh), xlab="", main="Combined LabNat Voters", col="purple", ylim=c(0,300), breaks=cutborders)
abline(v=0, col="darkred")
abline(v=30240, col="lightblue")
abline(v=median(intery$self), col="purple")
par(mfrow=c(1,1))
