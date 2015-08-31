setwd("~/Desktop/NZES")
library(rpart)
library(haven)
library(rpart.plot)
ds <- read_spss("NZES2011_Jun_18_rel.sav")
as <- ds
for (i in names(as)){
  as[[i]] <- as.numeric(as[[i]])
}

#keep running rpart, see what's important and decide what to do about it mostly excluding stuff

set <- which(names(as) == "njvoted"
             | names(as) == "njptyvote"
             | names(as) == "njelecvote"
             | names(as) == "jvalid"
             | names(as) == "jrefop"
             | names(as) == "jdidvote"
             | names(as) == "jrefmmp"
             | names(as) == "jrefvotemmp"
             | names(as) == "jtypevote"
             | names(as) == "jptywt"
             | names(as) == "wt2"
             | names(as) == "jfinwt"
             | names(as) == "wt6"
             | names(as) == "edwt"
             | names(as) == "jdecpvote"
             | names(as) == "jdecevote"
             | names(as) == "jrefvoteoth"
             | names(as) == "Jelect"
             | names(as) == "jsecondp"
             | names(as) == "jgofflike"
             | names(as) == "jreligiousity"
             | names(as) == "jyear"
             | names(as) == "wt1"
             | names(as) == "jnatlike"
             | names(as) == "jrelservices"
             | names(as) == "wt3"
             | names(as) == "jkeylike"
             | names(as) == "jtuilike"
             | names(as) == "jage"
             | names(as) == "jlablr"
             | names(as) == "Jsttyr"
             | names(as) == "jslflr"
             | names(as) == "Case11"
             | names(as) == "jsample"
             | names(as) == "jcomp"
             )

ls <- as[, -1*set]

for (i in names(ls)){
  x <- ls[[i]]
  x[x == 99] <- NA
  x[x == 9] <- NA
  ls[[i]] <- x
}

ls$jvoted <- as.factor(ls$jvoted)
levels(ls$jvoted) <- c("Voted", "Not Vote")
fit <- rpart(jvoted ~., data=ls, method="class")
rpart.plot(fit)
text(fit)

#Jelect jgofflike jreligiousity jyear, jnatlike
#str(ds$jreligiousity)
#attributes(ds$jreligiousity)
#####
gd <- as
gd$jvoted <- as.factor(gd$jvoted)
levels(gd$jvoted) <- c("Voted", "Abstained")
g1d <- table(gd$jvoted,gd$jgofflike)

barplot(g1d, col=c("#f89828","darkgrey"), main="Liking Phil Goff vs. casting votes", xlab = "How much you like Phil Goff (99=Don't Care)", legend = rownames(g1d))
sort(g1d[1,]/ (g1d[1,] + g1d[2,]))
#####
ages <- cut(gd$jage, seq(18,106,by=8))
g1d <- table(gd$jvoted,ages)
barplot(g1d, col=c("#f89828","darkgrey"), main="Ages vs. casting votes", xlab = "Age Ranges", legend = rownames(g1d), cex.names=0.6)
######
gd$jreligiousity <- as.factor(gd$jreligiousity)
levels(gd$jreligiousity) <- c("No Religion", "Not Very", "Somewhat", "Very", "Don't Know")
g1d <- table(gd$jvoted,gd$jreligiousity)
barplot(g1d, col=c("#f89828","darkgrey"), main="Religiosity vs. casting votes", xlab = "Religiosity", legend = rownames(g1d), cex.names=0.6)
########
g1d <- table(gd$jvoted,gd$jknow)
barplot(g1d, col=c("#f89828","darkgrey"), main="Political knowledge vs. casting votes", xlab = "Knowledge", legend = rownames(g1d), cex.names=0.6)
perc <- g1d[1,] / (g1d[1,] + g1d[2,])

plot(as.numeric(names(perc)),perc, ylim = c(0,1), type="l", col="#f89828", bty ="n", xlab="Knowledge", ylab="Proportion voting")
points(as.numeric(names(perc)),perc, ylim = c(0,1), pch=19, cex=0.5, col="#f89828")

## str(ds$jduty)
# 
gd$jduty <- as.factor(gd$jduty)
levels(gd$jduty) <- c("Strongly agree", "Agree", "Neither", "Disagree", "Strongly disagree", "Don't Know")
g1d <- table(gd$jvoted,gd$jduty)
barplot(g1d, col=c("#f89828","darkgrey"), main="Duty vs. casting votes", xlab = "voting is a citizen's duty", legend = rownames(g1d), cex.names=0.6)

perc <- g1d[1,] / (g1d[1,] + g1d[2,])

plot(c(1,2,3,4,5,6),perc, ylim = c(0,1), pch=19, cex=0.5, col="#f89828", bty ="n", xlab="voting is a citizen's duty", ylab="Proportion voting", xaxt="n")
lines(c(1,2,3,4,5),perc[1:5], ylim = c(0,1), col="#f89828")
text(c(1,2,3,4,5,6),perc - 0.12, labels= names(perc), pos=1, srt=90, cex=0.7)

#str(ds$jdiffvoting)
#attributes(ds$jdiffvoting)$labels
gd$jdiffvoting <- as.factor(gd$jdiffvoting)
levels(gd$jdiffvoting) <- c("Voting won't make\nany difference", "Voting won't make\nmuch difference", "Voting can make\nsome difference", "Voting can make a\nreasonable\ndifference", "Voting can make\na big difference", "Don't Know")
g1d <- table(gd$jvoted,gd$jdiffvoting)
barplot(g1d, col=c("#f89828","darkgrey"), main="Voting matters vs. casting votes", xlab = "does voting make a difference", legend = rownames(g1d), cex.names=0.6)

perc <- g1d[1,] / (g1d[1,] + g1d[2,])

plot(c(1,2,3,4,5,6),perc, ylim = c(0,1), pch=19, cex=0.5, col="#f89828", bty ="n", xlab="does voting make a difference", ylab="Proportion voting", xaxt="n")
lines(c(1,2,3,4,5),perc[1:5], ylim = c(0,1), col="#f89828")
text(c(1,2,3,4,5,6),perc - 0.12, labels= names(perc), pos=1, srt=90, cex=0.7)

#str(ds$jmpspacific)
gd$jmpspacific <- as.factor(gd$jmpspacific)
levels(gd$jmpspacific) <- c("More", "Same\nas now", "Fewer", "Depends on\ncandidate", "Don't Know")
g1d <- table(gd$jvoted,gd$jmpspacific)
barplot(g1d, col=c("#f89828","darkgrey"), main="Opinions about Pacific MPs vs. casting votes", xlab = "how many Pacific MPs", legend = rownames(g1d), cex.names=0.6)

perc <- g1d[1,] / (g1d[1,] + g1d[2,])

plot(c(1,2,3,4,5),perc, ylim = c(0,1), pch=19, cex=0.5, col="#f89828", bty ="n", xlab="how many Pacific MPs", ylab="Proportion voting", xaxt="n")
lines(c(1,2,3),perc[1:3], ylim = c(0,1), col="#f89828")
text(c(1,2,3,4,5),perc - 0.12, labels= names(perc), pos=1, srt=90, cex=0.7)

