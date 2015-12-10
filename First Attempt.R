pitcherInnings <- read.csv("/Volumes/DAILY/Baseball/Game Logs/Jose.csv")
rows <- nrow(pitcherInnings)
columns <- ncol(pitcherInnings)
reduced <- pitcherInnings[2:rows,2:columns]

#This converts the innings to thirds, i.e 6.1 to 6.333 and 4.2 to 4.666
for (i in 1:(rows-1)){
  if (isTRUE(all.equal(reduced[i,9] %% 1, .1 ))){
    reduced[i,9] <- reduced[i,9] + .233
  }else if(isTRUE(all.equal(reduced[i,9] %% 1,.2))){
    reduced[i,9] <- reduced[i,9] + .466
  }
  else{
    reduced[i,9] <- reduced[i,9]
  }
}

#Here we calculate the season xfip by creating arrays of the inning amounts and the xfip per each game
#To calculate this we take the sum of the product of inning * xfip and divide by the total innings
inning <- reduced[,9]
xfip <- reduced[,26]
x <- inning * xfip
seasonxfip <- sum(x)/sum(inning)
seasonxfip

denom <- 0
numerator <- 0
final <- c()
for (i in 1:(rows-1)){
  numerator <- numerator + (reduced[rows-i,26]*(reduced[rows - i,9]))
  denom <- denom + reduced[rows - i,9]
  maybe <- numerator/(denom)
  final <- append(final,maybe)
}
final
xaxis <- 1:(rows-1)
plot(xaxis,final)

#Here we create an array for strikeouts per game starting with the first game of season, that is why we work from bottom of data frame to top
k <- c()
for (i in 1:(rows-1)){
  strikeouts <- reduced[(rows-i),16]
  k <- append(k,strikeouts)
}
k

plot(final,k)
plot(k,final)


playerERA <- reduced[,24]
seasonERA <- sum(playerERA*inning)/sum(inning)
seasonERA

ERAnum <- 0
ERAden <- 0
ERAperGame <- c()
for (i in 1:(rows-1)){
  ERAnum <- ERAnum + (reduced[rows-i,24]*(reduced[rows - i,9]))
  ERAden <- ERAden + reduced[rows - i,9]
  ERA <- ERAnum/ERAden
  ERAperGame <- append(ERAperGame,ERA)
}
ERAperGame

plot(xaxis, final, type="l", lwd=2, col="blue", ylim=c(1, 15), xaxs="i", yaxs="i")
lines(xaxis,ERAperGame, lwd=2, col="red")
lines(xaxis,k,lwd=2,col="green")
legend("topleft", legend=c("xFip","ERA","K"), lwd=c(2,2), col=c("blue","red","green"))
fit <- lm(k~xaxis)
abline(fit)
otherfit <- lm(final~xaxis)
abline(otherfit)




