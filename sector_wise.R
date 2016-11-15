library("reshape2")
require(ggplot2)
sector.wise <- read.csv("/Users/dutishan/Desktop/sector-wise.csv") 
sector.wise <- sector.wise[11:15,]
colMax <- function(data) sapply(data, max, na.rm = TRUE)

total.mar.cap<-rowSums(sector.wise[,2:96])
sector.wise.normalized <- sector.wise

avg.sector.size<-colSums(sector.wise[,1:96])/5
max.sector.size<-colMax(sector.wise[,1:96])


sector.wise.normalized[2:96] <- sector.wise[2:96]/total.mar.cap
sector.wise.normalized.shifted <- sector.wise.normalized

for(i in 1:15) {
  for(j in 2:96) {
    sector.wise.normalized.shifted[i, j] <- sector.wise.normalized[i, j] - sector.wise.normalized[1, j]
  }
}

small <- avg.sector.size < 0.005 & max.sector.size < 0.03
small[1] <- T
sector.wise.normalized.shifted.small <- sector.wise.normalized.shifted[,small]
sector.wise.normalized.shifted.small.long <- melt(sector.wise.normalized.shifted.small, id="year")
ggplot(data=sector.wise.normalized.shifted.small.long, aes(x=year, y=value, colour=variable)) +
      geom_line()




big <- avg.sector.size > 1 &  avg.sector.size < 2
big[1] <- T
sector.wise.normalized.shifted.big <- sector.wise.normalized.shifted[,big]
sector.wise.normalized.shifted.big.long <- melt(sector.wise.normalized.shifted.big, id="year")
ggplot(data=sector.wise.normalized.shifted.big.long, aes(x=year, y=value, colour=variable)) +
  geom_line()





#medium <- avg.sector.size < 0.3 && avg.sector.size < 0.05
