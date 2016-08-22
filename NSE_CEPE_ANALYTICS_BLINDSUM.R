library(httr)
library(plyr)
library(XML)
library(stringr)
library(reshape2)
library(ggplot2)
library(data.table)
offset = 2
min.mul.fac = 2
max.mul.fac = 7
positive.beakout.value = 4
negative.beakout.value = -2
exp.dates<-c("28-01-2016","25-02-2016","31-03-2016","28-04-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"
gg.scrip.returns <- data.frame(company=c("scrip"), returns=c(0), buy.day=c(as.Date("2016-01-28")),  sell.day = c(as.Date("2016-01-28")))






get.symbols<-function(){
  url <- paste0("https://www.nseindia.com/content/fo/fo_underlyinglist.htm")
  response <- GET(url, user_agent("Mozilla"))
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables <- readHTMLTable(tf)
  symbols <- as.vector(tables[4]$'NULL'$Symbol)
  return(symbols)# symbols <- c("AJANTPHARM")
}
prepapre.csv.useful <- function(files){
  csv.useful <- data.frame()
  for (i in 1:length(files)) {
    csv_i <- read.csv(paste0(base,expiryDate,"/",files[i]))
    strname = paste0("CE.PE.",csv_i$Strike.Price[1])
    csv_i[strname] <- csv_i$CE.LTP + csv_i$PE.LTP
    if (i == 1) {
      csv.useful <- csv_i[c("Date", "Underlying.Value", strname)]
    } else {
      csv_i_needed <- csv_i[c("Date", "Underlying.Value", strname)]
      csv.useful <- merge(csv.useful, csv_i_needed, by.x=c("Date", "Underlying.Value"), by.y=c("Date", "Underlying.Value"), all = TRUE)
    }
    csv.useful$Date<-as.Date(csv.useful$Date)
    csv.useful<-csv.useful[with(csv.useful, order(Date)), ]
  }
  csv.useful$Value.Change<-c(0,diff(csv.useful$Underlying.Value)/csv.useful$Underlying.Value[-1])*100
  return(csv.useful)
}

get.firsts<- function(files, csv.useful){
  first.index<-list()
  first.index$all.strikes.day <-which.max(complete.cases(csv.useful))
  
  if(csv.useful$Value.Change[1]>=positive.beakout.value) {
    first.index$positive.breakout = 1
  } else {
    first.index$positive.breakout <-which.max(csv.useful$Value.Change>=positive.beakout.value)
    if(first.index$positive.breakout == 1) first.index$positive.breakout = -1
  }
  
  if(csv.useful$Value.Change[1] <= negative.beakout.value) {
    first.index$negative.breakout.index = 1
  } else {
    first.index$negative.breakout <-which.max(csv.useful$Value.Change <= negative.beakout.value)
    if(first.index$negative.breakout == 1)first.index$negative.breakout = -1
  }
  
  csv.useful_long <- melt(csv.useful[ , !(names(csv.useful) %in% c("Underlying.Value"))], id = "Date")
  strikes<-c()
  for(i in 1:length(files)){
    l<-(strsplit(as.character(unique(csv.useful_long$variable)),"E."))
    strikes<-c(strikes,as.vector(l[i])[[1]][3])
  }
  
  first.index$closest.above<-NULL
  for(i in 1:nrow(csv.useful)){
    strike.diff<-as.numeric(strikes)-csv.useful$Underlying.Value[i]
    temp = strike.diff
    temp[strike.diff<0]=NA
    first.index$closest.above <- c(first.index$closest.above, which.min(temp) + offset)
  }
  return(first.index)
}

get.mul.factor <- function(csv.useful, first.index){
  mulfac<-c()
  for (i in 1:length(files)) {
    csv_i <- read.csv(paste0(base,expiryDate,"/",files[i]))
    strname = paste0("CE.PE.",csv_i$Strike.Price[1])
    mulfac <- c(mulfac,csv.useful$Underlying.Value[first.index$all.strikes.day] / csv.useful[first.index$all.strikes.day, strname])
  }
  return(mulfac)
}

normalise.csv.useful <- function(csv.useful, mulfac){
  for (i in 1:length(files)) {
    csv_i <- read.csv(paste0(base,expiryDate,"/",files[i]))
    strname = paste0("CE.PE.",csv_i$Strike.Price[1])
    csv.useful[strname] <- csv.useful[strname] * last(mulfac)
  }
  return(csv.useful)
}

plot.csv <- function(csv.useful_long){
  csv.useful_long <- melt(csv.useful, id = "Date")
  ggplot(data = csv.useful_long,
         aes(x = Date, y = value, group = variable, colour = variable, linetype = variable, size = variable)
  ) +  geom_line()
}

algorithm <- function(scrip, csv.useful, first.index, mulfac) {
  scrip.returns <- data.frame()
  #chose a buy day and  buying the closest above strike option for positive breakout 
  buy.day <- first.index$positive.breakout
  strike.price.chosen.index<- NA
  if(buy.day>0 && min.mul.fac <= mulfac[first.index$closest.above[buy.day]-offset]&&mulfac[first.index$closest.above[buy.day]-offset] <= max.mul.fac) {
    strike.price.chosen.index <- first.index$closest.above[buy.day]
  }else {
    #No positive breakout
    #chose a buy day and  buying the closest below strike option for negative breakout 
    buy.day <- first.index$negative.breakout
    if(buy.day>0 && min.mul.fac <= mulfac[first.index$closest.above[buy.day]-offset-1]&&mulfac[first.index$closest.above[buy.day]-offset-1] <= max.mul.fac) {
      strike.price.chosen.index <- first.index$closest.above[buy.day] -1
    }
  }
  
  if(buy.day > 0 && !is.na(strike.price.chosen.index)){
    buy.at <- NA
    if(16 <= (nrow(csv.useful)-buy.day) && (nrow(csv.useful)-buy.day) <= 30) {
      #Since you are buying next day midday after the breakout
      buy.at <- (csv.useful[buy.day, strike.price.chosen.index]+ csv.useful[buy.day+1, strike.price.chosen.index])/2
    }
    if(!is.na(buy.at)){
      #Sell on monday eod or Tuesday opening
      #Sell X days before expiry midday
      #    X=5#Previous Thusrsday Midday
      #     X=4#Friday Midday
      #     X=3#Monday Midday
      #     X=2#Tuesday Midday
      #     X=1#Wednesday Midday
      X=0#No sell
      for(sell.day in (nrow(csv.useful)-X):(nrow(csv.useful)-X)) {
        sell.at <-(csv.useful[sell.day, strike.price.chosen.index]+csv.useful[sell.day-1*(X!=0), strike.price.chosen.index])/2
        returns = 100 * (sell.at - buy.at) / buy.at
        scrip.returns <- rbind(scrip.returns,data.frame(company=c(scrip),
                                                        returns=c(as.character(returns)), 
                                                        buy.day=c(csv.useful[buy.day,1]), 
                                                        sell.day=c(csv.useful[sell.day,1])))
        #print(returns[[1]][[1]])
      }
    }
  }
  return(scrip.returns);
}





for(expiryDate in exp.dates){
  g.scrip.returns <- data.frame(company=c("scrip"), returns=c(0), buy.day=c(as.Date("2016-01-28")),  sell.day = c(as.Date("2016-01-28")))
  symbols=get.symbols()#[10:184]
  for(s in symbols){
    tryCatch({
      files <- list.files(path = paste0(base,expiryDate,"/"), pattern = s)
      csv.useful <- prepapre.csv.useful(files)
      first.index <- get.firsts(files, csv.useful)
      mulfac <- get.mul.factor(csv.useful, first.index)
      csv.useful <- normalise.csv.useful(csv.useful, mulfac)
      #plot.csv(csv.useful_long)
      g.scrip.returns <- rbind(g.scrip.returns, algorithm(s, csv.useful, first.index, mulfac))
    }, error = function(error) {
    })
  }
  g.scrip.returns = g.scrip.returns[-1,]
  g.scrip.returns$returns<-as.numeric(g.scrip.returns$returns)
  gg.scrip.returns <- rbind(gg.scrip.returns, g.scrip.returns)
  print(paste0(mean(g.scrip.returns$returns), " ", nrow(g.scrip.returns)))
}

gg.scrip.returns = gg.scrip.returns[-1,]
print(paste0(mean(gg.scrip.returns$returns), " ", nrow(gg.scrip.returns)))