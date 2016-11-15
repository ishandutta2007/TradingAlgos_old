library(httr)
library(plyr)
library(XML)
library(stringr)
library(reshape2)
library(ggplot2)
library(data.table)
rm(list = ls())

exp.dates <- c("28-01-2016","25-02-2016","31-03-2016","28-04-2016")#,"26-05-2016","30-06-2016")
#exp.dates <- c("31-03-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"
output <- "Consolidated/"

gg.scrip.returns <- data.frame();
get.symbols <- function() {
  url <- paste0("https://www.nseindia.com/content/fo/fo_underlyinglist.htm")
  response <- GET(url, user_agent("Mozilla"))
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables <- readHTMLTable(tf)
  symbols <- as.vector(tables[4]$'NULL'$Symbol)
  return(symbols)# symbols <- c("AJANTPHARM")
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
symbols=get.symbols()#[10:184]
for(expiryDate in exp.dates) {
  #expiryDate = exp.dates[1]
  expiryDate.date <- as.Date(expiryDate, format="%d-%m-%Y")
  musthy.csv <- read.csv(paste0(base,"/","murthy_fraud.csv"))
  musthy.csv$DATE <- as.Date(musthy.csv$DATE, format="%d-%b-%y")
  musthy.csv$STOPLOSS.END <- as.numeric(musthy.csv$STOPLOSS.END)
  for(i in seq(30000,5,-5)) {
    musthy.csv$STRIKE <- gsub(paste0(i,"-PE"), paste0("PE.",i), musthy.csv$STRIKE)
    musthy.csv$STRIKE <- gsub(paste0(i,"-CE"), paste0("CE.",i), musthy.csv$STRIKE)
  }
  musthy.csv$SCRIP <- gsub("-CALL", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" -CALL", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" - CALL", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub("- CALL", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" CALL", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub("-PUT", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" -PUT", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" - PUT", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub("- PUT", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- gsub(" PUT", "", musthy.csv$SCRIP)
  musthy.csv$SCRIP <- trim(musthy.csv$SCRIP)
  
  exp.month = month(expiryDate.date)
  exp.year = year(expiryDate.date)
  musthy.csv.month<- musthy.csv[month(musthy.csv$DATE)==exp.month &  year(musthy.csv$DATE)==exp.year,]
  
  for(scrip in symbols) {
    if(!(scrip %in% c('CADILAHC', 'ONGC', 'MCDOWELL', 'BEML', 'DRREDDY'))){
      #scrip=symbols[36]#"EICHER"#CROMPGREAV"
      scrip="WIPRO"
      tryCatch({
        xyz <- read.csv(paste0(base,output,expiryDate,"/",scrip,"_",expiryDate,".csv"))
        musthy.csv.month.scrip <- musthy.csv.month[musthy.csv.month$SCRIP==scrip,]
        melted.xyz<-melt(xyz, id= "Date")
        melted.xyz.required <- melted.xyz[melted.xyz$variable  %in%  trim(musthy.csv.month.scrip$STRIKE),]
        melted.xyz.required$Date<-as.Date(melted.xyz.required$Date)
        for(row in musthy.csv.month.scrip){
          seach.xyz <- melted.xyz.required[melted.xyz.required$Date>=row$DATE,]
          stoploss.date = which.max(seach.xyz$value<=row$STOPLOSS.END)
          target.date = which.max(seach.xyz$value>=row$TARGETPRICE.START)
          if(stoploss.date==1 & target.date==1){
            row$if.rangebound.expired=seach.xyz$value[]
          }
        }
      }, error = function(error) {
      })
    }
  }
}


