base <- "/Users/dutishan/Desktop/NSE/"
eq.nse.list <- paste0(base,"EQUITY_L.csv")
list <- read.csv(eq.nse.list, header = T, colClasses = "character")
for(i in 10:56) {#1566
  url=paste0("http://www.google.com/finance/getprices?q=", list$SYMBOL[i], "&x=NSE&i=300&f=d%2Cc%2Ch%2Cl%2Co%2Cv&p=60d")
  download.file(url, paste0(base,list$SYMBOL[i],".csv"))
}




library(XML)
library(stringr)
url <- paste0(base,"table_all.html")
tables <- readHTMLTable(url)
  my.table <- tables[[12]]
  my.table <- my.table[-c(1, 2,3, 4), ]
  #my.table <- my.table[-is.na(my.table$Call.Date), ]
  names(my.table) <- c("Stock","Call.Date","Call.Rate","Target.Rate","Stoploss","Result","Paid?")
  full.table <- rbind(full.table,my.table)
