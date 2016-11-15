#squareoff.in
#15March:
-7505
#16March:
+7525+7514
#17March:
-7490
#21March:
7646
#22March:
-7682-7667+7692
#29March:
-7671
#30March
+7664+7775
#1April
-7715-7715+7752

date <- c(15,16,16,17,21,22,22,22,29,30,30,31,31,31)
price <- c(7505,7525,7514,7490,7646,7682,7667,7692,7671,7664,7775,7715,7715,7752)
type <- c("short","long","long","short","long","short","short","long","short","long","long","short","short","long")
df <- data.frame(date = date, price = price, type = type)

require(ggplot2)
ggplot(df, aes(x=date, y=price, colour=type)) +
  geom_point()+
  geom_line()

