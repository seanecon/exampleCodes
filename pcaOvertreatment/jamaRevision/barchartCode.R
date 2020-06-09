Period = c(1,1,2,2,3,3,4,4)                              
Sample = c("A","B","A","B","A","B","A","B")
Value1 = c(3,2,6,7,3,2,1,2)
Value2 = c(1,0,5,2,2,0,2,5)
x <- data.frame(Period,Sample,Value1,Value2)

v <- rbind(Value1,Value2)
barplot(v,beside=FALSE,names=levels(interaction(Period,Sample)),legend=TRUE)

x <- data.frame(
  Period = c(1,1,2,2,3,3,4,4),
  Sample = c("A","B","A","B","A","B","A","B"),
  Value1 = c(3,2,6,7,3,2,1,2),
  Value2 = c(1,0,5,2,2,0,2,5)
)

mx <- melt(x, id.vars=1:2)
ggplot(mx, aes(x=Period, y=value, fill=variable)) + 
  geom_bar(stat="identity")+scale_fill_manual(values = c("black", "grey80",'grey50'))
 + facet_grid(~Sample)


x <- data.frame(
  Period = c('lowsurvival','lowsurvival','midsurv','midsurv','highsurv','highsurv','junk','junk'),
  Sample = c("A","B","A","B","A","B","A","B"),
  Value1 = c(3,2,6,7,3,2,1,2),
  Value2 = c(1,0,5,2,2,5,2,5)
  
)



mx <- melt(subset(x,!Period=='junk'), id.vars=c('Period','Sample'))
ggplot(mx, aes(x=Period, y=value, fill=variable)) + 
  geom_bar(stat="identity") +scale_fill_manual(values = c("black", "grey80",'grey50'))+
  facet_grid(~Sample)+theme_bw() 
