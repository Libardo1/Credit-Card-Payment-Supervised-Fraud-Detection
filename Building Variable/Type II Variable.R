

load("Card_Payments_Cleaned.rda")

library(data.table)
library(dplyr)

data = as.data.table(data)


############
#Transaction Frequency:

lagcum = function(dt,n){
  dt1 = dt
  dt[,join_date:=(date-n)]
  dt1$join_date = dt1$date
  key1 = colnames(dt)[1]
  key2 = colnames(dt)[5]
  setkeyv(dt,c(key1,key2))
  setkeyv(dt1,c(key1,key2))
  dt2 = dt1[dt,roll=T,rollends=c(T,T)]
  return (dt2)
}

cname = function(name) c('date', paste0(name,'Daily'), paste0(name,'Cum'))

##requency of transaction - merchant 
daily_merch = data[,.(daily_merch=.N),by=.(date,merchnum)]
daily_merch=daily_merch[,.(date,daily_merch,cum_merch=cumsum(daily_merch)),by=merchnum]

merch = lagcum(daily_merch,3)
merch$merchLag3Cum = ifelse(merch$date>merch$join_date,merch$i.cum_merch,merch$i.cum_merch-merch$cum_merch)
merch = merch[,c(1,6,7,8,9)]
colnames(merch)[2:4]=cname('merch')

merch2 = lagcum(daily_merch,7)
merch$merchLag7Cum = ifelse(merch2$date>merch2$join_date,merch2$i.cum_merch,merch2$i.cum_merch-merch2$cum_merch)

merch2 = lagcum(daily_merch,14)
merch$merchLag14Cum = ifelse(merch2$date>merch2$join_date,merch2$i.cum_merch,merch2$i.cum_merch-merch2$cum_merch)

merch2 = lagcum(daily_merch,28)
merch$merchLag28Cum = ifelse(merch2$date>merch2$join_date,merch2$i.cum_merch,merch2$i.cum_merch-merch2$cum_merch)

##Frequency of transaction - card number

daily_card = data[,.(daily_card=.N),by=.(date,cardnum)]
daily_card=daily_card[,.(date,daily_card,cum_card=cumsum(daily_card)),by=cardnum]

card = lagcum(daily_card,3)
card$cardLag3Cum = ifelse(card$date>card$join_date,card$i.cum_card,card$i.cum_card-card$cum_card)
card = card[,c(1,6,7,8,9)]
colnames(card)[2:4]=cname('card')

card2 = lagcum(daily_card,7)
card$cardLag7Cum = ifelse(card2$date>card2$join_date,card2$i.cum_card,card2$i.cum_card-card2$cum_card)

card2 = lagcum(daily_card,14)
card$cardLag14Cum = ifelse(card2$date>card2$join_date,card2$i.cum_card,card2$i.cum_card-card2$cum_card)

card2 = lagcum(daily_card,28)
card$cardLag28Cum = ifelse(card2$date>card2$join_date,card2$i.cum_card,card2$i.cum_card-card2$cum_card)

##########
rm(merch2)
rm(daily_merch)

rm(card2)
rm(daily_card)
##########

data = left_join(data,merch[,-4],by=c('date'='date','merchnum'='merchnum'))
data = left_join(data,card[,-4],by=c('date'='date','cardnum'='cardnum'))

data_b = data[-c(1:10)]

save(data_b, file = "data_type_b.Rdata")
