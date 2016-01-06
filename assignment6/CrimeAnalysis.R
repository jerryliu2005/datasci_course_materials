
san<-read.csv("sanfrancisco_incidents_summer_2014.csv", head=T, sep=",",stringsAsFactors = F)




#plot(table(by_hour[Category='BURGLARY',]$Hour), type='l')


## Pick top 10 category
san$Category<-factor(san$Category)
count_by_category<-as.data.frame(table(san$Category))
names(count_by_category)<-c('Category', 'Count')
top_10_crimes_count<-head(count_by_category[order(-count_by_category[,2]),], n=10)


## by month
month<-strftime(strptime(san$Date, "%m/%d/%Y"), "%b")
month=factor(month, levels=c('Jun', 'Jul', 'Aug'))
by_month<-data.frame(Category=san$Category, Month=month)
top_10_crimes_by_month<-by_month[Category %in% top_10_crimes_count$Category,]
top_10_crimes_by_month$Category<-as.character(top_10_crimes_by_month$Category)
ggplot(data=data.frame(table(top_10_crimes_by_month)), aes(x=Month, y=Freq, group=Category, color=Category)) + geom_line() + geom_point()
ggsave(file="Sanfrancisco_top_10_crimes_summer2014_by_month.png")

## by hour
hour<-strftime(strptime(san$Time, "%H:%M"), "%H")
by_hour<-data.frame(Category=san$Category, Hour=hour)
top_10_crimes_by_hour<-by_hour[Category %in% top_10_crimes_count$Category,]
top_10_crimes_by_hour$Category<-as.character(top_10_crimes_by_hour$Category)
#table(top_10_crimes_by_hour)
ggplot(data=data.frame(table(top_10_crimes_by_hour)), aes(x=Hour, y=Freq, group=Category, color=Category)) + geom_line() + geom_point()
ggsave(file="Sanfrancisco_top_10_crimes_summer2014_by_hour.png")


## by desc
count_by_desc<-data.frame(table(factor(san$Descript)))
names(count_by_desc)<-c('Descript', 'Count')
top_10_crime_desc_count<-head(count_by_desc[order(-count_by_desc[,2]),], n=10)
ggplot(top_10_crime_desc_count, aes(x='', y=Count, fill=Descript)) + geom_bar(width=1, stat="identity") + coord_polar("y") 
ggsave(file="Sanfrancisco_top_10_crimes_summer2014_pie.png")                                                                                                                                   

## by day of week
by_dayweek<-san[,c('Category', 'DayOfWeek')]
top_10_crimes_by_dayweek<-by_dayweek[Category %in% top_10_crimes_count$Category,]
top_10_crimes_by_dayweek$Category<-as.character(top_10_crimes_by_dayweek$Category)
top_10_crimes_by_dayweek$DayOfWeek<-factor(top_10_crimes_by_dayweek$DayOfWeek, levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

ggplot(data=data.frame(table(top_10_crimes_by_dayweek)), aes(x=DayOfWeek, y=Freq, group=Category, color=Category))+ geom_line() + geom_point()
ggsave(file="Sanfrancisco_top_10_crimes_summer2014_by_day_of_week.png")
