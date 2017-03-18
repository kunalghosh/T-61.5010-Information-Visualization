setwd("~/Dropbox/Workspace/Course_Exercises/InfoVis/project")
df <- read.table("coffee.dat",sep="\t",header = TRUE)
df$Date <- as.Date(as.character(df$Date),format="%d-%b-%Y")
summary(df)
library(ggplot2)
ggplot(df,aes(x=Date,y=Cups))+geom_path()+geom_smooth()

# Aggregate data by month and week
library(lubridate)
library(plyr)
df$month <- floor_date(df$Date, "month")
df$week <- floor_date(df$Date, "week")

# Weekly Expenditure on Coffee
coffee.spendperweek = aggregate(Expenditure ~ week, data=df, FUN=sum)
ggplot(coffee.spendperweek,aes(x=week,y=Expenditure))+
  geom_line()+
  geom_smooth()+
  ylab("Expenditure on Coffee")+
  ggtitle("Weekly Expenditure on Coffee")

# Weekly Expenditure on Coffee (Bar plot)
ggplot(coffee.spendperweek,aes(x=week,y=Expenditure))+
  geom_bar(stat="identity",fill=I("grey50"))+
  geom_smooth(se=FALSE)+
  # geom_smooth(method="glm", formula = y ~ poly(x, 3))+
  ggtitle("Weekly Expenditure on Coffee") +
  ylab("Expenditure on Coffee")+
  xlab("Week") +
  scale_x_date(date_breaks = "2 week",date_labels = "%Y-%U")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Weekly cups Coffee (Bar plot)
coffee.cupsperweek = aggregate(Cups ~ week, data=df, FUN=sum)
ggplot(coffee.cupsperweek,aes(x=week,y=Cups))+
  geom_bar(stat="identity",fill=I("grey50"))+
  geom_smooth(se=FALSE)+
  # geom_smooth(method="glm", formula = y ~ poly(x, 3))+
  ggtitle("Cups of Coffee") +
  ylab("Cups of Coffee")+
  xlab("Week") +
  scale_x_date(date_breaks = "2 week",date_labels = "%Y-%U")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly Expenditure on Coffee (Bar plot)
coffee.spendpermonth = aggregate(Expenditure ~ month, data=df, FUN=sum)
ggplot(coffee.spendpermonth,aes(x=month,y=Expenditure))+
  geom_bar(stat="identity",fill=I("grey50"))+
  geom_smooth(se=FALSE)+# geom_smooth(method="glm", formula = y ~ poly(x, 3))+
  ggtitle("Monthly Expenditure on Coffee") +
  ylab("Expenditure on Coffee")+
  xlab("Month") +
  scale_x_date(date_breaks = "1 month",date_labels = "%Y-%b") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(color = "gray90", size = 0.8),
        axis.ticks = element_line(colour = "gray90"))


# Monthly cups Coffee (Bar plot)
coffee.cupspermonth = aggregate(Cups ~ month, data=df, FUN=sum)
ggplot(coffee.cupspermonth,aes(x=month,y=Cups))+
  geom_bar(stat="identity",fill=I("grey50"))+
  geom_smooth(se=FALSE)+
  # geom_smooth(method="glm", formula = y ~ poly(x, 3))+
  ggtitle("Cups of Coffee") +
  ylab("Cups of Coffee")+
  xlab("Month") +
  scale_x_date(date_breaks = "1 month",date_labels = "%Y-%b")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly cups Coffee + Expense per cup (Bar plot)
library(sqldf)
coffee <- aggregate(Cups ~ month, data=df, FUN=sum) # coffee$cupspermonth = aggregate(Cups ~ month, data=df, FUN=sum)
expense_percup_permonth <- sqldf("select sum(Expenditure)/sum(Cups) as Expense_percup,month from df group by month") # monthly
coffee_agg <- sqldf("select coffee.month as month, coffee.Cups as Cups, e.Expense_percup from coffee INNER JOIN expense_percup_permonth as e on coffee.month == e.month") # monthly

library(reshape2)
colnames(coffee_agg) <- c("Month","Cups","Expense(€) /Cup")
coffee_agg_facet <- melt(coffee_agg, measure.vars = c("Cups", "Expense(€) /Cup"))

ggplot(coffee_agg_facet,aes(x=Month,y=value))+
  geom_bar(stat="identity",fill="#7570b3",alpha=0.5) +#grey50
  geom_smooth(se=FALSE,color="#d95f02",size=0.5 , span=0.7)+
  # geom_smooth(se=FALSE,color="#d95f02",size=0.5, method="glm", formula = y ~ poly(x, 3))+
  ggtitle("My Coffee Stats") + # ylab("Cups of Coffee") +
  ylab("")+
  xlab("Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%Y-%b") +#scale_y_continuous(breaks = c(seq(0,120,by=10))) +
  facet_grid(variable ~ ., scales = "free_y") +
  annotate("point", x = as.Date(c("30-Oct-2015","18-Jan-2016","12-Feb-2016","4-Mar-2016","21-Mar-2016",
                          "9-Apr-2016"),format="%d-%b-%Y"), y = 0)

# Monthly cups Coffee + Expense on Coffee (Bar plot)
library(sqldf)
# coffee <- aggregate(Cups ~ month, data=df, FUN=sum) # coffee$cupspermonth = aggregate(Cups ~ month, data=df, FUN=sum)
# expense_permonth <- aggregate(Expense ~ month, data=df, FUN=sum) #sqldf("select sum(Expenditure) , month from df group by month") # monthly
coffee_agg <- sqldf("select month, sum(Cups) as Cups, sum(Expenditure) as 'Spend(€)' from df group by month") # monthly

library(reshape2)
colnames(coffee_agg) <- c("Month","Cups","Spend(€)")
coffee_agg_facet <- melt(coffee_agg, measure.vars = c("Cups", "Spend(€)"))

ggplot(coffee_agg_facet,aes(x=Month+15,y=value))+
  geom_bar(stat="identity",fill="#7570b3",alpha=0.7) +#grey50 width=1.5
  geom_smooth(se=FALSE,color="#d95f02",size=0.5 , span=0.7)+
  # geom_smooth(se=FALSE,color="#d95f02",size=0.5, method="glm", formula = y ~ poly(x, 3))+
  ggtitle("My Monthly Coffee Stats") + # ylab("Cups of Coffee") +
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "1 month",date_labels = "%Y-%b") +# %Y-%U scale_y_continuous(breaks = c(seq(0,120,by=10))) +
  facet_grid(variable ~ ., scales = "free_y") +
  annotate("point", x = as.Date(c("30-Oct-2015","18-Jan-2016","12-Feb-2016","4-Mar-2016","21-Mar-2016",
                                  "9-Apr-2016"),format="%d-%b-%Y"), y = 0) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(color = "gray90", size = 0.8),
        axis.ticks = element_line(colour = "gray90"))

# -------- All data in the same plot


# --------




