setwd("~/Dropbox/Workspace/Course_Exercises/InfoVis/project")
df <- read.table("kuitti2.dat",sep="\t")
colnames(df) <- c("Date","Expense","Description","Category","Remarks")

df$Date <- as.Date(as.character(df$Date),format="%d-%b-%Y")
df$Expense <- as.numeric(as.character(df$Expense))

# aggregate(data[,"score", drop=F], list(group=data$group), mean)
pie_df <- aggregate(Expense ~ Category,data=df, sum)

pie(pie_df$Expense, main="My Piechart"
    , col=rainbow(length(pie_df$Expense))
    ,labels=pie_df$Category
    ,explode=0.1)

new_data <- tbl_df(df)
require("sqldf")
pie_df2 <- sqldf("SELECT Category, floor(sum(Expense)) as Expense from df group by df.Category order by df.Expense asc")

library(plyr)
require("dplyr")
library(ggplot2)
minval <- (min(pie_df2$Expense)%/%10)*10 # Convert to lowest multiple of 10
maxval <- ((max(pie_df2$Expense)%/%10)+1)*10
ggplot(data=pie_df2, aes(x=reorder(Category,-Expense),y=Expense,fill=Category)) +
  geom_bar(stat="sum") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = c(seq(0,400,by=50),2590)) +
  ylab("Expense (Euros)") +
  xlab("Category") +
  ggtitle("Cumulative Expenditure (6 months)")

# Group dates by week
# http://stackoverflow.com/a/11398049
tmp <- list()
tmp$y <- format(df$Date, format="%Y")
tmp$w <- format(df$Date, format="%U")
tmp$m <- format(df$Date, format="%m")
tmp$dow <- as.numeric(format(df$Date, format="%w")) # Day of the week
tmp$nearest_date <- df$Date-tmp$dow #sunday_of_week
tmp$y[tmp$w=="00"] <- as.character(as.numeric(tmp$y[tmp$w=="00"])+1)
tmp$w[tmp$w =="00"] <- "1"
df$week <- paste(tmp$y, tmp$w, sep="-")
df$nearest_date <- tmp$nearest_date # paste(tmp$y, tmp$m, tmp$nearest_sunday, sep="-")
df <- sqldf("select * from df where week != 'NA-NA'")
weekly_total <- sqldf("Select week, Category, floor(sum(Expense)) as Expense from df group by df.week")

# --------------
library(lubridate)
library(sqldf)
df$month <- floor_date(df$Date, "month")
df_months <- sqldf("select sum(Expense) as Expense,Category,month from df group by month,Category")
ggplot(df_months, aes(x=month, y=Expense)) +
  geom_point()+
  geom_smooth(se=FALSE,color="#d95f02",size=0.5 , span=0.5)+
  facet_grid(Category ~ ., scales ="free_y")

# --------------

# Get the stream graph package
devtools::install_github("hrbrmstr/streamgraph")

# Experiment with ggplot movies database
library(streamgraph)
# library(dplyr) # dplyr already loaded
library(ggplot2movies)

movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) %>%
  streamgraph("genre", "n", "year") %>%
  sg_axis_x(20) %>%
  sg_colors("PuOr") %>%
  sg_legend(show=TRUE, label="Genres: ")

df %>%
  select(nearest_date, Category, Expense) %>%
  tidyr::gather(Category, Expense, -nearest_date) %>%
  group_by(nearest_date, Category) %>%
  streamgraph("Category","Expense","nearest_date") # %>%
  sg_axis_x(20) %>%
  sg_fill_brewer() %>%
  sg_colors() %>%
  # sg_colors(palette="PuOr") %>%
  sg_legend(show=TRUE, label="Expense: ")

sg <- streamgraph(data=df,key=Category,value=Expense,date=nearest_date) 
sg_axis_x(sg, 20)
sg_fill_brewer(sg)
sg_legend(sg,show=TRUE, label="Expense: ")

# Plot coffee data
coffee <- sqldf("Select week, floor(sum(Expense)) as Expense from df where Category='Coffee' group by df.week")
# all except rent
df_not_rent <- sqldf("Select df.nearest_date as week, Category,Expense from df where Expense > 2 and Category not in ('Rent','Lunch')")
# df_not_rent$week = as.Date(df_not_rent$week,format="%d-%m-%Y")
library(lubridate)
#df_not_rent <- df_not_rent[order(df_not_rent$week),]
df_not_rent$week <- lubridate::ymd(df$Date)
dplyr::arrange(df_not_rent, week)
ggplot(data=df_not_rent, aes(x=week, y=Expense,fill=Category)) +
  geom_bar(stat="sum") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,120,by=10)) +
  ylab("Expense (Euros)") +
  xlab("Category") +
  ggtitle("Cumulative Expenditure (6 months)")


# Plot the trend of Coffee
coffee <- sqldf("Select week, floor(sum(Expense)) as Expense from df where Category='Coffee' group by df.week")
ggplot(data=coffee,aes(x=week, y=Expense, group=1))+
  geom_line() +
  geom_point() +
  stat_smooth() +
  # geom_smooth(method = "loess", se = FALSE) +
  expand_limits(y=0) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")

# Plot density
ggplot(data=df, aes(x=Category)) + geom_density(aes(group=Category, colour=Category, fill=Category))

# Plot the trend all categories
# all except rent
df_not_rent <- sqldf("Select df.nearest_date as week, Category,Expense from df where Expense > 2 and Category not in ('Rent','Lunch')")
ggplot(data=df_not_rent,aes(x=week, y=Expense, group=Category))+
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  # geom_smooth(method = "loess", se = FALSE) +
  expand_limits(y=0) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")
