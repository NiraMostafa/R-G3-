s<-read.csv("G3_sydney_hobart_times.csv")
View(s)

# show all the rows that have NA
s[!complete.cases(s) , ]


str(s)
summary(s)

#Removing text in numeric values
s$Time<-gsub(" day", '',s$Time)
s$Time <- as.numeric(s$Time)

#3)Replace each NA in Time according to the mean of time by use (ITT) by use 
#Multiple imputation technique
missingTime <- s[is.na(s$Time), ]
library(tidyr)
clean <- drop_na(s)
x <- mean(clean$Time)
s[is.na(s$Time), 'Time']=x

#fill coloum of Code.Time.less.than.3
s$Code.Time.less.than.3[s$Time < 3] ='yes'
s$Code.Time.less.than.3[s$Time > 3] ='no'


#Computing new variable from existing one data
s$Rest <- s$fleet_start -
  s$fleet_finish

# Re_code year variable 
s$yearRange[s$Year <= 1970]="1945 _ 1970"
s$yearRange[s$Year > 1971 & s$Year <= 2000]="1971 _ 2000"
s$yearRange[s$Year >2000 ]="2000 _ .."

#Subset only "1945 _ 1970" year
sub1 <- s[s$yearRange =="1945 _ 1970" , ]

#Subset only "1971 _ 2000" year
sub2 <- s[s$yearRange =="1971 _ 2000" , ]

#Subset only "1971 _ 2000" year
sub3 <- s[s$yearRange =="1971 _ 2000" , ]

# Get only the first 20 rows
h<-head(s ,20)

# Get only the last 30 rows
t<-tail(s ,30)

#_____________ now the data is ready to visualization

#Data Visualizing using Histogram in R

library(ggplot2)
draw_hist <- ggplot(s, aes(Time))
draw_hist
draw_hist + geom_histogram()
draw_hist + geom_histogram(binwidth = 5)
draw_hist + geom_histogram(color="red")
draw_hist + geom_histogram(fill="blue")
draw_hist + geom_histogram(alpha = 0.5)
draw_hist + geom_histogram() + ggtitle("Time of sydney")
draw_hist + geom_histogram() + labs(x="Time", y="Numbers")

#Data Visualizing using Bar Chart in R
draw_bar <- ggplot(s, aes(x=Code.Time.less.than.3, fill = Time))
draw_bar + geom_bar()
draw_bar + geom_bar() + theme_light()
draw_bar + geom_bar() + labs(y="Time count",
                             title = "Code.Time.less.than.3 Rate")
draw_bar + geom_bar() + facet_wrap(~Year)

#display the effect of the time on rest (co_relation) using scatter plot,name the figure

#Data Visualizing using Scatter Plot in R
draw_sc <- ggplot(s, aes(Time, Rest))
draw_sc + geom_point(aes(color=Time)) + labs(x="time", y="fleet_start -fleet_finish")
draw_sc + geom_point(aes(color=Time)) + stat_smooth(se=FALSE)