#Dates in R ####
###Dealing with dates in R with base functions and lubridate

#we'll cover some basic stuff out of base function, but lubridate is way better as it can include time changes, leap years and so on. 

#__________________________________________________________________

#1- Convert a vector to a date format in R####
#19 juin 1994
date1='19-06-1994'# date must be  a character format. 
#here it is in format dd-mm-YYYY

as.Date(date1)#bad
as.Date(date1, format='%Y-%m-%d')#bad
#Why is it bad? 
#the default of as.Date function uses format YYYY-m-d.
#If the input format is not well specified first, it won't convert correctly.

as.Date(date1, format='%d-%m-%Y')#Gooood, 'cause we specified the format exactly as the input was formatted!

#2- Convert a vector into a date from other input formats####
date2='19-Juin-1994'
as.Date(date2, format='%d-%B-%Y')#here, I changed  %m for %B; 
#"%B" means: 'full month name in the current locale' (So in french for this computer, because the conversion works).
#for example: 
date2='19-June-1994'
as.Date(date2, format='%d-%B-%Y')#does not work because my Operating system (OS) is in french

#See ?strptime 
#for details about existing formats of date and their associate abbreviations. 

#__________________________________________________________________

#3- Extracting information from dates ####
date1='19-06-1994'
(date1=as.Date(date1, format='%d-%m-%Y'))#small hint here, parenthesis will print objects created from a function


format(date1, "%A")#Julian Day of the week

format(date1, "%j")#Day of year as decimal number (001–366) (julian day). 
format(as.Date('19-06-1996', format='%d-%m-%Y'), '%j')#gives a different result 'cause it is a leap year

format(date1, "%V")#Week of the year as decimal number (01–53) as defined in ISO 8601. 

#__________________________________________________________________
#4- format "Date-time"####

#There is two date-time formats, POSIXlt et POSIXct. (observe the final 2 letters of the formats)
# lt is for LOCAL TIME, it stores information as a list
# ct is for CALENDAR TIME, it stores the time since an origin, the UNIX epoch.
#
# the two functions are calling strptime() behind, as as.Date did. 

#From the help pages : 
# Class "POSIXct" represents the (signed) *number of seconds since the beginning of 1970* as a numeric vector. 
# Class "POSIXlt" is a named LIST of vectors representing different values (day, month, year, hour, min, seconds, time zone)"

(date.hour=strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
date=c("26/10/2016")
time=c("19:51:30")
(day<-paste(date,"T", time))

(day.time1=as.POSIXct(day,format="%d/%m/%Y T %H:%M:%S",tz="Europe/Paris"))

day.time1$year 
# what does it says? it is an atomic vector, it is a number from which you cannot extract an object!

(day.time2=as.POSIXlt(day,format="%d/%m/%Y T %H:%M:%S",tz="Europe/Paris"))
day.time2$year 
#here it works because this format of date stores a list of objects that you can extract.

#__________________________________________________________________

#5- Converting to a date-time format ####

as.POSIXlt(date1) #UTC is for "Universal time, coordinated'. By default, dates are converted to this format when there is no data on the time zone. 

#To know your OS local time:
Sys.time()

as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='') #This uses the system time of the operating system if it is not specified/ommited.

as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') #St-viateur bagel's city
as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='UTC') # a universal time zone

#__________________________________________________________________

#6- Date manipulation with base functions ####

#Time differences
date1=as.POSIXlt('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
date2=as.POSIXct('19-06-1999 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
date3=as.POSIXct('19-06-1995 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
date4=as.POSIXct('19-06-1995 22:00:00', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 

difftime(date2,date1, units='weeks')
difftime(date2,date1, units='days')
difftime(date3,date1, units='days')
difftime(date4,date3, units='min')

#extract information
date1=as.POSIXlt('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
format(date1,"%S")#seconds, extraction
format(date1,"%j")#Julian days, conversion

#__________________________________________________________________

#7- Now Lubridate package, a more convenient and intuitive way to deal with date ####

# To download lubridate: 
#install.packages("lubridate") # install.packages("tidyverse") if you want the whole bunch
require(lubridate)
#__________________________________________________________________

#8 - Parsing dates is made easier, we specify the format directly in the function.####
(date1=ymd_hms("2014-03-01 15:30:30"))
(date2=ydm_hms("2015-01-03 15:30:30"))
(date3=dmy_hms("01-03-2016 15:30:30"))

#__________________________________________________________________

#9 - extracting information#### 

#Simple functions to get and set components of a date-time, such as year(), month(), mday(), hour(), minute() and second():
(date1=dmy_hms('19-06-1994 20:08:58', tz='America/Montreal') )
(date1=dmy_hms('19-06-1994 T 20:08:58', tz='America/Montreal') )# so now you can relax about input format, not even needed to specify 'T'

day(date1)
wday(date1, label=T)
wday(date1)

month(date1)
month(date1, label=T)
#__________________________________________________________________

#10- Easier to manipulate time zones ####
date1

with_tz(date1, "America/Vancouver")# Changes printing, or converting hours to a desired time zone

force_tz(date1, "America/Vancouver")# force a new time zone but keep original time.

#__________________________________________________________________

#11 - dealing with time changes and leap years is explicit. ####

#lubridate implement new types of datasets. 

#durations: Measure the exact amount of time between two points, mathematical duration

#periods: Accurately track clock times despite leap years, leap seconds, and day light savings time

minutes(2)
dminutes(2)

leap_year(2011) ## regular year
ymd(20110101) + dyears(1)
ymd(20110101) + years(1)

leap_year(2012) ## leap year
ymd(20120101) + dyears(1)
ymd(20120101) + years(1)

#A duration year will always equal 365 days. 
#Periods, on the other hand, fluctuate the same way the timeline does to give intuitive results (clock time info). 
# the last new class of data is intervals :A summary of the time information between two points
# I won't talk about this

#__________________________________________________________________

#12 - everything is vectorized####
# making it easier to build functions and manipulate dates: 
last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}

last_day_week <- function(date) {
  ceiling_date(date, "week") - days(1)
}

date1
last_day(date1)# round a date to the last month of a date

last_day_week(ymd_hms('2019-11-06 12:00:00'))# round a date to the last day of the week

#__________________________________________________________________

#13- A simple example integrating date conversion/ manipulation / visualisation####

#Say we track temperature in a rapidly colding September month
df=data.frame(date=seq(ymd('2019-09-01'),ymd('2019-09-30'),by='days'),
              temp=seq(20, 5, length.out = 30)+ rnorm(30, 2.5, 2))
str(df)
#transforming the date as in a day in september for a further analysis
df$ds=(as.integer(yday(df$date))-
         yday(ymd('20190901'))+1)

require(ggplot2)
ggplot(data=df, aes(yday(date), temp)) +
  geom_point()+
  geom_smooth()+
  theme_bw()+
  xlab("Date (day of the year ; 0-366)")+
  ylab("Temperature (°C)")

#modelling how date impact average temperature

lm(temp~date, data=df)# see the intercept? it is huge because it goes back to a day in 1970 as dates are stored from 0 to as.integer(ymd(20191106))
lm1=lm(temp~ds, data=df)

df$pred=predict(lm1, df)

ggplot(data=df, aes(ds, temp)) +
  geom_point()+
  geom_line(aes(ds, pred))+
  theme_bw()+
  xlab("Date (day of Sept.)")+
  ylab("Temperature (°C)")


#So to know your dates and how it works help to understand how this format is integrated in a model and also how to integrate it so it gives intuitive and biologically relevant results. 



#14- Another great potential use is just to summarize temporal trends and visualing it fast####


middle_day <- function(date) {
  floor_date(date, "weeks") + days(3)
}#lets create a function to extract the middle day of each week

df$mdate=as.factor(as.character(middle_day(df$date)))

require(plyr)
df=ddply(df, "mdate", summarize, meanm=mean(temp), sdm=sd(temp))

ggplot(data=df, aes(mdate, meanm)) +
  geom_point()+
  geom_pointrange(aes(ymin=meanm-sdm,
                  ymax=meanm+sdm,
                  x=mdate))+
  theme_bw()+
  xlab("Date")+
  ylab("Temperature (mean ± sd; °C)")

#__________________________________________________________________

# 15- second example: formatting a column of non-uniform date formats ####

# A random old biologist tells you he has seen some negative trend of age of mortality in a deer population through years.
# You don't believe him 'cause he's an old lying fart. 
# You ask him for the data and want to see it yourself. Data don't lie!
# Surprisingly, he gives you the data (that he would normally jealously keep for himself) 

#15 a#### 
#- load the file and check it
death=read.delim('dates_in_R/death.txt') #or if you are on evil side : read.delim(file.choose())
str(death)#so we have some strange dates,
#some having character in it! Plus, date is in factor format
#
summary(death)
head(death);tail(death)

#15 b####
#- parse date and time to dates
death$date=as.character(death$date)
unique(death$date)

death$date2=dmy(death$date)# what failed to parse?

death[is.na(death$date2),]# ok so these are strange dates... normal, they're incomplete... and we don't want to modify this by attributing a date to that, as highlighted by Sandra during the workshop
death[death$date%in% c('>26.07.92'),] 
#So this date have been successfully parsed, but there is some information we lost, because the sign was telling that this date was uncertain..
death=death[order(death$date2),]

 
#15 c - fix the date problem ....THIS IS IMPORTANT####

# When parsing a date format to incomplete dates, R fill the missing information with the first value. In the case that we only have Year, day becomes day 01, and month becomes January. These are false information and here is a reminder for healthy practices:  
#
#1- When parsing dates to imported files, always keep the old date untouched. It will be the back-up!
#2- on first occasion, try to enter dates as 3 columns in your Excel (or homologue) datasheet builder. Otherwise, you can do it in R!
#3- Know the limit of your data. In this example, plotting the parsed dates with missing information (death$date) would give false results as there was no individuals that died in January.

#So to be sure nobody plot nothing, one would first need to put all the information we have in 3 columns. 

death$day=day(parse_date_time(death$date,
                              c('dmy', "dmY")))
death$month=month(parse_date_time(death$date,
                                  c('dmy', "dmY", 'mY')))
death$year=year(parse_date_time(death$date,
                                c('dmy', "dmY", 'mY','Y')))
head(death)
death[is.na(death$date2)==T,]#We did great! 

#But we could add a comment when missing information are present :

library(dplyr) # >= 0.7.0
death=death %>% 
  mutate(comment = as_factor(case_when(is.na(.$day)== T~ "incomplete",
                                       is.na(.$month)==T~ "incomplete",
                                       grepl('>',.$date)~"uncertain",
                                       TRUE ~ as.character('certain'))))

summary(death)                         
death[!death$comment=='certain',]#So now our dataset has all the information needed!!!
death[is.na(death$date2),]#And we did not parse dates to incomplete ones. 


#Let's now answer the question, Is there a negative trend of age at death through years... 

#let's plot all the data at once, and we are gonnna use only year as it is the only value we have for all dates
ggplot(death, aes(year, age))+
  geom_jitter(shape=21)+ 
  geom_smooth()+
  theme_bw()+
  xlab('Year')+
  ylab('Age')

#now the mean per year. 
df=death
df$year=factor(as.character(df$year))

require(plyr)
df=ddply(df, 'year', summarize, meanm=mean(age), sdm=sd(age), len=length(age))#length to compute the sample size

df
str(df)

ggplot(data=df, aes(year, meanm)) +
  geom_point()+
  geom_pointrange(aes(ymin=meanm-sdm,
                      ymax=meanm+sdm,
                      x=year))+
  scale_x_discrete(name="Year", 
                   breaks=c(as.character(seq(1987,2017,3))),
                   labels=c(as.character(seq(1987,2017,3))))+
  scale_y_continuous(name="Age at death (mean ± sd)",
                     limits=c(0,17))+
  annotate("text", x=df$year, y=16, label=as.character(df$len), size=2)+#number at the top= sample size per age
  theme_bw()
  
#So it seems the guy is just an old fart, not a lying one! 

#Thanks for listening!
