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
#If the input format is not well specified fisrt, it won't convert correctly.

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
(date1=as.Date(date1, format='%d-%m-%Y'))
#small hint here, parenthesis will print objects created from a function

format(date1, "%A")#Julian Day of the week
format(date1, "%j")#Day of year as decimal number (001–366) (julian day). 
format(date1, "%V")#Week of the year as decimal number (01–53) as defined in ISO 8601. 



#__________________________________________________________________
#4- le format "Date-time"####

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
format(date1,"%S")#seconds
format(date1,"%j")#Julian days

#__________________________________________________________________

#7- Now Lubridate package, a more convenient and intuitive way to deal with date ####

#More info can be found here: 
# https://lubridate.tidyverse.org/

# Potential tutorials 
# https://lubridate.tidyverse.org/articles/lubridate.html

# Cheatsheet: 
# https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

# To download lubridate: 
#install.packages("lubridate") # install.packages("tidyverse") if you want the whole bunch
require(lubridate)

#8 - Parsing dates is made easier, we specify the format directly in the function.####
(date1=ymd_hms("2014-03-01 15:30:30"))
(date2=ydm_hms("2015-01-03 15:30:30"))
(date3=dmy_hms("01-03-2016 15:30:30"))




#9 - extracting information#### 

#Simple functions to get and set components of a date-time, such as year(), month(), mday(), hour(), minute() and second():
date1=as.POSIXlt('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 

day(date1)
wday(date1, label=T)
wday(date1)

month(date1)
month(date1, label=T)


#10- Easier to manipulate time zones ####
date1

with_tz(date1, "America/Vancouver")# Changes printing, or converting hours to a desired time zone

force_tz(date1, "America/Vancouver")# force a new time zone but keep original time.


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


# intervals :A summary of the time information between two points
#EXAMPLE:

#13 - everything is vectorized####
# making it easier to build and manipulate dates: 
last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}

last_day_week <- function(date) {
  ceiling_date(date, "week") - days(1)
}

date1
last_day(date1)# round a date to the last month of a date

last_day_week(ymd_hms('2019-11-06 12:00:00'))# round a date to the last day of the week

#working on an example: 

#Suppose I got a bunch of mountain goat kids born from the 15 same females during 3 years at exact same date, around may 25th. I want to know 
# 1- what is their age once we are July 15th of the same year.
# 2- 
yday(ymd('2015-05-25'))

yday(ymd('2016-05-25'))
set.seed(111)
bdgoats=data.frame(
  year=c(rep(2014, times=15),
         rep(2015, times=15),
         rep(2016, times=15)),
  month=rep(05, times=45),
  day=c(rep(round(rnorm(n=15, mean=25, sd=2), digits=0),times=3))
)

bdgoats$date=ymd(paste(bdgoats$year, bdgoats$month,bdgoats$day))
bdgoats$age=dmy(paste('15 07', year(bdgoats$date))) - bdgoats$date
bdgoats

require(ggplot2)
ggplot(data=bdgoats, aes(y=age, x=factor(year))) +
  geom_point()
str(bdgoats)


