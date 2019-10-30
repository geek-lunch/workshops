#Dates in R ####
###Dealing with dates in R with base functions and lubridate

#we'll cover some basic stuff out of base function, but lubridate is way better as it can include time changes, leap years and so on. 

#More info can be found here: 
# https://lubridate.tidyverse.org/

# Potential tutorials 
# https://lubridate.tidyverse.org/articles/lubridate.html

# Cheatsheet: 
# https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

# To download lubridate: 

#install.packages("lubridate") # install.packages("tidyverse") if you want the whole bunch
require(tidyverse)
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

#Il existe deux formats, POSIXlt et POSIXct. 
#lt is for local time, it stores information as a list
#ct is for calendar time, it stores the time siince an origin, the UNIX epoch.
#
#les deux fonctions appellent strptime()

#Selon les pages d'aides: "Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 (in the UTC time zone) as a numeric vector. Class "POSIXlt" is a named list of vectors representing different values (day, month, year, hour, min, seconds, time zone)"

date.hour=strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S")
date=c("26/10/2016")
time=c("19:51:30")
day<-paste(date,"T", time)

day.time1=as.POSIXct(day,format="%d/%m/%Y T %H:%M:%S",tz="Europe/Paris")
day.time1
day.time1$year

day.time2=as.POSIXlt(day,format="%d/%m/%Y T %H:%M:%S",tz="Europe/Paris")
day.time2
day.time2$year


#5- convertir un vecteur en format date-time####

as.POSIXlt(date1) #UTC c'est pour "Universal time, coordinated'. Les dates sont converties dans ce format lorsqu'elles n'incluent pas de métadonnées sur le fuseau horaire. 

#Pour savoir votre fuseau horaire:
Sys.time()
as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='') #Utilise le fuseau horaire de l'ordi si les guillements sont vides (,tz='') ou si l'argument n'est pas spécifié.
as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
as.POSIXct('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='UTC')


#xx- Manipuler des dates ####

#Calculer des différences de temps
date1=as.POSIXlt('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
date2=as.POSIXct('19-06-1999 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 

difftime(date1,date2, units='weeks')

date1=as.POSIXlt('19-06-1994 20:08:58', format= '%d-%m-%Y %H:%M:%S', tz='America/Montreal') 
format(date1,"%S")#seconds
format(date1,"%j")#Julian days

