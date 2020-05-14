
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

#Introducing Month function:
month <- function(data) { start_date  <-  sapply(strsplit(as.character(data$Start.Time), " "), "[", 1)
 
 
month <- substr(x = start_date, 6, 7) }
month_chi <- month(chi)
chi['month'] <- month_chi
month_chi <- month(chi)
chi['month'] <- month_chi
month_wash <- month(wash)
wash['month'] <- month_wash

##call function for ny
month_ny <- month(ny)
ny['month'] <- month_ny
final = (ny$month)

##call function wash
month_wash <- month(wash)
wash['month'] <- month_wash
final = (wash$month)

##call function for chi
month_chi <- month(chi)
chi['month'] <- month_chi
final =(chi$month)


#For Washington:
   ggplot(aes(x = month), data = wash) +
   geom_bar(color = 'black', fill = '#99004C') +
   xlab('Months') +
   ylab('Rental Count')+ 
   ggtitle("Number of rentals per Month in washington") 

#Numeric confirmation:
table(wash$month)


#For Chicago:
   ggplot(aes(x = month), data = chi) +
   geom_bar(color = 'black', fill = '#99004C') +
   xlab('Months') +
   ylab('Rental Count')+ 
   ggtitle("Number of rentals per Month in Chicago") 

#Numeric confirmation:
   table(chi$month) 

#For Ny:
   ggplot(aes(x = month), data = ny) +
         geom_bar(color = 'black', fill = '#99004C') +
         xlab('Months') +
         ylab('Rental Count')+ 
         ggtitle("Number of rentals per Month in NY")  

#Numeric confirmation:
   table(ny$month)

#For Ny
     ggplot(aes(x = User.Type), data = ny) +
            geom_bar(color = 'black', fill = '#FF8000') +
            xlab('User.Type') +
            ylab('Rental Count')+ 
            ggtitle("Number of rentals by User Type in NY")+
            scale_y_continuous(name="Number of Rentals", labels = comma)

#Numeric confirmation:
table(ny$User.Type)

#Summary
by(ny$User.Type, ny$month, summary)

#For Washington:
   ggplot(aes(x = User.Type), data = wash) +
   geom_bar(color = 'black', fill = '#FF8000') +
   xlab('User.Type') +
   ylab('Rental Count')+ 
   ggtitle("Number of rentals by User Type in washington")+
   scale_y_continuous(name="Number of Rentals", labels = comma)

#Numeric Confirmation:
table(wash$User.Type)

#Summary
by(wash$User.Type, wash$month, summary)

#For Chicago:
   ggplot(aes(x = User.Type), data = chi) +
   geom_bar(color = 'black', fill = '#FF8000') +
   xlab('User.Type') +
   ylab('Rental Count')+ 
   ggtitle("Number of rentals by User Type in Chicago")+
   scale_y_continuous(name="Number of Rentals", labels = comma)

#Numeric Confirmation
table(chi$User.Type)

#Summary
by(chi$User.Type, chi$month, summary)

#For Ny
ggplot(aes(x = Gender), data = ny) +
        geom_bar(color = 'black', fill = '#3399FF') +
        xlab('Gender') +
        ylab('Gender')+ 
        ggtitle(" Rentals Counts for Gender in NY")+
        facet_wrap( ~ ny$month)+
        scale_y_continuous(name="Number of Rentals", labels = comma)

#Numeric Confirmation:
table(ny$gender)

#Summary
by(ny$Gender, ny$month, summary)

#For Chicago:
   ggplot(aes(x = Gender), data = chi) +
   geom_bar(color = 'black', fill = '#3399FF') +
   xlab('Gender') +
   ylab('Gender')+ 
   ggtitle(" Rentals Counts for Gender in Chicago")+
   facet_wrap( ~ chi$month)+
   scale_y_continuous(name="Number of Rentals", labels = comma)

#Numeric Confirmation:
table(chi$gender)

#Summary
by(chi$Gender, chi$month, summary)

#summary of data:
                                                          
#For Ny:                                    
table(ny$month)
table(ny$User.Type)
table(ny$Gender)

#For washington
table(wash$month)
table(wash$User.Type)
table(wash$Gender)

#For chicago
table(chi$month)
table(chi$User.Type)
table(chi$Gender)

#Washington has the largest user base over all between the three cities.
#The male demographic is the main and majority in all three cities.
#Rentals increase with each month from the beginning of the year.

system('python -m nbconvert Explore_bikeshare_data.ipynb')
