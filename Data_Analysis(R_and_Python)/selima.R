library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(UsingR)
library(ggcorrplot)
library(usmap)
library (tidyverse)
library (scales)


#df <- read_csv("/Users/selimajaoua/Documents/GitHub/MATH517team2/Data/Cases_per_age_group.csv")
#colnames(df)
#sum(df$`Percent of cases`)
#Plot the data :
#par(cex=0.5)
#with(iris,pie(df$`Percent of cases`, df$`Age group`,main = 'distribution of the population with covid by age group'))

#c= df$`Percent of cases`* df$`Percent of US population` /100
#par(cex = 0,5)
#with(iris, pie(c,df$`Age group`,main = 'ditribution of the population with covid by age group in the total population'))
###






df<-read_csv("/Users/selimajaoua/Documents/GitHub/MATH517team2/Data/case_per_month.csv")
colnames(df)
unique(df$state)
unique(df$month_number) #until 20, so delete this row : 
df = df[,-c(2)]
unique(df$month)
unique(df$year)
unique(df$end_date) # we can see that the period of collection of the data is from 2020-01-31 to 2021-09-30
# PLOT OF MORTALITY RATE AND NEW CASES  THROUGH TIME FOR THE US : 

df_us = df[,-c(4)] #delete state
unique (df_us$end_date)
 df_us = df_us %>%
group_by(end_date, year, month) %>% summarise_at(vars(new_deaths_per_month,new_cases_per_month),sum) %>%
ungroup()

plot(df_us$end_date, df_us$new_deaths_per_month,type = "o", ylab = "Deaths of Covid",xlab = "Time (in month)",main = "Evolution of COVID deaths in the US")
plot(df_us$end_date, df_us$new_cases_per_month, type = "o",ylab = "New Cases of Covid", xlab  ="Time (in month)", main = "Evolution of COVID cases in the US")
#mortality rate : 
us_population_2020 = 331002651
us_population_2021 = 333516610
nrow(df_us)
nrow (df_us[df_us$year == 2020,])
nrow (df_us[df_us$year == 2021,])
#let's check that the data is sorted : 
df_us$year #the data is small to check it just by printing it
#is.unsorted(df_us$year) ??
population = c(rep(us_population_2020, nrow(df_us[df_us$year == 2020,])) , rep(us_population_2021, nrow(df_us[df_us$year==2021,])))
population
df_us$population = population
ncol(df_us)
colnames(df_us)
mortality_rate = df_us$new_deaths_per_month/df_us$population
mortality_rate
df_us$mortality_10000 = mortality_rate * 10000 #units of death for 10,000 individuals
plot(df_us$end_date, df_us$mortality_10000, type = "o", ylab ="Mortality rate ", xlab = "Time (in months)",main = "Number of deaths for 10 000 individuals per month")
#doing the same thing for the new cases 
cases_rate = df_us$new_cases_per_month /df_us$population
cases_rate
df_us$cases_10000 = cases_rate * 10000
plot(df_us$end_date, df_us$cases_10000, type= "o", ylab = "Cases rate", xlab = "Time(in months) ",main = "Number of cases for 10 000 individuals per month")

#plot both at the same time : 
ggplot(df_us, aes(df_us$end_date)) + 
  geom_line(aes(y = df_us$cases_10000, colour = "Cases Rate ")) + 
  geom_line(aes(y = df_us$mortality_10000, colour = "Mortality Rate")) +
  ggtitle("Comparaison of Mortality Rate and Cases Rate ")+
  xlab("Time (in months)") + ylab("Number of death/case for 10 000 indiduals")
#2nd mortality :
new_cases = df_us$new_cases_per_month
cases = rep (0, length(new_cases))
for (i in 1:length(new_cases)){
  cases[i]= sum(new_cases[1:i])
}
df_us$cases = cases
ratio = df_us$new_deaths_per_month/df_us$cases
plot(df_us$end_date, ratio, type = "o", ylab ="Mortality rate ", xlab = "Time (in months)",main = "Ratio of the number of death and the new cases for 10 000 individuals per month")
#doing the same thing for the new cases 
#PLOT OF THE MORTALITY RATE AND NEW CASES THROUGH TIME FOR EACH STATE : 
ggplot (df, aes ( x = df$end_date, y =df$new_deaths_per_month, colour = df$state ))+ geom_line()
#as we can see : the  plot with the 49 states ( that we have ) is  burden 
#create a function that can compare the evolution of death between two states : 
plot_death_two_states <- function(state1, state2){
  df_two_states = df[ df$state == state1 | df$state ==  state2,]
  ggplot(df_two_states, aes ( x= df_two_states$end_date, y=df_two_states$new_deaths_per_month, colour = df_two_states$state))+ geom_line()
}
#try function : 
plot_death_two_states("New York", "Alabama")
#create a function that can compare the evolution of cases between two states : 
plot_cases_two_states <- function(state1, state2){
  df_two_states = df[df$state == state2 | df$state == state2 ,]
  ggplot(df_two_states, aes ( x= df$end_date, y=df_two_states$new_deaths_per_month, colour = df$state))+geom_line()
}
#try function : 
plot_death_two_states("New York", "Alabama")
#This can be help full when comparing two states that are neighboors 

