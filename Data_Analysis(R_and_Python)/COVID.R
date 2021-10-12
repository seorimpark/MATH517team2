library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(UsingR)
library(ggcorrplot)
library(usmap)
library (tidyverse)

df <- read.csv("Provisional_COVID-19_Deaths_by_Place_of_Death_and_Age.csv")


# Want to check if column 1, 2 or 3 have different values for the observations 
length(unique(df[["Month"]]))
length(unique(df[["Year"]]))
length(unique(df[["Data.as.of"]]))
length(unique(df[["Start.Date"]]))
length(unique(df[["End.Date"]]))
length(unique(df[["Footnote"]]))
# Delete the column about month,year, and date of analysis
df = df[,-c(1)]#delete data of analysis column because it is the same value 
df = df[,-c(4)]
df= df[,-c(5)]  #delete month column because all the values are missing



#Delete every observations with missing variables :
#df = df[!(df$Footnote == "One or more data cells have counts between 1-9 and have been suppressed in accordance with NCHS confidentiality standards."), ]


#Let's plot the number of deaths depending on the place of death : 

df1 = df[!(df$Age.group == "0-17 years"|df$Age.group == "18-29 years"|df$Age.group== "30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df1)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df2 = df[!(df$Age.group == "All Ages"|df$Age.group == "18-29 years"|df$Age.group== "30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df2)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))


df3 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df3)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df4 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "18-29 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df4)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df5 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "18-29 years"|df$Age.group=="30-39 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df5)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df6 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "18-29 years"|df$Age.group=="30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="65-74 years"|df$Age.group=="85 years and over"),]
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df6)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df7 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "18-29 years"|df$Age.group=="30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="85 years and over"),]
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df7)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))

df8 = df[!(df$Age.group == "All Ages"|df$Age.group == "0-17 years"|df$Age.group== "18-29 years"|df$Age.group=="30-39 years"|df$Age.group=="40-49 years"|df$Age.group=="50-64 years"|df$Age.group=="65-74 years"),]
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= COVID.19.Deaths))
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.Deaths))
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= Total.Deaths))
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= Influenza.Deaths))
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia..Influenza..or.COVID.19.Deaths))
ggplot(data = df8)+ geom_point(mapping = aes(x = Place.of.Death, y= Pneumonia.and.COVID.19.Deaths))


