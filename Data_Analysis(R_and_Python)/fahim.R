library(readr)
library(dplyr) # Useful for reading variables containing spaces
library(ggplot2)
library(reshape)

data = read_csv("Provisional_COVID-19_Deaths_by_Place_of_Death_and_Age.csv")

head(data)
colnames(data)

# Taking away useless variables
data = data[,-c(1,2,3,5,6,14,16,17)]
colnames(data)

# Looking at values taken by certain variables
unique(data$`Age group`)
unique(data$State)
unique(data$Group)
unique(data$`Place of Death`)

####### Comparing deaths and causes #######

# Filtering, deleting and ordering columns
datag = filter(data, `Age group` == "All Ages" & State == "United States" 
               & Group == "By Total" & `Place of Death` == "Total - All Places of Death") 
datag = subset(datag, select=-c(`Age group`,State,Group,`Place of Death`))
datag = datag[c(1,2,4,5,3)]

# Verifying `HHS Region`==0 before deleting
colnames(datag)
unique(datag$`HHS Region`)
datag = subset(datag, select=-c(`HHS Region`))
colnames(datag)

# Converting to a frame and preserving order
datag = data.frame(cause=colnames(datag), deaths=as.numeric(datag))
datag$cause <- factor(datag$cause, levels = datag$cause[order(datag$deaths)])

# Barplot
ggplot(data=datag, aes(x=cause, y=deaths)) + geom_bar(stat="identity")


####### Comparing age and causes #######

# Filtering, deleting and ordering columns
age = filter(data, `Age group` != "All Ages" & State == "United States" 
               & Group == "By Total" & `Place of Death` == "Total - All Places of Death") 
age = subset(age, select=-c(State,Group,`Place of Death`))

# Verifying `HHS Region`==0 before deleting
unique(age$`HHS Region`)
age = subset(age, select=-c(`HHS Region`))

# Changing columns order
age = age[c(1,2,4,5,3)]

# Converting to frame and getting percentage
colnames(age)
dim(age)
age = data.frame(age)
age = age[,1:4]
age

# Reshaping to use ggplot
age = melt(age, id=c("Age.group"))
names(age) = c("group", "cause", "deaths")
age

# Barplot
ggplot(data=age, aes(fill=cause, x=group, y=`deaths`)) + 
  geom_bar(position="dodge",stat="identity")


####### Comparing places and causes #######

# Filtering, deleting and ordering columns
place = filter(data, `Age group` == "All Ages" & State == "United States" 
             & Group == "By Total" & `Place of Death` != "Total - All Places of Death") 
place = subset(place, select=-c(State,Group,`Age group`))

# Verifying `HHS Region`==0 before deleting
unique(place$`HHS Region`)
place = subset(place, select=-c(`HHS Region`))

# Changing columns order
place = place[c(1,2,4,5,3)]

# Converting to frame and removing places that does not have that much deaths (<=1000)
colnames(place)
dim(place)
place = data.frame(place)
place = place[place$COVID.19.Deaths>1000,1:4]
place

# Reshaping to use ggplot
place = melt(place, id=c("Place.of.Death"))
names(place) = c("place", "cause", "deaths")
place

# Barplot
ggplot(data=place, aes(fill=cause, x=place, y=`deaths`)) + 
    scale_x_discrete(guide = guide_axis(n.dodge=3)) + 
    geom_bar(position="dodge",stat="identity")


####### US map #######

library(usmap)

# Filtering, deleting and ordering columns
map = filter(data, `Age group` == "All Ages" & State != "United States" 
               & Group == "By Total" & `Place of Death` == "Total - All Places of Death") 
map = subset(map, select=-c(`Place of Death`,Group,`Age group`))

# Verifying `HHS Region`!=0 before deleting
unique(map$`HHS Region`)
map = subset(map, select=-c(`HHS Region`))

# Changing columns order and removing Influenza deaths
map = map[c(1,2,4,3)]
head(map)

# Removing 2 states to use usmap library
map = map[map$State!="New York City" & map$State!="Puerto Rico",]

# Converting to frame
dim(map)
map = data.frame(map)
map = map[order(map$State),]

# Adding extra columns to use usmap library
map$fips = statepop$fips
map$abbr = statepop$abbr

# Taking percentage of deaths
map[,2:3] = map[,2:3]/map[,4]

# Verification
map

# Plot Covid
plot_usmap(data=map, values="COVID.19.Deaths", labels=TRUE, exclude = "DC") +
  scale_fill_gradient(low="white", high="red", name = NULL) +
  ggtitle("Covid death rate (US, 2020-2021)") +
  theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme(legend.position = c(0.2, -0.15),
        legend.direction = 'horizontal',
        legend.key.width = unit(0.8, "in"),
        legend.key.height = unit(0.4, "in"),
        legend.text = element_text(size=15))

# Plot Pneumonia
plot_usmap(data=map, values="Pneumonia.Deaths", labels=TRUE, exclude = "DC") +
  scale_fill_gradient(low="white", high="red", name = NULL) +
  ggtitle("Pneumonia death rate (US, 2020-2021)") +
  theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme(legend.position = c(0.2, -0.15),
        legend.direction = 'horizontal',
        legend.key.width = unit(0.8, "in"),
        legend.key.height = unit(0.4, "in"),
        legend.text = element_text(size=15))


