
library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr) # Useful for reading variables containing spaces
library(ggplot2)
library(stringr) # Labels overlap
library(scales) # Labels value format

data = read_csv("Provisional_COVID-19_Deaths_by_Place_of_Death_and_Age.csv")

head(data)
colnames(data)

# Taking away useless variables
data = data[,-c(1,2,3,14,16,17)]
colnames(data)

# Looking at values taken by certain variables
unique(data$`Age group`)
unique(data$State)
unique(data$Group)
unique(data$`Place of Death`)
unique(data$Year)
unique(data$Month)

####### Comparing age and nb of deaths #######

# Filtering, deleting and ordering columns
place = filter(data, `Age group` == "All Ages" & State == "United States" 
               & Group == "By Month" & `Place of Death` != "Total - All Places of Death") 

# Verifying `HHS Region`==0 before deleting
unique(place$`HHS Region`)

# Verifying Year and Month are taking correct values
unique(place$Year)
unique(place$Month[place$Year==2020])
unique(place$Month[place$Year==2021])

# Taking only necessary variables
place = subset(place, select=c(Year, Month, `Place of Death`, `COVID-19 Deaths`))

# Converting to frame
colnames(place)
dim(place)
place = data.frame(place)
place

# Level order for the plot
level_order = aggregate(place$COVID.19.Deaths, by=list(Category=place$Place.of.Deat), FUN=sum)
level_order = level_order[order(level_order$x),]$Category
level_order

# Getting min and max of death
min = 0
max = max(place$COVID.19.Deaths)

################################################

# Define UI for application that draws a barplot
ui <- fluidPage(

    # Application title
    titlePanel("Interactive barplot"),
    
    # Vertical space
    HTML("<br>"),
    
    # Instructions
    "Select month. Data cover the period from January 2020 till September 2021.",
    
    # Vertical space
    HTML("<br><br>"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderTextInput(
            inputId    = "time",
            label      = "Month :",
            choices    = c(paste(substring(month.name, 1, 3), "2020"),
                           paste(substring(month.name[1:9], 1, 3), "2021")),
            selected   = "Sep 2021",
            animate    = TRUE,
            width      = "100%"
            )
          ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("Barplot")
        )
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Barplot <- renderPlot({
    
      # Extracting month and year
      time <- unlist( strsplit(input$time, " ") )
      month = as.numeric(factor(time[1], levels = substring(month.name, 1, 3)))
      year = time[2]
      
      # Filtering
      place_time = filter(place, Year == year & Month == month ) 
      
      # Barplot
      ggplot(data=place_time, aes(x=factor(Place.of.Death, level=level_order), y=COVID.19.Deaths)) + 
        geom_bar(stat="identity", fill='steelblue') +
        coord_flip() +
        ggtitle("Covid deaths per place of death") +
        theme(plot.title = element_text(size=16, hjust = 0.5)) +
        theme(axis.text.x = element_text(face="bold", color="black",size=8, angle=0),
              axis.text.y = element_text(face="bold", color="black",size=8, angle=0)) + 
        labs(x = NULL, y = 'deaths') +  
        scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
        scale_y_continuous(labels = format_format(big.mark = " ", decimal.mark = ",", 
                                                  scientific = FALSE), limits=c(min,max))
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
