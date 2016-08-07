# Developing Data Products Assignment
# by Chan Chee-Foong
# on 4 Aug 2016
# for Data Science Specialisation
# of Johns Hopkins University
#

# Preparing the libraries

if(!is.element('shiny', installed.packages()[,1])) {
    install.packages('shiny')
}

if(!is.element('rCharts', installed.packages()[,1])) {
    install.packages('rCharts')
}

if(!is.element('dplyr', installed.packages()[,1])) {
    install.packages('dplyr')
}

if(!is.element('lubridate', installed.packages()[,1])) {
    install.packages('lubridate')
}

library(shiny)
library(rCharts)
library(dplyr)
library(lubridate)


# Downloading of Data from Source

datadir <- "./data"
datafile <- "resale-flat-prices-based-on-registration-date-from-march-2012-onwards.csv"
zipfile <- "resale-flat-prices.zip"

datadirfile <- paste(datadir, datafile, sep="/")
zipdirfile <- paste(datadir, zipfile, sep="/")

if (!file.exists(datadirfile)) {
    dir.create(datadir)
    url <- "https://data.gov.sg/dataset/7a339d20-3c57-4b11-a695-9348adfd7614/download"
    download.file(url, destfile = zipdirfile, mode = 'wb')
    unzip (zipdirfile, exdir = datadir)
}


# Preparing the Data
       
txnData <- read.csv(datadirfile)
town <- as.character(unique(txnData$town))
names(town) <- town

updatedDt <- tail(txnData$month,1)
toYear <- as.numeric(substr('2016-05',1,4))
year <- as.character(c(2012:toYear))
names(year) <- year


# Defining UI for application

shinyUI(fluidPage(
  

    # Application Title and Summary

    titlePanel("Singapore Public Housing Resale Prices"),
    h4(paste('All prices in S$k unless otherwise stated.  Data updated as of',updatedDt)),
    h5(
        'by Chan Chee-Foong for Developing Data Product Assignment of the 
        Data Science Specialisation Course by Johns Hopkins University'
    ),
    h5('Summary:'),
    h5(
        'This application downloads the latest Resale Flat Prices (starting 2012) from 
        http://data.gov.sg and summarises the information in 4 charts: 
        (a) Median price per month by flat type in a town 
        (b) Average price trend per quarter by flat type in a town
        (c) Number of sales recorded in each quarter by flat type in a town
        (d) Average price per sqft in each quarter by flat type in a town'
    ),
    h5('Instructions:'),
    h5('1. Filter the dataset by town in Singapore'),
    h5('2. Filter the dataset by year range'),
    h5('3. Switch how you wish to see bar plot inside Number of Sales'),
    hr(),

    
    # Defining inputs on the side Bar
      
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="id1", "Select Town",
                        town, 
                        selected = 'ANG MO KIO'),
            selectInput(inputId="id2", "Select Year (From)",
                        year, 
                        selected = '2012'),
            selectInput(inputId="id3", "Select Year (To)",
                        year, 
                        selected = '2016'),
            radioButtons("id4", "Stacked Bar Chart for Number of Sales",
                               c("Yes" = "1", "No" = "0")),
            br()
        ),

                
        # Defining the plots to display
        
        mainPanel(
            tabsetPanel(
                tabPanel("Median Price", plotOutput("boxPlot", width=600)),
                tabPanel("Average Price Trend", plotOutput("tsPlot", width=600)),
                tabPanel("Number of Sales", plotOutput("countPlot", width=600)),
                tabPanel("Price per Sqft", plotOutput("psfPlot", width=600))
            )
        )
    )

))
