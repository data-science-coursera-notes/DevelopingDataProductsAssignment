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

if(!is.element('ggplot2', installed.packages()[,1])) {
    install.packages('ggplot2')
}

if(!is.element('reshape2', installed.packages()[,1])) {
    install.packages('reshape2')
}

library(shiny)
library(rCharts)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)


# Downloading the data from source

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

# Preparing the data and required fields

txnData <- read.csv(datadirfile)

txnData$size <- sapply(strsplit(as.character(txnData$flat_type)," "), function(str) str[[1]])
txnData[!(txnData$size %in% c(1:5)),'size'] <- 6
txnData$size <- as.numeric(txnData$size)

sqm2sqft <- 10.7639

txnData$floor_area_sqft <- sqm2sqft * txnData$floor_area_sqm
txnData$psf <- txnData$resale_price/txnData$floor_area_sqft


## Monthly data for boxplot

Data <- txnData %>% group_by(month, town, flat_type) %>% summarise(avg_price = mean(resale_price), 
                                                                   number_of_sales = n(), 
                                                                   avg_psf = mean(psf))

Data$date <- ymd(paste0(Data$month,'-01'))
Data$date <- as.Date(Data$date)
Data$year <- year(Data$date)


## Quarterly data for price trends, number of sales and price per sqft

txnData$date <- ymd(paste0(txnData$month,'-01'))
txnData$date <- as.Date(txnData$date)
txnData$mth <- month(txnData$date, label=TRUE)
txnData$quarter <- 'Q'
txnData[as.character(txnData$mth) %in% c('Jan','Feb','Mar'),]$quarter <- '.Q1'
txnData[as.character(txnData$mth) %in% c('Apr','May','Jun'),]$quarter <- '.Q2'
txnData[as.character(txnData$mth) %in% c('Jul','Aug','Sep'),]$quarter <- '.Q3'
txnData[as.character(txnData$mth) %in% c('Oct','Nov','Dec'),]$quarter <- '.Q4'
txnData$year <- year(txnData$date)
txnData$quarter <- paste0(txnData$year,txnData$quarter)

qData <- txnData %>% group_by(quarter, town, flat_type) %>% summarise(avg_sales = mean(resale_price), 
                                                                   number_of_sales = n(), 
                                                                   avg_psf = mean(psf))

qData$year <- substr(qData$quarter,1,4)



# Defining server logic 
shinyServer(function(input, output) {

    # Subsetting the dataset by filtering the town and year range
    PlotData <- reactive({
        Data[Data$town == as.character(input$id1) & 
             Data$year %in% c(as.character(input$id2):as.character(input$id3)),]
    })
    
    qPlotData <- reactive({
        qData[qData$town == as.character(input$id1) & 
                  qData$year %in% c(as.character(input$id2):as.character(input$id3)),]
    })

    
    # Median Price Boxplot Output
    output$boxPlot <- renderPlot({

        ggplot(PlotData(), aes(x=town, y=avg_price/1000,fill=flat_type)) +
            geom_boxplot() +
            facet_grid(.~flat_type) +
            labs(x=paste(as.character(input$id1), 'Town'), y="MEDIAN PRICE (S$k)") +
            theme(legend.position="top", legend.title = element_text(face = "bold")) +
            scale_fill_discrete("FLAT TYPE") +
            theme(axis.text.x=element_blank())

    })

    # Average Price Trend Output
    output$tsPlot <- renderPlot({

        pData <- qPlotData()
        ggplot(pData, aes(x=quarter, y=avg_sales/1000, group=flat_type, colour=flat_type)) +
            geom_line(size = 1.5) +
            geom_point(size = 3) +
            xlab("QUARTER") + 
            ylab("AVERAGE PRICE PER UNIT (S$k)") +
            theme_light() +
            theme(legend.position="top", legend.title = element_text(face = "bold")) +
            scale_colour_discrete("FLAT TYPE") +
            guides(fill=guide_legend(nrow=1)) +
            theme(axis.title.x = element_text(size=12),
                  axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

    })

    
    # Number of Sales Output
    output$countPlot <- renderPlot({
        
        pData <- qPlotData()
        Stacked <- ifelse(as.character(input$id4) == 1,TRUE,FALSE)
        if (Stacked) {
            ggplot(data=pData, aes(x=quarter, y=number_of_sales, fill=flat_type)) +
                theme(legend.position="top") +
                geom_bar(stat="identity") + 
                xlab("QUARTER") + 
                ylab("NUMBER OF TRANSACTIONS") +
                theme(legend.position="top", legend.title = element_text(face = "bold")) +
                scale_colour_discrete("FLAT TYPE") +
                guides(fill=guide_legend(nrow=1)) +
                theme(axis.title.x = element_text(size=12),
                      axis.text.x  = element_text(angle=90, vjust=0.5, size=8))
        } else {
            ggplot(data=pData, aes(x=quarter, y=number_of_sales, fill=flat_type)) +
                theme(legend.position="top") +
                geom_bar(stat="identity", position=position_dodge()) +
                xlab("QUARTER") + 
                ylab("NUMBER OF TRANSACTION") +
                theme(legend.position="top", legend.title = element_text(face = "bold")) +
                scale_colour_discrete("FLAT TYPE") +
                guides(fill=guide_legend(nrow=1)) +
                theme(axis.title.x = element_text(size=12),
                      axis.text.x  = element_text(angle=90, vjust=0.5, size=8))
        }
        
    })
    
    # Price per Sqft Output
    output$psfPlot <- renderPlot({
        
        pData <- qPlotData()
        ggplot(pData, aes(x=quarter, y=avg_psf,group=flat_type, colour=flat_type)) +
            geom_line(size = 1.5) +
            geom_point(size = 3) +
            xlab("QUARTER") + 
            ylab("AVERAGE PRICE PER SQFT (S$)") +
            theme_light() +
            theme(legend.position="top", legend.title = element_text(face = "bold")) +
            scale_colour_discrete("FLAT TYPE") +
            guides(fill=guide_legend(nrow=1)) +
            theme(axis.title.x = element_text(size=12),
                  axis.text.x  = element_text(angle=90, vjust=0.5, size=8))
        
    })

})

