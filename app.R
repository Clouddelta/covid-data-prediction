library(dplyr)
library(shiny)
library(ggplot2)
covid_data=read.csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
names(covid_data)[1]='Date'
canada_covid=covid_data%>%filter(Country=='Canada')

#doesn't work
#download.file('https://covid19.who.int/who-data/vaccination-data.csv',
#              destfile = "C:\\vaccination-data.csv")
# vac_data=..... here are some problems on encoding 
#names(vac_data)[c(1,5)]=c('Country','Date')
#canada_vac=vac_data%>%filter(Country=='Canada')
ui <- fluidPage(
  titlePanel("Predict the number of deaths in Canada"),
  sidebarLayout(
    # Sidebar panel for inputs ----
      sidebarPanel(
      # Input: Slider for the number of days ----
                    sliderInput(inputId = "days",
                      label = "Number of days:",
                      min = 2,
                      max = 10,
                      value = 5)
                              ),
       mainPanel(
          plotOutput("plot")
                )
  )
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
      l=input$days
      last_five_day_deaths=tail(canada_covid,n=l)%>%select(Date,New_deaths)
      test=last_five_day_deaths
      test[,2]=canada_covid[c(81:(80+l)),]%>%select(New_deaths)
      names(test)[2]='predict_deaths'
      canada_deaths_data=left_join(last_five_day_deaths,test)
      ggplot(data=canada_deaths_data,aes(x=as.Date(Date)))+
      geom_line(aes(y=New_deaths,colour='New_deaths'))+
      geom_line(aes(y=predict_deaths,colour='ratio'))+
      scale_x_date(date_labels = "%m-%Y")+xlab("") +ylab("") +
      theme_minimal()+
      scale_colour_manual('',
                          breaks=c('New_deaths','predict_deaths'),
                          values=c('New_deaths'="#69b3a2",'predict_deaths'="steelblue"))+
      geom_vline(xintercept = as.Date("2021-09-07"),color='red',linetype='dashed')

    })
}
shinyApp(ui, server)
