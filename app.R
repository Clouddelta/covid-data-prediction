if(!require('dplyr')){
  install.packages('dplyr')
  stopifnot(require('dplyr'))
}
if(!require('shiny')){
  install.packages('shiny')
  stopifnot(require('shiny'))
}
if(!require('ggplot2')){
  install.packages('ggplot2')
  stopifnot(require('ggplot2'))
}
if(!require('TSA')){
  install.packages('TSA')
  stopifnot(require('TSA'))
}
if(!require('tseries')){
  install.packages('tseries')
  stopifnot(require('tseries'))
}
if(!require('tidyr')){
  install.packages('tidyr')
  stopifnot(require('tidyr'))
}

covid_data=read.csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
names(covid_data)[1]='Date'
canada_covid=covid_data%>%filter(Date>='2020-12-29'&Country=='Canada')

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
                      max = 5,
                      value = 3)
                              ),
       mainPanel(
          plotOutput("plot")
                )
  )
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
      l=input$days
      ####deaths prediction by ts
      V_neg.log.return<-canada_covid$New_deaths
      V_neg.log.return<-log(V_neg.log.return+1)
      #V_neg.log.return<--log(V_neg.log.return)
      #Split the last five values
      V_last.L=V_neg.log.return[(length(V_neg.log.return)-l+1):(length(V_neg.log.return))]
      V_without.last.five=V_neg.log.return[1:(length(V_neg.log.return)-l)]
      Min=10000
      P=0
      D=0
      Q=0
      actual = V_last.L
      for(p in 0:4){
        for(d in 0:1){
          for(q in 0:4){
            model <- arima(V_without.last.five, order=c(p,d,q))
            forecast <- predict(model,n.ahead=l)$pred
            MSFE<-sum((forecast-actual)^2/l)
            if(MSFE<Min){
              Min=MSFE
              P=p
              D=d
              Q=q
            }
          }
        }
      }
      #c(P,D,Q)
      TS<-arima(V_without.last.five, order=c(P,D,Q))
      #TS$coef
      predict_deaths=exp(predict(TS,n.ahead=l)$pred)-1
      last_five_day_deaths=tail(canada_covid,n=l)%>%select(Date,New_deaths)
      last_thirty=tail(canada_covid,n=30+l)%>%
        select(Date,New_deaths)%>%
        mutate(predict_deaths=New_deaths)%>%
        head(-l)
#      test=last_five_day_deaths
#      test[,2]=canada_covid[c(81:(80+l)),]%>%select(New_deaths)
#      names(test)[2]='predict_death'
      canada_deaths_data=last_five_day_deaths%>%
        cbind(predict_deaths)%>%
        bind_rows(last_thirty)%>%arrange(Date)
      ggplot(data=canada_deaths_data,aes(x=as.Date(Date)))+
        geom_line(aes(y=predict_deaths,colour='predict_deaths'))+
      geom_line(aes(y=New_deaths,colour='New_deaths'))+
      scale_x_date(date_labels = "%m-%d")+xlab("") +ylab("") +
      theme_minimal()+
      scale_colour_manual('',
                          breaks=c('New_deaths','predict_deaths'),
                          values=c('New_deaths'="steelblue",'predict_deaths'='#69b3a2')
                          )
  })
}
shinyApp(ui, server)
