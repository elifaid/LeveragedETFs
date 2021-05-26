#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyquant)
library(plotly)
library(scales)

ui <- fluidPage(

    # Application title
    titlePanel("Simulating Leveraged ETFs using Quantmod"),
    uiOutput("Message"),
    # Sidebar with a inputs 
    sidebarLayout(
        sidebarPanel(

            dateInput("date", 
                               h3("Date input"), 
                               value = "2000-01-01"),
            textInput("ticker","ticker",value="^sp500tr"),
            numericInput("ER","Expense ratio (%)",value=1),
            numericInput("leverage","Leverage (x) ",value=3),
            numericInput("initial","Initial Amount ",value=0),
            numericInput("cashflow","Monthly contribution ($) ",value=1000),
            checkboxInput("LOG","Logarithmic Y axis?",value=TRUE),
            checkboxInput("MA","Moving Average strategy?",value=FALSE),
            uiOutput("message"),
            width = 2
        ),

        # Show plots
        mainPanel(
            plotlyOutput("Cashflowplot",height="720px",width="1200"),
            plotlyOutput("Comparison",height="720px",width="1200"),
            plotlyOutput("Difference",height="720px",width="1500"),
            
            fluidRow( #two plots side by side
                column(6,plotlyOutput("CAGRPlot",height="720px")),
                column(6,plotlyOutput("Different_Leverages",height="720px"))
            )
            
        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$Comparison <- renderPlotly({
       g<-ggplot(data=Main_dataset(),aes(x=date))+geom_line(aes(color = "Simulated",y=new_val))+
            xlab("Year")+
            ylab("Growth of $1")+
            ggtitle(paste0("Growth of ",
                           input$leverage[1],
                           "x leveraged ",
                           gsub('^\\^|\\^$', '', Main_dataset()$symbol[1]),
                           " since ",
                           Main_dataset()$date[1]))+
            theme(plot.title = element_text(size=24, face="bold",margin = margin(10, 0, 10, 0)))+
            theme_light()+
            scale_x_date(date_labels = "%Y",date_breaks="5 years")+
            geom_line(aes(y=adjusted/adjusted[1],color='Original'))+
            scale_colour_manual(name = 'Legend', 
                                values =c('Simulated'='steelblue','Original'='red',"Moving Average"="green"))+
            theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
       g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                      minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
       g<-g+if(input$MA==TRUE){geom_line(aes(y=underlying200ma/adjusted[1],color='Moving Average'))}
        
    })
    output$Difference <- renderPlotly({
            ggplot(data=Main_dataset(),aes(x=date))+geom_line(aes(y=new_val/(adjusted/adjusted[1])))+
            xlab("Year")+
            ggtitle("Leveraged/Original")+
            theme(plot.title = element_text(size=24, face="bold",margin = margin(10, 0, 10, 0)))+
            theme_light()+
            scale_x_date(date_labels = "%Y",date_breaks="5 years")

    })
    output$Cashflowplot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        today<-Cashflows()%>%filter(row_number()==n())%>%select(date)%>%pull()
        g<-ggplot(data=Cashflows(),aes(x=date))+
            geom_line(aes(y=DCA_val,color = "Value"))+
            xlab("Year")+
            ylab("Balance")+
            ggtitle(paste0("Initial Amount of $",input$initial, " with $",
                           input$cashflow,
                           " Monthly contributions"))+
            theme_light()+
            theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))+
            scale_x_date(date_labels = "%Y")+
            geom_line(aes(y=new_line,color='Contributions'))+
            scale_colour_manual(name = 'Legend',values = "red",labels="Amount Contributed")+
            geom_text(data=. %>% filter(date =="2020-02-20"|
                                            date =="2007-10-09"|
                                            date =="2000-03-10"|
                                            date ==today ),aes(y=DCA_val,label=dollar(DCA_val),vjust=-1.2))+
            geom_text(data=. %>% filter(date =="2020-03-23"|
                                            date =="2009-03-05"|
                                            date =="2002-10-09"),aes(y=DCA_val,label=dollar(DCA_val),vjust=1))+
            scale_colour_manual(name = 'Legend', 
                                values =c('Value'='steelblue','Contributions'='red'))+scale_y_continuous(labels=scales::dollar_format())
            g<-g+if(input$LOG==TRUE){scale_y_continuous(breaks =10^(-10:10),
                                                       labels=scales::dollar_format(),
                                                       minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)),
                                                       trans= "log10")}    
            
        
    })
    
    output$CAGRPlot <- renderPlotly({
        # Rolling One year average
        h<-Main_dataset()%>%filter(row_number()==n()) %>% select(CAGR)%>% pull()
        ggplot(data=Main_dataset(),aes(x=date,y=Yr_rolling_average))+geom_line(color="steelblue")+
            xlab("Year")+
            ylab("Return (%)")+
            ggtitle("Rolling One Year Return")+
            theme_light()+
            scale_y_continuous(labels = scales::percent)+
            scale_x_date(date_labels = "%Y",date_breaks="5 years")+
            geom_hline(yintercept=h,linetype = "dotted")

    })
    output$DailyPerformance <- renderPlotly({
        # Rolling One year average
        ggplot(data=Main_dataset(),aes(x=date,y=growth))+geom_line(color="steelblue")+
            xlab("Year")+
            ylab("Return (%)")+
            ggtitle("Rolling One Year Return")+
            theme_light()+
            scale_y_continuous(labels = scales::percent)+
            scale_x_date(date_labels = "%Y",date_breaks="5 years")

    })

    Main_dataset_load<-reactive({
        tq_get(input$ticker, get = "stock.prices", from = input$date)%>%
            select(symbol,date,adjusted)%>% drop_na(adjusted)
        })
    Main_dataset<- reactive({
        Main_dataset_load()%>%
            mutate(growth=(adjusted/lag(adjusted)-1))%>%
            mutate(growth=ifelse(row_number()==1,0,growth)) %>%
            mutate(new_val=cumprod(1+growth*input$leverage-(input$ER/100)/252))%>%
            mutate(CAGR=((new_val/new_val[1])^(1/(as.numeric(difftime(as.Date(date), as.Date(date[1])))/(60*60*24*365))))-1)%>%
            mutate(Yr_rolling_average=new_val/(lag(new_val,253))-1)%>%
            mutate(underlying200ma=SMA(adjusted,200))%>%
            mutate(under200=ifelse(lag(adjusted)<underlying200ma,TRUE,FALSE))
        
    })

    Cashflows<-reactive( # only buy when below 200 ma
        {
            dates<-subset(Main_dataset(), !duplicated(substr(date, 1, 7), fromLast = FALSE))
            Main_dataset() %>%
                
                mutate(contrib=ifelse(row_number()==1,
                                      input$initial,
                                      ifelse(date %in% dates$date,
                                            input$cashflow,
                                            0)
                                      ))%>%
                mutate(shares=contrib/new_val)%>%
                mutate(total_shares=cumsum(shares))%>%
                mutate(DCA_val=total_shares*new_val)%>%
                mutate(new_line=cumsum(contrib))
            
            
        })

    output$Different_Leverages<-renderPlotly({
        
        get_final_value<- function(leverage){
            x$growth[1]=0
            x<-x %>% mutate(leverage=leverage)
            x<-x %>% mutate(new_val=cumprod(1+growth*leverage -(input$ER/100)/ifelse(input$ticker=="ETH-USD",365,232)))%>% last() %>% select(new_val)%>%pull()#Leveraged Growth after expenses
            return(x)
        }
        
        x<-tq_get(input$ticker, get = "stock.prices", from = input$date)
        x<-x%>%select(symbol,date,adjusted)%>% drop_na(adjusted)
        x<-x %>%  mutate(growth=(adjusted/lag(adjusted)-1))
        
        ticker<-gsub('^\\^|\\^$', '',x$symbol[1])
        start<-x$date[1]
        
        y1<-seq(from=0,to=10,by=0.01)
        y2<-unlist(lapply(y1,get_final_value))
        data<-tibble(y1,y2)%>%rename(leverage=y1,final_value=y2)
        
        ggplotly(ggplot(data=data,aes(y=final_value,x=leverage))+
            geom_line(color = "steelblue", size=1.2)+
            theme_light()+
            geom_text(data=. %>%filter(final_value==max(final_value)),aes(label=paste0(trimws(leverage), 
                                                                                       "x Leverage: ",
                                                                                      sprintf("$ %3.0f",max(final_value)) ),
                                                                          y=1.05*final_value
                                                                          ))+
            ggtitle(paste0("Different leverage ratios for ",ticker, " since ", start))+
            xlab("Leverage")+
            ylab("Growth of  $1")+
            geom_point(data=. %>%filter(final_value==max(final_value)))+
            theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))
            )
    
    })
    output$Message<- renderUI({
        HTML("Sp500 total return up to 1988: ^SP500TR, earlier ^GSPC <br>
             Nasdaq 100 ^NDX")
    })    
    output$message<- renderUI({
        x<-Main_dataset()%>%filter(row_number()==n())
        HTML(paste("As of",
              x %>% select(date)%>% pull(),
              ":<br> Total Contributions: ",
              dollar_format()(sum(Cashflows()$contrib)),
              "<br> Final Value:", 
              dollar_format()(Cashflows()%>%filter(row_number()==n()) %>% select(DCA_val)%>% pull()),
              "<br> CAGR of (for inital amount only):",
              percent(x %>% select(CAGR)%>% pull(),.11)))
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
