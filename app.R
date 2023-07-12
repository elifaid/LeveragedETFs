library(shiny) #dashboard
library(tidyverse) #data manipulation
library(tidyquant) #stock prices
library(plotly) #interactive charts
library(scales) #percentage scales for charts
library(DT) #better way to make bottom table
library(zoo) #dates
library(shinyBS) #popups and other things
library(ExtDist)

IR <- tq_get("^IRX",from="1960-01-01") %>% 
  select(date,adjusted) %>% 
  na.locf(fromLast=TRUE) %>% 
  rename(RATE=adjusted) %>% 
  mutate(RATE=RATE/100)
LIBOR<-IR

ui <- fluidPage(
  
  # Application title
  titlePanel("Simulating Leveraged ETFs using Quantmod"),
  uiOutput("Message1"), # available tickers
  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      
      dateInput("date",   
                h3("Date input"), 
                value = "1987-01-01"),     # Default values for sim, works faster when you start from scratch versus just updating the info
      textInput("ticker","ticker",value="^sp500tr"),
      numericInput("ER","Expense ratio (%)",value=2),
      numericInput("leverage","Leverage (x) ",value=3),
      numericInput("initial","Initial Amount ",value=1000),
      numericInput("cashflow","Monthly contribution ($) ",value=100),
      checkboxInput("LOG","Logarithmic Y axis?",value=TRUE),
      numericInput("MA","Moving Average:",value=200),
      selectInput("Strategy","Strategy under MA?",choices=c("Cash","Go to 1x","-1")),
      selectInput("Strategy_CAGR","Strategy CAGR choice?",choices=c("Original","Leveraged","EMA","SMA")),
      width = 2
    ),
    
    # Show plots
    mainPanel( width=10,
      tabsetPanel(type = "tabs",
                  tabPanel("Main Data",
                           plotlyOutput("Cashflowplot",height="720px"),
                           plotlyOutput("Comparison",height="720px"),
                           plotlyOutput("Comparison2",height="720px"),
                           fluidRow( #two plots side by side
                             column(6,plotlyOutput("Different_MAs",height="720px")),
                             column(6,plotlyOutput("Different_Leverages",height="720px"))
                             
                           ),
                           plotlyOutput("Drawdown",height="720px"),
                           dataTableOutput("Results_Table") 
                  ),
                  tabPanel("Supplementary",
                           plotlyOutput("Intraday",height="720px"),
                           plotlyOutput("showdays",height="720px"),
                           plotlyOutput("Histogram",height="720px"),
                           plotlyOutput("Composite",height="720px"),
                           plotlyOutput("BorrowCosts",height="720px")),
                  tabPanel("Simulation",
                           uiOutput("Message2"),
                           numericInput("target_length","Simulation Length (252 trading days per year)",value=100),
                           numericInput("mean_return","Mean return (Daily %)",value=0.0468),
                           numericInput("stdev","Standard Deviation of return (Daily)",value=0.006505),
                           numericInput("borrow_sim","Cost of Leverage (Annual %)",value=5.25),
                           plotlyOutput("Simulation",height="720px"),
                           plotlyOutput("Simulation2",height="720px"),
                           plotlyOutput("Simulation3",height="720px"),
                           plotlyOutput("Simulation4",height="720px")

                  ),
                  tabPanel("Fixed Income Test",
                           selectInput("duration",label="Years",choices = c(1,2,3,5,7,10,20,30),selected = 30),
                           plotlyOutput("Bonds1",height="720px"),
                           plotlyOutput("Bonds2",height="720px"))
                  #tabPanel("Rebalancing Portfolios",
                  #         DTOutput("myTable"),
                  #         plotlyOutput("Equity_Fixed",height = "720px"))
        )
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  CombinedData<-reactive({
    dates<-subset(Main_dataset(), !duplicated(substr(date, 1, 7), fromLast = FALSE)) # first of the month in the data
    x<-left_join(Main_dataset(),bondData(),by=c("date"="Index")) %>% 
      mutate(return2=return2/return2[1],return3=return3/return3[1]) %>% 
      select(date,growth,new_growth,return,return3,leveragedreturn,new_val) %>%
      mutate(
        isquarterend=ifelse(date %in% dates$date,TRUE,FALSE),
        balance1=0.5,
        balance2=1-balance1,
        port_value=balance1*new_val+balance2*return3
      ) %>% 
      mutate(
        balance1=if_else(isquarterend==TRUE,balance1*port_value,lag(balance1)),
        balance2=ifelse(isquarterend==TRUE,
                        balance2*port_value,
                        dplyr::lag(balance2)
        ))

    })
  output$myTable<-renderDT({
    CombinedData()
  })
  output$Equity_Fixed <-renderPlotly({
    ggplot(CombinedData(),aes(x=date))+
      geom_line(aes(y=return3,color="bonds"))+
      geom_line(aes(y=new_val,color="Sp500"))+
      geom_line(aes(y=port_value,color="Portfolio"))+
      theme_bw()->g
    g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                           labels=scales::label_comma(),
                                           minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
  })
  bondDataLoad <- reactive({
    symbols<-paste0("DGS",input$duration)
    q<-getSymbols(symbols,src='FRED',auto.assign=FALSE)
    q<-q%>% fortify.zoo %>% as.tibble %>% rename(yield=2) %>% drop_na(yield)
    fedfunds<-getSymbols("RIFSPFFNB",src='FRED',auto.assign=FALSE) %>% 
      fortify.zoo %>% as.tibble %>% rename(fedfunds=RIFSPFFNB) %>% drop_na(fedfunds)
    q<-left_join(q,fedfunds,by=c("Index"))
    })
  bondData <- reactive({
    maturity=as.numeric(input$duration)
    bondDataLoad() %>% fill(fedfunds,.direction = "down") %>% 
      mutate(duration=(1-(1/(1+0.5*yield/100)^(2*maturity)))/(yield/100),
             convexity=(2/(yield/100)^2)*(1-(1/(1+yield/200)^(maturity*2)))-(maturity*2)/((yield/100)*(1+yield/200)^(maturity*2+1)),
             return=ifelse(row_number()==1,0,
                           (((1+(lag(yield))/100)^(1/252)-1))-
                             duration*(yield/100-lag(yield/100))+
                             1/2*convexity*(yield/100-lag(yield/100))^2),
             return2=cumprod(1+return),
             leveragedreturn=input$leverage*return-(input$leverage-1)*((1+((fedfunds+input$ER)/100))^(1/365)-1),
             return3=cumprod(1+leveragedreturn),
             inverted=ifelse(yield<fedfunds,"yes","no")
             
      ) ->x
  })
  output$Bonds1<-renderPlotly({
    bondData() %>% ggplot(aes(x=as.Date(Index)))+
      geom_line(aes(y=yield,color="yield"))+
      geom_line(aes(y=duration,color="duration"))+
      geom_line(aes(y=convexity,color="convexity"))+
      geom_line(aes(y=fedfunds,color="FFR"))+
      geom_line(aes(y=return2,color="Total return"))+
      ggtitle(paste0("Select stats of ",input$duration," year US treasury"))+
      ylab("Value")+xlab("Year")+theme_bw()->g
    g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                           labels=scales::label_comma(),
                                           minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
  })
  output$Bonds2<-renderPlotly({
    bondData() %>% ggplot(aes(x=as.Date(Index)))+
      geom_line(aes(y=return2,color="Unleveraged"))+
      geom_line(aes(y=return3,color="Leveraged"))+
      ggtitle(paste0("Performance of ",input$leverage,"times leveraged and unleveraged  ",input$duration," year US treasury"))+
              labs(subtitle = paste0("Cost of Leverage = EFFR + ",input$ER,"%"))+
      ylab("Value")+xlab("Year")+theme_bw()->g
    g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                           labels=scales::label_comma(),
                                           minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
    ggplotly(g) %>% layout(title = list(text = paste0("Performance of ",input$leverage," times leveraged and unleveraged ",input$duration," year US treasury",
                                                      '<br>',
                                                      '<sup>',
                                                      "Cost of Leverage = EFFR + ",input$ER,"%",
                                                      '</sup>'
                                                      )))
    
  })
  SimulationData <- reactive({
    no_of_days <- input$target_length
    starting_price <- 1
    #set.seed(101) #Set seed for reproducibility of the random numbers
    daily_mean<-input$mean_return/100 
    daily_std_dev<-input$stdev
    
    no_of_sims <- 252
    returns_list <- matrix(0, nrow = no_of_sims, ncol = no_of_days) #define matrices
    prices_list1 <- matrix(0, nrow = no_of_sims, ncol = no_of_days+1)
    prices_list2 <- matrix(0, nrow = no_of_sims, ncol = no_of_days+1) 
    
    #Note: returns_list and prices_list are actually matrices, I just chose a poor name
    
    for(i in 1:no_of_sims) { # for loop - 500 iterations
      returns_list[i,] <- rLaplace(no_of_days, mu=daily_mean, b=daily_std_dev) #Generate random variables
      prices_list1[i,] <- cumprod(c(starting_price, 1+(returns_list[i,]*input$leverage-((input$ER+(input$leverage-1)*input$borrow_sim)/25200))))#Calculate cumulative product
      prices_list2[i,] <- cumprod(c(starting_price, 1+(returns_list[i,])))#Calculate cumulative product
    }
    
    #Drawing a plot of 50 first simulations
    prices_list2<-as.data.frame(prices_list2)
    prices_list2<-prices_list2 %>% gather(Day,Value,"V2":paste0("V",no_of_days+1))
    prices_list2<-prices_list2%>% group_by(Day) %>% mutate(Cycle=row_number(),Day=sub('V', '',Day)) %>% select(-V1)
    as.factor(prices_list2$Day)
    
    prices_list1<-as.data.frame(prices_list1)
    prices_list1<-prices_list1 %>% gather(Day,Value,"V2":paste0("V",no_of_days+1))
    prices_list1<-prices_list1%>% group_by(Day) %>% mutate(Cycle=row_number(),Day=sub('V', '',Day)) %>% select(-V1)
    as.factor(prices_list1$Day)
    
    prices_list<-merge(prices_list1,prices_list2,by=c("Day","Cycle")) %>% 
      rename(Original="Value.y",Leveraged="Value.x") %>% gather(Type,Value,Original:Leveraged)
    })
  output$Simulation <-renderPlotly({
    ggplot(SimulationData(),aes(x=as.numeric(Day),
                           y=as.numeric(Value),
                           color=Type,
                           group=as.character(Cycle)))+
      geom_line(alpha=0.25)+theme_bw()+
      scale_y_log10()+
      ylab("Return (Growth of $1)")+
      xlab("Trading Day")+
      ggtitle(paste0("Monte Carlo of ",
                     input$target_length,
                     " Simulations of inputted values"
                     ))+geom_smooth(method="lm",se=FALSE,aes(group=1),color="red")

  })
  output$Simulation2 <-renderPlotly({
    maxData<-SimulationData() %>% filter(Type=="Original") %>% select(Value) %>% max() #default to limits to edge of oringal data, or else to stretched out by leveraged data
    SimulationData() %>% filter(Day==input$target_length+1) %>% 
    ggplot(aes(Value,color=Type))+
      geom_line(stat = "ecdf")+ #empirical cumulative distribution function 
      theme_bw()+
      ggtitle("Cumulative Distribution of returns")+
      xlab("Return (Growth of $1)")+
      ylab("Probability")+
      coord_cartesian(xlim = c(0, maxData))
    
  })
  output$Simulation3 <-renderPlotly({
    maxData<-SimulationData() %>% filter(Type=="Original") %>% select(Value) %>% max()
    SimulationData() %>% filter(Day==input$target_length+1) %>% 
      ggplot(aes(Value,color=Type))+
      geom_density()+
      theme_bw()+
      ggtitle("Distribution of returns")+
      xlab("Return (Growth of $1)")+
      ylab("Probability")+
      coord_cartesian(xlim = c(0, maxData))
    
  })
  output$Simulation4 <-renderPlotly({
    SimulationData() %>% pivot_wider(names_from = Type,values_from = Value) %>% 
      select(-Cycle) %>% group_by(Day) %>% 
      summarize(Value=median(Leveraged/Original),
                                  q1 = quantile(Leveraged/Original, p = .25),
                                  q2 = quantile(Leveraged/Original, p = .75)) %>% 
      pivot_longer(!Day,names_to = "Type",values_to = "Value") %>% 
      ggplot(aes(y=Value,x=as.numeric(Day),color=Type))+geom_line()+theme_bw()+
      xlab("Day")+ylab("ratio of Leveraged return/Unleveraged Return")+
      ggtitle("Comparison of returns of leveraged and unleveraged returns")+
      geom_abline(intercept = 1, slope = input$mean_return/100-input$borrow_sim/25200)

  })
  output$Comparison <- renderPlotly({
    g<-ggplot(data=Main_dataset(),aes(x=date))+
      geom_line(aes(color = "Simulated",y=new_val))+
      geom_line(aes(color = "Simulated (SMA Strategy)",y=new_val_sma))+
      geom_line(aes(color = "Simulated (EMA Strategy)",y=new_val_ema))+
      geom_line(aes(y=SMA1,color='Simple Moving Average'))+
      geom_line(aes(y=EMA1,color='Exponential Moving Average'))+
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
      geom_line(aes(y=adjusted,color='Original'))+
      scale_colour_manual(values =c('Simulated'='steelblue',
                                    'Original'='red',
                                    "Exponential Moving Average"="green",
                                    "Simple Moving Average"="yellow",
                                    "Simulated (SMA Strategy)"="grey",
                                    "Simulated (EMA Strategy)"="Blue"))+
      theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
    g<-g+if(input$LOG==TRUE){scale_y_log10(breaks =10^(-10:10),
                                           labels=scales::label_comma(),
                                           minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9)))}
    
  })
  output$Comparison2 <- renderPlotly({
    g<-ggplot(data=Main_dataset() %>% mutate(new_val=
                                               if(input$Strategy_CAGR=="Leveraged"){new_val}
                                             else if(input$Strategy_CAGR=="Original"){adjusted}
                                             else if(input$Strategy_CAGR=="SMA"){new_val_sma}
                                             else if(input$Strategy_CAGR=="EMA"){new_val_ema}
      ) %>% 
                mutate(new_val_cagr=new_val^(1/(as.numeric(difftime(date,min(date),units = "days")/365.25)))-1,
                                             new_val_5yr=((new_val/(lag(new_val,252*5)))^(1/5))-1,
                                             new_val_2yr=((new_val/(lag(new_val,252*2)))^(1/2))-1,
                                             new_val_10yr=((new_val/(lag(new_val,252*10)))^(1/10))-1
                                             ),aes(x=date))+
      geom_line(aes(color = "Cumulative CAGR",y=new_val_cagr))+
      geom_line(aes(y=new_val_2yr,color='2 year CAGR'))+
      geom_line(aes(y=new_val_10yr,color='10 year CAGR'))+
      xlab("Year")+
      ylab("CAGR (%)")+
      ggtitle(paste0("CAGRs of ",
                     gsub('^\\^|\\^$', '', Main_dataset()$symbol[1]),
                     " strategies since ",
                     Main_dataset()$date[1]))+
      theme(plot.title = element_text(size=24, face="bold",margin = margin(10, 0, 10, 0)))+
      theme_light()+
      scale_x_date(date_labels = "%Y",date_breaks="5 years")+
      geom_line(aes(y=new_val_5yr,color='5 year CAGR'))+
      scale_y_continuous(labels = scales::percent,limits=c(-1,1))+
      scale_colour_manual(values =c('Cumulative CAGR'='steelblue',
                                    '5 year CAGR'='red',
                                    "10 year CAGR"="green",
                                    "2 year CAGR"="yellow"))+
      theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
    
  })
  output$Difference <- renderPlotly({
    ggplot(data=Main_dataset(),aes(x=date))+geom_line(aes(y=new_val_sma/new_val_ema))+
      xlab("Year")+
      ylab("SMA/EMA")+
      ggtitle("using SMA vs EMA")+
      theme(plot.title = element_text(size=24, face="bold",margin = margin(10, 0, 10, 0)))+
      theme_light()+
      scale_x_date(date_labels = "%Y",date_breaks="5 years")
    
  })
  output$Cashflowplot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    today<-Cashflows()%>%filter(row_number()==n())%>%select(date)%>%pull()
    g<-ggplot(data=Cashflows(),aes(x=date))+
      geom_line(aes(y=DCA_val,color = "Leveraged"))+
      geom_line(aes(y=DCA_val2,color = "Unleveraged"))+
      geom_line(aes(y=DCA_val3,color = "Leveraged (SMA Strategy)"))+
      geom_line(aes(y=DCA_val4,color = "Leveraged (EMA Strategy)"))+
      xlab("Year")+
      ylab("Balance")+
      ggtitle(paste0("Initial Amount of $",input$initial, " with $",
                     input$cashflow,
                     " Monthly contributions"))+
      theme_light()+
      theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))+
      scale_x_date(date_labels = "%Y")+
      geom_line(aes(y=Contributions,color='Contributions'))+
      scale_colour_manual(name = 'Legend', 
                          values =c('Leveraged'='steelblue',
                                    'Contributions'='green',
                                    'Unleveraged'='red',
                                    "Leveraged (SMA Strategy)"="Grey",
                                    "Leveraged (EMA Strategy)"="Blue"))+scale_y_continuous(labels=scales::dollar_format())
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
      theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))+
      scale_y_continuous(labels = scales::percent)+
      scale_x_date(date_labels = "%Y",date_breaks="5 years")+
      geom_hline(yintercept=h,linetype = "dotted")
    
  })
  output$BorrowCosts <- renderPlotly({
    # US 3 month yield
    g<-ggplot(data=LIBOR,aes(x=as.Date(date),y=RATE,text=RATE*100))+
      geom_line(color="steelblue")+
      xlab("Year")+
      ylab("3 Month US Treasury Rate (%)")+
      ggtitle("Borrowing Costs")+
      theme_light()+ 
      theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))+
      scale_y_continuous(labels = scales::percent)+
      scale_x_date(date_labels = "%Y",date_breaks="5 years")
    ggplotly(g,tooltip = c("x","text"))
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
  output$showdays <- renderPlotly({  # Daily performance separated by if it is above or below the inputted moving average
    # Mean Daily returns based on inputted moving average
    ggplot(data=Main_dataset(),aes(y=growth,x=undersma,fill=undersma))+ geom_bar(stat = "summary", fun = "mean")+
      xlab("Moving Average")+
      ylab("Return (%)")+
      ggtitle("Daily returns Versus Moving Average")+
      theme_light()+
      scale_y_continuous(labels = scales::percent)
  })
  
  output$Intraday <- renderPlotly({  # Daily performance separated by if it is above or below the inputted moving average
    # Inter vs Intra Day
    x<-Main_dataset_load()%>% 
      mutate(InterDay=ifelse(row_number()==1,0,(open/lag(close)-1)))%>%
      mutate(IntraDay=close/open-1)%>%
      mutate(InterDay_value=cumprod(1+InterDay))%>%
      mutate(IntraDay_value=cumprod(1+IntraDay))%>%
      mutate(adjusted=adjusted/adjusted[1])%>%
      mutate(difference=InterDay_value/IntraDay_value)%>%
      select(date,IntraDay_value,InterDay_value,adjusted)%>%
      pivot_longer(!date,names_to = "Type", values_to = "Value")
    
    ggplot(data=x,aes(y=Value,x=date,color=Type))+
      theme_light()+
      geom_line()+
      xlab("Date")+
      ylab("Growth of $1")+
      ggtitle(paste0("Intra vs Inter day performance of ", gsub('^\\^|\\^$', '', input$ticker)," since ",input$date))+
      scale_color_manual(values = c("red", "darkgreen","steelblue"))+
      theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))+scale_y_log10(labels=scales::label_number())
    
  })
  output$Drawdown <- renderPlotly({
    # Drawdown for each strategy
    x<-Main_dataset()%>% 
      mutate(CumMax=cummax(adjusted))%>% 
      mutate(Original=-(1-(adjusted/CumMax)))%>% 
      mutate(CumMax=cummax(new_val))%>% 
      mutate(Leveraged=-(1-(new_val/CumMax)))%>% 
      mutate(CumMax=cummax(new_val_ema))%>% 
      mutate(Leveraged_ema=-(1-(new_val_ema/CumMax)))%>% 
      mutate(CumMax=cummax(new_val_sma))%>% 
      mutate(Leveraged_sma=-(1-(new_val_sma/CumMax)))%>%
      select(date,Original,Leveraged,Leveraged_ema,Leveraged_sma)%>%
      pivot_longer(!date,names_to = "Type",values_to="Drawdown")
    
    ggplot(data=x,aes(y=Drawdown,x=date,color=Type))+
      theme_light()+
      geom_line()+
      xlab("Date")+
      ylab("Drawdown (%)")+
      ggtitle(paste0("Drawdown of ", gsub('^\\^|\\^$', '', input$ticker)," since ",input$date))+
      scale_color_manual(values = c("red", "darkgreen","steelblue","Black"))+
      theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))+scale_y_continuous(labels=scales::label_percent())
    
  })
  output$Histogram <- renderPlotly({
    Main_dataset_load() %>% 
      tq_transmute(adjusted, periodReturn, period = "daily",col_rename = "return") %>%
      ggplot(aes(x=return))+
      geom_density(adjust = 1/5)+
      ggtitle("Daily Returns")+
      theme_light()+
      scale_x_continuous(labels = label_percent()) 
  })
  
  output$Composite <- renderPlotly({  # This Year compared to average of all previous years
    composite<-Main_dataset_load()%>%
      tq_transmute(adjusted, periodReturn, period = "daily",col_rename = "return") %>% 
      mutate(date=yday(date))%>%
      group_by(date)%>%
      summarise(avg_return=mean(return),count=n())%>%
      add_row(date=0,avg_return=0, .before = 1)%>%
      mutate(composite_value=cumprod(1+(avg_return)))%>%
      mutate(date=as.Date(date,origin="2022-12-31"),type="Composite")
    
    YTD<-Main_dataset_load()%>% 
      filter(date>"2022-12-31")%>% 
      mutate(composite_value=adjusted/adjusted[1])%>%
      select(date,composite_value)%>%mutate(type="Year to Date")
    
    Final_Data<-bind_rows(composite,YTD)
    
    ggplot(data=Final_Data,aes(y=composite_value,x=date,color=type))+
      geom_line()+
      scale_x_date(date_breaks = "months",date_labels = "%b")+
      theme_light()+
      theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))+
      ggtitle(paste("Average Yearly Composite of", gsub('^\\^|\\^$', '',input$ticker)," since", input$date))+
      ylab("Growth (Dec 31 = 100%)")+
      xlab("Date")+
      scale_colour_manual(values = c("steelblue", "red"))+
      scale_y_continuous(labels = label_percent()
      )
    
  })
  
  Main_dataset_load<-reactive({ # load yahoo finance data using quantmod and keep only date,ticker and split adjsuted closing price
    tq_get(input$ticker, get = "stock.prices", from = input$date)%>%
      drop_na(adjusted) %>% inner_join(LIBOR,by="date")
  })
  Main_dataset<- reactive({
    Main_dataset_load()%>%
      mutate(adjusted=adjusted/adjusted[1])%>% #normalizing data
      mutate(growth=ifelse(row_number()==1,0,adjusted/lag(adjusted)-1)) %>% # Calculate daily performance
      mutate(leverage=input$leverage)%>% # adding leverage and Expense ratio from input, since they will be changeable depending to MA status
      mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
      mutate(new_growth=growth*leverage-(ER/100)/252) %>% 
      mutate(new_val=cumprod(1+new_growth))%>% #Value of simulated Leveraged ETF
      mutate(CAGR=((new_val/new_val[1])^(1/(as.numeric(difftime(as.Date(date), as.Date(date[1])))/(60*60*24*365))))-1)%>% # finding CAGR by each day,
      mutate(SMA1=SMA(adjusted,input$MA))%>% #Simple and Exponential Moving Averages
      mutate(EMA1=EMA(adjusted,input$MA))%>%
      mutate(undersma=ifelse(adjusted<SMA1,"Yes","No"))%>% # SMA  Strategy
      mutate(action=ifelse(is.na(undersma),"BUY",
                           ifelse(undersma=="Yes","SELL","BUY"))) %>%
      mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash", # performance when under MA based on input
                                                                              0,                      # cash=0, 1x=1, -1=-1
                                                                              ifelse(input$Strategy=="-1", 
                                                                                     -1,
                                                                                     1)),leverage),leverage))%>%  
      mutate(ER=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                        -RATE,
                                                                        ifelse(input$Strategy=="-1", 
                                                                               1,
                                                                               0)),ER),ER))%>%  
      mutate(new_val_sma=cumprod(1+growth*leverage-(ER/100)/252))%>% # calculating value with SMA strategy
      mutate(leverage=input$leverage)%>%
      mutate(ER=input$ER+(RATE*(leverage-1))*100) %>% 
      mutate(underema=ifelse(adjusted<EMA1,"Yes","No"))%>% # EMA  Strategy
      mutate(action=ifelse(is.na(underema),"BUY",
                           ifelse(underema=="Yes","SELL","BUY"))) %>%
      mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                              0,
                                                                              ifelse(input$Strategy=="-1", 
                                                                                     -1,
                                                                                     1)),leverage),leverage))%>%  
      mutate(ER2=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                         -RATE,
                                                                         ifelse(input$Strategy=="-1", 
                                                                                1,
                                                                                0)),ER),ER))%>%  
      mutate(new_val_ema=cumprod(1+growth*leverage-(ER2/100)/252)) # calculating value with EMA strategy
    
    
  })
  
  Cashflows<-reactive( # only buy when below given ma
    {
      dates<-subset(Main_dataset(), !duplicated(substr(date, 1, 7), fromLast = FALSE)) # first of the month in the data
      Main_dataset() %>%
        
        mutate(contrib=ifelse(row_number()==1, # deciding if you should buy
                              input$initial,
                              ifelse(date %in% dates$date,
                                     input$cashflow,
                                     0)
        ))%>%
        mutate(shares=contrib/new_val)%>% #calculating how many theoretical shares you have and then find their monetary value
        mutate(total_shares=cumsum(shares))%>%
        mutate(DCA_val=total_shares*new_val)%>%
        
        mutate(shares3=contrib/new_val_sma)%>% # repeat for each strategy
        mutate(total_shares3=cumsum(shares3))%>%
        mutate(DCA_val3=total_shares3*new_val_sma)%>%
        
        mutate(shares2=contrib/adjusted)%>%
        mutate(total_shares2=cumsum(shares2))%>%
        mutate(DCA_val2=total_shares2*adjusted)%>%
        mutate(Contributions=cumsum(contrib))%>%
        
        mutate(shares4=contrib/new_val_ema)%>%
        mutate(total_shares4=cumsum(shares3))%>%
        mutate(DCA_val4=total_shares3*new_val_ema)
      
    })
  output$Different_MAs<-renderPlotly({
    #turning the same formulas into functions so they can be used for varying leverages and MAs
    get_sma_value<- function(SMA_input,leverage){
      x<-Main_dataset_load() %>% 
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(SMA1=SMA(adjusted,SMA_input))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(undersma=ifelse(adjusted<SMA1,"Yes","No"))%>% # SMA  Strategy
        mutate(action=ifelse(is.na(undersma),"BUY",
                             ifelse(undersma=="Yes","SELL","BUY"))) %>%
        mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                                0,
                                                                                ifelse(input$Strategy=="-1", 
                                                                                       -1,
                                                                                       1)),leverage),leverage))%>%
        mutate(ER=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                          -RATE,
                                                                          ifelse(input$Strategy=="-1", 
                                                                                 1,
                                                                                 0)),ER),ER))%>%  
        mutate(new_val_sma=cumprod(1+growth*leverage-(ER/100)/252))%>%slice(n()) %>% select(new_val_sma)%>%pull()#Leveraged Growth after expenses
      return(x)
    }
    get_ema_value<- function(EMA_input,leverage){
      x<-Main_dataset_load() %>% 
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(SMA1=EMA(adjusted,EMA_input))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(undersma=ifelse(adjusted<SMA1,"Yes","No"))%>% # SMA  Strategy
        mutate(action=ifelse(is.na(undersma),"BUY",
                             ifelse(undersma=="Yes","SELL","BUY"))) %>%
        mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                                0,
                                                                                ifelse(input$Strategy=="-1", 
                                                                                       -1,
                                                                                       1)),leverage),leverage))%>%
        mutate(ER=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                          -RATE,
                                                                          ifelse(input$Strategy=="-1", 
                                                                                 1,
                                                                                 0)),ER),ER))%>%  
        mutate(new_val_sma=cumprod(1+growth*leverage-(ER/100)/252))%>%slice(n()) %>% select(new_val_sma)%>%pull()#Leveraged Growth after expenses
      return(x)
    }
    get_final_value<- function(leverage){
      x<-Main_dataset_load()%>%
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(new_val=cumprod(1+growth*leverage -(ER/100)/252))%>% 
        slice(n()) %>% 
        select(new_val)%>%
        pull()#Leveraged Growth after expenses
      return(x)
    }
    
    y1<-seq(from=10,to=500,by=10) #MA from 10 to 500
    
    y2<-unlist(lapply(y1,get_sma_value,leverage=input$leverage))
    y4<-unlist(lapply(y1,get_ema_value,leverage=input$leverage))
    y3<-tibble(y1,y2,y4)%>%rename(MA=y1,SMA=y2,EMA=y4)%>% #combining data
      pivot_longer(!MA,names_to = "Type of MA", values_to = "Final Value") #wide to long
    
    final_value<-get_final_value(input$leverage)
    
    ggplotly(ggplot(data=y3,aes(x=MA,y=`Final Value`,colour=`Type of MA`))+
               geom_line()+
               theme_light()+
               theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))+
               geom_hline(yintercept = final_value,linetype = "dashed")+
               geom_text(color='Black',aes(150,final_value*1.025),label = paste0("Buy and Hold value: ",sprintf("%0.2f", round(final_value, digits = 2))))+
               ggtitle(" Different Moving Averages ")+
               xlab("Moving Average (Days)")+
               ylab("Growth of  $1"), tooltip=c("Final Value","MA"))
    
    
    
    
  })
  
  output$Different_Leverages<-renderPlotly({
    get_sma_value<- function(SMA_input,leverage){
      x<-Main_dataset_load() %>% 
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(SMA1=SMA(adjusted,SMA_input))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(undersma=ifelse(adjusted<SMA1,"Yes","No"))%>% # SMA  Strategy
        mutate(action=ifelse(is.na(undersma),"BUY",
                             ifelse(undersma=="Yes","SELL","BUY"))) %>%
        mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                                0,
                                                                                ifelse(input$Strategy=="-1", 
                                                                                       -1,
                                                                                       1)),leverage),leverage))%>%
        mutate(ER=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                          -RATE,
                                                                          ifelse(input$Strategy=="-1", 
                                                                                 1,
                                                                                 0)),ER),ER))%>%  
        mutate(new_val_sma=cumprod(1+growth*leverage-(ER/100)/252))%>%slice(n()) %>% select(new_val_sma)%>%pull()#Leveraged Growth after expenses
      return(x)
    }
    get_ema_value<- function(EMA_input,leverage){
      x<-Main_dataset_load() %>% 
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(EMA1=EMA(adjusted,EMA_input))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(underema=ifelse(adjusted<EMA1,"Yes","No"))%>% # SMA  Strategy
        mutate(action=ifelse(is.na(underema),"BUY",
                             ifelse(underema=="Yes","SELL","BUY"))) %>%
        mutate(leverage=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                                0,
                                                                                ifelse(input$Strategy=="-1", 
                                                                                       -1,
                                                                                       1)),leverage),leverage))%>%
        mutate(ER=ifelse(row_number()>1,ifelse(lag(action)=="SELL",ifelse(input$Strategy=="Cash",
                                                                          -RATE,
                                                                          ifelse(input$Strategy=="-1", 
                                                                                 1,
                                                                                 0)),ER),ER))%>%  
        mutate(new_val_ema=cumprod(1+growth*leverage-(ER/100)/252))%>%slice(n()) %>% select(new_val_ema)%>%pull()#Leveraged Growth after expenses
      return(x)
    }
    get_final_value<- function(leverage){
      x<-Main_dataset_load() %>% 
        mutate(leverage=leverage)%>%
        mutate(adjusted=adjusted/adjusted[1])%>%
        mutate(growth=ifelse(row_number()==1,0,(adjusted/lag(adjusted)-1)))%>%
        mutate(ER=input$ER+(RATE*(leverage-1))*100)%>%
        mutate(new_val=cumprod(1+growth*leverage-(ER)/100/252))%>%
        slice(n()) %>% select(new_val)%>%pull() # Leveraged Growth after expenses
      return(x)
    }
    
    x<-tq_get(input$ticker, get = "stock.prices", from = input$date)
    x<-x%>%select(symbol,date,adjusted)%>% drop_na(adjusted)
    x<-x %>%  mutate(growth=(adjusted/lag(adjusted)-1))
    
    ticker<-gsub('^\\^|\\^$', '',x$symbol[1])
    start<-x$date[1]
    
    y1<-seq(from=-1,to=10,by=0.1)
    y2<-unlist(lapply(y1,get_final_value))
    y3<-unlist(lapply(y1,get_sma_value,SMA_input=input$MA))
    y4<-unlist(lapply(y1,get_ema_value,EMA_input=input$MA))
    data<-tibble(y1,y2,y3,y4)%>%rename(leverage=y1,final_value=y2,sma_value=y3,ema_value=y4)%>% 
      pivot_longer(!leverage,names_to = "Strategy", values_to = "Final Value")
    
    ggplot(data=data,aes(y=`Final Value`,x=leverage))+
      geom_line(aes(color=Strategy))+
      theme_light()+
      ggtitle(paste0("Different leverage ratios for ",ticker, " since ", start))+
      xlab("Leverage")+
      ylab("Growth of  $1")+
      theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)))+
      geom_point(data=. %>%group_by(Strategy)%>%filter(`Final Value` == max(`Final Value`)))+
      geom_text(data=. %>%group_by(Strategy)%>%filter(`Final Value` == max(`Final Value`)),color='Black',aes(y=1.05*`Final Value`,
                                                                                                             label=paste0(trimws(leverage), 
                                                                                                                          "x Leverage: ",
                                                                                                                          sprintf("$%3.0f",`Final Value`))))
    
    
  })
  output$Message1<- renderUI({
    HTML("Sp500 total return up to 1988: ^SP500TR, earlier ^GSPC <br>
             Nasdaq 100: ^NDX<br>
             Bitcoin: BTC-USD<br>
             Ethereum: ETH-USD")
  })
  
  output$Message2<- renderUI({
    HTML(paste0(input$ticker," MEAN: ",round(mean(Main_dataset()$growth)*100,4),"<br>",
             input$ticker," STD DEV: ",round(STDEV(Main_dataset()$growth)*100,4)))
  })  
  output$Results_Table<- DT::renderDataTable({
    datatable(Cashflows()%>% filter(row_number()==n()|  # most recent
                                      date =="2020-02-20"| # precovid top
                                      date =="2002-10-09"| # post dotcom bottom
                                      date =="2007-10-09"| # pre GFC top
                                      date =="2009-03-05"| # GFC bottom
                                      date =="2000-03-10"| # dotcom top
                                      date =="2020-03-23"|  # covid bottom
                                      date =="2022-01-04"   #QE infinity Top
    )%>% 
      select(date,Contributions,DCA_val2,DCA_val,DCA_val3,DCA_val4),colnames = c('Date','Contributions', 'Unleveraged', 'Leveraged', 'Leveraged with SMA','Leveraged with EMA'))%>%
      formatCurrency(c('Contributions', 'DCA_val', 'DCA_val2', 'DCA_val3','DCA_val4'), currency = ' $',
                     mark = ',', before = TRUE)
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)