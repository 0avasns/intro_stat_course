# Shiny app to display p values for selected distributions
# v.1.1 # added abs to norm/t values
# 2020-10-07
# Athanassios Protopapas 
# protopap@gmail.com

library(shiny)

df_tabs <- tabsetPanel(
  id="extrainput",
  type="hidden",
  tabPanel("normal",
           selectInput("ntails",
                       "Test directionality:",
                       c("One-tailed"=1,"Two-tailed"=2),
                       selected=2)
  ),
  tabPanel("t",
           sliderInput("df",
                       "Degrees of freedom:",
                       min = 1,
                       max = 100,
                       value = 10),
           selectInput("ntailst",
                       "Test directionality:",
                       c("One-tailed"=1,"Two-tailed"=2),
                       selected=2)
  ),
  tabPanel("chisq",
           sliderInput("dfchi",
                       "Degrees of freedom:",
                       min = 1,
                       max = 20,
                       value = 1)
  ),
  tabPanel("F",
           sliderInput("df1",
                       "Degrees of freedom 1:",
                       min = 1,
                       max = 20,
                       value = 1),
           sliderInput("df2",
                       "Degrees of freedom 2:",
                       min = 1,
                       max = 100,
                       value = 10)
  )
)

cvals <- list("normal","t","chisq","F")
names(cvals) <- c("normal","t","\u03C7\u00B2","F")

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Compute p values"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("dst", "Distribution:", choices=cvals, multiple=FALSE),
      df_tabs,
      numericInput("stat",
                  "Enter the value of your statistic:",
                  min = 0.0,
                  max = 100.0,
                  value = NA)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("CritVal"),
      tags$head(tags$style("#CritVal{font-size: 20px;}"))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    updateTabsetPanel(session,"extrainput",selected=input$dst)
  })
  
  getvals <- reactive ({

    if (is.na(input$stat) | is.null(input$stat)) {
      return(NULL)
    }
    
    #ntail <- 2
    sval <- input$stat
    tstr <- " "
    ntails=1
    
    if(input$dst=="normal") {
      minx <- -4
      maxx <- 4
      dfunc <- dnorm
      mstr <- "Normal distribution"
      sval <- abs(sval)
      ntails <- as.numeric(input$ntails)
      pval <- (1.0-pnorm(sval))*ntails
      tstr <- paste(" for a ",ifelse(ntails==1,"one","two"),"-tailed test ",sep="")
    } else if(input$dst=="t") {
      minx <- -4
      maxx <- 4
      dfunc <- function(x,...){dt(x,df=input$df,...)}
      mstr <- paste("t distribution with",input$df,ifelse(input$df==1,"degree","degrees")," of freedom")
      sval <- abs(sval)
      ntails <- as.numeric(input$ntailst)
      pval <- (1.0-pt(sval,input$df))*ntails
      tstr <- paste(" for a ",ifelse(ntails==1,"one","two"),"-tailed test ",sep="")
    } else if(input$dst=="chisq") {
      minx <- 0
      maxx <- 100
      dfunc <- function(x,...){dchisq(x,df=input$dfchi,...)}
      mstr <- bquote(chi^2~"distribution with"~.(input$dfchi)~.(ifelse(input$dfchi==1,"degree","degrees"))~" of freedom")
      pval <- 1.0-pchisq(sval,input$dfchi)
    } else if(input$dst=="F") {
      minx <- 0
      maxx <- 5
      dfunc <- function(x,...){df(x,df1=input$df1,df2=input$df2,...)}
      mstr <- paste("F distribution with",input$df1,"and",input$df2,"degrees of freedom")
      pval <- 1.0-pf(sval,input$df1,input$df2)
    } 
    x<-seq(from=minx,to=maxx,length.out=2000)
    y<-dfunc(x)
    
    if (pval >= 0.001) { 
      pstr <- sprintf("%.3f",pval) 
    } else {
        pstr <- "< 0.001"
    }
    valstr <- sprintf("The p value%sis %s",tstr,pstr)
    
    return(list(x=x,y=y,mstr=mstr,q=sval,pval=pval,minx=minx,maxx=maxx,ntails=ntails,valstr=valstr))
    
  })
  
  output$distPlot <- renderPlot({

    if(is.null(getvals())) { return(NULL) }
        
    x <- getvals()$x
    y <- getvals()$y
    minx <- getvals()$minx
    maxx <- getvals()$maxx
    plot(x,y,type="l",las=1,cex.main=2,cex.lab=1.5,
         xlim=c(minx,maxx),ylim=c(0,ifelse(input$dst=="F",0.8,0.4)),
         main=getvals()$mstr,xlab="",ylab="Probability density")
    q <- getvals()$q
    if (q>minx & q<maxx) {
      x1 <- x[which(x>=q)]
      y1 <- y[which(x>=q)]
      points(x1,y1,type="h",col="grey70")
      points(x1,y1,type="l")
      if ((input$dst=="t" & getvals()$ntails==2) | (input$dst=="normal" & getvals()$ntails==2) ) {
        x1 <- x[which(x<= -q)]
        y1 <- y[which(x<= -q)]
        points(x1,y1,type="h",col="grey70")
        points(x1,y1,type="l")
      }
    }
    
  })
  
  output$CritVal <- renderText({
    if(is.null(getvals())) { return(NULL) } else { getvals()$valstr }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

