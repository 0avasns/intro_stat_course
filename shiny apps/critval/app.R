# Shiny app to display critical values for selected distributions
# v.1.2 (slider/field option)
# 2020-07-16
# Athanassios Protopapas 
# protopap@gmail.com

library(shiny)

DEFVAL = 95

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

conf_tabs <- tabsetPanel(
  id="confinput",
  type="hidden",
  tabPanel("slider",
           sliderInput("conf",
                       "Desired confidence level:",
                       min = 50,
                       max = 99,
                       step = 1,
                       value = DEFVAL)
  ),
  tabPanel("numinp",
           numericInput("conf2",
                       "Desired confidence level:",
                       min = 50,
                       max = 99.999,
                       step = 0.001,
                       value = DEFVAL)
  )
)

cvals <- list("normal","t","chisq","F")
names(cvals) <- c("normal","t","\u03C7\u00B2","F")

fstr <- function(x) {
  if (round(x)==x) { return("%.0f") }
  xc <- as.character(x)
  return ( sprintf("%%.%df",nchar(xc)-regexpr(".",xc,fixed=T)[1] ) )
}

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Compute critical values"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("dst", "Distribution:", choices=cvals, multiple=FALSE),
        df_tabs,
        conf_tabs,
        actionButton("sliderButton", "Slider"),
        actionButton("numinpButton", "Field")
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

  sliderState <- TRUE
  values <- reactiveValues(cval = DEFVAL)
  observe({
    cval1 <- input$conf
    cval2 <- input$conf2
    if (sliderState) {
      values$cval <- cval1
    } else {
      values$cval <- cval2
    }
  })
  observe({
      updateTabsetPanel(session,"extrainput",selected=input$dst)
  })
  observeEvent(input$sliderButton,{
    updateTabsetPanel(session,"confinput",selected="slider")
    sliderState <<- TRUE
    values$cval <- max(min(round(values$cval,0),99),50)
    updateSliderInput(session,"conf",value=values$cval)
    })
  observeEvent(input$numinpButton,{
    updateTabsetPanel(session,"confinput",selected="numinp")
    sliderState <<- FALSE
    updateNumericInput(session,"conf2",value=values$cval)
    })
  
  getvals <- reactive ({
    cval <- values$cval
    #ntail <- 2
    pval <- 1.0-cval/100.0
    tstr <- " "
    ntails=1
    
    if(input$dst=="normal") {
      minx <- -4
      maxx <- 4
      dfunc <- dnorm
      mstr <- "Normal distribution"
      ntails <- as.numeric(input$ntails)
      q <- qnorm(1.0-pval/ntails)
      tstr <- paste(" for a ",ifelse(ntails==1,"one","two"),"-tailed test ",sep="")
    } else if(input$dst=="t") {
      minx <- -4
      maxx <- 4
      dfunc <- function(x,...){dt(x,df=input$df,...)}
      mstr <- paste("t distribution with",input$df,ifelse(input$df==1,"degree","degrees")," of freedom")
      ntails <- as.numeric(input$ntailst)
      q <- qt(1.0-pval/ntails,input$df)
      tstr <- paste(" for a ",ifelse(ntails==1,"one","two"),"-tailed test ",sep="")
    } else if(input$dst=="chisq") {
      minx <- 0
      maxx <- 100
      dfunc <- function(x,...){dchisq(x,df=input$dfchi,...)}
      mstr <- bquote(chi^2~"distribution with"~.(input$dfchi)~.(ifelse(input$dfchi==1,"degree","degrees"))~" of freedom")
      q <- qchisq(1.0-pval,input$dfchi)
    } else if(input$dst=="F") {
      minx <- 0
      maxx <- 5
      dfunc <- function(x,...){df(x,df1=input$df1,df2=input$df2,...)}
      mstr <- paste("F distribution with",input$df1,"and",input$df2,"degrees of freedom")
      q <- qf(1.0-pval,input$df1,input$df2)
    } 
    x<-seq(from=minx,to=maxx,length.out=2000)
    y<-dfunc(x)
    
    pstr <- sprintf(ifelse(sliderState,"%d",fstr(cval)),cval)
    valstr <- sprintf("The critical value%sat the %s%% level is %.3f",tstr,pstr,q)
    
    return(list(x=x,y=y,mstr=mstr,q=q,minx=minx,maxx=maxx,ntails=ntails,valstr=valstr))
    
  })
  
  output$distPlot <- renderPlot({
    
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
    getvals()$valstr
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

