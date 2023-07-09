# Shiny app to sample from different distributions
# v.1.2 (df selector only visible if t distribution selected)
# (see v.1.1 for inactivation of always-visible df selector -- warning: source distribution bug!)
# 2020-07-13
# Athanassios Protopapas 
# protopap@gmail.com

library(shiny)
library(signs)

df_tabs <- tabsetPanel(
  id="extrainput",
  type="hidden",
  tabPanel("normal"),
  tabPanel("t",
           sliderInput("df",
                       "Degrees of freedom:",
                       min = 1,
                       max = 100,
                       value = 10)
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Distribution sampler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("dst", "Distribution:", choices=c("normal","t"), multiple=FALSE),
      df_tabs,
      sliderInput("nsamp",
                  "Number of samples:",
                  min = 5,
                  max = 1000,
                  step = 5,
                  value = 30),
      checkboxInput("ldens", "Show density curve (blue)", FALSE),
      checkboxInput("lnorm", "Show normal curve (purple)", FALSE),
      checkboxInput("ldpop", "Show source distribution (red)", FALSE),
      actionButton("goButton", "Sample")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
#      textOutput("PopVals"),
      textOutput("SamVals"),
#      tags$head(tags$style("#PopVals{font-size: 16px;}")),
      tags$head(tags$style("#SamVals{font-size: 16px;}"))
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if(input$dst=="t") {
      updateTabsetPanel(session,"extrainput",selected="t")
    } else {
      updateTabsetPanel(session,"extrainput",selected="normal")
    }
  })
  
  runSim <- reactive({
    
    nsamp <- input$nsamp
    input$goButton
    
    if (input$dst=="t") { 
      data<-rt(nsamp,input$df)
      xl <- -4
      xr <- 4
    } else {
      data<-rnorm(nsamp)
      xl <- -4
      xr <- 4
    }

    m<-mean(data)
    s<-sd(data)
    d<-density(data)
    xn<-seq(xl,xr,length=100)

    pvalstr <- sprintf("Population mean = %.2f ; standard deviation = %.2f",0.0,1.0)
    svalstr <- sprintf("Sample mean = %s ; standard deviation = %.2f",signs(m,accuracy=.01,add_plusses=FALSE),s)
    
    return(list(data=data,m=m,s=s,d=d,xn=xn,xl=xl,xr=xr,pvalstr=pvalstr,svalstr=svalstr))
    
  })
  
  output$distPlot <- renderPlot({
    
    #
    histdata <- runSim()$data
    nsamp <- input$nsamp
    xleft <-runSim()$xl
    xright<-runSim()$xr
    histdatap <- histdata[histdata>=xleft & histdata<=xright]
    hist(histdatap,freq=T,xlim=c(xleft,xright),breaks=(1:17-9)/2,
         ylim=c(0,nsamp%/%2+1),las=1,xlab="",cex.main=2,cex.lab=1.5,col="grey90",
         ylab="Number of samples",main=paste(nsamp,"samples from a",input$dst,"distribution",ifelse(input$dst=="t",paste("(df =",input$df,")"),"")),axes=F) # a bar for each count
    xrange<-xright-xleft+1
    xpts<-pretty(xleft:xright)
    axis(side=1,at=xpts,labels=signs(xpts,accuracy=1,add_plusses=TRUE),padj=-0.5)
    axis(side=2,las=1)
    
    #
    
    ldens <- input$ldens
    lnorm <- input$lnorm
    ldpop <- input$ldpop
    
    d <- runSim()$d
    xn <- runSim()$xn
    if (ldpop) {
      if (input$dst=="t") {
        lines(xn,nsamp/2*dt(xn,input$df),col="red",lwd=2,lty=c("12"))
      } else {
        lines(xn,nsamp/2*dnorm(xn),col="red",lwd=2,lty=c("12"))
      }
    }
    if (lnorm) lines(xn,nsamp/2*dnorm(xn,runSim()$m,runSim()$s),col="purple",lwd=2,lty=c("32")) 
    if (ldens) lines(d$x,nsamp/2*d$y,col="blue",lwd=1,lty=1)
    
  })
  
  output$PopVals <- renderText({
    runSim()$pvalstr
  })
  
  output$SamVals <- renderText({
    runSim()$svalstr
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


