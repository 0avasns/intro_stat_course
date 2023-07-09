# Shiny app to simulate coin tosses by multiple people
# v.1.1
# 2020-06-30
# Athanassios Protopapas 
# protopap@gmail.com

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Coin toss simulator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("persons",
                     "Number of people:",
                     min = 5,
                     max = 1000,
                     step = 5,
                     value = 100),
         sliderInput("tosses",
                     "Number of coins:",
                     min = 5,
                     max = 1000,
                     step = 5,
                     value = 10),
         checkboxInput("ldens", "Show density curve", FALSE),
         checkboxInput("lbinom", "Show binomial distribution", FALSE),
         checkboxInput("lnorm", "Show normal distribution", FALSE),
         actionButton("goButton", "Toss!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
  
  runSim <- reactive({

    persons <- input$persons
    tosses <- input$tosses
    input$goButton
    
    data<-numeric(persons)
    for (i in 1:persons) {
      ser<-rbinom(tosses,1,0.5)
      data[i]<-sum(ser)
    }
    xleft <-max(0,tosses/2-2*sqrt(tosses))-1
    xright<-min(tosses,tosses/2+2*sqrt(tosses))
    
    d<-density(data,bw=log(tosses,10))
    xn<-seq(xleft,xright,length=100)/tosses
    Xb<-seq(0,tosses)
    
    return(list(data=data,persons=persons,tosses=tosses,d=d,xn=xn,Xb=Xb))
    
  })
  
  output$distPlot <- renderPlot({

    #
    persons<-runSim()$persons
    tosses<-runSim()$tosses
    histdata <- runSim()$data
    xleft <-max(0,tosses/2-2*sqrt(tosses))-1
    xright<-min(tosses,tosses/2+2*sqrt(tosses))
    hist(histdata,freq=T,xlim=c(xleft,xright),
         ylim=c(0,2*persons/sqrt(tosses)),las=1,breaks=-1:tosses,xlab="",cex.main=2,cex.lab=1.5,col="grey90",
         ylab="Number of persons",main=paste(persons,"people toss a coin",tosses,"times each"),axes=F) # a bar for each count
    xrange<-xright-xleft+1
    xpts<-pretty(xleft:xright)
    mtext("Number 'heads'",side=1,line=4,cex=1.5)
    axis(side=1,at=xpts-0.5,labels=xpts,padj=-0.5)
    axis(side=1,at=xpts-0.5,labels=paste(round(xpts/tosses*100,1),"%",sep=""),padj=1.5)
    axis(side=2,las=1)
    
    #
    
    ldens <- input$ldens
    lbinom <- input$lbinom
    lnorm <- input$lnorm
    
    d <- runSim()$d
    if (ldens) lines(d$x-0.5,persons*d$y,col="blue",lwd=1.5)
    xn <- runSim()$xn
    if (lnorm) lines(tosses*xn-0.5,persons*dnorm(tosses*xn,tosses/2,tosses/2/sqrt(tosses)),col="red",lwd=2) 
    Xb <- runSim()$Xb
    if (lbinom) lines(Xb-0.5,persons*dbinom(Xb,tosses,0.5),col="green")
    
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

