# Shiny app to display probabilities on the normal distribution
# v.1.3
# 2020-07-16
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

val_tabs <- tabsetPanel(
  id="valinput",
  type="hidden",
  tabPanel("slider",
           sliderInput("val",
                       "Value:",
                       min = -4,
                       max = 4,
                       step = 0.01,
                       value = 0)
  ),
  tabPanel("numinp",
           numericInput("val2",
                        "Value:",
                        min = -4,
                        max = 4,
                        step = 0.01,
                        value = 0)
  )
)

area_tabs <- tabsetPanel(
  id="areainput",
  type="hidden",
  tabPanel("zt",
           radioButtons("typ", "Choose area", choices=c("None","Inside","Tail","Up to","Two tail"))
           ),
  tabPanel("Fchi",
           radioButtons("typ2", "Choose area", choices=c("None","Tail","Up to"))
           )
)

cvals <- list("normal","t","chisq","F")
names(cvals) <- c("normal","t","\u03C7\u00B2","F")

sigd <- function(x) {
  if (round(x)==x) { return(1) }
  xc <- as.character(x)
  return ( 10^(-(nchar(xc)-regexpr(".",xc,fixed=T)[1])) )
}

# Define UI for application
ui <- fluidPage(
   
  withMathJax(),
  # Application title
   titlePanel("Probability display"),
   # Sidebar with inputs
   sidebarLayout(
      sidebarPanel(
        selectInput("dst", "Distribution:", choices=cvals, multiple=FALSE),
        df_tabs,
        val_tabs,
        actionButton("sliderButton", "Slider"),
        actionButton("numinpButton", "Field"),
        HTML("<br><br>"),
        area_tabs,
        #radioButtons("typ", "Choose area", choices=c("None","Inside","Tail","Up to","Two tail")),
         sliderInput("lim",
                     "Graph display limits:",
                     min=1,
                     max=10,
                     step=1,
                     value=4)
      ),
      
      # Output
      mainPanel(
         plotOutput("distPlot"),
         uiOutput("distVal"),
         textOutput("distProb"),
         tags$head(tags$style("#distProb{font-size: 20px;}")),
         tags$head(tags$style("#distVal{font-size: 20px;}"))
      )
   )
)


server <- function(input, output, session) {

  sliderState <- TRUE
  values <- reactiveValues(vval = 0)
  values <- reactiveValues(atyp = "None")
  observe({
    vval1 <- input$val
    vval2 <- input$val2
    if (sliderState) {
      values$vval <- vval1
    } else {
      values$vval <- vval2
    }
  })
  observe({
    atyp1 <- input$typ
    atyp2 <- input$typ2
    if (input$dst %in% c("normal","t")) {
      values$atyp <- atyp1
    } else {
      values$atyp <- atyp2
    }
  })
  observe({
    updateTabsetPanel(session,"extrainput",selected=input$dst)
  })
  observe({
    updateTabsetPanel(session,"areainput",selected=ifelse(input$dst %in% c("normal","t"),"zt","Fchi"))
  })
  observeEvent(input$sliderButton,{
    updateTabsetPanel(session,"valinput",selected="slider")
    sliderState <<- TRUE
    values$vval <- max(min(round(values$vval,2),4),-4)
    updateSliderInput(session,"val",value=values$vval)
  })
  observeEvent(input$numinpButton,{
    updateTabsetPanel(session,"valinput",selected="numinp")
    sliderState <<- FALSE
    updateNumericInput(session,"val2",value=values$vval)
  })
  

  
  output$distPlot <- renderPlot({
    
    t <- values$vval
    limits <- input$lim
    
    if(input$dst=="normal") {
      minx <- -limits
      maxx <- limits
      dfunc <- dnorm
      mstr <- "Normal distribution"
      xstr <- "z"
    } else if(input$dst=="t") {
      minx <- -limits
      maxx <- limits
      dfunc <- function(x,...){dt(x,df=input$df,...)}
      mstr <- paste("t distribution with",input$df,ifelse(input$df==1,"degree","degrees"),"of freedom")
      xstr <- "t"
    } else if(input$dst=="chisq") {
      minx <- 0
      maxx <- limits
      dfunc <- function(x,...){dchisq(x,df=input$dfchi,...)}
      mstr <- bquote(chi^2~"distribution with"~.(input$dfchi)~.(ifelse(input$dfchi==1,"degree","degrees"))~"of freedom")
      xstr <- expression(chi^2)
    } else if(input$dst=="F") {
      minx <- 0
      maxx <- limits
      dfunc <- function(x,...){df(x,df1=input$df1,df2=input$df2,...)}
      mstr <- paste("F distribution with",input$df1,"and",input$df2,"degrees of freedom")
      xstr <- "F"
    } 
    
    x<-seq(from=minx,to=maxx,length.out=2000)
    y<-dfunc(x)
    
    plot(x,y,type="l",las=1,cex.main=2,cex.lab=1.5,
         xlim=c(minx,maxx),ylim=c(0,ifelse(input$dst=="F",0.8,0.4)),
         main=mstr,xlab=xstr,ylab="Probability density")
    
    if (is.na(t)) {
      t<-0
    } else {
      if ( (t>=minx)&(t<=maxx)) {
        points(t,dfunc(t),type="h",col="red")		
      }
    }

    if ( (input$dst %in% c("normal","t")) | (t>=0) ) { # only positive values allowed for F/chisq
      
      if (values$atyp=="Tail") {
        if(abs(t)<limits) {
          x1<-seq(from=t,to=limits,length.out=round(1000*(limits-t)))
          points(x1,dfunc(x1),type="h",col="grey70")	
          points(t,dfunc(t),type="h",col="red")
        }
        points(x,y,type="l")
        
      } else if (values$atyp=="Up to") {
        if(abs(t)<limits) {
          x1<-seq(from=(-limits),to=(t),length.out=round(1000*(limits+t)))
          points(x1,dfunc(x1),type="h",col="grey70")	
          points(t,dfunc(t),type="h",col="red")
        }       
        points(x,y,type="l")
      }
      
    }
    if (input$dst %in% c("normal","t")) { # options valid only for symmetric distributions
      
      if (values$atyp=="Two tail") {
        t<-abs(t)
        if(t<limits) {
          x1<-seq(from=(-limits),to=(-t),length.out=round(1000*(limits-t)))
          points(x1,dfunc(x1),type="h",col="grey70")	
          points(-t,dfunc(t),type="h",col="red")
          
          x1<-seq(from=t,to=limits,length.out=round(1000*(limits-t)))
          points(x1,dfunc(x1),type="h",col="grey70")	
          points(t,dfunc(t),type="h",col="red")
        }       
        points(x,y,type="l")
        
      } else if (values$atyp=="Inside") {
        t<-abs(t)
        x1<-seq(from=(-t),to=t,length.out=round(1000*2*t))
        points(x1,dfunc(x1),type="h",col="grey70")	
        
        points(-t,dfunc(-t),type="h",col="red")
        points(t,dfunc(t),type="h",col="red")
        
        points(x,y,type="l")
        
      } 
      
    } 
  })
  
  getpval <- reactive({
    
    t <- values$vval
    if ( is.na(t) | is.null(t) ) {
      pval <- NULL
      return(list(pstr="",valstr=""))
    }
    
    limits <- input$lim
    if (input$dst=="t") {
      pfunc <- function(x,...){pt(x,df=input$df,...)}
      valstr <- paste("t",signs(t,accuracy=ifelse(sliderState,0.01,sigd(t)),add_plusses=TRUE),sep="=")
    } else if (input$dst=="normal") {
      pfunc <- pnorm
      valstr <- paste("z",signs(t,accuracy=ifelse(sliderState,0.01,sigd(t)),add_plusses=TRUE),sep="=")
    } else if (input$dst=="chisq") {
      pfunc <- function(x,...){pchisq(x,df=input$dfchi,...)}
      if (t >= 0) {
        valstr <- paste("\\(\\chi^2\\)",signs(t,accuracy=ifelse(sliderState,0.01,sigd(t)),add_plusses=FALSE),sep="=")
      } else {
        valstr <- paste("Negative values of \\(\\chi^2\\) are not possible")
      }
    } else if (input$dst=="F") {
      pfunc <- function(x,...){pf(x,df1=input$df1,df2=input$df2,...)}
      if (t >= 0) {
        valstr <- paste("F",signs(t,accuracy=ifelse(sliderState,0.01,sigd(t)),add_plusses=FALSE),sep="=")
      } else {
        valstr <- paste("Negative values of F are not possible")
      }
    } else { valstr <- "" }
  
    if (input$dst %in% c("normal","t")) {
      if (values$atyp=="Tail") {
        pval <- 1-pfunc(t)
      } else if (values$atyp=="Up to") {
        pval <- pfunc(t)
      } else if (values$atyp=="Inside") {
        t<-abs(t)
        pval <- 1-(2*(1-pfunc(t)))
      } else if (values$atyp=="Two tail") {
        t<-abs(t)
        pval <- 2*(1-pfunc(t))
      } else { pval <- NULL }
    } else if ( t>= 0) { # Only positive values allowed for F and chi square
      if (values$atyp=="Tail") {
        pval <- 1-pfunc(t)
      } else if (values$atyp=="Up to") {
        pval <- pfunc(t)
      } else { pval <- NULL }      
    } else { pval <- NULL }
    
    if(!is.null(pval)) {
      # https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
      pstr <- paste("Probability:",formatC(signif(pval,digits=3), digits=3,format="fg", flag="#"))
    } else {
      pstr <- ""
    }
    return(list(pstr=pstr,valstr=valstr))
  })

  
  output$distProb <- renderText({
    getpval()$pstr
  })
  output$distVal <- renderUI({
    withMathJax(helpText(getpval()$valstr))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

