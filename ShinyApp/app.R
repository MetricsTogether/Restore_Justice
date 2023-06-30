#### Environment and library Loading ####

library(shiny)
library(tidyverse)
library(lubridate)
library(stringi)
library(readxl)
library(censusapi)
library(tigris)
library(sf)
library(DT)

### Uploading the RJ Color Scheme

# defining the color scheme for Restore Justice
mypal <- c("#213159", "#3D6098","#9fc5e8","#F04B4C","#80475E","#FEC20E", "#b4b4b4")

cvi_colours = list(
  RJ_colors = c("#213159","#3D6098","#9fc5e8","#F04B4C","#80475E","#FEC20E", "#b4b4b4")
)

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


cvi_palettes("RJ_colors", type = "discrete")


scale_colour_cvi_d = function(name) {
  ggplot2::scale_colour_manual(values = cvi_palettes(name,
                                                     type = "discrete"))
}

scale_fill_cvi_d = function(name) {
  ggplot2::scale_fill_manual(values = cvi_palettes(name,
                                                   type = "discrete"))
}


# ### Read in latest IDOC data ###
# 
# 

full_IDOC <- read_rds("fullIDOC.rds")

full_IDOC <- full_IDOC %>% mutate("Age Range" = case_when(age < 18 ~ "<18",
                                                          age >18 & age<26 ~ "18 to 25",
                                                          age >=26 & age<36 ~"26 to 35",
                                                          age >=36 & age<51 ~ "36 to 50",
                                                          age >=51 & age<65 ~ "51 to 65",
                                                          age >65 ~ "over 65")) %>%
  mutate("TIS Class"= case_when(grepl("85%",`Truth in Sentencing`) ~"85%",
                                grepl("75%",`Truth in Sentencing`) ~"75%",
                                grepl("100%",`Truth in Sentencing`) ~"100%",
                                grepl("Day-for-Day",`Truth in Sentencing`) & year(`Current Admission Date`)>1998 ~"Post-TIS 50%",
                                grepl("Day-for-Day",`Truth in Sentencing`) & year(`Current Admission Date`)<=1998 ~"Pre-TIS 50%"
  ))



## pull OMR
OMR <- read_csv("OMR.csv")



# Load functions to clean ACS data
yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

fips_to_acs <- function(data){
  t <- left_join(data,fips_codes,by=c("state"="state_code","county"="county_code"))
  t
}



#### Shiny app ####


### UI (this is the visual component to the app) ###

ui <- navbarPage(
  
  ## Running header with identification of local data (link) and last update

  
  # Application title
  titlePanel(div(span(paste0("Restore Justice IDOC Data. Last Update: ", today())),
                 textOutput(outputId = "txttimeperiod"),
                 style={'padding-bottom: 35px; font-size: 10pt'},

                 
              
                 )), 

  
  #Quickstats panel
  # tabPanel("Quick Stats",
  #          mainPanel(
  #            "How many people",
  #            selectInput("first select",
  #                        label="select",
  #                        choices = c("were sentenced","under (age)","will exit"),
  #                        selected = "were sentenced"),
  #            "before 2018"
  #            #dataTableOutput("quickstats")
  #            # How many people dropdown (were sentenced, under (age), will exit)
  #          )),
# Calling the Layout


tabPanel("Data Explorer",
         h4("Welcome to the Restore Justice Data Portal!"),
         h5("Explore data from the Illinois Department of Corrections (IDOC) Prison Stock files below by selecting a time period and maximum age at sentence at which to explore."),


  sidebarPanel(
    selectInput("timeperiod","Time Period",choices =sort(unique(full_IDOC$FileName),decreasing = TRUE)),
    numericInput("SentAge", "Individuals Sentenced before age:", value = 100),
    tableOutput("Statistics"),
    #actionButton(inputId = "runreport",label = "Run Website Report")
    uiOutput("ui_open_tab_button")
    #tabs
  ),
  
# Main Panel   
  mainPanel(
    # with a tabset
    tabsetPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Crime Class",
                           tabsetPanel(
                           tabPanel("Age",
                                    plotOutput("plot1"),
                                    dataTableOutput("table1"),
                                    downloadButton("download1","Download")),
                           tabPanel("Race",plotOutput("plot2"),
                                    dataTableOutput("table2"),
                                    downloadButton("download2","Download")))),
                  
                  
                  tabPanel("Sentence Date",
                           tabsetPanel(
                           tabPanel("Age", plotOutput("plot3"),
                                    dataTableOutput("table3"),
                                    downloadButton("download3","Download")),
                           tabPanel("Race",plotOutput("plot4"),
                                    dataTableOutput("table4"),
                                    downloadButton("download4","Download")))),
                  tabPanel("Length of Sentence",
                           tabsetPanel(
                             tabPanel("Age",
                                      plotOutput("plot7"),
                                      dataTableOutput("table7"),
                                      downloadButton("download7","Download")),
                             tabPanel("Race",plotOutput("plot8"),
                                      dataTableOutput("table8"),
                                      downloadButton("download8","Download")))),
                  
                  tabPanel("Truth In Sentencing", 
                           tabsetPanel(
                           tabPanel("Age", plotOutput("plot5"),
                                    dataTableOutput("table5"),
                                    downloadButton("download5","Download")),
                           tabPanel("Race",plotOutput("plot6"),
                                    dataTableOutput("table6"),
                                    downloadButton("download6","Download")))),
                  
                  )
      ),

    
  ),


),



tabPanel("Incidents Tool",
         mainPanel(
           selectInput(inputId = "OMRvar",label = "Select Incident Type:",choices = c(unique(OMR$`Incident Type`))),
           plotOutput("OMR")
           # How many people (were sentenced, age, will exit)
         )),

tabPanel("Geographic Explorer",
         mainPanel(
           plotOutput("ILmap")
           
         )),

#downloadButton("downloadData", "Download"),
tags$img(src="RJLogo.png",align="right"),
tags$img(src="MTLogo.png",align="bottomright"),
h6("For questions or issues with the app contact info@metricstogether.com or see our github at https://github.com/MetricsTogether/RestoreJustice"),

tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }")) # center text

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$ui_open_tab_button <- renderUI({
    shiny::a(
      h4(paste0("Run Report: ",input$timeperiod),
         class = "btn btn-default action-button",
         style = "fontweight:600"),
      target = "_blank",
      href = paste0("https://www.metricstogether.com/restore-justice-",input$timeperiod)
    )
  })
  output$runreport <- renderUI({
    tags$iframe(src=paste0("https://www.metricstogether.com/restore-justice-",input$timeperiod), height=1000, width="100%")

  })

full_IDOC$`Sentence Years` <- as.numeric(full_IDOC$`Sentence Years`)
  
stock_sent <- reactive({full_IDOC %>% 
    filter(FileName == input$timeperiod, `Sentence Age`<input$SentAge) %>% 
    mutate("Sentence Year"=year(`Sentence Date`))%>% 
    mutate("Sentence Years2"= case_when(`Sentence Years`>100 ~ 100,                   
                                        `Sentence Years`<100 ~ `Sentence Years`))})

                              

  
output$txttimeperiod <- renderText({input$timeperiod})

#  possible alternatives (graphs, statistics, and the ability to pull raw data )  
  
  output$plot1 <- renderPlot({
    ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`Crime Class`,fill=stock_sent()$`Age Range`))+
      labs(title="People Committed by Crime Class", x="Crime Class",fill="Age Range")+scale_fill_cvi_d("RJ_colors")
  }) 
  output$table1 <- renderDataTable({
    stock_sent() %>% 
      group_by(`Age Range`,`Crime Class`) %>% 
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`)},
    filter = 'top'
  )

  output$download1 <- downloadHandler(
    filename = function(){"SentenceData.csv"}, 
    content = function(fname){
      write.csv(stock_sent() %>%  
                  group_by(`Age Range`,`Crime Class`) %>% 
                  summarize(n()) %>%
                  pivot_wider(names_from = `Age Range`, values_from = `n()`),
                fname)
    }
  )

  output$plot2 <- renderPlot({
    ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`Crime Class`,
                           fill=stock_sent()$`Race`))+
      labs(title="People Committed by Crime Class", x="Crime Class",fill="Race")+scale_fill_cvi_d("RJ_colors")
  })
  output$table2 <- renderDataTable({
    stock_sent() %>%
      group_by(`Race`,`Crime Class`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Race`, values_from = `n()`)},
    filter = 'top'
  )

  output$download2 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv(stock_sent() %>%
                  group_by(`Race`,`Crime Class`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Race`, values_from = `n()`),
                fname)
    }
  )


  output$plot3 <- renderPlot({
    ggplot()+
      geom_histogram(mapping=aes(x=stock_sent()$`Sentence Year`,
                                 fill=stock_sent()$`Age Range`))+
      labs(title="People sentenced per year", x="Sentence Year",fill="Age Range")+scale_fill_cvi_d("RJ_colors")
  })
  output$table3 <- renderDataTable({
    stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
            group_by(`Age Range`,`Sentence Year`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
      arrange(-`Sentence Year`)},
    filter = 'top'
  )

  output$download3 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv(stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                  group_by(`Age Range`,`Sentence Year`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
                  arrange(-`Sentence Year`),
                fname)
    }
  )

 output$plot4 <- renderPlot({
   ggplot()+
     geom_histogram(mapping=aes(x=stock_sent()$`Sentence Year`,
                                fill=stock_sent()$`Race`))+
     labs(title="People sentenced per year", x="Sentence Year",fill="Race")+scale_fill_cvi_d("RJ_colors")
 })

 output$table4 <- renderDataTable({
   stock_sent() %>%
     filter(`Sentence Age`<input$SentAge) %>%
     group_by(`Race`,`Sentence Year`) %>%
     summarize(n()) %>%
     pivot_wider(names_from = `Race`, values_from = `n()`) %>%
     arrange(-`Sentence Year`)},
 filter = 'top'
 )

 output$download4 <- downloadHandler(
   filename = function(){"SentenceData.csv"},
   content = function(fname){
     write.csv(stock_sent() %>%
                 filter(`Sentence Age`<input$SentAge) %>%
                 group_by(`Race`,`Sentence Year`) %>%
                 summarize(n()) %>%
                 pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                 arrange(-`Sentence Year`),
               fname)
   }
 )

  output$plot5 <- renderPlot({
    ggplot()+
      geom_bar(mapping=aes(x=stock_sent()$`TIS Class`,
                                 fill=stock_sent()$`Age Range`))+
      labs(title="People per TIS Class", x="TIS Class",fill="Age Range")+scale_fill_cvi_d("RJ_colors")  })

  output$table5 <- renderDataTable({
    stock_sent() %>%
      group_by(`Age Range`,`TIS Class`) %>%
      summarize(n()) %>%
      pivot_wider(names_from = `Age Range`, values_from = `n()`)},
    filter = 'top'
  )

  output$download5 <- downloadHandler(
    filename = function(){"SentenceData.csv"},
    content = function(fname){
      write.csv( stock_sent() %>%
                   group_by(`Age Range`,`TIS Class`) %>%
                   summarize(n()) %>%
                   pivot_wider(names_from = `Age Range`, values_from = `n()`),
                fname)
    }
  )

    output$plot6 <- renderPlot({
      ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`TIS Class`,
                                   fill=stock_sent()$`Race`))+
        labs(title="People per TIS Class", x="TIS Class",fill="Race")+scale_fill_cvi_d("RJ_colors")})



 output$table6 <- renderDataTable({
   stock_sent() %>%
     group_by(`Race`,`TIS Class`) %>%
     summarize(n()) %>%
     pivot_wider(names_from = `Race`, values_from = `n()`)},
   filter = 'top'
 )

output$download6 <- downloadHandler(
  filename = function(){"SentenceData.csv"},
  content = function(fname){
    write.csv(  stock_sent() %>%
                  group_by(`Race`,`TIS Class`) %>%
                  summarize(n()) %>%
                  pivot_wider(names_from = `Race`, values_from = `n()`),
               fname)
  }
)
    output$plot7 <- renderPlot({
      ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`Sentence Years2`,
                             fill=stock_sent()$`Age Range`))+
        labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Age Range")+scale_fill_cvi_d("RJ_colors") })

    output$table7 <- renderDataTable({
      stock_sent() %>%
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Age Range`,`Sentence Years2`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
        arrange(`Sentence Years2`)},
      filter = 'top'
    )

    output$download7 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Age Range`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Age Range`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )

    output$plot8 <- renderPlot({
      ggplot()+
        geom_bar(mapping=aes(x=stock_sent()$`Sentence Years2`,
                             fill=stock_sent()$`Race`))+
        labs(title="Years Sentenced", x="Sentence Length (Years)",fill="Race")+scale_fill_cvi_d("RJ_colors") })

    output$table8 <- renderDataTable({
      stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
        filter(`Sentence Age`<input$SentAge) %>%
        group_by(`Race`,`Sentence Years2`) %>%
        summarize(n()) %>%
        pivot_wider(names_from = `Race`, values_from = `n()`) %>%
        arrange(`Sentence Years2`)},
      filter = 'top'
    )

    output$download8 <- downloadHandler(
      filename = function(){"SentenceData.csv"},
      content = function(fname){
        write.csv(  stock_sent() %>% mutate("Sentence Year"=year(`Sentence Date`)) %>%
                      filter(`Sentence Age`<input$SentAge) %>%
                      group_by(`Race`,`Sentence Years2`) %>%
                      summarize(n()) %>%
                      pivot_wider(names_from = `Race`, values_from = `n()`) %>%
                      arrange(`Sentence Years2`),
                    fname)
      }
    )

  output$Statistics <-renderTable(
    tibble("Number of People" = length(unique(stock_sent()$`IDOC #`)),
          "IDOC Age" = mean(stock_sent()$age,na.rm=TRUE),
           "IDOC Percent Black" = (length(stock_sent()$Race[stock_sent()$Race=="Black" & stock_sent()$`Sentence Age`<input$SentAge])/length(stock_sent()$Race))*100,
           "IDOC Percent Male" = (length(stock_sent()$Sex[stock_sent()$Sex == "Male"& stock_sent()$`Sentence Age`<input$SentAge])/length(stock_sent()$Sex))*100)

  )





  output$data <- renderDataTable({
    vals <- c(input$selection)

    stock_sent()$`Birth Date` <- as.character(stock_sent()$`Birth Date`)
    stock_sent()$`Current Admission Date`<- as.character(stock_sent()$`Current Admission Date`)
    stock_sent()$`Projected Mandatory Supervised Release Date`<- as.character(stock_sent()$`Projected Mandatory Supervised Release Date`)
    stock_sent()$`Projected Discharge Date` <- as.character(stock_sent()$`Projected Discharge Date`)
    stock_sent()$`Custody Date` <- as.character(stock_sent()$`Custody Date`)
    stock_sent()$`Sentence Date` <- as.character(stock_sent()$`Sentence Date`)


    stock_sent() %>%
      dplyr::filter(between(`Custody Age`,vals[1],vals[2]))

  }
  )



output$OMR <- renderPlot({

  Type_sum <- OMR %>%
    filter(`Incident Type`==input$OMRvar)%>%
    group_by(Prison.Type,date) %>%
    summarize("Number of Incidents"=sum(Incidents,na.rm = TRUE))

  ggplot(Type_sum) +
   geom_line( aes(x = date,
      y = `Number of Incidents`,
      colour = Prison.Type
    ) )
})

output$ILmap <- renderPlot({

  IL_counties <- tigris::counties(state=17) %>%
    select(county = NAME,geometry) %>%
    st_transform(4326)

  ggplot()+
    geom_sf(data=IL_counties,fill="white")+
    theme_classic()+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
    labs(fill="")
})




  output$downloadData <- downloadHandler({
    filename = function() {
      paste("IDOC", stock_date, ".csv", sep = "")
    }

    content = function(file) {
      write.csv(output$data, file, row.names = FALSE)
    }
  })

  #, sanitize.text.function = function(x) x

  
}

#issues : dates are coming out hella weird
#       : SUPER slow due to large file size
#       : Not particularly useful (maybe visualizations rather than table?)

# Run the application 
shinyApp(ui = ui, server = server)
