# Load necessary packages

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(data.table)
library(readxl)
library(plotrix)
library(DT)
library(R.utils)
library(colourpicker)
library(writexl)

### 1. Create graphical user interface definition of the shiny app

ui<-dashboardPage(
  dashboardHeader(title=span(tagList(icon("database"),"SPIN"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data",tabName = "data",icon=icon("upload")),
      menuItem("SVHC and Overall Trend",tabName = "trend",icon = icon("chart-bar")),
      menuItem("Trend without Intermediates",tabName = "intermediate",icon = icon("chart-bar")),
      menuItem("Problematic Chemical Trend",tabName = "carco",icon=icon("chart-bar")),
      menuItem("Complete Data",tabName = "frame",icon = icon("table")))),
  dashboardBody(
    tabItems(
      tabItem(tabName="data",
              fluidRow(box(width=4,column(12,offset=3,h2(strong("Upload Data"))),background = "light-blue")),
              fluidRow(box(p(h2("SPIN data"),
                             tags$ol(
                               tags$li("Download the current SPIN database (MS Access database with all data) on the",
                                       tags$a(href="http://spin2000.net/?page_id=54","official website")),
                               tags$li("Unzip the MS Access file and open it"),
                               tags$li("You need to create a new query using this SQL-statement:",
                                       tags$code(br(),
                                                 "SELECT *",
                                                 br(),
                                                 "FROM Spinname AS sp",
                                                 br(),
                                                 "INNER JOIN UseT AS u",
                                                 br(),
                                                 "ON u.PID=sp.PID;")),
                               tags$li("You will get a new data table. Save this query under 
                               a appropriate name and export this table. It is important that you export this file as 
                               a .txt. When you get in the export menu it is crucial that you do not maintain the format, 
                               otherwise a lot of data will be lost"),
                               tags$li("You have to convert the .txt to a .txt.gz since the .txt format is too big to upload. 
                                       You can do this for example with",tags$a(href="https://www.7-zip.de/","7-Zip")))),
                           p(h2("Candidate-list"),
                             tags$ol(
                               tags$li("Download the",strong("English"),"version of the current Candidate-list as a .xlsx on the",
                                       tags$a(href="https://echa.europa.eu/candidate-list-table","official ECHA website")))),
                           p(h2("Authorisation-list"),
                             tags$ol(
                               tags$li("Download the",strong("English"),"version of the current Authorisation-list as a .xlsx on the",
                                       tags$a(href="https://echa.europa.eu/authorisation-list","official ECHA website")))),width=12)),
              fluidRow(box(width = 12,
                           fileInput(inputId = "spin",label = strong("SPIN data"),accept = ".txt.gz"))),
              fluidRow(box(width = 12,
                           fileInput(inputId = "svhc",label = strong("Candidate-list"),accept = ".xlsx"))),
              fluidRow(box(width = 12,
                           fileInput(inputId = "authorisation",label = "Authorisation-list",accept = ".xlsx"))),
              fluidRow(box(title = "Upload Status",width = 4,column(9,tableOutput(outputId = "test"))))),
      tabItem(tabName="trend",
              fluidRow(box(width=4,background = "light-blue",column(12,offset = 1,h2(strong("SVHC and Overall Trend")))),
                       column(12,h2("Trend of All, Single and SVHC Substances"))),
              fluidRow(box(title="Controls",width = 4,
                           column(9,selectInput(inputId = "countries",label = strong("Country"), choices = c("SE","DK","NO","FI"),
                                                selected = "SE"),
                                  selectInput(inputId = "year",label=strong("Years Since"),choices = seq(2000,2017),
                                              selected = 2000),
                                  selectInput(inputId = "type",label = strong("Type"),choices = c("SVHC","All","Cas")),
                                  conditionalPanel("input.type=='Cas'",textInput(inputId = "cas",strong("Enter Cas-number"),
                                                                                 placeholder = "50-00-0")),
                                  checkboxInput(inputId = "glm","Lm"),
                                  conditionalPanel("input.glm==true",selectInput(inputId = "glm_year",label = "Lm Since",
                                                                                 choices = seq(2000,2017))),
                                  colourInput(inputId = "col_first",label = strong("Colour for plot"),value = "#91CFEE")),
                           column(5,downloadButton("downloadplot","Download Plot")),
                           column(5,downloadButton("downloadplot_back","Download Data"))),
                       box(width = 7,column(plotOutput(outputId = "barplot"),width = 12))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "normal_country",label = strong("Country"),
                                                                             choices = c("SE","DK","FI","NO")),
                                                               selectInput(inputId = "normal_year",label = strong("Reference Year"),
                                                                           choices = seq(2000,2017))),
                           column(5,downloadButton("downloadnormalrel","Download Plot")),
                           column(5,downloadButton("downloadnormalrel_back","Download Data")),
                           column(12,br(),helpText("For the year 2000 there is no data for Finland which is acknowledged as 0 tonnes of chemicals. 
                                                   Therefore, the relative chemical production in finland with the reference year 2000 is infinite big."))),
                       box(width = 7,column(12,plotOutput(outputId = "normal_rel_plot")))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "normal_rel_country",label = strong("Country"),
                                                                             choices = c("SE","NO","DK","FI")),
                                                               selectInput(inputId = "normal_rel_year",label = strong("Reference Year"),
                                                                           choices = seq(2000,2017)),
                                                               selectInput(inputId = "compare_rel_year",label = strong("Comparison Year"),
                                                                           choices = seq(2000,2017)),
                                                               radioButtons(inputId = "normal_rel_datatype",label = strong("File Type"),
                                                                            choices = c("csv","xlsx")),
                                                               downloadButton("downloadnormal_rel_table","Download Table"))),
                       box(width = 7,column(12,p("In this table the five chemicals with the biggest differences of the chemical production between the comparison and
                                                 reference year in tonnes and per type of chemicals are displayed. Negative differences 
                                                 indicate a smaller production amount
                                                 in the comparison year."),br(),dataTableOutput(outputId = "normal_table")))),
              fluidRow(column(12,h2("Completeness of The SPIN Database"))),
              fluidRow(box(width = 4,column(12,p("Here you can see the fraction of single SVHC entries from the 
                                                                                       candidate-list which can be found in the SPIN database.",
                                                 br(),
                                                 br(),"For data newer than the year 2017, single SVHC entries with more than one Cas-number
                                                                                       are counted as two candidate-list entries.",
                                                 br(),
                                                 br()),
                                            downloadButton("downloadpie","Download Plot"))),
                       box(width = 7,column(12,plotOutput(outputId = "pie")))),
              fluidRow(column(12,h2("Trend of Single Substances with Trigger Dates"))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "time_year",label = "Years Since",
                                                                             choices = seq(as.Date(ISOdate(2000,1,1)),as.Date(ISOdate(2017,1,1)),"years")),
                                                               checkboxInput(inputId = "single_country","Single country"),
                                                               conditionalPanel("input.single_country==true",
                                                                                selectInput(inputId = "country_choice",label = strong("Country"),
                                                                                            choices = c("SE","NO","DK","FI"))),
                                                               checkboxInput(inputId = "time_glm","Lm"),
                                                               conditionalPanel("input.time_glm==true",
                                                                                selectInput(inputId = "time_glm_year",label = "Lm Since",
                                                                                            choices = seq(as.Date(ISOdate(2000,1,1)),
                                                                                                          as.Date(ISOdate(2017,1,1)),"years"))),
                                                               textInput(inputId = "time_cas",strong("Enter Cas-number"),
                                                                         placeholder = "50-00-0",value = "50-00-0")),
                           column(5,downloadButton("downloadtimeline","Download Plot")),
                           column(5,downloadButton("downloadtimeline_back","Download Data")),
                           br(),
                           column(12,br(),helpText("The data from SPIN is just given as years. 
                                                   For this plot the data for every year was plotted at the first of January 
                                                   (DOI = Date of inclusion, LAD = Latest application date, SD = Sunset Date)."))),
                       box(width = 7,column(12,plotOutput(outputId = "timeline"))))),
      tabItem(tabName = "carco",
              fluidRow(box(width = 5,background = "light-blue",column(12,offset = 1,h2(strong("Problematic Chemical Trend")))),
                       column(12,h2("Trend of Problematic Chemicals"))),
              fluidRow(box(width = 4,column(9,textInput(inputId = "add_cas",label = strong("Add Cas-number"),value = "50-00-0",placeholder = "50-00-0"),
                                            selectInput(inputId = "add_type",label = strong("Add to which type"),choices =c("carcinogenic","equivalent concern",
                                                                                                                            "mutagenic","PBT/vPVB","toxic for reproduction")),
                                            textOutput(outputId = "extra_info_prob"),
                                            tags$head(tags$style("#extra_info_prob{color:red}")),
                                            br(),
                                            actionButton(inputId = "add_action","Add"),
                                            actionButton(inputId = "add_remove","Remove"),
                                            p(br()),
                                            radioButtons(inputId = "add_datatype",label = strong("File Type"),choices = c("csv","xlsx")),
                                            downloadButton("downloadcarcinogenictable","Download Table"))),
                       box(width=7,column(12,p("The classification of chemicals based on their properties can be seen in this Table. 
                                               You can add additional Cas-numbers."),
                                          dataTableOutput(outputId = "carc_tab")))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "carco_country",label = strong("Country"),
                                                                             choices = c("SE","NO","DK","FI")),
                                                               selectInput(inputId = "carco_type",label = strong("Type"),
                                                                           choices = c("All","carcinogenic","equivalent concern",
                                                                                       "mutagenic","PBT/vPVB","toxic for reproduction")),
                                                               selectInput(inputId = "carco_year",label = strong("Years Since"),
                                                                           choices = seq(2000,2017)),
                                                               conditionalPanel("input.carco_type!='All'",colourInput(inputId = "col_third",
                                                                                                                      label = strong("Colour for plot"),
                                                                                                                      value = "#F8766D"))),
                           column(5,downloadButton("downloadcarcoplot","Download Plot")),
                           column(5,downloadButton("downloadcarcoplot_back","Download Data"))),
                       box(width = 7,column(12,plotOutput(outputId = "carcino")))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "carc_rel_year",label = strong("Reference Year"),
                                                                             choices = seq(2000,2017)),
                                                               selectInput(inputId = "carc_rel_country",choices = c("SE","NO","FI","DK"),
                                                                           label = strong("Country")),
                                                               selectInput(inputId = "carc_rel_type",label = strong("Type"),
                                                                           choices = c("All","carcinogenic","equivalent concern",
                                                                                       "mutagenic","PBT/vPVB","toxic for reproduction")),
                                                               conditionalPanel("input.carc_rel_type!='All'",
                                                                                colourInput(inputId = "col_fourth",label = strong("Colour for plot"),
                                                                                            value = "#F8766D"))),
                           column(5,downloadButton("downloadcarcorel","Download Plot")),
                           column(5,downloadButton("downloadcarcorel_back","Download Data"))),
                       box(width = 7,column(12,plotOutput(outputId = "carc_rel_plot")))),
              fluidRow(box(title = "Controls",width = 4,column(9,selectInput(inputId = "carc_table_country",label = strong("Country"),
                                                                             choices = c("SE","NO","DK","FI")),
                                                               selectInput(inputId = "carc_table_year",label = strong("Reference Year"),
                                                                           choices = seq(2000,2017)),
                                                               selectInput(inputId = "comparison_table_year",label = strong("Comparison Year"),
                                                                           choices = seq(2000,2017)),
                                                               selectInput(inputId = "carc_table_type",label = strong("Type"),
                                                                           choices = c("All","carcinogenic","equivalent concern","mutagenic",
                                                                                       "PBT/vPVB","toxic for reproduction")),
                                                               radioButtons(inputId = "carc_table_datatype",label = strong("File Type"),
                                                                            choices = c("csv","xlsx")),
                                                               downloadButton("downloadcarctable","Download Table"))),
                       box(width = 7,column(12,p("In this table the five chemicals with the biggest differences of the chemical production between the comparison and
                                                 reference year in tonnes and per type of problematic chemicals are displayed. Negative differences 
                                                 indicate a smaller production amount
                                                 in the comparison year."),br(),
                                            dataTableOutput(outputId = "carctable"))))),
      tabItem(tabName = "intermediate",
              fluidRow(box(width = 5,background = "light-blue",column(12,offset=1,h2(strong("Trend without Intermediates")))),
                       column(12,h2("Trend of SVHCs without Intermediates"))),
              fluidRow(box(width = 4,column(9,textInput(inputId = "inter_cas",label = strong("Add Cas-number"),value = "50-00-0",placeholder = "50-00-0"),
                                            textOutput("extra_info_inter"),
                                            tags$head(tags$style("#extra_info_inter{color:red}")),
                                            br(),
                                            actionButton(inputId = "inter_action","Add"),
                                            actionButton(inputId = "inter_remove","Remove"),
                                            p(br()),
                                            radioButtons(inputId = "inter_datatype",label = strong("File Type"),choices = c("csv","xlsx")),
                                            downloadButton("downloadintertable","Download Table"))),
                       box(width=7,column(12,p("The substances you see in the table are the substances 
                                               which are classified as intermediates. These get not plotted in the following plot. 
                                               You can add Cas-numbers."),
                                          dataTableOutput(outputId = "inter_tab")))),
              fluidRow(box(title="Controls",width = 4,
                           column(9,selectInput(inputId = "inter_countries",label = strong("Country"), choices = c("SE","DK","NO","FI"),
                                                selected = "SE"),
                                  selectInput(inputId = "inter_year",label=strong("Years Since"),choices = seq(2000,2017),
                                              selected = 2000),
                                  checkboxInput(inputId = "inter_glm","Lm"),
                                  conditionalPanel("input.inter_glm==true",selectInput(inputId = "inter_glm_year",
                                                                                       label = "Lm Since",choices = seq(2000,2017))),
                                  colourInput(inputId = "col_second",label = strong("Colour for plot"),value = "#91CFEE")),
                           column(5,downloadButton("downloadinterplot","Download Plot")),
                           column(5,downloadButton("downloadinterplot_back","Download Data"))),
                       box(width = 7,column(plotOutput(outputId = "interplot"),width = 12)))),
      tabItem(tabName = "frame",
              fluidRow(box(width = 4,background = "light-blue",column(12,offset = 3,h2(strong("Complete Data"))))),
              fluidRow(column(12,p())),
              fluidRow(box(title = "Controls",width = 3,column(9,selectInput(inputId = "dataframe",label = strong("Select Data Frame"),
                                                                             choices = c("Complete","Total")),
                                                               selectInput(inputId = "country_frame",label = strong("Country"),
                                                                           choices = c("SE","NO","FI","DK","Nordic")),
                                                               selectInput(inputId = "year_frame",label = strong("Years Since"),
                                                                           seq(2000,2017)),
                                                               checkboxInput(inputId = "year_filter","Single year"),
                                                               conditionalPanel("input.year_filter==true",selectInput(inputId = "year_filter_frame",
                                                                                                                      label = strong("Year"),choices = seq(2000,2017))),
                                                               selectInput(inputId = "type_frame",choices = c("SVHC","All","Cas"),
                                                                           label = strong("Type")),
                                                               conditionalPanel("input.type_frame=='Cas'",textInput(inputId = "cas_frame",
                                                                                                                    strong("Enter Cas-number"),placeholder = "50-00-0")),
                                                               conditionalPanel("input.type_frame=='SVHC' & input.dataframe=='Complete'",
                                                                                checkboxInput(inputId = "group","Grouped")),
                                                               radioButtons(inputId = "frame_datatype",label = strong("File Type"),
                                                                            choices = c("csv","xlsx")),
                                                               downloadButton("downloadtable","Download Table"))),
                       box(width = 9,column(12,dataTableOutput(outputId = "table"))))))))


### 2. Create the function that will be executed when the shiny app is used

server<-function(input, output, session) {
  
  ## 2.1 Select options
  
  # Increase the maximum upload size
  options(shiny.maxRequestSize = 30*1024^2)
  
  # Set the theme for the plots that can be displayed and downloaded
  theme_set(theme_light())
  
  ## 2.2 Make lists based on already available data reactive
  
  # Check the time period of the data from the SPIN database and update the choices to filter the years
  observe({
    input_file1<-input$spin
    if(is.null(input_file1)){
      return("Error")
    }
    abfrage2<-fread(input_file1$datapath,header = F)
    colnames(abfrage2)<-c("PID","cas_no","nametype","name","PID2","country","year","exist","confident",
                          "consumer","number","amount","timestamp")
    abfrage2$amount<-gsub(",",".",abfrage2$amount)
    abfrage2$amount<-as.numeric(abfrage2$amount)
    temp_data<-abfrage2 %>%
      select(-nametype,-PID2,-exist,-confident,-consumer,-number,-timestamp) %>%
      mutate(cas_no=as.character(cas_no),
             name=as.character(name),
             amount=amount/10,
             country=as.factor(country))
    
    updateSelectInput(session,"year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"glm_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"normal_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"normal_rel_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"compare_rel_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"time_year",choices = seq(as.Date(ISOdate(min(temp_data$year),1,1)),
                                                        as.Date(ISOdate(max(temp_data$year),1,1)),"years"))
    updateSelectInput(session,"time_glm_year",choices = seq(as.Date(ISOdate(min(temp_data$year),1,1)),
                                                            as.Date(ISOdate(max(temp_data$year),1,1)),"years"))
    updateSelectInput(session,"carco_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"carc_rel_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"carc_table_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"comparison_table_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"inter_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"inter_glm_year",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"year_frame",choices = seq(min(temp_data$year),max(temp_data$year)))
    updateSelectInput(session,"year_filter_frame",choices = seq(min(temp_data$year),max(temp_data$year)))
  })
  
  # Supplement data which lists problematic chemicals and divides them into different categories.
  # Chemicals can be added to and removed from the list
  init_carc<-read_xlsx("./data/carcinogenic.xlsx") %>%
    separate_rows(cas_no,sep = ", ") %>%
    select(name=Name,type,cas_no)
  
  # Make the list reactive
  carc<-reactiveValues(data=init_carc)
  
  # Supplement data which lists SVHCs which can be considered intermediates.
  # Chemicals can be added to and removed from the list
  init_inter<-data.frame(cas_no=c("1317-36-8","107-06-2","75-56-9","50-32-8","79-06-1","107-15-3"),name=c("Lead monoxide (lead oxide)","1,2-dichloroethane","Methyloxirane (Propylene oxide)","Benzo[def]chrysene (Benzo[a]pyrene)","Acrylamide","Ethylenediamine")) %>%
    mutate(cas_no=as.character(cas_no),
           name=as.character(name))
  
  # Make the list reactive
  inter<-reactiveValues(data=init_inter)
  
  # Make the help text for intermediate list
  infotext_inter <- reactiveValues(text="")
  
  # Make the help text for problematic chemical list
  infotext_prob <- reactiveValues(text="")
  
  # Remove chemicals from the list with problematic chemicals
  observeEvent(input$add_remove,{
    if (!input$add_cas %in% (pull(filter(carc$data,type==input$add_type),cas_no))){
      infotext_prob$text <- "This cas-number is not in the list. Check if you selected the right type"
    } else {
      carc$data<-carc$data[-which(carc$data$cas_no==input$add_cas & carc$data$type==input$add_type),]
      infotext_prob$text <- ""
    }
  })
  
  # Add chemicals to the list with problematic chemicals
  observeEvent(input$add_action,{
    if (length(spin_data() %>% #pick only cas which are in svhc list (in new_candidates) and distinct since every cas has a few names
               distinct(cas_no,year,country,.keep_all = T) %>%
               filter(cas_no==input$add_cas) %>%
               pull(name))==0){
      infotext_prob$text <- "This Cas-number cannot be found in SPIN"
    } else {
      if (!input$add_cas %in% (pull(filter(carc$data,type==input$add_type),cas_no))){
        carc$data<-carc$data %>%
          add_row(name=unique(spin_data() %>% #pick only cas which are in svhc list (in new_candidates) and distinct since every cas has a few names
                                distinct(cas_no,year,country,.keep_all = T) %>%
                                filter(cas_no==input$add_cas) %>%
                                pull(name)),type=input$add_type,cas_no=input$add_cas)
        infotext_prob$text <- "" 
      } else {
        infotext_prob$text <- "The Cas-number is already in the list"
      }
    }
  })
  
  # Remove chemicals from the list with SVHC which can be considered as intermediates
  observeEvent(input$inter_remove,{
    if (!input$inter_cas %in% inter$data$cas_no){
      infotext_inter$text <- "This Cas-number is not in the list"
    } else {
      inter$data<-inter$data %>%
        filter(cas_no!=input$inter_cas) 
      infotext_inter$text <- ""
    }
  })
  
  # Add chemicals to the list with SVHC which can be considered as intermediates
  observeEvent(input$inter_action,{
    if (length(spin_data() %>% 
               distinct(cas_no,year,country,.keep_all = T) %>%
               filter(cas_no==input$inter_cas) %>%
               pull(name))==0){
      infotext_inter$text <- "This Cas-number cannot be found in SPIN"
    } else {
      if (!input$inter_cas %in% inter$data$cas_no){
        inter$data<-inter$data %>%
          add_row(name=unique(spin_data() %>%
                                distinct(cas_no,year,country,.keep_all = T) %>%
                                filter(cas_no==input$inter_cas) %>%
                                pull(name)),cas_no=input$inter_cas)
        infotext_inter$text <- "" 
      } else {
        infotext_inter$text <- "This Cas-number is already in the list"
      }
    }
  })
  
  ## 2.3 Clean and analyse input data
  
  # Clean the input data from the SPIN database for further cleaning and analysis
  spin_data<-reactive({
    input_file1<-input$spin
    if(is.null(input_file1)){
      return("Error")
    }
    abfrage2<-fread(input_file1$datapath,header = F)
    colnames(abfrage2)<-c("PID","cas_no","nametype","name","PID2","country","year","exist","confident",
                          "consumer","number","amount","timestamp")
    abfrage2$amount<-gsub(",",".",abfrage2$amount)
    abfrage2$amount<-as.numeric(abfrage2$amount)
    abfrage2 %>%
      select(-nametype,-PID2,-exist,-confident,-consumer,-number,-timestamp) %>%
      mutate(cas_no=as.character(cas_no),
             name=as.character(name),
             amount=amount/10,
             country=as.factor(country))
  })
  
  # Join SPIN data and data from candidate-list and clean it
  join_data<-reactive({
    spin_data() %>%
      distinct(cas_no,year,country,.keep_all = T) %>%
      inner_join(candidate_data(),by="cas_no") %>%
      distinct(cas_no,year,country,.keep_all = T) %>%
      arrange(country,year)
  })
  
  # Differentiate chemicals in SPIN database in normal chemicals and SVHCs
  normal_table_data<-reactive({
    test<-(spin_data() %>%
             distinct(year,country,cas_no,.keep_all = T))$cas_no %in% candidate_data()$cas_no
    all_combined<-spin_data() %>%
      distinct(year,country,cas_no,.keep_all = T)
    all_combined$svhc<-"Normal"
    all_combined$svhc[test]<-"SVHC"
    all_combined
  })
  
  # Analyse the realtive production amounts of normal and SVHC chemicals
  normal_rel_data<-reactive({
    test<-(spin_data() %>%
             distinct(year,country,cas_no,.keep_all = T))$cas_no %in% candidate_data()$cas_no
    all_combined<-spin_data() %>%
      distinct(year,country,cas_no,.keep_all = T)
    all_combined$svhc<-"Normal"
    all_combined$svhc[test]<-"SVHC"
    
    normal<-all_combined %>%
      group_by(country,year,svhc) %>%
      summarize(all=sum(amount)) %>%
      filter(country==input$normal_country & svhc=="Normal") 
    
    svhc<-all_combined %>%
      group_by(country,year,svhc) %>%
      summarize(all=sum(amount)) %>%
      filter(country==input$normal_country & svhc=="SVHC") 
    
    g <- normal %>%
      add_column(svhc_amount = pull(select(svhc,all),all)) %>%
      filter(year>=input$normal_year)
    
    g %>%
      mutate(verg_normal=(all-g$all[1])/g$all[1],
             verg_svhc=(svhc_amount-g$svhc_amount[1])/g$svhc_amount[1])
  })
  
  # Clean the SPIN data of the problematic chemicals
  carc_data<-reactive({
    if (input$carco_type=="All"){
      spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        inner_join(carc$data,by="cas_no") %>% 
        group_by(year,country,type) %>%
        summarize(total_amount=sum(amount)) %>%
        filter(year>=input$carco_year) %>%
        filter(country==input$carco_country)
    } else {
      spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        inner_join(carc$data,by="cas_no") %>% 
        group_by(year,country,type) %>%
        summarize(total_amount=sum(amount)) %>%
        filter(year>=input$carco_year) %>%
        filter(country==input$carco_country) %>%
        filter(type==input$carco_type)
    }
  })
  
  # Analyse the trend of the problematic chemicals
  carc_rel_data<-reactive({
    if (input$carc_rel_type=="All"){
      rel_se<-spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        filter(year>=input$carc_rel_year,
               country==input$carc_rel_country) %>%
        inner_join(carc$data,by="cas_no") %>% 
        group_by(year,type) %>%
        summarize(total=sum(amount)) %>%
        group_by(type,.add = T) 
      
      rel_se %>%
        add_column(compared=rep(rel_se$total[1:5],times=length(rel_se$total)/5)) %>%
        mutate(rela=(total-compared)/compared)
    } else {
      rel_se<-spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        filter(year>=input$carc_rel_year,
               country==input$carc_rel_country) %>%
        inner_join(carc$data,by="cas_no") %>%
        group_by(year,type) %>%
        summarize(total=sum(amount)) %>%
        group_by(type,.add = T) 
      
      rel_se %>%
        add_column(compared=rep(rel_se$total[1:5],times=length(rel_se$total)/5)) %>%
        mutate(rela=(total-compared)/compared) %>%
        filter(type==input$carc_rel_type) 
    }
  })
  
  # Clean the data from the candidate-list
  candidate_data<-reactive({
    input_file2<-input$svhc
    if (is.null(input_file2)){
      return("Error")
    }
    candidate_list<-read_xlsx(input_file2$datapath,skip=3) %>%
      clean_names()
    new_candidates<-data.frame(Stoffname=candidate_list$substance_name,cas_no=candidate_list$cas_no,Datum=candidate_list$date_of_inclusion)
    new_candidates$Datum<-as.Date(new_candidates$Datum,format="%d/%m/%Y")
    new_candidates$cas_no<-as.character(new_candidates$cas_no)
    new_candidates$Stoffname<-as.character(new_candidates$Stoffname)
    new_candidates %>% #same as candidates but the dates and the other datatypes are correctly formated
      separate_rows(cas_no,sep = ", ")
  })
  
  # Clean the SPIN data for the download of the complete data
  grouped_data<-reactive({
    input_file2<-input$svhc
    candidate_list<-read_xlsx(input_file2$datapath,skip=3) %>%
      clean_names()
    cnt_list<-1
    cnt<-1
    
    # Some candidates do not have a support document which makes it not able to identify if several entries are just sub-entries of 
    # one over-entry since all the sub-entry have the same support document. Instead if there are several sequential entries withouth support
    # document they are assumed to be sub-entries of one over-entry
    candidate_list$support_document[which(is.na(candidate_list$support_document))] <- "SVHC entries without support document"
    
    for (i in 2:length(candidate_list$substance_name)){
      if (candidate_list$support_document[i]==candidate_list$support_document[i-1]){
        cnt_list<-c(cnt_list,cnt)
      } else {
        cnt<-cnt+1
        cnt_list<-c(cnt_list,cnt)
      }
    }
    candidate_list$group<-cnt_list
    alternative<-candidate_list %>%
      group_by(group) %>%
      slice(1:1) %>%
      select(group_name=substance_name,group)
    
    gr<-candidate_list %>%
      inner_join(alternative,by="group") %>%
      select(group_name,cas_no,group)
    
    if (input$country_frame=="Nordic"){
      if (input$year_filter){
        temp<-spin_data() %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          inner_join(gr,by="cas_no") %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          filter(year==input$year_filter_frame) %>%
          group_by(country,year,group_name,group) %>%
          summarize(total=sum(amount)) %>%
          arrange(country,group_name,year) %>%
          inner_join(candidate_list,by="group") %>%
          select(country,year,group_name,cas_no,total) %>%
          distinct(year,group_name,cas_no,.keep_all = T) %>%
          filter(cas_no!="-")
        
        aggregate(cas_no~country+year+group_name+total,data = temp,paste0,collapse=",") %>%
          arrange(country,group_name,year)
      } else {
        temp<-spin_data() %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          inner_join(gr,by="cas_no") %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          filter(year>=input$year_frame) %>%
          group_by(country,year,group_name,group) %>%
          summarize(total=sum(amount)) %>%
          arrange(country,group_name,year) %>%
          inner_join(candidate_list,by="group") %>%
          select(country,year,group_name,cas_no,total) %>%
          distinct(year,group_name,cas_no,.keep_all = T) %>%
          filter(cas_no!="-")
        
        aggregate(cas_no~country+year+group_name+total,data = temp,paste0,collapse=",") %>%
          arrange(country,group_name,year)
      }
    } else {
      if (input$year_filter){
        temp<-spin_data() %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          inner_join(gr,by="cas_no") %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          filter(country==input$country_frame) %>%
          filter(year==input$year_filter_frame) %>%
          group_by(country,year,group_name,group) %>%
          summarize(total=sum(amount)) %>%
          arrange(country,group_name,year) %>%
          inner_join(candidate_list,by="group") %>%
          select(country,year,group_name,cas_no,total) %>%
          distinct(year,group_name,cas_no,.keep_all = T) %>%
          filter(cas_no!="-")
        
        aggregate(cas_no~country+year+group_name+total,data = temp,paste0,collapse=",") %>%
          arrange(country,group_name,year)
      } else {
        temp<-spin_data() %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          inner_join(gr,by="cas_no") %>%
          distinct(cas_no,year,country,.keep_all = T) %>%
          filter(country==input$country_frame) %>%
          filter(year>=input$year_frame) %>%
          group_by(country,year,group_name,group) %>%
          summarize(total=sum(amount)) %>%
          arrange(country,group_name,year) %>%
          inner_join(candidate_list,by="group") %>%
          select(country,year,group_name,cas_no,total) %>%
          distinct(year,group_name,cas_no,.keep_all = T) %>%
          filter(cas_no!="-")
        
        aggregate(cas_no~country+year+group_name+total,data = temp,paste0,collapse=",") %>%
          arrange(country,group_name,year)
      }
    }
  })
  
  # Clean data from the authorisation-list
  auth_data<-reactive({
    input_file3<-input$authorisation
    if (is.null(input_file3)){
      return("Error")
    }
    read_xlsx(input_file3$datapath,skip = 3) %>%
      clean_names() %>%
      separate_rows(cas_no,sep = ", ")%>%
      mutate(sunset_date=as.Date(sunset_date,format="%d/%m/%Y"),
             latest_application_date=as.Date(latest_application_date,format="%d/%m/%Y"))
  })
  
  # Clean SPIN data for the trend of single chemicals
  time_data<-reactive({
    spin_data() %>%
        mutate(year=as.Date(ISOdate(year,1,1))) %>%
        left_join(candidate_data(),by="cas_no") %>%
        select(-Stoffname) %>%
        left_join(auth_data(),by="cas_no") %>%
        select(name,cas_no,country,year,amount,Datum,sunset_date,latest_application_date) %>%
        distinct(cas_no,country,year,.keep_all = T)
  })
  
  # Clean data for some plots
  plot3_data<-reactive({
    if (input$type=="All"|input$type=="Cas"){
      spin_data() %>%
        distinct(year,country,cas_no,.keep_all = T)
    } else if (input$type=="SVHC"){
      join2<-spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        inner_join(candidate_data(),by="cas_no") %>%
        distinct(cas_no,year,country,.keep_all = T) %>%
        arrange(country,year)
      
      join2 %>%
        group_by(year,country) %>%
        summarize(total_amount=sum(amount)) %>%
        ungroup()
    }
  })
  
  # Clean data for some plots
  plot4_data<-reactive({
    if (input$type_frame=="All"|input$type_frame=="Cas"){
      spin_data() %>%
        distinct(year,country,cas_no,.keep_all = T)
    } else if (input$type_frame=="SVHC"){
      join2<-spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        inner_join(candidate_data(),by="cas_no") %>%
        distinct(cas_no,year,country,.keep_all = T) %>%
        arrange(country,year)
      
      join2 %>%
        group_by(year,country) %>%
        summarize(total_amount=sum(amount)) %>%
        ungroup()
    }
  })
  
  ## 2.4 Render tables,text and plots
  
  # Render text for the intermediate list
  output$extra_info_inter <- renderText({
    infotext_inter$text
  })
  
  # Render text for the problematic chemical list
  output$extra_info_prob <- renderText({
    infotext_prob$text
  })
  
  # Potential output for debugging
  output$debug<-renderTable({
  })
  
  # Display if all the input data was successfully uploaded withouth any errors
  output$test<-renderText({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"Unsuccesful upload of all files")
    )
    "Succesful upload of all files"
  })
  
  # Table displaying five biggest differences to a reference year in tonnes and per type of chemicals
  output$normal_table<-renderDataTable({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the table can be generated")
    )
    normal_table_data() %>%
      group_by(country,year,svhc,cas_no) %>%
      summarize(total=sum(amount)) %>%
      filter(country==input$normal_rel_country) %>%
      inner_join(normal_table_data() %>%
                   group_by(country,year,svhc,cas_no) %>%
                   summarize(total=sum(amount)) %>%
                   filter(country==input$normal_rel_country) %>%
                   filter(year==input$normal_rel_year),by=c("cas_no","country","svhc")) %>%
      select(year=year.x,svhc,country,cas_no,-year.y,total=total.x,comp_total=total.y) %>%
      mutate(difference=total-comp_total) %>%
      filter(year==input$compare_rel_year) %>%
      arrange(desc(abs(difference))) %>%
      slice(1:5) %>%
      ungroup() %>%
      left_join(normal_table_data() %>%
                  distinct(name,cas_no) %>%
                  select(name,cas_no),by="cas_no") %>%
      select(year,country,type=svhc,cas_no,name,difference)
  })
  
  # Table displaying the SVHCs chemicals considered as intermediates
  output$inter_tab<-renderDataTable({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the table is shown")
    )
    if (input$inter_action==0 & input$inter_remove==0){
      init_inter
    } else {
      inter$data
    }
  })
  
  # Table displaying problematic chemicals
  output$carc_tab<-renderDataTable({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the table is shown")
    )
    if (input$add_action==0 & input$add_remove==0){
      init_carc
    } else {
      carc$data
    }
  })
  
  # Table displaying five biggest differences to a reference year in tonnes and per type of problematic chemicals
  output$carctable<-renderDataTable({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the table can be generated")
    )
    if (input$carc_table_type=="All"){
      spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        filter(year==input$comparison_table_year,
               country==input$carc_table_country) %>%
        inner_join(carc$data,by="cas_no") %>%
        select(-name.x) %>%
        group_by(country,year,cas_no,name=name.y,type) %>%
        summarize(total=sum(amount)) %>%
        inner_join(spin_data() %>% 
                     distinct(cas_no,year,country,.keep_all = T) %>%
                     filter(year==input$carc_table_year,
                            country==input$carc_table_country) %>%
                     inner_join(carc$data,by="cas_no") %>%
                     select(-name.x) %>%
                     group_by(country,year,cas_no,name=name.y,type) %>%
                     summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
        select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
        mutate(difference=total-total_sum) %>%
        ungroup() %>%
        group_by(country,year,type) %>%
        arrange(desc(abs(difference))) %>%
        slice(1:5) %>%
        ungroup() %>%
        select(Year=year,Type=type,Cas=cas_no,name,difference)
    } else {
      spin_data() %>% 
        distinct(cas_no,year,country,.keep_all = T) %>%
        filter(year>input$carc_table_year,
               country==input$carc_table_country) %>%
        inner_join(carc$data,by="cas_no") %>%
        select(-name.x) %>%
        filter(type==input$carc_table_type) %>%
        group_by(country,year,cas_no,name=name.y,type) %>%
        summarize(total=sum(amount)) %>%
        inner_join(spin_data() %>% 
                     distinct(cas_no,year,country,.keep_all = T) %>%
                     filter(year==input$carc_table_year,
                            country==input$carc_table_country) %>%
                     inner_join(carc$data,by="cas_no") %>%
                     select(-name.x) %>%
                     filter(type==input$carc_table_type) %>%
                     group_by(country,year,cas_no,name=name.y,type) %>%
                     summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
        select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
        mutate(difference=total-total_sum) %>%
        ungroup() %>%
        group_by(country,year,type) %>%
        arrange(desc(abs(difference))) %>%
        slice(1:5) %>%
        ungroup() %>%
        select(Year=year,Type=type,Cas=cas_no,name,difference)
    }
  })
  
  # Table displaying the complete data
  output$table<-renderDataTable({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the table can be generated")
    )
    if (input$country_frame!="Nordic"){
      if (input$dataframe=="Complete"){
        if (input$type_frame=="All"){
          if (input$year_filter){
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(country==input$country_frame) %>%
              filter(year==input$year_filter_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          } else {
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(country==input$country_frame) %>%
              filter(year>=input$year_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          }
        } else if(input$type_frame=="SVHC"){
          if (input$year_filter){
            if (input$group){
              grouped_data() %>%
                rename(tonnes=total)
            } else {
              spin_data() %>% 
                distinct(cas_no,year,country,.keep_all = T) %>%
                inner_join(candidate_data(),by="cas_no") %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                arrange(country,year) %>%
                filter(country==input$country_frame) %>%
                filter(year==input$year_filter_frame) %>%
                select(-PID,-Stoffname,-Datum) %>%
                rename(tonnes=amount) %>%
                arrange(year,country)
            }
          } else {
            if (input$group){
              grouped_data() %>%
                rename(tonnes=total)
            } else {
              spin_data() %>% 
                distinct(cas_no,year,country,.keep_all = T) %>%
                inner_join(candidate_data(),by="cas_no") %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                arrange(country,year) %>%
                filter(country==input$country_frame) %>%
                filter(year>=input$year_frame) %>%
                select(-PID,-Stoffname,-Datum) %>%
                rename(tonnes=amount) %>%
                arrange(year,country)
            }
          }
        } else if (input$type_frame=="Cas"){
          if (input$year_filter){
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(country==input$country_frame) %>%
              filter(year==input$year_filter_frame) %>%
              filter(cas_no==input$cas_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          } else {
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(country==input$country_frame) %>%
              filter(year>=input$year_frame) %>%
              filter(cas_no==input$cas_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          }
        }
      } else if (input$dataframe=="Total"){
        if (input$type_frame=="All"){
          if (input$year_filter){
            plot4_data() %>%
              group_by(country,year) %>%
              summarize(all=sum(amount)) %>%
              filter(country==input$country_frame) %>%
              filter(year==input$year_filter_frame) %>%
              rename(tonnes=all) %>%
              arrange(year,country)
          } else {
            plot4_data() %>%
              group_by(country,year) %>%
              summarize(all=sum(amount)) %>%
              filter(country==input$country_frame) %>%
              filter(year>=input$year_frame) %>%
              rename(tonnes=all) %>%
              arrange(year,country)
          }
        } else if (input$type_frame=="SVHC"){
          if (input$year_filter){
            plot4_data() %>%
              filter(country==input$country_frame) %>%
              filter(year==input$year_filter_frame) %>%
              rename(tonnes=total_amount) %>%
              arrange(year,country)
          } else {
            plot4_data() %>%
              filter(country==input$country_frame) %>%
              filter(year>=input$year_frame) %>%
              rename(tonnes=total_amount) %>%
              arrange(year,country)
          }
        }
      }
    } else if (input$country_frame=="Nordic"){
      if (input$dataframe=="Complete"){
        if (input$type_frame=="All"){
          if (input$year_filter){
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(year==input$year_filter_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          } else {
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(year>=input$year_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          }
        } else if(input$type_frame=="SVHC"){
          if (input$year_filter){
            if (input$group){
              grouped_data() %>%
                rename(tonnes=total)
            } else {
              spin_data() %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                inner_join(candidate_data(),by="cas_no") %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                arrange(country,year) %>%
                filter(year==input$year_filter_frame) %>%
                select(-PID,-Stoffname,-Datum) %>%
                rename(tonnes=amount) %>%
                arrange(name,year,country)
            }
          } else {
            if (input$group){
              grouped_data() %>%
                rename(tonnes=total)
            } else {
              spin_data() %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                inner_join(candidate_data(),by="cas_no") %>%
                distinct(cas_no,year,country,.keep_all = T) %>%
                arrange(country,year) %>%
                filter(year>=input$year_frame) %>%
                select(-PID,-Stoffname,-Datum) %>%
                rename(tonnes=amount) %>%
                arrange(name,year,country)
            }
          }
        } else if (input$type_frame=="Cas"){
          if (input$year_filter){
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(year==input$year_filter_frame) %>%
              filter(cas_no==input$cas_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          } else {
            spin_data() %>%
              distinct(year,country,cas_no,.keep_all = T) %>%
              filter(year>=input$year_frame) %>%
              filter(cas_no==input$cas_frame) %>%
              select(-PID) %>%
              rename(tonnes=amount) %>%
              arrange(year,country)
          }
        }
      } else if (input$dataframe=="Total"){
        if (input$type_frame=="All"){
          if (input$year_filter){
            plot4_data() %>%
              group_by(country,year) %>%
              summarize(all=sum(amount)) %>%
              filter(year==input$year_filter_frame) %>%
              rename(tonnes=all) %>%
              arrange(year,country)
          } else {
            plot4_data() %>%
              group_by(country,year) %>%
              summarize(all=sum(amount)) %>%
              filter(year>=input$year_frame) %>%
              rename(tonnes=all) %>%
              arrange(year,country)
          }
        } else if (input$type_frame=="SVHC"){
          if (input$year_filter){
            plot4_data() %>%
              filter(year==input$year_filter_frame) %>%
              rename(tonnes=total_amount) %>%
              arrange(year,country)
          } else {
            plot4_data() %>%
              filter(year>=input$year_frame) %>%
              rename(tonnes=total_amount) %>%
              arrange(year,country)
          }
        }
      }
    }
  })
  
  # Trend of problematic chemicals relative to a reference year
  output$carc_rel_plot<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    if (input$carc_rel_type=="All"){
      ggplot(carc_rel_data(),aes(year,rela,fill=type))+
        geom_col(position = position_dodge(width=0.5),width=0.5)+
        scale_y_continuous(labels = scales::percent_format())+
        scale_x_continuous(breaks=seq(input$carc_rel_year,max(carc_rel_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        scale_fill_manual("Problematic Chemicals",values = c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))+
        labs(title=glue::glue("Trend of Problematic Chemicals in {input$carc_rel_country} relative to {input$carc_rel_year}"),y="Effective Change",x="")
    } else {
      ggplot(carc_rel_data(),aes(year,rela,fill=type))+
        geom_col(position = position_dodge(width=0.5),width=0.5)+
        scale_y_continuous(labels = scales::percent_format())+
        scale_x_continuous(breaks=seq(input$carc_rel_year,max(carc_rel_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        scale_fill_manual("Problematic Chemicals",values = input$col_fourth)+
        labs(title=glue::glue("Trend of Problematic Chemicals in {input$carc_rel_country} relative to {input$carc_rel_year}"),y="Effective Change",x="")
    }
  })
  
  # Trend of normal and SVHC chemicals relative to a reference year
  output$normal_rel_plot<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    normal_rel_data() %>%
      select(-all,-svhc,-svhc_amount,SVHC=verg_svhc,Normal=verg_normal) %>%
      gather(Type,value,-country,-year) %>%
      ggplot(aes(year,value,fill=Type,width=0.4))+
      geom_histogram(stat="identity",position = "dodge")+
      scale_y_continuous("Effective Change",labels = scales::percent_format())+
      scale_x_continuous(breaks = seq(min(normal_rel_data()$year),max(normal_rel_data()$year)))+
      labs(x="",title = glue::glue("Trend of SVHCs and Normal chemicals in {input$normal_country} relative to {input$normal_year}"))+
      expand_limits(y=c(0,0.75))+
      theme(axis.text.x = element_text(angle=45,hjust = 1))
  })
  
  # Trend of problematic chemicals
  output$carcino<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    if(input$carco_type=="All"){
      carc_data() %>%
        ggplot(aes(year,total_amount,fill=type))+
        geom_area()+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$carco_country}"),labels = scales::comma_format())+
        scale_fill_discrete("Problematic Chemicals")+
        labs(x="",title = glue::glue("Trend of Problematic Chemicals in {input$carco_country}"))+
        scale_x_continuous(breaks=seq(input$carco_year,max(carc_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    } else{
      carc_data() %>%
        ggplot(aes(year,total_amount,fill=type))+
        geom_area()+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$carco_country}"),labels = scales::comma_format())+
        scale_fill_manual("Problematic Chemicals",values = input$col_third)+
        labs(x="",title = glue::glue("Trend of Problematic Chemicals in {input$carco_country}"))+
        scale_x_continuous(breaks=seq(input$carco_year,max(carc_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    }
  })
  
  # Trend of Single Substances with Trigger Dates
  output$timeline<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    if(input$single_country){
      
      Basis<-time_data() %>%
        filter(year>=input$time_year) %>%
        filter(country==input$country_choice) %>%
        filter(cas_no==input$time_cas)
      
      doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      
      time_plot<-ggplot()+
        geom_line(data=Basis,aes(year,amount))+
        expand_limits(y=0)+
        coord_cartesian(xlim = c(as.Date(glue::glue("{min(Basis$year)}-01-01")),as.Date(glue::glue("{max(Basis$year)}-01-01"))))+
        geom_rect(data = doi,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="DOI"),alpha=0.2)+
        geom_rect(data = lad,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="LAD"),alpha=0.2)+
        geom_rect(data = sd,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="SD"),alpha=0.2)+
        scale_fill_manual("",values = c("yellow","orange","red"))+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        scale_y_continuous(labels=scales::comma_format())+
        labs(x="",y=glue::glue("Tonnes used per annum in {Basis$country[1]}"),title = glue::glue("Trend of {Basis$name[1]} in {Basis$country[1]}"))+
        theme(axis.text.x = element_text(angle=45,vjust = -0.25))
      
      if (input$time_glm){
        print(time_plot+
                geom_smooth(method = "lm",data = subset(Basis,year>=input$time_glm_year),se=F,aes(year,amount)))
      } else{
        print(time_plot)
      }
      
    } else {
      
      Basis<-time_data() %>%
        filter(year>=input$time_year) %>%
        filter(cas_no==input$time_cas)
      
      
      doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
      
      
      time_plot<-ggplot()+
        geom_line(data=Basis,aes(year,amount,col=country))+
        expand_limits(y=0)+
        coord_cartesian(xlim = c(as.Date(glue::glue("{min(Basis$year)}-01-01")),as.Date(glue::glue("{max(Basis$year)}-01-01"))))+
        geom_rect(data = doi,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="DOI"),alpha=0.2)+
        geom_rect(data = lad,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="LAD"),alpha=0.2)+
        geom_rect(data = sd,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="SD"),alpha=0.2)+
        scale_fill_manual("",values = c("yellow","orange","red"))+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        scale_y_continuous(labels=scales::comma_format())+
        labs(x="",y=glue::glue("Tonnes used per annum in Nordic Countries"),title = glue::glue("Trend of {Basis$name[1]} in Nordic Countries"))+
        theme(axis.text.x = element_text(angle=45,vjust = -0.25))
      
      if (input$time_glm){
        print(time_plot+
                geom_smooth(method = "lm",data = subset(Basis,year>=input$time_glm_year),se=F,aes(year,amount)))
      } else{
        print(time_plot)
      }
    }
  })
  
  # Fraction of single SVHC entries from the candidate-list which can be found in the SPIN database
  output$pie<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    SPIN<-round((sum((unique(candidate_data()$cas_no) %in% spin_data()$cas_no))-4)/(length(unique(candidate_data()$cas_no))-6),2)*100
    n_SPIN<-round(((length(unique(candidate_data()$cas_no))-6)-(sum((unique(candidate_data()$cas_no) %in% spin_data()$cas_no))-4))/(length(unique(candidate_data()$cas_no))-6),2)*100
    name<-c(paste0(SPIN,"%"),paste0(n_SPIN,"%"))
    pie3D(c(SPIN,n_SPIN),theta = 0.9,col = c("darkred","darkblue"),main="SVHCs in SPIN")
    text(-0.6,0.7,name[1],cex = 1.5)
    text(0.7,-0.7,name[2],cex = 1.5)
    legend("topright",legend = c("in SPIN","not in SPIN"),col=c("darkred","darkblue"),pch=16)
  })
  
  # Trend of SVHCs without Intermediates
  output$interplot<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    basis<-join_data() %>% 
      anti_join(inter$data,by="cas_no") %>%
      group_by(year,country) %>%
      summarize(total_amount=sum(amount)) %>%
      ungroup() %>%
      filter(country==input$inter_countries) %>%
      filter(year>=input$inter_year)
    plo<- basis%>%
      ggplot(aes(year,total_amount))+
      geom_bar(stat="identity",fill=input$col_second)+
      scale_y_continuous(glue::glue("Tonnes per annum used in {input$inter_countries}"),labels = scales::comma_format())+
      labs(x="",title = glue::glue("Trend of SVHCs in {input$inter_countries} without Intermediates"))+
      scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
      theme(axis.text.x = element_text(angle=45,hjust = 1))
    if (input$inter_glm){
      print(plo+
              geom_smooth(method = "lm",se=F,aes(year,total_amount),data = subset(basis,year>=input$inter_glm_year)))
    } else{
      print(plo)
    }
    
  })
  
  # Trend of SVHCs, single or all chemicals
  output$barplot<-renderPlot({
    shiny::validate(
      need((input$spin!="" & input$svhc!="" & input$authorisation!=""),"All files need to be uploaded before the plot can be generated")
    )
    if (input$type=="SVHC"){
      basis<-plot3_data() %>%
        filter(country==input$countries) %>%
        filter(year>=input$year)
      plo<- basis%>%
        ggplot(aes(year,total_amount))+
        geom_bar(stat="identity",fill=input$col_first)+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
        labs(x="",title = glue::glue("Trend of SVHCs in {input$countries}"))+
        scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
      if (input$glm){
        print(plo+
                geom_smooth(method = "lm",se=F,aes(year,total_amount),data = subset(basis,year>=input$glm_year)))
      } else{
        print(plo)
      }
    } else if (input$type=="All"){
      basis<-plot3_data() %>%
        group_by(country,year) %>%
        summarize(all=sum(amount)) %>%
        filter(country==input$countries) %>%
        filter(year>=input$year)
      plo<- basis %>%
        ggplot(aes(year,all))+
        geom_bar(stat="identity",fill=input$col_first)+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
        scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
        labs(x="",title = glue::glue("Trend of Chemicals in {input$countries}"))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
      if (input$glm){
        print(plo+
                geom_smooth(method = "lm",se=F,aes(year,all),data = subset(basis,year>=input$glm_year)))
      } else{
        print(plo)
      }
    } else if (input$type=="Cas"){
      basis<-plot3_data() %>%
        group_by(country,year,cas_no) %>%
        summarize(all=sum(amount)) %>%
        filter(country==input$countries) %>%
        filter(year>=input$year) %>%
        filter(cas_no==input$cas)
      plo<- basis %>%
        ggplot(aes(year,all))+
        geom_bar(stat="identity",fill=input$col_first)+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
        scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
        labs(x="",title = glue::glue("Trend of {plot3_data() %>%
                                     filter(cas_no==input$cas) %>%
                                     select(name) %>%
                                     slice(1:1)} in {input$countries}"))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
      if (input$glm){
        print(plo+
                geom_smooth(method = "lm",se=F,aes(year,all),data = subset(basis,year>=input$glm_year)))
      } else{
        print(plo)
      }
    }
  })
  
  ## 2.5 Download handler for tables and plots
  
  # Data of the table displaying the problematic chemicals
  output$downloadcarcinogenictable<-downloadHandler(
    filename = function(){
      if (input$add_datatype=="xlsx"){
        paste("problematic_chemicals","xlsx",sep = ".")
      } else if (input$add_datatype=="csv"){
        paste("problematic_chemicals","csv",sep = ".")
      }
    },
    content = function(file){
      if (input$add_datatype=="xlsx"){
        write_xlsx(carc$data,file)
      } else if (input$add_datatype=="csv"){
        write.csv(carc$data,file)
      }
    }
  )
  
  # Data of the trend of normal and SVHC chemicals relative to a reference year
  output$downloadnormalrel_back<-downloadHandler(
    filename = function(){
      paste("spin_rel_trend_data","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(
        normal_rel_data() %>%
          select(-all,-svhc,-svhc_amount,SVHC=verg_svhc,Normal=verg_normal) %>%
          gather(Type,value,-country,-year) %>%
          rename(type=Type,effective_change=value),file)
    }
  )
  
  # Plot of the trend of normal and SVHC chemicals relative to a reference year
  output$downloadnormalrel<-downloadHandler(
    filename = function(){
      paste("spin_rel_trend","png",sep = ".")
    },
    content = function(file){
      normal_plot<-normal_rel_data() %>%
        select(-all,-svhc,-svhc_amount,SVHC=verg_svhc,Normal=verg_normal) %>%
        gather(Type,value,-country,-year) %>%
        ggplot(aes(year,value,fill=Type,width=0.4))+
        geom_histogram(stat="identity",position = "dodge")+
        scale_y_continuous("Effective Change",labels = scales::percent_format())+
        scale_x_continuous(breaks = seq(min(normal_rel_data()$year),max(normal_rel_data()$year)))+
        labs(x="",title = glue::glue("Trend of SVHCs and Normal chemicals in {input$normal_country} relative to {input$normal_year}"))+
        expand_limits(y=c(0,0.75))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
      ggsave(file,plot=print(normal_plot),device="png")
    }
  )
  
  # Data of table displaying five biggest differences to a reference year in tonnes and per type of chemicals
  output$downloadnormal_rel_table<-downloadHandler(
    filename = function(){
      if (input$normal_rel_datatype=="csv"){
        paste("spin_all_rel","csv",sep = ".")
      } else if (input$normal_rel_datatype=="xlsx"){
        paste("spin_all_rel","xlsx",sep = ".")
      }
    },
    content = function(file){
      if (input$normal_rel_datatype=="csv"){
        write.csv(
          normal_table_data() %>%
            group_by(country,year,svhc,cas_no) %>%
            summarize(total=sum(amount)) %>%
            filter(country==input$normal_rel_country) %>%
            inner_join(normal_table_data() %>%
                         group_by(country,year,svhc,cas_no) %>%
                         summarize(total=sum(amount)) %>%
                         filter(country==input$normal_rel_country) %>%
                         filter(year==input$normal_rel_year),by=c("cas_no","country","svhc")) %>%
            select(year=year.x,svhc,country,cas_no,-year.y,total=total.x,comp_total=total.y) %>%
            mutate(difference=total-comp_total) %>%
            filter(year==input$compare_rel_year) %>%
            arrange(desc(abs(difference))) %>%
            slice(1:5) %>%
            ungroup() %>%
            rename(current_tonnes=total,reference_tonnes=comp_total),file)
      } else if (input$normal_rel_datatype=="xlsx"){
        write_xlsx(
          normal_table_data() %>%
            group_by(country,year,svhc,cas_no) %>%
            summarize(total=sum(amount)) %>%
            filter(country==input$normal_rel_country) %>%
            inner_join(normal_table_data() %>%
                         group_by(country,year,svhc,cas_no) %>%
                         summarize(total=sum(amount)) %>%
                         filter(country==input$normal_rel_country) %>%
                         filter(year==input$normal_rel_year),by=c("cas_no","country","svhc")) %>%
            select(year=year.x,svhc,country,cas_no,-year.y,total=total.x,comp_total=total.y) %>%
            mutate(difference=total-comp_total) %>%
            filter(year==input$compare_rel_year) %>%
            arrange(desc(abs(difference))) %>%
            slice(1:5) %>%
            ungroup() %>%
            rename(current_tonnes=total,reference_tonnes=comp_total),file)
      }
    }
  )
  
  # Data of the table displaying the SVHCs chemicals considered as intermediates
  output$downloadintertable<-downloadHandler(
    filename = function(){
      if (input$inter_datatype=="xlsx"){
        paste("intermediates","xlsx",sep = ".")
      } else if (input$inter_datatype=="csv"){
        paste("intermediates","csv",sep = ".")
      }
    },
    content = function(file){
      if (input$inter_datatype=="xlsx"){
        write_xlsx(inter$data,file)
      } else if (input$inter_datatype=="csv"){
        write.csv(inter$data,file)
      }
    }
  )
  
  # Data of the trend of SVHCs without Intermediates
  output$downloadinterplot_back<-downloadHandler(
    filename = function(){
      paste("spin_inter_trend_data","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(
        basis<-join_data() %>% 
          anti_join(inter$data,by="cas_no") %>%
          group_by(year,country) %>%
          summarize(total_amount=sum(amount)) %>%
          ungroup() %>%
          filter(country==input$inter_countries) %>%
          filter(year>=input$inter_year) %>%
          rename(tonnes=total_amount),file)
    }
  )
  
  # Plot of the trend of SVHCs without Intermediates
  output$downloadinterplot<-downloadHandler(
    filename = function(){
      paste("spin_inter_trend","png",sep = ".")
    },
    content = function(file){
      basis<-join_data() %>% 
        anti_join(inter$data,by="cas_no") %>%
        group_by(year,country) %>%
        summarize(total_amount=sum(amount)) %>%
        ungroup() %>%
        filter(country==input$inter_countries) %>%
        filter(year>=input$inter_year)
      plo<- basis%>%
        ggplot(aes(year,total_amount))+
        geom_bar(stat="identity",fill=input$col_second)+
        scale_y_continuous(glue::glue("Tonnes per annum used in {input$inter_countries}"),labels = scales::comma_format())+
        labs(x="",title = glue::glue("Trend of SVHCs in {input$inter_countries} without Intermediates"))+
        scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
      if (input$inter_glm){
        actual_plot<-plo+
          geom_smooth(method = "lm",se=F,aes(year,total_amount),data = subset(basis,year>=input$inter_glm_year))
      } else{
        actual_plot<-plo
      }
      ggsave(file,plot=print(actual_plot),device="png")
    }
  )
  
  # Data of the table displaying five biggest differences to a reference year in tonnes and per type of problematic chemicals
  output$downloadcarctable<-downloadHandler(
    filename = function(){
      if (input$carc_table_datatype=="csv"){
        paste("spin_problematic_chemicals_difference","csv",sep = ".")
      } else if (input$carc_table_datatype=="xlsx"){
        paste("spin_problematic_chemicals_difference","xlsx",sep = ".")
      }
    },
    content = function(file){
      if (input$carc_table_datatype=="csv"){
        write.csv(
          if (input$carc_table_type=="All"){
            spin_data() %>% 
              distinct(cas_no,year,country,.keep_all = T) %>%
              filter(year==input$comparison_table_year,
                     country==input$carc_table_country) %>%
              inner_join(carc$data,by="cas_no") %>%
              select(-name.x) %>%
              group_by(country,year,cas_no,name=name.y,type) %>%
              summarize(total=sum(amount)) %>%
              inner_join(join_data() %>%
                           filter(year==input$carc_table_year,
                                  country==input$carc_table_country) %>%
                           inner_join(carc$data,by="cas_no") %>%
                           select(-name.x) %>%
                           group_by(country,year,cas_no,name=name.y,type) %>%
                           summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
              select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
              mutate(difference=total-total_sum) %>%
              ungroup() %>%
              group_by(country,year,type) %>%
              arrange(desc(abs(difference))) %>%
              slice(1:5) %>%
              ungroup() %>%
              select(Year=year,Type=type,Cas=cas_no,name,difference)
          } else {
            spin_data() %>% 
              distinct(cas_no,year,country,.keep_all = T) %>%
              filter(year==input$comparison_table_year,
                     country==input$carc_table_country) %>%
              inner_join(carc$data,by="cas_no") %>%
              select(-name.x) %>%
              filter(type==input$carc_table_type) %>%
              group_by(country,year,cas_no,name=name.y,type) %>%
              summarize(total=sum(amount)) %>%
              inner_join(join_data() %>%
                           filter(year==input$carc_table_year,
                                  country==input$carc_table_country) %>%
                           inner_join(carc$data,by="cas_no") %>%
                           select(-name.x) %>%
                           filter(type==input$carc_table_type) %>%
                           group_by(country,year,cas_no,name=name.y,type) %>%
                           summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
              select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
              mutate(difference=total-total_sum) %>%
              ungroup() %>%
              group_by(country,year,type) %>%
              arrange(desc(abs(difference))) %>%
              slice(1:5) %>%
              ungroup() %>%
              select(Year=year,Type=type,Cas=cas_no,name,difference)
          }, file)
      } else if (input$carc_table_datatype=="xlsx"){
        write_xlsx(
          if (input$carc_table_type=="All"){
            spin_data() %>% 
              distinct(cas_no,year,country,.keep_all = T) %>%
              filter(year==input$comparison_table_year,
                     country==input$carc_table_country) %>%
              inner_join(carc$data,by="cas_no") %>%
              select(-name.x) %>%
              group_by(country,year,cas_no,name=name.y,type) %>%
              summarize(total=sum(amount)) %>%
              inner_join(join_data() %>%
                           filter(year==input$carc_table_year,
                                  country==input$carc_table_country) %>%
                           inner_join(carc$data,by="cas_no") %>%
                           select(-name.x) %>%
                           group_by(country,year,cas_no,name=name.y,type) %>%
                           summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
              select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
              mutate(difference=total-total_sum) %>%
              ungroup() %>%
              group_by(country,year,type) %>%
              arrange(desc(abs(difference))) %>%
              slice(1:5) %>%
              ungroup() %>%
              select(Year=year,Type=type,Cas=cas_no,name,difference)
          } else {
            spin_data() %>% 
              distinct(cas_no,year,country,.keep_all = T) %>%
              filter(year==input$comparison_table_year,
                     country==input$carc_table_country) %>%
              inner_join(carc$data,by="cas_no") %>%
              select(-name.x) %>%
              filter(type==input$carc_table_type) %>%
              group_by(country,year,cas_no,name=name.y,type) %>%
              summarize(total=sum(amount)) %>%
              inner_join(join_data() %>%
                           filter(year==input$carc_table_year,
                                  country==input$carc_table_country) %>%
                           inner_join(carc$data,by="cas_no") %>%
                           select(-name.x) %>%
                           filter(type==input$carc_table_type) %>%
                           group_by(country,year,cas_no,name=name.y,type) %>%
                           summarize(total=sum(amount)),by=c("country","cas_no","name","type")) %>%
              select(year=year.x,-year.y,total=total.x,total_sum=total.y,type,country,cas_no,name) %>%
              mutate(difference=total-total_sum) %>%
              ungroup() %>%
              group_by(country,year,type) %>%
              arrange(desc(abs(difference))) %>%
              slice(1:5) %>%
              ungroup() %>%
              select(Year=year,Type=type,Cas=cas_no,name,difference)
          }, file)
      }
    }
  )
  
  # Data of the trend of problematic chemicals relative to a reference year
  output$downloadcarcorel_back<-downloadHandler(
    filename = function(){
      paste("spin_rel_problematic_chemicals","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(carc_rel_data() %>%
                   select(year,type,tonnes=total,effective_change=rela),file)
    }
  )
  
  # Plot of the trend of problematic chemicals relative to a reference year
  output$downloadcarcorel<-downloadHandler(
    filename = function(){
      paste("spin_rel_poblematic_chemicals","png",sep=".")
    },
    content = function(file){
      if (input$carc_rel_type=="All"){
        carc_rel<-ggplot(carc_rel_data(),aes(year,rela,fill=type))+
          geom_col(position = position_dodge(width=0.5),width=0.5)+
          scale_y_continuous(labels = scales::percent_format())+
          scale_x_continuous(breaks=seq(input$carc_rel_year,max(carc_rel_data()$year)))+
          theme(axis.text.x = element_text(angle=45,hjust=1))+
          scale_fill_manual("Problematic Chemicals",values = c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))+
          labs(title=glue::glue("Trend of Problematic Chemicals in {input$carc_rel_country} relative to {input$carc_rel_year}"),y="Effective Change",x="")
      } else {
        carc_rel<-ggplot(carc_rel_data(),aes(year,rela,fill=type))+
          geom_col(position = position_dodge(width=0.5),width=0.5)+
          scale_y_continuous(labels = scales::percent_format())+
          scale_x_continuous(breaks=seq(input$carc_rel_year,max(carc_rel_data()$year)))+
          theme(axis.text.x = element_text(angle=45,hjust=1))+
          scale_fill_manual("Problematic Chemicals",values = input$col_fourth)+
          labs(title=glue::glue("Trend of Problematic Chemicals in {input$carc_rel_country} relative to {input$carc_rel_year}"),y="Effective Change",x="")
      }
      ggsave(file,plot=print(carc_rel),device="png")
    }
  )
  
  # Data of the trend of problematic chemicals
  output$downloadcarcoplot_back<-downloadHandler(
    filename = function(){
      paste("spin_area_prob_data","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(carc_data()%>%
                   rename(tonnes=total_amount),file)
    }
  )
  
  # Plot of the trend of problematic chemicals
  output$downloadcarcoplot<-downloadHandler(
    filename = function(){
      paste("spin_area_prob","png",sep = ".")
    },
    content = function(file){
      if(input$carco_type=="All"){
        final_plot<-carc_data() %>%
          ggplot(aes(year,total_amount,fill=type))+
          geom_area()+
          scale_y_continuous(glue::glue("Tonnes per annum used in {input$carco_country}"),labels = scales::comma_format())+
          scale_fill_discrete("Problematic Chemicals")+
          labs(x="",title = glue::glue("Trend of Problematic Chemicals in {input$carco_country}"))+
          scale_x_continuous(breaks=seq(input$carco_year,max(carc_data()$year)))+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      } else{
        final_plot<-carc_data() %>%
          ggplot(aes(year,total_amount,fill=type))+
          geom_area()+
          scale_y_continuous(glue::glue("Tonnes per annum used in {input$carco_country}"),labels = scales::comma_format())+
          scale_fill_manual("Problematic Chemicals",values = input$col_third)+
          labs(x="",title = glue::glue("Trend of Problematic Chemicals in {input$carco_country}"))+
          scale_x_continuous(breaks=seq(input$carco_year,max(carc_data()$year)))+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      }
      ggsave(file,plot=print(final_plot),device="png")
      
    }
  )
  
  # Data of the trend of Single Substances with Trigger Dates
  output$downloadtimeline_back<-downloadHandler(
    filename = function(){
      paste("spin_timeline_data","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(
        if(input$single_country){
          
          Basis<-time_data() %>%
            filter(year>=input$time_year) %>%
            filter(country==input$country_choice) %>%
            filter(cas_no==input$time_cas)
          
          doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          
          Basis %>%
            select(name,cas_no,country,year,tonnes=amount) %>%
            add_column(DOI=as.Date(doi$xmin,format="%d/%m/%Y"),
                       LAD=as.Date(lad$xmin,format="%d/%m/%Y"),
                       SD=as.Date(sd$xmin,format="%d/%m/%Y")) %>%
            arrange(country,year)
          
        } else {
          
          Basis<-time_data() %>%
            filter(year>=input$time_year) %>%
            filter(cas_no==input$time_cas)
          
          doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
          
          Basis %>%
            select(name,cas_no,country,year,tonnes=amount) %>%
            add_column(DOI=as.Date(doi$xmin,format="%d/%m/%Y"),
                       LAD=as.Date(lad$xmin,format="%d/%m/%Y"),
                       SD=as.Date(sd$xmin,format="%d/%m/%Y")) %>%
            arrange(country,year)
          
        },file)
    }
  )
  
  # Plot of the trend of Single Substances with Trigger Dates
  output$downloadtimeline<-downloadHandler(
    filename = function(){
      paste("spin_timeline","png",sep = ".")
    },
    content = function(file){
      if(input$single_country){
        
        Basis<-time_data() %>%
          filter(year>=input$time_year) %>%
          filter(country==input$country_choice) %>%
          filter(cas_no==input$time_cas)
        
        doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        
        time_plot<-ggplot()+
          geom_line(data=Basis,aes(year,amount))+
          expand_limits(y=0)+
          coord_cartesian(xlim = c(as.Date(glue::glue("{min(Basis$year)}-01-01")),as.Date(glue::glue("{max(Basis$year)}-01-01"))))+
          geom_rect(data = doi,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="DOI"),alpha=0.2)+
          geom_rect(data = lad,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="LAD"),alpha=0.2)+
          geom_rect(data = sd,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="SD"),alpha=0.2)+
          scale_fill_manual("",values = c("yellow","orange","red"))+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          scale_y_continuous(labels=scales::comma_format())+
          labs(x="",y=glue::glue("Tonnes used per annum in {Basis$country[1]}"),title = glue::glue("Trend of {Basis$name[1]} in {Basis$country[1]}"))+
          theme(axis.text.x = element_text(angle=45,vjust = -0.25))
        
        if (input$time_glm){
          actual_plot<-time_plot+
            geom_smooth(method = "lm",data = subset(Basis,year>=input$time_glm_year),se=F,aes(year,amount))
        } else{
          actual_plot<-time_plot
        }
        
      } else {
        
        Basis<-time_data() %>%
          filter(year>=input$time_year) %>%
          filter(cas_no==input$time_cas)
        
        doi<-data.frame(xmin=Basis$Datum[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        lad<-data.frame(xmin=Basis$latest_application_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        sd<-data.frame(xmin=Basis$sunset_date[1],xmax=as.Date("3000-01-01"),ymin=-Inf,ymax=Inf)
        
        time_plot<-ggplot()+
          geom_line(data=Basis,aes(year,amount,col=country))+
          expand_limits(y=0)+
          coord_cartesian(xlim = c(as.Date(glue::glue("{min(Basis$year)}-01-01")),as.Date(glue::glue("{max(Basis$year)}-01-01"))))+
          geom_rect(data = doi,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="DOI"),alpha=0.2)+
          geom_rect(data = lad,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="LAD"),alpha=0.2)+
          geom_rect(data = sd,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill="SD"),alpha=0.2)+
          scale_fill_manual("",values = c("yellow","orange","red"))+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          scale_y_continuous(labels=scales::comma_format())+
          labs(x="",y=glue::glue("Tonnes used per annum in Nordic Countries"),title = glue::glue("Trend of {Basis$name[1]} in Nordic Countries"))+
          theme(axis.text.x = element_text(angle=45,vjust = -0.25))
        
        if (input$time_glm){
          actual_plot<-time_plot+
            geom_smooth(method = "lm",data = subset(Basis,year>=input$time_glm_year),se=F,aes(year,amount))
        } else{
          actual_plot<-time_plot
        }
      }
      ggsave(file,plot=print(actual_plot),device="png")
    }
  )
  
  # Plot displaying the fraction of single SVHC entries from the candidate-list which can be found in the SPIN database
  output$downloadpie<-downloadHandler(
    filename = function(){
      paste("spin_pie","png",sep = ".")
    },
    content = function(file){
      png(file)
      SPIN<-round((sum((unique(candidate_data()$cas_no) %in% spin_data()$cas_no))-4)/(length(unique(candidate_data()$cas_no))-6),2)*100
      n_SPIN<-round(((length(unique(candidate_data()$cas_no))-6)-(sum((unique(candidate_data()$cas_no) %in% spin_data()$cas_no))-4))/(length(unique(candidate_data()$cas_no))-6),2)*100
      name<-c(paste0(SPIN,"%"),paste0(n_SPIN,"%"))
      pie3D(c(SPIN,n_SPIN),theta = 0.9,col = c("darkred","darkblue"),main="SVHCs in SPIN")
      text(-0.6,0.7,name[1],cex = 1.5)
      text(0.7,-0.7,name[2],cex = 1.5)
      legend("topright",legend = c("in SPIN","not in SPIN"),col=c("darkred","darkblue"),pch=16)
      dev.off()
    }
  )
  
  # Data of the table displaying the complete data
  output$downloadtable<-downloadHandler(
    filename = function(){
      if (input$frame_datatype=="csv"){
        paste("spin_table","csv",sep = ".")
      } else if (input$frame_datatype=="xlsx"){
        paste("spin_table","xlsx",sep = ".")
      }
    },
    content = function(file){
      if (input$frame_datatype=="csv"){
        write.csv(
          if (input$country_frame!="Nordic"){
            if (input$dataframe=="Complete"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              } else if(input$type_frame=="SVHC"){
                if (input$year_filter){
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(country==input$country_frame) %>%
                      filter(year==input$year_filter_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                } else {
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(country==input$country_frame) %>%
                      filter(year>=input$year_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                }
              } else if (input$type_frame=="Cas"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              }
            } else if (input$dataframe=="Total"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                }
              } else if (input$type_frame=="SVHC"){
                if (input$year_filter){
                  plot4_data() %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                }
              }
            }
          } else if (input$country_frame=="Nordic"){
            if (input$dataframe=="Complete"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year==input$year_filter_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year>=input$year_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              } else if(input$type_frame=="SVHC"){
                if (input$year_filter){
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(year==input$year_filter_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                } else {
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(year>=input$year_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                }
              } else if (input$type_frame=="Cas"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year==input$year_filter_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year>=input$year_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              }
            } else if (input$dataframe=="Total"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                }
              } else if (input$type_frame=="SVHC"){
                if (input$year_filter){
                  plot4_data() %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                }
              }
            }
          },file)
      } else if (input$frame_datatype=="xlsx"){
        write_xlsx(
          if (input$country_frame!="Nordic"){
            if (input$dataframe=="Complete"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              } else if(input$type_frame=="SVHC"){
                if (input$year_filter){
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(country==input$country_frame) %>%
                      filter(year==input$year_filter_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                } else {
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  }
                  spin_data() %>% 
                    distinct(cas_no,year,country,.keep_all = T) %>%
                    inner_join(candidate_data(),by="cas_no") %>%
                    distinct(cas_no,year,country,.keep_all = T) %>%
                    arrange(country,year) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    select(-PID,-Stoffname,-Datum) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              } else if (input$type_frame=="Cas"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              }
            } else if (input$dataframe=="Total"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                }
              } else if (input$type_frame=="SVHC"){
                if (input$year_filter){
                  plot4_data() %>%
                    filter(country==input$country_frame) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    filter(country==input$country_frame) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                }
              }
            }
          } else if (input$country_frame=="Nordic"){
            if (input$dataframe=="Complete"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year==input$year_filter_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year>=input$year_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              } else if(input$type_frame=="SVHC"){
                if (input$year_filter){
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(year==input$year_filter_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                } else {
                  if (input$group){
                    grouped_data() %>%
                      rename(tonnes=total)
                  } else {
                    spin_data() %>% 
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      inner_join(candidate_data(),by="cas_no") %>%
                      distinct(cas_no,year,country,.keep_all = T) %>%
                      arrange(country,year) %>%
                      filter(year>=input$year_frame) %>%
                      select(-PID,-Stoffname,-Datum) %>%
                      rename(tonnes=amount) %>%
                      arrange(year,country)
                  }
                }
              } else if (input$type_frame=="Cas"){
                if (input$year_filter){
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year==input$year_filter_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                } else {
                  spin_data() %>%
                    distinct(year,country,cas_no,.keep_all = T) %>%
                    filter(year>=input$year_frame) %>%
                    filter(cas_no==input$cas_frame) %>%
                    select(-PID) %>%
                    rename(tonnes=amount) %>%
                    arrange(year,country)
                }
              }
            } else if (input$dataframe=="Total"){
              if (input$type_frame=="All"){
                if (input$year_filter){
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    group_by(country,year) %>%
                    summarize(all=sum(amount)) %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=all) %>%
                    arrange(year,country)
                }
              } else if (input$type_frame=="SVHC"){
                if (input$year_filter){
                  plot4_data() %>%
                    filter(year==input$year_filter_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                } else {
                  plot4_data() %>%
                    filter(year>=input$year_frame) %>%
                    rename(tonnes=total_amount) %>%
                    arrange(year,country)
                }
              }
            }
          },file)
      }
    }
  )
  
  # Data of the trend of SVHCs, single or all chemicals
  output$downloadplot_back<-downloadHandler(
    filename = function(){
      paste("spin_plot_data","xlsx",sep = ".")
    },
    content = function(file){
      write_xlsx(
        if (input$type=="SVHC"){
          plot3_data() %>%
            filter(country==input$countries) %>%
            filter(year>=input$year) %>%
            rename(tonnes=total_amount)
        } else if (input$type=="All"){
          plot3_data() %>%
            group_by(country,year) %>%
            summarize(all=sum(amount)) %>%
            filter(country==input$countries) %>%
            filter(year>=input$year) %>%
            rename(tonnes=all)
        } else if (input$type=="Cas"){
          plot3_data() %>%
            group_by(country,year,cas_no) %>%
            summarize(all=sum(amount)) %>%
            filter(country==input$countries) %>%
            filter(year>=input$year) %>%
            filter(cas_no==input$cas) %>%
            rename(tonnes=all)
        },file)
    }
  )
  
  # Plot of the trend of SVHCs, single or all chemicals
  output$downloadplot<-downloadHandler(
    filename = function(){
      paste("spin_plot","png",sep = ".")
    },
    content = function(file){
      if (input$type=="SVHC"){
        basis<-plot3_data() %>%
          filter(country==input$countries) %>%
          filter(year>=input$year)
        plo<- basis%>%
          ggplot(aes(year,total_amount))+
          geom_bar(stat="identity",fill=input$col_first)+
          scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
          labs(x="",title = glue::glue("Trend of SVHCs in {input$countries}"))+
          scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
        if (input$glm){
          actual_plot<-plo+
            geom_smooth(method = "lm",se=F,aes(year,total_amount),data = subset(basis,year>=input$glm_year))
        } else{
          actual_plot<-plo
        }
      } else if (input$type=="All"){
        basis<-plot3_data() %>%
          group_by(country,year) %>%
          summarize(all=sum(amount)) %>%
          filter(country==input$countries) %>%
          filter(year>=input$year)
        plo<- basis %>%
          ggplot(aes(year,all))+
          geom_bar(stat="identity",fill=input$col_first)+
          scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
          scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
          labs(x="",title = glue::glue("Trend of Chemicals in {input$countries}"))+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
        if (input$glm){
          actual_plot<-plo+
            geom_smooth(method = "lm",se=F,aes(year,all),data = subset(basis,year>=input$glm_year))
        } else{
          actual_plot<-plo
        }
      } else if (input$type=="Cas"){
        basis<-plot3_data() %>%
          group_by(country,year,cas_no) %>%
          summarize(all=sum(amount)) %>%
          filter(country==input$countries) %>%
          filter(year>=input$year) %>%
          filter(cas_no==input$cas)
        plo<- basis %>%
          ggplot(aes(year,all))+
          geom_bar(stat="identity",fill=input$col_first)+
          scale_y_continuous(glue::glue("Tonnes per annum used in {input$countries}"),labels = scales::comma_format())+
          scale_x_continuous(breaks=seq(input$year,max(plot3_data()$year)))+
          labs(x="",title = glue::glue("Trend of {plot3_data() %>%
                                     filter(cas_no==input$cas) %>%
                                     select(name) %>%
                                     slice(1:1)} in {input$countries}"))+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
        if (input$glm){
          actual_plot<-plo+
            geom_smooth(method = "lm",se=F,aes(year,all),data = subset(basis,year>=input$glm_year))
        } else{
          actual_plot<-plo
        }
      }
      ggsave(file,plot=print(actual_plot),device = "png")
    }
  )
}

shinyApp(ui = ui, server = server)

