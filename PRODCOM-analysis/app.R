library(shiny)
library(colourpicker)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(leaflet)
library(sp)
library(rworldmap)
library(DT)
library(surveytoolbox)
library(lubridate)
library(haven)
library(writexl)
library(readxl)
library(colourpicker)
library(mapview)
library(labelled)
webshot::install_phantomjs()


ui <- dashboardPage(
  dashboardHeader (title=span(tagList(icon("database"),"PRODCOM"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data",tabName = "Upload_Data",icon = icon("upload")),
      menuItem("Map",tabName = "Map",icon = icon("map")),
      menuItem("EU Plots",tabName = "eu_plot",icon = icon("chart-bar")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Upload_Data",
              fluidRow(box(width=3,background = "light-blue",column(12,offset = 2,h2(strong("Upload Data")))),
                       box(width = 12,h3(strong("Where to download the data from")),
                           p("You can download the data on the official",tags$a(href="https://ec.europa.eu/eurostat/de/web/prodcom/data/database","Eurostat website")),
                           h3(strong("Total production data")),
                           p("You need the data for the total prodcution in the spss format"))),
              fluidRow(box(width=12,fileInput("sav",accept = ".sav",label = strong("Total production data")))),
              fluidRow(box(width = 12,helpText("Here you can add substances based on their prccode, only listed substances can be used. When more than one prccode is in the list and 'Grouped' is selected in the control panel for the plots, the tonnes will be summed up"),
                           br(),
                           dataTableOutput("list"),
                           br(),
                           numericInput("code",label = strong("Codes to Add"),value = 20141353),
                           textOutput("extra_info"),
                           tags$head(tags$style("#extra_info{color:red}")),
                           br(),
                           actionButton("add","Add"),
                           actionButton("remove","Remove"),
                           actionButton("remove_all","Reset"))),
              fluidRow(box(width = 12,helpText("Here you can upload a file when you have too many codes and do not want to enter every single one. Just upload a file in the xlsx format with one column named 'Code'. The current selection will be discarded and the uploaded selection will be used"),
                           br(),
                           fileInput("upload_codes",label = strong("Codes"),accept = ".xlsx")))),
      tabItem(tabName="eu_plot",
              fluidRow(box(width = 3,background = "light-blue",column(12,offset = 3,h2(strong("EU Plots"))))),
              fluidRow(box(title = "Controls",width = 3,
                           selectInput("eu",label = strong("EU Countries"),choices = c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
                           selectInput("eu_years",label = strong("Years Since"),choices = seq(as.Date(ISOdate(2008,1,1)),as.Date(ISOdate(2018,1,1)),"years")),
                           conditionalPanel("input.eu_grouped!=true",selectInput("substance_eu",label = strong("Select Substance"),choices = 20135125)),
                           checkboxInput("eu_grouped","Grouped"),
                           colourInput("eu_col",label = strong("Colour for Plot"),value = "#91CFEE"),
                           downloadButton("download_eu","Download Plot"),
                           downloadButton("download_eu_data","Download Data")),
                       box(width = 9,
                           plotOutput("boxplot"))),
              fluidRow(box(title = "Controls",width = 3,
                           checkboxInput("single_year","Single Year"),
                           conditionalPanel("input.single_year==true",selectInput("box_year",label = strong("Year"),choices = seq(as.Date(ISOdate(2008,1,1)),as.Date(ISOdate(2018,1,1)),"years"))),
                           conditionalPanel("input.box_grouped!=true",selectInput("substance_box",label = strong("Select Substance"),choices = 20135125)),
                           checkboxInput("box_grouped","Grouped"),
                           conditionalPanel("input.single_year==true & input.box_grouped==false",colourInput("box_col",label = strong("Colour for Plot"),value = "#91CFEE")),
                           downloadButton("download_box","Download Plot"),
                           downloadButton("download_box_data","Download Data")),
                       box(width = 9,
                           plotOutput("real_box")))),
      tabItem(tabName = "Map",
              fluidRow(absolutePanel(fixed = TRUE,
                                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                     width = 500, height = "auto", style = "opacity: 1; z-index: 10;",
                                     box(background = "light-blue",width = 12,textOutput("info"),
                                         br(),
                                         plotOutput("country_plot"),
                                         conditionalPanel("input.plot_grouped!=true",selectInput("substance_plot",label = strong("Select Substance"),choices = 20135125)),
                                         checkboxInput("plot_grouped","Grouped"),
                                         downloadButton("download_plot","Download Plot"),
                                         downloadButton("download_plot_data","Download Data"),
                                         downloadButton("mapdownload","Download Map"))),
                       tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                       leafletOutput("map"))))))


server <- function(input, output, session) {
  theme_set(theme_light())
  
  eu_data<-reactive({
    input_data<-input$sav
    dataset<-read_sav(input_data$datapath) %>%
      clean_names()

    dataset<-dataset %>%
      mutate_at(vars(indicators,decl,period,flags_footnotes),as_factor) %>%
      extract(period,"year",regex = "(\\d{4})",remove = T)

    dataset$year<-as.Date(ISOdate(dataset$year,1,1))
    dataset <- dataset %>%
      filter(indicators=="PRODQNT",
             value!=":") %>%
      mutate(value=as.character(as_factor(value))) %>%
      mutate(value=as.numeric(value)/1000,
             temp_filt=prccode)
    
    val_labels(dataset$temp_filt) <- NULL
    dataset %>%
      mutate(temp_filt=as.numeric(temp_filt))
    
  })
  
  
  init_codes<-data.frame("Prccode"=20135125,"Name"="Chromates and dichromates; peroxochromates")
  prcodes<-reactiveValues(codes=init_codes)
  
  info_text <- reactiveValues(text="")
  
  output$debug <- renderTable({
  })
  
  observeEvent(input$sav,{
    slice_index<-NULL
    for (i in 1:length(prcodes$codes$Prccode)){
      if (!(prcodes$codes$Prccode[i] %in% eu_data()$prccode)){
        print(prcodes$codes$Prccode[i] %in% eu_data()$prccode)
        slice_index<-c(slice_index,i)
        print(glue::glue("Element number {i} is not a real code"))
      }
    }

    if (length(slice_index!=0)){
      temp_data<-prcodes$codes %>%
        slice(-slice_index) %>%
        select(-Name)
      prcodes$codes<-temp_data %>%
        add_column(Name=sapply(temp_data$Prccode,function(x) unique(eu_data() %>% filter(temp_filt==x) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))))

    } else {
      prcodes$codes<-prcodes$codes %>%
        select(-Name) %>%
        add_column(Name=sapply(prcodes$codes$Prccode,function(x) unique(eu_data() %>% filter(temp_filt==x) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))))
    }
  })
  
  observeEvent(input$remove_all,{
    prcodes$codes<-data.frame("Prccode"=20135125,"Name"="Chromates and dichromates; peroxochromates")
  })
  
  observeEvent(input$upload_codes,{
    input_upload<-input$upload_codes
    input_data2<-input$sav
    if (is.null(input_data2)){
      data<-read_xlsx(input_upload$datapath)
      prcodes$codes<-data.frame("Prccode"=data$Code,"Name"=rep("Can not be assigned yet - upload the data",length(data$Code)))
    } else {
      data<-read_xlsx(input_upload$datapath)
      prcodes$codes<-data.frame("Prccode"=data$Code) %>%
        add_column(Name=sapply(data$Code,function(x) unique(eu_data() %>% filter(temp_filt==x) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))))
    }
  })
  
  observeEvent(input$add,{
    input_data3<-input$sav
    if (is.null(input_data3)){
      if (input$code %in% prcodes$codes$Prccode){
        info_text$text <- "Already in the list"
      } else {
        prcodes$codes<-prcodes$codes %>%
          add_row(Prccode=input$code,Name="Can not be assigned yet - upload the data")
        info_text$text <- ""
      }
    } else {
      if (!(input$code %in% eu_data()$prccode)){
        info_text$text <- "Not in the list" 
      } else {
        if (input$code %in% prcodes$codes$Prccode){
          info_text$text <- "Already in the list"
        } else {
          prcodes$codes<-prcodes$codes %>%
            add_row(Prccode=input$code,Name=unique(eu_data() %>% filter(temp_filt==input$code) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode)))
          info_text$text <- ""
        }
      }
    }
  })
  
  
  observeEvent(input$remove,{
    if (!(input$code %in% prcodes$codes$Prccode)){
      info_text$text <- "Not in the list" 
    } else {
      prcodes$codes<-prcodes$codes%>%
        slice(-which(prcodes$codes$Prccode==input$code))
      info_text$text <- ""
    }
    
  })
  
  output$list<-renderDataTable({
    prcodes$codes
  })
  
  output$extra_info <- renderText({
    info_text$text
  })
  
  coords2country<-function(points)
  {  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    # indices$REGION   # returns the continent (7 continent model)
    return(indices$ADMIN)  #returns country name
    #indices$ISO3 # returns the ISO3 code 
  }
  
  observe({
    input_data<-input$sav
    if(is.null(input_data)){
      return("Error")
    }
    dataset<-read_sav(input_data$datapath) %>%
      clean_names()

    dataset<-dataset %>%
      mutate_at(vars(indicators,decl,period,flags_footnotes),as_factor) %>%
      extract(period,"year",regex = "(\\d{4})",remove = T)

    dataset$year<-as.Date(ISOdate(dataset$year,1,1))
    dataset %>%
      filter(indicators=="PRODQNT",
             value!=":") %>%
      mutate(value=as.character(as_factor(value))) %>%
      mutate(value=as.numeric(value)/1000)

    if(input$eu_grouped){
      dataset1<-dataset %>%
        filter(prccode==input$substance_eu)
    } else {
      dataset1<-dataset
    }

    if(input$box_grouped){
      dataset2<-dataset %>%
        filter(prccode==input$substance_box)
    } else {
      dataset2<-dataset
    }

    updateSelectInput(session,"eu_years",choices = seq(min(dataset1$year),max(dataset1$year),"years"))
    updateSelectInput(session,"box_year",choices = seq(min(dataset2$year),max(dataset2$year),"years"))
  })
  
  observe({
    updateSelectInput(session,"substance_plot",choices = prcodes$codes$Prccode)
    updateSelectInput(session,"substance_eu",choices = prcodes$codes$Prccode)
    updateSelectInput(session,"substance_box",choices = prcodes$codes$Prccode)
  })
  
  eu_countries<-reactive({
    eu_data() %>%
      distinct(decl) %>%
      pull(decl)
  })
  
  countries<-eventReactive(input$sav,{
    countries<-rgdal::readOGR("./geodata/countries.geojson")
    countries<-subset(countries,ADMIN %in% eu_countries())
  })
  
  plot_country<-eventReactive(input$map_shape_click,{
    p<-input$map_shape_click
    click_country<-data.frame(long=p$lng[1],lat=p$lat[1])
    as.character(coords2country(click_country))
  })
  
  prd<-reactive({
    if (input$plot_grouped){
      verg<-eu_data() %>%
        filter(prccode %in% prcodes$codes$Prccode) %>%
        group_by(decl) %>%
        summarize(total=sum(value)) %>%
        mutate(decl=as.character(decl)) %>%
        filter(!(decl %in% c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
               decl %in% countries()$ADMIN) %>%
        arrange(decl)
      
      data.frame(decl=countries()$ADMIN) %>%
        left_join(verg,by="decl") %>%
        mutate(total=coalesce(total,0)) %>%
        pull(total)
    } else {
      verg<-eu_data() %>%
        filter(prccode==input$substance_plot) %>%
        group_by(decl) %>%
        summarize(total=sum(value)) %>%
        mutate(decl=as.character(decl)) %>%
        filter(!(decl %in% c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
               decl %in% countries()$ADMIN) %>%
        arrange(decl)
      
      data.frame(decl=countries()$ADMIN) %>%
        left_join(verg,by="decl") %>%
        mutate(total=coalesce(total,0)) %>%
        pull(total)
    }
  })
  
  output$info<-renderText({
    "The plot for the country you click on will be displayed here"
  })
  
  output$country_plot<-renderPlot({
    shiny::validate(
      need(input$sav!="","Upload the file containing the total production")
    )
    shiny::validate(
      need(input$map_shape_click!="","Klick on a country to display the production")
    )
    if (input$plot_grouped){
      verg<-eu_data() %>%
        filter(prccode %in% prcodes$codes$Prccode) %>%
        group_by(decl) %>%
        summarize(total=sum(value)) %>%
        mutate(decl=as.character(decl)) %>%
        filter(!(decl %in% c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
               decl %in% countries()$ADMIN) %>%
        arrange(decl)
      
      matching<-data.frame(decl=countries()$ADMIN) %>%
        left_join(verg,by="decl") %>%
        mutate(total=coalesce(total,0))
    } else {
      verg<-eu_data() %>%
        filter(prccode==input$substance_plot) %>%
        group_by(decl) %>%
        summarize(total=sum(value)) %>%
        mutate(decl=as.character(decl)) %>%
        filter(!(decl %in% c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
               decl %in% countries()$ADMIN) %>%
        arrange(decl)
      
      matching<-data.frame(decl=countries()$ADMIN) %>%
        left_join(verg,by="decl") %>%
        mutate(total=coalesce(total,0))
    }
    
    if (input$plot_grouped){
      eu_data() %>%
        filter(decl==plot_country()) %>%
        filter(prccode %in% prcodes$codes$Prccode) %>%
        group_by(year,decl) %>%
        summarize(total=sum(value)) %>%
        ggplot(aes(year,total))+
        geom_bar(stat = "identity",fill=pal(sqrt(prd()))[which(matching$decl==plot_country())],width = 250)+
        scale_y_continuous(labels = scales::comma_format())+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        labs(title = paste(strwrap(glue::glue("Total Production of Selected Substances in {plot_country()}"),width = 70),collapse = "\n"),x="",y="Tonnes",subtitle = "Sums of The Selected substances for Every Year")+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    } else {
      eu_data() %>%
        filter(decl==plot_country()) %>%
        filter(prccode==input$substance_plot) %>%
        group_by(year,decl) %>%
        summarize(total=sum(value)) %>%
        ggplot(aes(year,total))+
        geom_bar(stat = "identity",fill=pal(sqrt(prd()))[which(matching$decl==plot_country())],width = 250)+
        scale_y_continuous(labels = scales::comma_format())+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        labs(title = paste(strwrap(glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_plot) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in {plot_country()}"),width = 70),collapse = "\n"),x="",y="Tonnes")+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    }
  })
  
  output$download_plot<-downloadHandler(
    filename = function(){
      paste(glue::glue("{plot_country()}_plot"),"png",sep = ".")
    },
    content = function(file){
      verg<-eu_data() %>%
        filter(prccode %in% prcodes$codes$Prccode) %>%
        group_by(decl) %>%
        summarize(total=sum(value)) %>%
        mutate(decl=as.character(decl)) %>%
        filter(!(decl %in% c("EU27TOTALS_2007","EU27TOTALS_2020","EUROPEAN UNION (28)")),
               decl %in% countries()$ADMIN) %>%
        arrange(decl)
      
      matching<-data.frame(decl=countries()$ADMIN) %>%
        left_join(verg,by="decl") %>%
        mutate(total=coalesce(total,0))
      
      if(input$plot_grouped){
        country_pl<-eu_data() %>%
          filter(decl==plot_country()) %>%
          filter(prccode %in% prcodes$codes$Prccode) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(year,total))+
          geom_bar(stat = "identity",fill=pal(prd())[which(matching$decl==plot_country())],width = 250)+
          scale_y_continuous(labels = scales::comma_format())+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          labs(title = paste(strwrap(glue::glue("Total Production of Selected Substances in {plot_country()}"),width = 80),collapse = "\n"),x="",y="Tonnes",subtitle = "Sums of The Selected substances for Every Year")+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      } else {
        country_pl<-eu_data() %>%
          filter(decl==plot_country()) %>%
          filter(prccode==input$substance_plot) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(year,total))+
          geom_bar(stat = "identity",fill=pal(prd())[which(matching$decl==plot_country())],width = 250)+
          scale_y_continuous(labels = scales::comma_format())+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          labs(title = paste(strwrap(glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_plot) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in {plot_country()}"),width = 80),collapse = "\n"),x="",y="Tonnes")+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      }
      ggsave(file,plot=print(country_pl),device="png")
    }
  )
  
  output$download_plot_data<-downloadHandler(
    filename =  function(){
      paste(glue::glue("{plot_country()}_plot_data"),"xlsx",sep = ".")
    },
    content = function(file){
      if (input$plot_grouped){
        write_xlsx(
          eu_data() %>%
            filter(decl==plot_country()) %>%
            filter(prccode %in% prcodes$codes$Prccode) %>%
            group_by(year,decl) %>%
            summarize(total=sum(value)) %>%
            select(country=decl,year,tonnes=total)
          ,file)
      } else {
        write_xlsx(
          eu_data() %>%
            filter(decl==plot_country()) %>%
            filter(prccode==input$substance_plot) %>%
            group_by(year,decl) %>%
            summarize(total=sum(value)) %>%
            select(country=decl,year,tonnes=total)
          ,file)
      }
    }
  )
  
  output$boxplot<-renderPlot({
    shiny::validate(
      need(input$sav!="","Upload the file containing the total production")
    )
    if (input$eu_grouped){
      eu_data() %>%
        filter(decl==input$eu) %>%
        filter(prccode %in% prcodes$codes$Prccode) %>%
        filter(year>=input$eu_years) %>%
        group_by(year,decl) %>%
        summarize(total=sum(value)) %>%
        ggplot(aes(year,total))+
        geom_bar(stat = "identity",fill=input$eu_col,width = 250)+
        scale_y_continuous(labels = scales::comma_format())+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        labs(title = glue::glue("Total Production of Selected Substances in {input$eu}"),x="",y="Tonnes",subtitle = "Sums of The Selected substances for Every Year")+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    } else {
      eu_data() %>%
        filter(decl==input$eu) %>%
        filter(prccode==input$substance_eu) %>%
        filter(year>=input$eu_years) %>%
        group_by(year,decl) %>%
        summarize(total=sum(value)) %>%
        ggplot(aes(year,total))+
        geom_bar(stat = "identity",fill=input$eu_col,width = 250)+
        scale_y_continuous(labels = scales::comma_format())+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_eu) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in {input$eu}"),x="",y="Tonnes")+
        theme(axis.text.x = element_text(angle=45,hjust = 1))
    }
  })
  
  output$download_eu<-downloadHandler(
    filename = function(){
      paste(glue::glue("{input$eu}_plot"),"png",sep=".")
    },
    content = function(file){
      if (input$eu_grouped){
        eu_plot<-eu_data() %>%
          filter(decl==input$eu) %>%
          filter(prccode %in% prcodes$codes$Prccode) %>%
          filter(year>=input$eu_years) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(year,total))+
          geom_bar(stat = "identity",fill=input$eu_col,width = 250)+
          scale_y_continuous(labels = scales::comma_format())+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          labs(title = glue::glue("Total Production of Selected Substances in {input$eu}"),x="",y="Tonnes",subtitle = "Sums of The Selected substances for Every Year")+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      } else {
        eu_plot<-eu_data() %>%
          filter(decl==input$eu) %>%
          filter(prccode==input$substance_eu) %>%
          filter(year>=input$eu_years) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(year,total))+
          geom_bar(stat = "identity",fill=input$eu_col,width = 250)+
          scale_y_continuous(labels = scales::comma_format())+
          scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
          labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_eu) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in {input$eu}"),x="",y="Tonnes")+
          theme(axis.text.x = element_text(angle=45,hjust = 1))
      }
      ggsave(file,plot = print(eu_plot),device = "png")
    }
  )
  
  output$download_eu_data<-downloadHandler(
    filename = function(){
      paste(glue::glue("{input$eu}_plot_data"),"xlsx",sep=".")
    },
    content = function(file){
      write_xlsx(
        eu_data() %>%
          filter(prccode %in% prcodes$codes$Prccode) %>%
          filter(year>=input$eu_years) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          select(country=decl,year,tonnes=total)
        ,file)
    }
  )
  
  pal<-colorNumeric("viridis",NULL)
  
  output$map<-renderLeaflet({
    leaflet(countries()) %>%
      addProviderTiles(provider=providers$OpenStreetMap)  %>%
      addPolygons(stroke = T,color = "grey",fillColor = ~pal(sqrt(prd())),fillOpacity = 1,label = countries()$ADMIN) %>%
      setView(lat = 55.759453,lng = 24.588715,zoom = 2.5) %>%
      addLegend(pal = pal,values = ~prd(),opacity = 1,title = "Total Production [t]")
    
  })
  
  export_map<-reactive({
    leaflet(countries()) %>%
      addProviderTiles(provider=providers$OpenStreetMap)  %>%
      addPolygons(stroke = T,color = "grey",fillColor = ~pal(sqrt(prd())),fillOpacity = 1,label = countries()$ADMIN) %>%
      setView(lat = 56.075041,lng = 19.659250,zoom = 2.5) %>%
      addLegend(pal = pal,values = ~prd(),opacity = 1,title = "Total Production [t]") 
  })
  
  output$mapdownload<-downloadHandler(
    filename = function(){
      paste("map_europe","png",sep = ".")
    },
    content = function(file){
      mapshot(x=export_map(),
              cliprect="viewport",
              selfcontained=F,
              file = file)
    }
  )
  
  
  output$real_box<-renderPlot({
    shiny::validate(
      need(input$sav!="","Upload the file containing the total production")
    )
    if (input$box_grouped){
      if (input$single_year){
        eu_data() %>%
          filter(prccode %in% prcodes$codes$Prccode) %>%
          filter(year==input$box_year) %>%
          group_by(decl,prccode) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(decl,total))+
          geom_boxplot()+
          labs(title = glue::glue("Total Production of Selected Substances in Europe in {year(input$box_year)}"),x="",y="Tonnes")+
          scale_y_sqrt(labels = scales::comma_format())+
          theme(axis.text.x = element_text(angle=90,hjust = 1))
      } else {
        eu_data() %>%
          filter(prccode %in% prcodes$codes$Prccode) %>%
          group_by(decl,prccode) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(decl,total))+
          geom_boxplot()+
          labs(title = glue::glue("Total Production of Selected Substances in Europe from {year(min(eu_data()$year))} to {year(max(eu_data()$year))}"),x="",y="Tonnes",subtitle = "Tonnes of a Substance for Every Year Summed Up")+
          scale_y_sqrt(labels = scales::comma_format())+
          theme(axis.text.x = element_text(angle=90,hjust = 1))
      }
    } else {
      if (input$single_year){
        eu_data() %>%
          filter(prccode==input$substance_box) %>%
          filter(year==input$box_year) %>%
          ggplot(aes(decl,value))+
          geom_bar(stat = "identity",fill=input$box_col)+
          labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_box) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in Europe in {year(input$box_year)}"),x="",y="Tonnes")+
          scale_y_sqrt(labels = scales::comma_format())+
          theme(axis.text.x = element_text(angle=90,hjust = 1))
      } else {
        eu_data() %>%
          filter(prccode==input$substance_box) %>%
          group_by(year,decl) %>%
          summarize(total=sum(value)) %>%
          ggplot(aes(decl,total))+
          geom_boxplot()+
          labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_box) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in Europe from {year(min(eu_data()$year))} to {year(max(eu_data()$year))}"),x="",y="Tonnes")+
          scale_y_sqrt(labels = scales::comma_format())+
          theme(axis.text.x = element_text(angle=90,hjust = 1))
      }
    }
  })
  
  output$download_box<-downloadHandler(
    filename = function(){
      paste("Europe_plot","png",sep = ".")
    },
    content = function(file){
      if (input$box_grouped){
        if (input$single_year){
          box_plot<-eu_data() %>%
            filter(prccode %in% prcodes$codes$Prccode) %>%
            filter(year==input$box_year) %>%
            group_by(decl,prccode) %>%
            summarize(total=sum(value)) %>%
            ggplot(aes(decl,total))+
            geom_boxplot()+
            labs(title = glue::glue("Total Production of Selected Substances in Europe in {year(input$box_year)}"),x="",y="Tonnes")+
            scale_y_sqrt(labels = scales::comma_format())+
            theme(axis.text.x = element_text(angle=90,hjust = 1))
        } else {
          box_plot<-eu_data() %>%
            filter(prccode %in% prcodes$codes$Prccode) %>%
            group_by(decl,prccode) %>%
            summarize(total=sum(value)) %>%
            ggplot(aes(decl,total))+
            geom_boxplot()+
            labs(title = glue::glue("Total Production of Selected Substances in Europe from {year(min(eu_data()$year))} to {year(max(eu_data()$year))}"),x="",y="Tonnes",subtitle = "Tonnes of a Substance for Every Year Summed Up")+
            scale_y_sqrt(labels = scales::comma_format())+
            theme(axis.text.x = element_text(angle=90,hjust = 1))
        }
      } else {
        if (input$single_year){
          box_plot<-eu_data() %>%
            filter(prccode==input$substance_box) %>%
            filter(year==input$box_year) %>%
            ggplot(aes(decl,value))+
            geom_bar(stat = "identity",fill=input$box_col,width = 250)+
            labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_box) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in Europe in {year(input$box_year)}"),x="",y="Tonnes")+
            scale_y_sqrt(labels = scales::comma_format())+
            theme(axis.text.x = element_text(angle=90,hjust = 1))
        } else {
          box_plot<-eu_data() %>%
            filter(prccode==input$substance_box) %>%
            group_by(year,decl) %>%
            summarize(total=sum(value)) %>%
            ggplot(aes(decl,total))+
            geom_boxplot()+
            labs(title = glue::glue("Total Production of {unique(eu_data() %>% filter(prccode==input$substance_box) %>% mutate(prccode=as_factor(prccode)) %>% pull(prccode))} in Europe from {year(min(eu_data()$year))} to {year(max(eu_data()$year))}"),x="",y="Tonnes")+
            scale_y_sqrt(labels = scales::comma_format())+
            theme(axis.text.x = element_text(angle=90,hjust = 1))
        }
      }
      ggsave(file,plot = print(box_plot),device = "png")
    }
  )
  
  output$download_box_data<-downloadHandler(
    filename = function(){
      paste("Europe_plot_data","xlsx",sep = ".")
    },
    content = function(file){
      if (input$box_grouped){
        if (input$single_year){
          write_xlsx(
            eu_data() %>%
              filter(prccode %in% prcodes$codes$Prccode) %>%
              filter(year==input$box_year) %>%
              group_by(decl,prccode) %>%
              summarize(total=sum(value)) %>%
              add_column(year=input$box_year) %>%
              select(country=decl,year,tonnes=total) 
            ,file)
        } else {
          write_xlsx(
            eu_data() %>%
              filter(prccode %in% prcodes$codes$Prccode) %>%
              group_by(decl,prccode) %>%
              summarize(total=sum(value)) %>%
              add_column(year=input$box_year) %>%
              select(country=decl,year,tonnes=total)
            ,file)
        }
      } else {
        if (input$single_year){
          write_xlsx(
            eu_data() %>%
              filter(prccode==input$substance_box) %>%
              filter(year==input$box_year) %>%
              select(country=decl,year,tonnes=value)
            ,file)
        } else {
          write_xlsx(
            eu_data() %>%
              filter(prccode==input$substance_box) %>%
              group_by(year,decl) %>%
              summarize(total=sum(value)) %>%
              select(country=decl,year,tonnes=total)
            ,file)
        }
      }
    }
  )
}

shinyApp(ui = ui, server = server)

