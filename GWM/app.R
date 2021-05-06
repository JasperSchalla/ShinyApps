library(shiny)
library(shinydashboard)
library(colourpicker)
library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(scales)
library(data.table)
library(spatstat) 
library(maptools) 

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = span(tagList(icon("tint"),"GWM"))),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Datenuplopad",tabName = "upload_data",icon = icon("upload")),
        menuItem("Metadaten",tabName = "meta_data",icon=icon("chart-area")),
        menuItem("Grundwassermessstellen",tabName = "wells_data",icon = icon("layer-group"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload_data",
              fluidRow(box(width = 12,background = "navy",
                           column(width = 12,offset = 4,h1("Datenupload")))),
              fluidRow(box(width = 12,background = "navy",
                           br(),
                           fileInput("df_sachsen",label=strong("Datensatz Sachsen"),accept = ".csv"),
                           br(),
                           fileInput("df_sachsen_anhalt",label = strong("Datensatz Sachsen-Anhalt"),accept = ".csv"))),
              fluidRow(box(width = 12,background = "navy",
                           p("Upload Status")))
              ),
      tabItem(tabName = "wells_data",
              fluidRow(
                absolutePanel(fixed = TRUE,
                              draggable = FALSE, top = 150, left="auto" ,right = "auto", bottom = "auto",
                              width = 600, height = "auto", style = "opacity: 1; z-index: 10;",
                              box(background = "navy",
                                  h3("Kartenoptionen"),
                                  br(),
                                  selectInput("marker_loc",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                              selected = "Sachsen + Sachsen-Anhalt"),
                                  checkboxInput("show_descr_values",label=strong("Kennwerte zeigen")),
                                  conditionalPanel("input.show_descr_values==true",
                                                   selectInput("descr_values",label = strong("Kennwerte"),
                                                               choices = c("HW","MHW","MW","MNW","NW"),
                                                               selected = "HW")),
                                  checkboxInput("show_period",label=strong("Zeitreiheneigenschaften zeigen")),
                                  conditionalPanel("input.show_period==true",
                                                   selectInput("period",choices = c("Zeitreihenlaenge","Zeitreihenintervall"),
                                                               label = strong("Zeitreiheneigenschaften"),
                                                               selected = "Zeitreihenlaenge")),
                                  checkboxInput("show_th",label=strong("Thiessens Polygone zeigen")),
                                  checkboxInput("show_time_series",label=strong("Zeitreihen zeigen")),
                                  conditionalPanel("input.show_time_series==true",dateRangeInput("df_years",label=strong("Jahre der Zeitreihen"),start = "1900-01-01",end="2021-01-01",
                                                                     format="dd.mm.yyyy",separator = " - ")))),
              conditionalPanel("input.show_time_series==true",
                               absolutePanel(fixed = TRUE,
                                             draggable = TRUE, top = 150, left=800 ,right = "auto", bottom = "auto",
                                             width = 1500, height = "auto", style = "opacity: 1; z-index: 10;",
                                             box(background = "navy",plotOutput("time_series")))),
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              leafletOutput("map"))
      ),
      tabItem(tabName = "meta_data",
              fluidRow(box(width = 12,background = "navy",column(width = 12,offset = 4,h1("Metadaten")))),
              fluidRow(
                box(width = 3,background = "navy",
                    selectInput("marker_loc_meta1",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                selected = "Sachsen + Sachsen-Anhalt"),
                    sliderInput("bins_meta1",label = strong("Anzahl an Balken"),min=5,max=100,value = 30),
                    conditionalPanel("input.marker_loc_meta1!='Sachsen + Sachsen-Anhalt'",
                                     colourInput("col_meta1",label = strong("Farbauswahl"),value = "#91CFEE"))),
                box(width = 9,background = "navy",plotOutput("meta1"))
              ),
              fluidRow(
                box(width = 3,background = "navy",
                    selectInput("marker_loc_meta2",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                selected = "Sachsen + Sachsen-Anhalt"),
                    sliderInput("bins_meta2",label = strong("Anzahl an Balken"),min=5,max=100,value = 30),
                    conditionalPanel("input.marker_loc_meta2!='Sachsen + Sachsen-Anhalt'",
                                     colourInput("col_meta2",label = strong("Farbauswahl"),value = "#91CFEE"))),
                box(width = 9,background = "navy",plotOutput("meta2")))
            )
    )
  )
)

server <- function(input, output, session){
  theme_set(theme_bw())
  options(shiny.maxRequestSize=1e9)
  
  
  col_sachsen <- "#EEAD0E"
  col_sachsen_anhalt <- "#009ACD"
  name_vec <- c("hw","mhw","mw","mnw","nw")
  germany <- st_read(".\\geo_data\\DEU_adm1.shp") %>%
    filter(NAME_1 %in% c("Sachsen","Sachsen-Anhalt"))
  orig_crs <- st_crs(st_read(".\\geo_data\\crs_holder_sachsen.shp"))
  orig_crs2 <- st_crs(st_read(".\\geo_data\\crs_holder_sachsen_anhalt.shp"))
  
  df_sachsen <- reactive({
    fread(input$df_sachsen$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen"]
  })
  
  df_sachsen_anhalt <- reactive({
    fread(input$df_sachsen_anhalt$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen-Anhalt"]
  })
  
  df_all <- reactive({
    rbind(
      dplyr::select(df_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt,-hohensystem,
                    -ostwert,-nordwert),
      dplyr::select(df_sachsen_anhalt(),-rechtswert,-hochwert))
  })

  sf_sachsen <- reactive({
    st_as_sf(as.data.frame(unique(df_sachsen(),by="mkz")),coords = c("ostwert","nordwert"),crs=orig_crs) %>%
      st_transform(crs=4326)
  })
  
  sf_sachsen_anhalt <- reactive({
    st_as_sf(as.data.frame(unique(df_sachsen_anhalt(),by="mkz")),coords = c("rechtswert","hochwert"),crs=orig_crs2) %>%
      st_transform(crs=4326)
  })
  
  sf_all <- reactive({
    rbind(
      dplyr::select(sf_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt,-hohensystem),
      sf_sachsen_anhalt())
  })
  
  time_properties_sachsen <-reactive({
    
    interval_sachsen <- df_sachsen()[,.(time_diff=diff(messzeitpunkt)),by=.(mkz)] %>%
      .[,.(mean_diff=round(mean(as.numeric(time_diff)),1)),by=.(mkz)]
    
    length_sachsen <- df_sachsen()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)]
    
    sf_sachsen() %>%
      left_join(interval_sachsen[length_sachsen,on="mkz"],by="mkz") %>%
      mutate_at(vars(mean_diff,period),function(x) round(x,2)) %>%
      add_column(loc="Sachsen")
    
  })
  
  time_properties_sachsen_anhalt <-reactive({
    
    interval_sachsen_anhalt <- df_sachsen_anhalt()[,.(time_diff=diff(messzeitpunkt)),by=.(mkz)] %>%
      .[,.(mean_diff=round(mean(as.numeric(time_diff)),1)),by=.(mkz)]
    
    length_sachsen_anhalt <- df_sachsen_anhalt()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)]
    
    sf_sachsen_anhalt() %>%
      left_join(interval_sachsen_anhalt[length_sachsen_anhalt,on="mkz"],by="mkz") %>%
      mutate_at(vars(mean_diff,period),function(x) round(x,2)) %>%
      add_column(loc="Sachsen-Anhalt")
    
  })
  
  th_poly_sachsen <- reactive({
    sf_orig <- st_as_sf(as.data.frame(unique(df_sachsen(),by="mkz")),coords = c("ostwert","nordwert"),crs=orig_crs)
    th <- as(dirichlet(as.ppp(sf_orig)),"SpatialPolygons")
    proj4string(th) <- proj4string(as(sf_orig,"Spatial"))
    
    st_as_sf(th) %>%
      st_transform(crs=4326) %>%
      st_intersection(germany[germany$NAME_1=="Sachsen",]) %>%
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2))
  })
  
  th_poly_sachsen_anhalt <- reactive({
    sf_orig <- st_as_sf(as.data.frame(unique(df_sachsen_anhalt(),by="mkz")),coords = c("rechtswert","hochwert"),crs=orig_crs2)
    th <- as(dirichlet(as.ppp(sf_orig)),"SpatialPolygons")
    proj4string(th) <- proj4string(as(sf_orig,"Spatial"))
    
    st_as_sf(th) %>%
      st_transform(crs=4326) %>%
      st_intersection(germany[germany$NAME_1=="Sachsen Anhalt",]) %>%
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2))
  })
  
  th_poly_all <- reactive({
    sf_orig <- sf_all() %>%
      st_transform(orig_crs)
    th <- as(dirichlet(as.ppp(sf_orig)),"SpatialPolygons")
    proj4string(th) <- proj4string(as(sf_orig,"Spatial"))
    
    st_as_sf(th) %>%
      st_transform(crs=4326) %>%
      st_intersection(germany) %>%
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2))

  })
  
  observe({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    
    print(head(th_poly_sachsen()))
    #print(head(df_all()))
  })
  
  time_properties_all <- reactive({
    rbind(
      dplyr::select(time_properties_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt,-hohensystem),
      time_properties_sachsen_anhalt())
  })

  
  period2str <- function(arg){
      if (arg=="Zeitreihenlaenge"){
        return("period")
      } else {
        return("mean_diff")
      } 
  }
  
  period2title <- function(arg){
    if (arg=="Zeitreihenlaenge"){
      return("Jahre")
    } else {
      return("Tage")
    } 
  }
  
  
  mkz_clicked <- eventReactive(input$map_marker_click,{
      click <- input$map_marker_click
      if (is.null(click)){
        return()
      }

      click$id
  })
  
  pal_fun <-colorNumeric("Blues",NULL)
  pal_th <-colorNumeric("YlOrRd",NULL)
  d_pal_fun <- colorFactor(c(col_sachsen,col_sachsen_anhalt),NULL)
  
  popup_content_descr_sachsen <- reactive({
    paste("<strong>HW: </strong>",sf_sachsen()$hw,"m","<br>",
          "<strong>MHW:</strong>",sf_sachsen()$mhw,"m","<br>",
          "<strong>MW: </strong>",sf_sachsen()$mw,"m","<br>",
          "<strong>MNW:</strong>",sf_sachsen()$mnw,"m","<br>",
          "<strong>NW: </strong>",sf_sachsen()$nw,"m")
  })  
  
  
  popup_content_descr_sachsen_anhalt <- reactive({
    paste("<strong>HW: </strong>",sf_sachsen_anhalt()$hw,"m","<br>",
          "<strong>MHW:</strong>",sf_sachsen_anhalt()$mhw,"m","<br>",
          "<strong>MW: </strong>",sf_sachsen_anhalt()$mw,"m","<br>",
          "<strong>MNW:</strong>",sf_sachsen_anhalt()$mnw,"m","<br>",
          "<strong>NW: </strong>",sf_sachsen_anhalt()$nw,"m")
  })  
  
  popup_content_descr_both <- reactive({
    paste("<strong>HW: </strong>",sf_all()$hw,"m","<br>",
          "<strong>MHW:</strong>",sf_all()$mhw,"m","<br>",
          "<strong>MW: </strong>",sf_all()$mw,"m","<br>",
          "<strong>MNW:</strong>",sf_all()$mnw,"m","<br>",
          "<strong>NW: </strong>",sf_all()$nw,"m")
  })  
  
  popup_content_period_sachsen <- reactive({
    paste("<strong>Zeitreihenlaenge:        </strong>",time_properties_sachsen()$period,"Jahre","<br>",
          "<strong>Zeitreihenintervalle:    </strong>",time_properties_sachsen()$mean_diff,"Tage")
  })  
  
  popup_content_period_sachsen_anhalt <- reactive({
    paste("<strong>Zeitreihenlaenge:    </strong>",time_properties_sachsen_anhalt()$period,"Jahre","<br>",
          "<strong>Zeitreihenintervalle:</strong>",time_properties_sachsen_anhalt()$mean_diff,"Tage")
  })  
  
  popup_content_period_both <- reactive({
    paste("<strong>Zeitreihenlaenge:    </strong>",time_properties_all()$period,"Jahre","<br>",
          "<strong>Zeitreihenintervalle:</strong>",time_properties_all()$mean_diff,"Tage")
  })  
  
  th_sachsen_popup <- reactive({
    paste("<strong>Flaeche:   </strong>",th_poly_sachsen()$area,"km<sup>2</sup>","<br>",
          "<strong>Mittelwert:</strong> ",round(mean(th_poly_sachsen()$area),2),"km<sup>2</sup>")
  })
  
  th_sachsen_anhalt_popup <- reactive({
    paste("<strong>Flaeche:   </strong>",th_poly_sachsen_anhalt()$area,"km<sup>2</sup>","<br>",
          "<strong>Mittelwert:</strong>",round(mean(th_poly_sachsen_anhalt()$area),2),"km<sup>2</sup>")
  })
  
  th_all_popup <- reactive({
    paste("<strong>Flaeche:   </strong>",th_poly_all()$area,"km<sup>2</sup>","<br>",
          "<strong>Mittelwert:</strong>",round(mean(th_poly_all()$area),2),"km<sup>2</sup>")
  })
  
  default_popup_sachsen <- reactive({
    paste("<strong>Messtellenname:</strong>",sf_sachsen()$messstellenname,
          "<strong>Kennzeichnungsnummer:</strong>",sf_sachsen()$mkz)
  })
  
  default_popup_sachsen_anhalt <- reactive({
    paste("<strong>Messtellenname:</strong>",sf_sachsen_anhalt()$messstellenname,
          "<strong>Kennzeichnungsnummer:</strong>",sf_sachsen_anhalt()$mkz)
  })
  
  default_popup_all <- reactive({
    paste("<strong>Messtellenname:</strong>",sf_all()$messstellenname,
          "<strong>Kennzeichnungsnummer:</strong>",sf_all()$mkz)
  })
  
  output$map <- renderLeaflet({
    
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    
    
    if (input$marker_loc=="Sachsen"){
      temp_leaflet <- sf_sachsen() %>%
        leaflet() %>%
        #addProviderTiles(provider=providers$OpenStreetMap)  %>%
        addTiles()  
    } else if (input$marker_loc=="Sachsen-Anhalt"){
      temp_leaflet <- sf_sachsen_anhalt() %>%
        leaflet() %>%
        #addProviderTiles(provider=providers$OpenStreetMap)  %>%
        addTiles()  
    } else {
      temp_leaflet <- sf_all() %>%
        leaflet() %>%
        #addProviderTiles(provider=providers$OpenStreetMap)  %>%
        addTiles()  
    }
    
    if (input$show_descr_values & !input$show_period & !input$show_th){
      if (input$marker_loc=="Sachsen"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
            popup = popup_content_descr_sachsen(),
            layerId = sf_sachsen()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                    title = input$descr_values) 
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
            popup = popup_content_descr_sachsen_anhalt(),
            layerId = sf_sachsen_anhalt()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                    title = input$descr_values) 
      } else {
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
            popup = popup_content_descr_both(),
            layerId = sf_all()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                    title = input$descr_values) 
      }
    } else if (!input$show_descr_values & input$show_period & !input$show_th) {
      if (input$marker_loc=="Sachsen"){
        time_properties_sachsen() %>%
          leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles() %>%
          addCircleMarkers(
            radius=2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(period2str(input$period)))),
            popup = popup_content_period_sachsen(),
            layerId = time_properties_sachsen()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(period2str(input$period))),opacity = 1,
                    title = period2title(input$period))
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        time_properties_sachsen_anhalt() %>%
          leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles() %>%
          addCircleMarkers(
            radius=2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(period2str(input$period)))),
            popup = popup_content_period_sachsen_anhalt(),
            layerId = time_properties_sachsen_anhalt()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(period2str(input$period))),opacity = 1,
                    title = period2title(input$period))
      } else {
        time_properties_all() %>%
          leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles() %>%
          addCircleMarkers(
            radius=2,
            stroke = T,
            opacity = 1,
            color = ~pal_fun(eval(as.symbol(period2str(input$period)))),
            popup = popup_content_period_both(),
            layerId = time_properties_all()$mkz) %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(period2str(input$period))),opacity = 1,
                    title = period2title(input$period)) 
      }
      
    } else if (input$show_th & !input$show_descr_values & !input$show_period ){
      
      if (input$marker_loc=="Sachsen"){
        leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles()  %>%
          addPolygons(data = th_poly_sachsen(),
                      opacity = 1,
                      fillOpacity = 1,
                      color="grey",
                      fillColor = ~pal_th(area),
                      popup = th_sachsen_popup(),
                      layerId = th_poly_sachsen()$area,
                      stroke = T,
                      weight = 1) %>%
          addLegend("bottomright",pal = pal_th,values = th_poly_sachsen()$area,opacity = 1,title = "Flaeche [km<sup>2</sup>]")
        
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles()  %>%
          addPolygons(data = th_poly_sachsen_anhalt(),
                      opacity = 1,
                      fillOpacity = 1,
                      color="grey",
                      fillColor = ~pal_th(area),
                      popup = th_sachsen_anhalt_popup(),
                      stroke = T,
                      weight = 1) %>%
          addLegend("bottomright",pal = pal_th,values = th_poly_sachsen_anhalt()$area,opacity = 1,title = "Flaeche [km<sup>2</sup>]")
      } else {
        leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles()  %>%
          addPolygons(data = th_poly_all(),
                      opacity = 1,
                      fillOpacity = 1,
                      color="grey",
                      fillColor = ~pal_th(area),
                      popup = th_all_popup(),
                      stroke = T,
                      weight = 1) %>%
          addLegend("bottomright",pal = pal_th,values = th_poly_all()$area,opacity = 1,title = "Flaeche [km<sup>2</sup>]")
      }
      
    } else {
      if (input$marker_loc=="Sachsen"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen()$mkz,
            color = col_sachsen,
            popup = default_popup_sachsen())
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen_anhalt()$mkz,
            color = col_sachsen_anhalt,
            popup = default_popup_sachsen_anhalt())
      } else {
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_all()$mkz,
            color = ~d_pal_fun(loc),
            popup = default_popup_all()) %>%
          addLegend("bottomright",pal=d_pal_fun,values = ~loc,opacity = 1,title = "Bundesland") 
      }
    }
  })
  
  output$time_series <- renderPlot({
    
    
    shiny::validate(
      need(input$map_marker_click!="","Es muss erst auf einen Datenpunkt gecklickt werden, dessen Zeitreihe angezeigt werden soll")
    )
    
    if (input$marker_loc=="Sachsen"){
      plot_templ <- df_sachsen()[mkz==mkz_clicked() & messzeitpunkt>=as.Date(input$df_years[1]) & messzeitpunkt<=as.Date(input$df_years[2]),.(messzeitpunkt,wert)]
      title <- glue::glue("Grundwasserstand am Messpunkt {unique(df_sachsen(),by='mkz')[mkz==mkz_clicked(),messstellenname]}")
    } else if (input$marker_loc=="Sachsen-Anhalt"){
      plot_templ <- df_sachsen_anhalt()[mkz==mkz_clicked() & messzeitpunkt>=as.Date(input$df_years[1]) & messzeitpunkt<=as.Date(input$df_years[2]),.(messzeitpunkt,wert)]
      title <- glue::glue("Grundwasserstand am Messpunkt {unique(df_sachsen_anhalt(),by='mkz')[mkz==mkz_clicked(),messstellenname]}")
    } else {
      if (nrow(df_sachsen()[mkz==mkz_clicked()])>0){
        plot_templ <- df_sachsen()[mkz==mkz_clicked() & messzeitpunkt>=as.Date(input$df_years[1]) & messzeitpunkt<=as.Date(input$df_years[2]),.(messzeitpunkt,wert)]
        title <- glue::glue("Grundwasserstand am Messpunkt {unique(df_sachsen(),by='mkz')[mkz==mkz_clicked(),messstellenname]}")
      } else {
        plot_templ <- df_sachsen_anhalt()[mkz==mkz_clicked() & messzeitpunkt>=as.Date(input$df_years[1]) & messzeitpunkt<=as.Date(input$df_years[2]),.(messzeitpunkt,wert)]
        title <- glue::glue("Grundwasserstand am Messpunkt {unique(df_sachsen_anhalt(),by='mkz')[mkz==mkz_clicked(),messstellenname]}")
      }
    }
    
    plot_templ %>%
      ggplot(aes(messzeitpunkt,wert))+
      geom_line(col="darkblue")+
      geom_point(col="darkblue")+
      labs(x="",y="",title = title)+
      scale_y_reverse()
  })
  
  output$meta1 <- renderPlot({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    if (input$marker_loc_meta1=="Sachsen"){
        df_sachsen()[,.(time_diff=diff(messzeitpunkt)),by=.(mkz)][,.(mean_diff=round(mean(as.numeric(time_diff)),1)),by=.(mkz)] %>%
        ggplot(aes(mean_diff))+
        geom_histogram(bins=input$bins_meta1,fill=input$col_meta1,col="black")+
        scale_x_log10()+
        labs(title = "Mittleres Messintervall in Sachsen",x="Tage",y="Anzahl an Messpunkten")
    } else if (input$marker_loc_meta1=="Sachsen-Anhalt"){
      df_sachsen_anhalt()[,.(time_diff=diff(messzeitpunkt)),by=.(mkz)][,.(mean_diff=round(mean(as.numeric(time_diff)),1)),by=.(mkz)] %>%
        ggplot(aes(mean_diff))+
        geom_histogram(bins=input$bins_meta1,fill=input$col_meta1,col="black")+
        scale_x_log10()+
        labs(title = "Mittleres Messintervall in Sachsen-Anhalt",x="Tage",y="Anzahl an Messpunkten")
    } else {
      df_all()[,.(time_diff=diff(messzeitpunkt)),by=.(mkz)][,.(mean_diff=round(mean(as.numeric(time_diff)),1)),by=.(mkz)] %>%
        unique(df_all(),by="mkz")[,.(loc,mkz)][.,on="mkz"] %>%
        ggplot(aes(mean_diff,fill=loc))+
        geom_histogram(bins=input$bins_meta1,col="black")+
        scale_x_log10()+
        labs(title = "Mittleres Messintervall in Sachsen-Anhalt und Sachsen",x="Tage",y="Anzahl an Messpunkten")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual("Bundesland",values = c(col_sachsen,col_sachsen_anhalt))
      }
  })
  
  output$meta2 <- renderPlot({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    if (input$marker_loc_meta2=="Sachsen"){
      df_sachsen()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)] %>%
        ggplot(aes(period))+
        geom_histogram(bins=input$bins_meta2,fill=input$col_meta2,col="black")+
        labs(title="Laenge der Messreihen in Sachsen",x="Tage",y="Anzahl an Messpunkten")
    } else if (input$marker_loc_meta2=="Sachsen-Anhalt"){
      df_sachsen_anhalt()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)] %>%
        ggplot(aes(period))+
        geom_histogram(bins=input$bins_meta2,fill=input$col_meta2,col="black")+
        labs(title="Laenge der Messreihen in Sachsen-Anhalt",x="Tage",y="Anzahl an Messpunkten")
    } else {
      df_all()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)] %>%
        unique(df_all(),by="mkz")[,.(loc,mkz)][.,on="mkz"] %>%
        ggplot(aes(period,fill=loc))+
        geom_histogram(bins=input$bins_meta2,col="black")+
        labs(title="Laenge der Messreihen in Sachsen und Sachsen-Anhalt",x="Tage",y="Anzahl an Messpunkten")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual("Bundesland",values = c(col_sachsen,col_sachsen_anhalt))
    }
  })
  
}

shinyApp(ui = ui, server = server)
