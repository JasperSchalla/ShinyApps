options(rgl.useNULL=T)

library(shiny)
library(shinydashboard)
library(colourpicker)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(htmltools)
library(scales)
library(data.table)
library(spatstat) 
library(maptools)
library(leaflet.extras)
library(rgdal)
library(gstat)
library(factoextra)
library(trend)
library(trend)
library(zoo)
library(rayshader)
library(rgl)
library(rglwidget)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = span(tagList(icon("tint"),"GWM"))),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Datenuplopad",tabName = "upload_data",icon = icon("upload")),
        menuItem("Metadaten",tabName = "meta_data",icon=icon("chart-area")),
        menuItem("Grundwassermessstellen",tabName = "wells_data",icon = icon("layer-group")),
        menuItem("Ausgewaehlte Shapefiles",tabName = "selected_tab",icon=icon("table"))
      )
  ),
  dashboardBody(id="container_all",
    tags$head(
      tags$style(type="text/css","#option_panel {max-height: 520px;overflow-y:auto;overflow-x:hidden;}")
    ),
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
                           p("Upload Status"),textOutput("upload_status")))
              ),
      tabItem(tabName = "wells_data",
              fluidRow(
                absolutePanel(fixed = T,draggable = F,
                              top = 200, left="auto" ,right = "auto", bottom = "auto",
                              width = 350, height = "auto", style = "opacity: 1; z-index: 10;",
                              box(id="option_panel",width = 12,background = "navy",
                                  h3("Kartenoptionen"),
                                  br(),
                                  selectInput("marker_loc",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                              selected = "Sachsen + Sachsen-Anhalt"),
                                  checkboxInput("show_descr_values",label=strong("Kennwerte zeigen")),
                                  conditionalPanel("input.show_descr_values==true",
                                                   div(id="descr_container",selectInput("descr_values",label = "Kennwerte",
                                                               choices = c("HW","MHW","MW","MNW","NW"),
                                                               selected = "HW")),
                                                   tags$style(type="text/css","#descr_container {color:#9dbccf;}"),
                                                   div(id="interpolation_container",checkboxInput("interpolation",label = strong("Ordinary Kriging"))),
                                                   tags$style(type="text/css","#interpolation_container {color:#9dbccf;}"),
                                                   div(id="res_container",sliderInput("interpolation_res","Aufloesung des Interpolation-Rasters [m]",min = 1000,max=15000,value = 2500)),
                                                   tags$style(type="text/css","#res_container {color:#9dbccf;}"),
                                                   helpText("Die Berechnung kann einige Zeit in Anspruch nehmen"),
                                                   div(id="rayshader_container",checkboxInput("rayshader",label = strong("Rayshader"))),
                                                   tags$style(type="text/css","#rayshader_container {color:#9dbccf;}")),
                                  checkboxInput("show_outliers",label = strong("Ausreisser ausblenden")),
                                  conditionalPanel("input.show_outliers==true",div(id="outliers_container",numericInput("outliers_lower",label = strong("Untere Grenze [m u. GOK]"),min = -1000,max=1000,value=-100),
                                                                                   numericInput("outliers_upper",label = strong("Obere Grenze [m u. GOK]"),min = -1000,max=1000,value=100)),
                                                   tags$style(type="text/css","#outliers_container {color:#9dbccf;}")),
                                  checkboxInput("show_period",label=strong("Zeitreiheneigenschaften zeigen")),
                                  conditionalPanel("input.show_period==true",
                                                   div(id="period_container",selectInput("period",choices = c("Zeitreihenlaenge","Zeitreihenintervall"),
                                                               label = strong("Zeitreiheneigenschaften"),
                                                               selected = "Zeitreihenlaenge")),
                                                   tags$style(type="text/css","#period_container {color:#9dbccf;}")),
                                  checkboxInput("show_th",label=strong("Thiessens Polygone zeigen")),
                                  conditionalPanel("input.marker_loc=='Sachsen'",checkboxInput("show_alt",label=strong("Hoehensysteme anzeigen"))),
                                  checkboxInput("show_time_series",label=strong("Zeitreihen zeigen")),
                                  conditionalPanel("input.show_time_series==true",div(id="df_years_container",dateRangeInput("df_years",label=strong("Jahre der Zeitreihen"),start = "1900-01-01",end="2021-01-01",
                                                                     format="dd.mm.yyyy",separator = " - ")),
                                                   tags$style(type="text/css","#df_years_container {color:#9dbccf;}")),
                                  checkboxInput("show_cluster",label=strong("Cluster")),
                                  conditionalPanel("input.show_cluster==true",
                                                   div(id="cluster_filter_container",checkboxInput("cluster_filter",label=strong("Filtern"))),
                                                   tags$style(type="text/css","#cluster_filter_container {color:#9dbccf;}"),
                                                   helpText("Zeitreihen, die kuerzer als 10 Jahre sind und vor 1990 enden werden herausgefiltert"),
                                                   div(id="k_map_container",sliderInput("k_map",label=strong("K"),min = 2,max=10,value = 3)),
                                                   tags$style(type="text/css","#k_map_container {color:#9dbccf;}")),
                                  checkboxInput("show_trend",label = strong("Trends")),
                                  conditionalPanel("input.show_trend==true",
                                                   div(id="trend_last_container",checkboxInput("trend_last",label = strong("Fuer die letzten 10 Jahre"))),
                                                   tags$style(type="text/css","#trend_last_container {color:#9dbccf;}"),
                                                   helpText("Zeitreihen muessen mindestens 10 Jahre lang sein")),
                                  br(),
                                  textOutput("no_selected"),
                                  br(),
                                  column(width=12,offset=1,downloadButton("download_selected","Shapefiles exportieren")),
                                  br(),
                                  br())),
              conditionalPanel("input.show_time_series==true",
                               absolutePanel(fixed = TRUE,
                                             draggable = TRUE, top = 150, left=800 ,right = "auto", bottom = "auto",
                                             width = 1500, height = "auto", style = "opacity: 1; z-index: 10;",
                                             box(background = "navy",plotOutput("time_series")))),
              conditionalPanel("input.rayshader==true",
                               absolutePanel(fixed = TRUE,
                                             draggable = F, top = 160, left=600 ,right = "auto", bottom = "auto",
                                             width = "auto", height = "auto", style = "opacity: 1; z-index: 10;",
                                             box(background = "navy",rglwidgetOutput("gw_surface")))),
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              conditionalPanel("input"),
              leafletOutput("map"))
      ),
      tabItem(tabName = "selected_tab",
              fluidRow(box(width = 12,
                           dataTableOutput("selected_download")))
              
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
                box(width = 9,background = "navy",plotOutput("meta2"))),
              fluidRow(
                box(width = 3,background = "navy",
                    selectInput("marker_loc_meta3",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                selected = "Sachsen + Sachsen-Anhalt"),
                    dateRangeInput("gw_change_years",label="Jahre der Zeitreihen",start = "1900-01-01",end="2021-01-01",
                                   format="yyyy",separator = " - ",startview = "year")),
                box(width = 9,background = "navy",plotOutput("meta3"))),
              fluidRow(
                box(width=3,background="navy",
                    selectInput("marker_loc_meta4",label=strong("Bundesland"),choices = c("Sachsen + Sachsen-Anhalt","Sachsen","Sachsen-Anhalt"),
                                selected = "Sachsen + Sachsen-Anhalt"),
                    sliderInput("k",label=strong("Cluster"),min=2,max=10,value=2)),
                box(width=9,background="navy",
                    plotOutput("cluster_plot"),
                    plotOutput("cluster_info"))
                )
            )
    )
  )
)

server <- function(input, output, session){
  theme_set(theme_bw())
  options(shiny.maxRequestSize=1e9)
  options(scipen=999)
  options(shiny.sanitize.errors = FALSE)
  set.seed(400)
  
  col_sachsen <- viridis::viridis(3)[2]#"#EEAD0E" 
  col_sachsen_anhalt <- viridis::viridis(3)[3]#"#009ACD"
  name_vec <- c("hw","mhw","mw","mnw","nw")
  germany <- st_read("./geo_data/DEU_adm1.shp") %>%
    filter(NAME_1 %in% c("Sachsen","Sachsen-Anhalt"))
  orig_crs <- st_crs(st_read("./geo_data/crs_holder_sachsen.shp"))
  orig_crs2 <- st_crs(st_read("./geo_data/crs_holder_sachsen_anhalt.shp"))
  unit_values <- "m u. GOK"
  counter <- 1
  
  # UpdateSession
  
  observe({
    updateSliderInput(session,"k",min = input$boundaries[1],max=input$boundaries[2],value = ceiling(input$boundaries[2]-input$boundaries[1]))
    updateSliderInput(session,"k_map",min = input$boundaries[1],max=input$boundaries[2],value = ceiling(input$boundaries[2]-input$boundaries[1]))
  })
  
  selected_shp <- reactiveValues(data=NULL)
  
  df_sachsen <- reactive({
    
    if (!input$show_outliers){
      
      fread(input$df_sachsen$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen"] %>%
        mutate(messzeitpunkt=as.Date(messzeitpunkt)) 
      
    } else {
      
      fread(input$df_sachsen$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen"] %>%
        mutate(messzeitpunkt=as.Date(messzeitpunkt)) %>%
        .[hw<=input$outliers_upper & hw>=input$outliers_lower &
             mhw<=input$outliers_upper & mhw>=input$outliers_lower &
             mw<=input$outliers_upper & mw>=input$outliers_lower &
             mnw<=input$outliers_upper & nw>=input$outliers_lower]
      
    }
  
  })
  
  df_sachsen_anhalt <- reactive({
    
    if (!input$show_outliers){
     
      fread(input$df_sachsen_anhalt$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen-Anhalt"] %>%
        filter(!is.na(rechtswert)) %>%
        mutate(messzeitpunkt=as.Date(messzeitpunkt))
       
    } else {
     
      fread(input$df_sachsen_anhalt$datapath)[,(name_vec):=round(.SD,2),.SDcols=name_vec][,loc:="Sachsen-Anhalt"] %>%
        filter(!is.na(rechtswert)) %>%
        mutate(messzeitpunkt=as.Date(messzeitpunkt)) %>%
        .[hw<=input$outliers_upper & hw>=input$outliers_lower &
            mhw<=input$outliers_upper & mhw>=input$outliers_lower &
            mw<=input$outliers_upper & mw>=input$outliers_lower &
            mnw<=input$outliers_upper & nw>=input$outliers_lower]
       
    }
    
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
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2)) %>%
      rowid_to_column("id")
  })
  
  th_poly_sachsen_anhalt <- reactive({
    sf_orig <- st_as_sf(as.data.frame(unique(df_sachsen_anhalt(),by="mkz")),coords = c("rechtswert","hochwert"),crs=orig_crs2)
    th <- as(dirichlet(as.ppp(sf_orig)),"SpatialPolygons")
    proj4string(th) <- proj4string(as(sf_orig,"Spatial"))
    
    st_as_sf(th) %>%
      st_transform(crs=4326) %>%
      st_intersection(germany[germany$NAME_1=="Sachsen-Anhalt",]) %>%
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2)) %>%
      rowid_to_column("id")
  })
  
  th_poly_all <- reactive({
    sf_orig <- sf_all() %>%
      st_transform(orig_crs)
    th <- as(dirichlet(as.ppp(sf_orig)),"SpatialPolygons")
    proj4string(th) <- proj4string(as(sf_orig,"Spatial"))
    
    st_as_sf(th) %>%
      st_transform(crs=4326) %>%
      st_intersection(germany) %>%
      mutate(area=round(as.numeric(st_area(geometry))/1e6,2)) %>%
      rowid_to_column("id")

  })
  
  observe({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
  })
  
  time_properties_all <- reactive({
    rbind(
      dplyr::select(time_properties_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt,-hohensystem),
      time_properties_sachsen_anhalt())
  })
  
  grid <- reactive({
    if (input$marker_loc=="Sachsen-Anhalt"){
      state <- germany[germany$NAME_1=="Sachsen-Anhalt",] %>%
        st_transform(crs=orig_crs2) 
    } else if (input$marker_loc=="Sachsen"){
      state <- germany[germany$NAME_1=="Sachsen",] %>%
        st_transform(crs=orig_crs2) 
    } else {
      state <- germany %>%
        st_union() %>%
        st_transform(crs=orig_crs2)  %>%
        st_as_sf()
    }
    
    state %>%
      st_make_grid(cellsize = input$interpolation_res,what = "centers") %>%
      st_intersection(state) %>%
      as("Spatial")
  })
  
  kriged <- reactive({
    
    if (input$marker_loc=="Sachsen-Anhalt"){
      missing <- sf_sachsen_anhalt()[which(is.na(sf_sachsen_anhalt()[,name_vec]),arr.ind = T)[,1],]$mkz
      if (length(missing)>0){
        sp_obj <- as(st_transform(sf_sachsen_anhalt() %>% filter(!(mkz %in% missing)),crs=orig_crs2),"Spatial")
      } else {
        sp_obj <- as(st_transform(sf_sachsen_anhalt(),crs=orig_crs2),"Spatial") 
      }
    } else if (input$marker_loc=="Sachsen"){
      missing <- sf_sachsen()[which(is.na(sf_sachsen()[,name_vec]),arr.ind = T)[,1],]$mkz
      if (length(missing)>0){
        sp_obj <- as(st_transform(sf_sachsen() %>% filter(!(mkz %in% missing)),crs=orig_crs2),"Spatial")
      } else {
        sp_obj <- as(st_transform(sf_sachsen(),crs=orig_crs2),"Spatial") 
      }
    } else {
      missing <- sf_all()[which(is.na(sf_all()[,name_vec]),arr.ind = T)[,1],]$mkz
      if (length(missing)>0){
        sp_obj <- as(st_transform(sf_all() %>% filter(!(mkz %in% missing)),crs=orig_crs2),"Spatial")
      } else {
        sp_obj <- as(st_transform(sf_all(),crs=orig_crs2),"Spatial") 
      }
    }

    vario_fit <- automap::autofitVariogram(eval(as.symbol(tolower(input$descr_values)))~1,sp_obj)
    ok <- krige(eval(as.symbol(tolower(input$descr_values)))~1,model=vario_fit$var_model,locations=sp_obj[-zerodist(sp_obj)[,1],],newdata=grid())
    pred_grid <- ok["var1.pred"]
    gridded(pred_grid) <- T
    fullgrid(pred_grid) <- T
    
    if (input$marker_loc=="Sachsen-Anhalt"){
      state <- germany[germany$NAME_1=="Sachsen-Anhalt",] %>%
        st_transform(crs=orig_crs2) 
    } else if (input$marker_loc=="Sachsen"){
      state <- germany[germany$NAME_1=="Sachsen",] %>%
        st_transform(crs=orig_crs2) 
    } else {
      state <- germany %>%
        st_union() %>%
        st_transform(crs=orig_crs2) %>%
        st_as_sf()
    }
    
    raster::crop(raster::mask(raster::raster(pred_grid),state),state)
    
  })
  
  sf_selected <- reactive({
    
    req(input$map_draw_stop)
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    polygon <- st_polygon(list(do.call(rbind,lapply(coords,function(x){c(x[[1]][1],x[[2]][1])}))))
  
    if (input$show_th & !any(c(input$show_desc_values,input$show_period))){
      if (input$marker_loc=="Sachsen"){
        temp_shp <- th_poly_sachsen()[st_intersects(polygon,th_poly_sachsen())[[1]],] %>%
          pull(id)
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        temp_shp <- th_poly_sachsen_anhalt()[st_intersects(polygon,th_poly_sachsen_anhalt())[[1]],] %>%
          pull(id)
      } else {
        temp_shp <- th_poly_all()[st_intersects(polygon,th_poly_all())[[1]],] %>%
          pull(id)        
      }

    } else {
      temp_shp <- sf_all()[st_intersects(polygon,sf_all())[[1]],] %>%
        pull(mkz)
    }

    temp_shp
    
  })
  
  observe({
    
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden"),
      need(!is.null(sf_selected())," ")
      
    )
    
    #input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & !input$show_cluster & !input$show_trend
    
    if (input$show_descr_values & !input$show_period & !input$show_th & !input$show_alt & !input$show_cluster & !input$show_trend){
      if (input$marker_loc=="Sachsen"){
        
        selected_shp$data <- sf_sachsen() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,hw,mhw,mw,mnw,nw)
        
      } else if (input$marker_loc=="Sachsen-Anhalt"){

        selected_shp$data <- sf_sachsen_anhalt() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,hw,mhw,mw,mnw,nw)
        
      } else {
        
        selected_shp$data <- sf_sachsen() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,hw,mhw,mw,mnw,nw,loc)
      }
    } else if (!input$show_descr_values & input$show_period & !input$show_th & !input$show_alt & !input$show_cluster & !input$show_trend) {
      if (input$marker_loc=="Sachsen"){
        
        selected_shp$data <- time_properties_sachsen() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,laenge=period,intervall=mean_diff)

      } else if (input$marker_loc=="Sachsen-Anhalt"){
    
        selected_shp$data <- time_properties_sachsen_anhalt() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,laenge=period,intervall=mean_diff)
        
      } else {

        selected_shp$data <- time_properties_all() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(mkz,messstellenname,laenge=period,intervall=mean_diff)
        
      }

    } else if (input$show_th & !input$show_descr_values & !input$show_period & !input$show_alt & !input$show_cluster & !input$show_trend){

      if (input$marker_loc=="Sachsen"){
        selected_shp$data <- th_poly_sachsen() %>%
          filter(id %in% sf_selected()) %>%
          dplyr::select(area) %>%
          st_join(dplyr::select(sf_all(),geometry,mkz,messstellenname))

      } else if (input$marker_loc=="Sachsen-Anhalt"){

        selected_shp$data <- th_poly_sachsen_anhalt() %>%
          filter(id %in% sf_selected()) %>%
          dplyr::select(area) %>%
          st_join(dplyr::select(sf_all(),geometry,mkz,messstellenname))
        
      } else {

        selected_shp$data <- th_poly_all() %>%
          filter(id %in% sf_selected()) %>%
          dplyr::select(area) %>%
          st_join(dplyr::select(sf_all(),geometry,mkz,messstellenname))
        
      }

    } else if (input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & !input$show_cluster & !input$show_trend){
      
      selected_shp$data <- dplyr::select(sf_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt) %>%
        filter(mkz %in% sf_selected()) %>%
        dplyr::select(-hw,-mhw,-mw,-mnw,-nw,-messzeitpunkt,-einheit,-wert)
      
    } else if (!input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & input$show_cluster & !input$show_trend){
      
      cluster_df <- df_var_map()[complete.cases(df_var_map()),.(mkz,cluster=kmeans(scale(df_var_map()[complete.cases(df_var_map()),.(min_var,max_var)]),input$k_map,nstart=25)$cluster)]
      if (input$marker_loc=="Sachsen"){
        
        selected_shp$data <- sf_sachsen() %>%
          filter(mkz %in% sf_selected()) %>%
          left_join(cluster_df,by="mkz") %>%
          dplyr::select(mkz,messstellenname,cluster)
        
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        
        selected_shp$data <- sf_sachsen_anhalt() %>%
          filter(mkz %in% sf_selected()) %>%
          left_join(cluster_df,by="mkz") %>%
          dplyr::select(mkz,messstellenname,cluster)
      } else {
        
        selected_shp$data <- sf_all() %>%
          filter(mkz %in% sf_selected()) %>%
          left_join(cluster_df,by="mkz") %>%
          dplyr::select(mkz,messstellenname,cluster)
      }
      
    } else if (!input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & !input$show_cluster & input$show_trend){
      
      selected_shp$data <- sf_sign() %>%
        filter(mkz %in% sf_selected()) %>%
        dplyr::select(mkz,messstellenname,p_value,slope)

    } else {
      if (input$marker_loc=="Sachsen"){

        selected_shp$data <- dplyr::select(sf_sachsen(),-abflussjahr,-wert_in_cm_unter_gelande,-messwert_in_cm_unter_messpunkt,-hohensystem) %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(-hw,-mhw,-mw,-mnw,-nw,-messzeitpunkt,-einheit,-wert)
        
      } else if (input$marker_loc=="Sachsen-Anhalt"){

        selected_shp$data <- sf_sachsen_anhalt() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(-hw,-mhw,-mw,-mnw,-nw,-messzeitpunkt,-einheit,-wert)
        
      } else {
        
        selected_shp$data <- sf_all() %>%
          filter(mkz %in% sf_selected()) %>%
          dplyr::select(-hw,-mhw,-mw,-mnw,-nw,-messzeitpunkt,-einheit,-wert)

      }
    }
    
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
  
  pal_fun <-colorNumeric("viridis",NULL,na.color = NA)
  pal_fun2 <-colorNumeric("viridis",NULL,na.color = NA)
  pal_th <-colorNumeric("viridis",NULL,na.color = NA)
  pal_alt <- colorFactor("viridis",NULL,na.color = NA)
  d_pal_fun <- colorFactor(c(col_sachsen,col_sachsen_anhalt),NULL,na.color = NA)
  

  
  popup_content_descr_sachsen <- reactive({
    paste("<strong>HW: </strong>",sf_sachsen()$hw,unit_values,"<br>",
          "<strong>MHW:</strong>",sf_sachsen()$mhw,unit_values,"<br>",
          "<strong>MW: </strong>",sf_sachsen()$mw,unit_values,"<br>",
          "<strong>MNW:</strong>",sf_sachsen()$mnw,unit_values,"<br>",
          "<strong>NW: </strong>",sf_sachsen()$nw,unit_values)
  })  
  
  
  popup_content_descr_sachsen_anhalt <- reactive({
    paste("<strong>HW: </strong>",sf_sachsen_anhalt()$hw,unit_values,"<br>",
          "<strong>MHW:</strong>",sf_sachsen_anhalt()$mhw,unit_values,"<br>",
          "<strong>MW: </strong>",sf_sachsen_anhalt()$mw,unit_values,"<br>",
          "<strong>MNW:</strong>",sf_sachsen_anhalt()$mnw,unit_values,"<br>",
          "<strong>NW: </strong>",sf_sachsen_anhalt()$nw,unit_values)
  })  
  
  popup_content_descr_both <- reactive({
    paste("<strong>HW: </strong>",sf_all()$hw,unit_values,"<br>",
          "<strong>MHW:</strong>",sf_all()$mhw,unit_values,"<br>",
          "<strong>MW: </strong>",sf_all()$mw,unit_values,"<br>",
          "<strong>MNW:</strong>",sf_all()$mnw,unit_values,"<br>",
          "<strong>NW: </strong>",sf_all()$nw,unit_values)
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
    paste("<strong>Messtellenname:</strong>",sf_sachsen()$messstellenname,"<br>",
          "<strong>Kennzeichnungsnummer:</strong>",sf_sachsen()$mkz)
  })
  
  default_popup_sachsen_anhalt <- reactive({
    paste("<strong>Messtellenname:</strong>",sf_sachsen_anhalt()$messstellenname,"<br>",
          "<strong>Kennzeichnungsnummer:</strong>",sf_sachsen_anhalt()$mkz)
  })
  
  default_popup_all <- reactive({
    paste("<strong>Messtellenname:</strong>",sf_all()$messstellenname,"<br>",
          "<strong>Kennzeichnungsnummer:</strong>",sf_all()$mkz)
  })
  
  alt_popup_sachsen <- reactive({
    paste("<strong>Hoehensystem:</strong>",sf_sachsen()$hohensystem)
  })
  
  trend_popup <- reactive({
    paste("<strong>Sen-Steigung:</strong>",round(sf_sign()$slope,5),"<br>",
          "<strong>P-Wert Kendall-Test</strong>",round(sf_sign()$p_value,5))
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
    
    if (input$show_descr_values & !input$show_period & !input$show_th & !input$show_alt & !input$show_cluster & !input$show_trend){
      if (input$interpolation){
          leaflet() %>%
          #addProviderTiles(provider=providers$OpenStreetMap)  %>%
          addTiles() %>%
          addRasterImage(kriged(),colors = pal_fun2,opacity = 0.7) %>%
          addLegend("bottomright",pal=pal_fun2,values=raster::values(kriged()),
                    title = paste0(input$descr_values," [",unit_values,"]"))
      } else {
        if (input$marker_loc=="Sachsen"){
          temp_leaflet %>%
            addCircleMarkers(
              radius = 2,
              stroke = T,
              opacity = 1,
              color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
              popup = popup_content_descr_sachsen(),
              layerId = sf_sachsen()$mkz) %>%
            addDrawToolbar(
              targetGroup='draw',
              polylineOptions=FALSE,
              markerOptions = FALSE,
              circleOptions = F,
              polygonOptions = F,
              circleMarkerOptions = F) %>%
            hideGroup("draw") %>%
            addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                      title = paste0(input$descr_values," [",unit_values,"]"))
            
        } else if (input$marker_loc=="Sachsen-Anhalt"){
          temp_leaflet %>%
            addCircleMarkers(
              radius = 2,
              stroke = T,
              opacity = 1,
              color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
              popup = popup_content_descr_sachsen_anhalt(),
              layerId = sf_sachsen_anhalt()$mkz) %>%
            addDrawToolbar(
              targetGroup='draw',
              polylineOptions=FALSE,
              markerOptions = FALSE,
              circleOptions = F,
              polygonOptions = F,
              circleMarkerOptions = F) %>%
            hideGroup("draw") %>%
            addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                      title = paste0(input$descr_values," [",unit_values,"]")) 
        } else {
          temp_leaflet %>%
            addCircleMarkers(
              radius = 2,
              stroke = T,
              opacity = 1,
              color = ~pal_fun(eval(as.symbol(tolower(input$descr_values)))),
              popup = popup_content_descr_both(),
              layerId = sf_all()$mkz) %>%
            addDrawToolbar(
              targetGroup='draw',
              polylineOptions=FALSE,
              markerOptions = FALSE,
              circleOptions = F,
              polygonOptions = F,
              circleMarkerOptions = F) %>%
            hideGroup("draw") %>%
            addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(tolower(input$descr_values))),opacity = 1,
                      title = paste0(input$descr_values," [",unit_values,"]")) 
        } 
      }
    } else if (!input$show_descr_values & input$show_period & !input$show_th & !input$show_alt & !input$show_cluster & !input$show_trend) {
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
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
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
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
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
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal = pal_fun,values = ~eval(as.symbol(period2str(input$period))),opacity = 1,
                    title = period2title(input$period)) 
      }
      
    } else if (input$show_th & !input$show_descr_values & !input$show_period & !input$show_alt & !input$show_cluster & !input$show_trend){
      
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
                      stroke = T,
                      weight = 1) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
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
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
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
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal = pal_th,values = th_poly_all()$area,opacity = 1,title = "Flaeche [km<sup>2</sup>]")
      }
      
    } else if (input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & !input$show_cluster & !input$show_trend){
      temp_leaflet %>%
        addCircleMarkers(
          radius = 2,
          stroke = T,
          opacity = 1,
          layerId = sf_sachsen()$mkz,
          color = ~pal_alt(hohensystem),
          popup = alt_popup_sachsen()) %>%
        addDrawToolbar(
          targetGroup='draw',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          circleOptions = F,
          polygonOptions = F,
          circleMarkerOptions = F) %>%
        hideGroup("draw") %>%
        addLegend("bottomright",pal = pal_alt,values = ~hohensystem,opacity = 1,title = "Hoehensystem")
    } else if (input$show_cluster & !input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period & !input$show_trend){
      cluster_df <- df_var_map()[complete.cases(df_var_map()),.(mkz,cluster=kmeans(scale(df_var_map()[complete.cases(df_var_map()),.(min_var,max_var)]),input$k_map,nstart=25)$cluster)]
      if (input$marker_loc=="Sachsen"){
        
        pts_cluster <- sf_sachsen() %>%
          left_join(cluster_df,by="mkz")
        
        pts_cluster %>%
          leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen()$mkz,
            color = ~pal_alt(cluster),
            popup = default_popup_sachsen()) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal=pal_alt,values = ~cluster,opacity = 1,title = "Cluster")
        
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        pts_cluster <- sf_sachsen_anhalt() %>%
          left_join(cluster_df,by="mkz")
        
        pts_cluster %>%
          leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen_anhalt()$mkz,
            color = ~pal_alt(cluster),
            popup = default_popup_sachsen_anhalt()) %>%
        addDrawToolbar(
          targetGroup='draw',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          circleOptions = F,
          polygonOptions = F,
          circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal=pal_alt,values = ~cluster,opacity = 1,title = "Cluster")
        
      } else {
        pts_cluster <- sf_all() %>%
          left_join(cluster_df,by="mkz")
        
        pts_cluster %>%
          leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_all()$mkz,
            color = ~pal_alt(cluster),
            popup = default_popup_all()) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal=pal_alt,values = ~cluster,opacity = 1,title = "Cluster")
      }
      
    } else if (input$show_trend & !input$show_cluster & !input$show_alt & !input$show_th & !input$show_descr_values & !input$show_period) {
      sf_sign() %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          radius = 2,
          stroke = T,
          opacity = 1,
          color = ~pal_fun(slope),
          layerId = sf_sign()$mkz,
          popup = trend_popup()
        ) %>%
        addDrawToolbar(
          targetGroup='draw',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          circleOptions = F,
          polygonOptions = F,
          circleMarkerOptions = F) %>%
        hideGroup("draw") %>%
        addLegend("bottomright",pal=pal_fun,values=~slope,opacity = 1,title = "San-Steigung")
    } else {
      if (input$marker_loc=="Sachsen"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen()$mkz,
            color = col_sachsen,
            popup = default_popup_sachsen()) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw")
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_sachsen_anhalt()$mkz,
            color = col_sachsen_anhalt,
            popup = default_popup_sachsen_anhalt()) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw")
      } else {
        temp_leaflet %>%
          addCircleMarkers(
            radius = 2,
            stroke = T,
            opacity = 1,
            layerId = sf_all()$mkz,
            color = ~d_pal_fun(loc),
            popup = default_popup_all()) %>%
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleOptions = F,
            polygonOptions = F,
            circleMarkerOptions = F) %>%
          hideGroup("draw") %>%
          addLegend("bottomright",pal=d_pal_fun,values = ~loc,opacity = 1,title = "Bundesland")
      }
    }
  })
  
  
  output$upload_status <- renderText({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Daten noch nicht vollstaendig eingelesen")
    )
    
    if (nrow(df_sachsen())>0 & nrow(df_sachsen_anhalt())>0){
      "Daten vollstaendig eingelesen"
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
      labs(x="",y= unit_values,title = title)+
      scale_y_continuous(labels = comma)
      #scale_y_reverse(labels=comma)
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
        labs(title="Laenge der Messreihen in Sachsen",x="Jahre",y="Anzahl an Messpunkten")
    } else if (input$marker_loc_meta2=="Sachsen-Anhalt"){
      df_sachsen_anhalt()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)] %>%
        ggplot(aes(period))+
        geom_histogram(bins=input$bins_meta2,fill=input$col_meta2,col="black")+
        labs(title="Laenge der Messreihen in Sachsen-Anhalt",x="Jahre",y="Anzahl an Messpunkten")
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
  
  output$meta3 <- renderPlot({
    
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )    
    
    if (input$marker_loc_meta3=="Sachsen"){
      cmp_mean <- df_sachsen()[,.(cmp_mean_gw=mean(wert)),by=.(mkz)]
      gw_change_temp <- cmp_mean[(df_sachsen()[,.(year=year(messzeitpunkt),wert,mkz)][,.(mean_gw=mean(wert)),by=.(mkz,year)]),on="mkz"][,`:=`(gw_change=cmp_mean_gw-mean_gw)]
      #breaks <- c(rev(c(0,1, round(abs(min(gw_change_temp$gw_change,na.rm = T)),0)-20))*-1,c(0, 1, round(max(gw_change_temp$gw_change,na.rm = T),0)-20))
    } else if (input$marker_loc_meta3=="Sachsen-Anhalt"){
      cmp_mean <- df_sachsen_anhalt()[,.(cmp_mean_gw=mean(wert)),by=.(mkz)]
      gw_change_temp <- cmp_mean[(df_sachsen_anhalt()[,.(year=year(messzeitpunkt),wert,mkz)][,.(mean_gw=mean(wert)),by=.(mkz,year)]),on="mkz"][,`:=`(gw_change=cmp_mean_gw-mean_gw)]
      #breaks <- c(rev(c(0,1, round(abs(min(gw_change_temp$gw_change,na.rm = T)),0)-20))*-1,c(0, 1, round(max(gw_change_temp$gw_change,na.rm = T),0)-20))
    } else {
      cmp_mean <- df_all()[,.(cmp_mean_gw=mean(wert)),by=.(mkz)]
      gw_change_temp <- cmp_mean[(df_all()[,.(year=year(messzeitpunkt),wert,mkz)][,.(mean_gw=mean(wert)),by=.(mkz,year)]),on="mkz"][,`:=`(gw_change=cmp_mean_gw-mean_gw)]
      #breaks <- c(rev(c(0,1, round(abs(min(gw_change_temp$gw_change,na.rm = T)),0)-20))*-1,c(0, 1, round(max(gw_change_temp$gw_change,na.rm = T),0)-20))
    }
    
    breaks <- c(-10,-1,-0.1,0,0.1,1,10)
    
    gw_change_temp %>%
      filter(year>=year(input$gw_change_years[1]) & year<=year(input$gw_change_years[2])) %>%
      ggplot(aes(year,mkz,fill=gw_change,group=mkz))+
      geom_tile()+
      scale_fill_viridis_c("GW Veraenderung [m]",trans = scales::pseudo_log_trans(sigma = 0.001),breaks=breaks,na.value = "transparent")+
      labs(x="",title = "Abweichung des Grundwasserstandes vom langjaehrigen Mittel")+
      theme_bw()+
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y= element_blank())
    
  })
  
  output$selected_download <- renderDataTable({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    selected_shp$data
  })
  
  output$no_selected <- renderText({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    
    if (length(selected_shp$data)!=0){
      paste("Anzahl an ausgewaehlten Objekten: ",nrow(selected_shp$data)) 
    } else {
      paste("Anzahl an ausgewaehlten Objekten: ",0)
    }
  })

  df_var <- reactive({
    if (input$cluster_filter){
      
      if (input$marker_loc_meta4=="Sachsen"){
        mkz_filtered <- df_sachsen()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_sachsen()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else if (input$marker_loc_meta4=="Sachsen-Anhalt"){
        mkz_filtered <- df_sachsen_anhalt()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_sachsen_anhalt()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else {
        mkz_filtered <- df_sachsen_all()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_all()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      }
    } else {
      if (input$marker_loc_meta4=="Sachsen"){
        df_sachsen()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else if (input$marker_loc_meta4=="Sachsen-Anhalt"){
        df_sachsen_anhalt()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else {
        df_all()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } 
    }
  })
  
  df_var_map <- reactive({
    if (input$cluster_filter){
      if (input$marker_loc=="Sachsen"){
        mkz_filtered <- df_sachsen()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_sachsen()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        mkz_filtered <- df_sachsen_anhalt()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_sachsen_anhalt()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else {
        mkz_filtered <- df_all()[,.(period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25),
                                        end_date=max(messzeitpunkt)),by=.(mkz)][period>=10 & end_date>=as.Date("1991-01-01")][,mkz]
        df_all()[mkz %in% mkz_filtered][,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } 
    } else {
      if (input$marker_loc=="Sachsen"){
        df_sachsen()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else if (input$marker_loc=="Sachsen-Anhalt"){
        df_sachsen_anhalt()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      } else {
        df_all()[,.(mw,wert,mkz)][,var:=wert-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)][,range_var:=abs(min_var-max_var)] 
      }
    }
  })
  
  mkz2date <- function(data,period_len,filter=F,start_year=NULL){
    joined_data <- period_len[mkz==data$mkz]
    if (filter){
      start_year <- start_year
    } else {
      start_year <- joined_data$min_date 
    }
    end_year <- joined_data$max_date
    clean_data <- data[,-1]
    final_data <- clean_data[,order(as.numeric(names(clean_data)))]
    start_index <- which(as.numeric(names(final_data))==start_year)
    end_index <- which(as.numeric(names(final_data))==end_year)
    values <- unlist(final_data[,start_index:end_index])
    if (length(values)<10){
      return(NULL)
    } else if (sum(is.na(unlist(final_data[,start_index:end_index])))>0 & any(!is.na(unlist(final_data[,start_index:end_index])))){
      return(ts(na.approx(values),start = start_year,end=end_year))
    } 
    return(ts(unlist(values),start = start_year,end=end_year))
  }
  
  sf_sign <- reactive({
    if (input$marker_loc=="Sachsen"){
      if (input$trend_last){
        ts_mat <- as_tibble(df_sachsen()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          filter(year>=2011) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA)
      } else {
        ts_mat <- as_tibble(df_sachsen()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA) 
      }
      
      period_len <- df_sachsen()[,.(min_date=year(min(messzeitpunkt)),max_date=year(max(messzeitpunkt)),
                                    period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)]
    } else if (input$marker_loc=="Sachsen-Anhalt"){
      if (input$trend_last){
        ts_mat <- as_tibble(df_sachsen_anhalt()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          filter(year>=2011) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA)
      } else {
        ts_mat <- as_tibble(df_sachsen_anhalt()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA) 
      }
      
      period_len <- df_sachsen_anhalt()[,.(min_date=year(min(messzeitpunkt)),max_date=year(max(messzeitpunkt)),
                                           period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)]
    } else {
      if (input$trend_last){
        ts_mat <- as_tibble(df_all()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          filter(year>2011) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA)
      } else {
        ts_mat <- as_tibble(df_all()[,.(datum=messzeitpunkt,wert,mkz)]) %>%
          mutate(datum=as.Date(datum)) %>%
          mutate(year=year(datum)) %>%
          group_by(mkz,year) %>%
          summarize(mean_gw=mean(wert,na.rm = T)) %>%
          pivot_wider(id_cols = mkz,names_from=year,values_from=mean_gw,values_fill=NA)
      }
      
      period_len <- df_all()[,.(min_date=year(min(messzeitpunkt)),max_date=year(max(messzeitpunkt)),
                                period=as.numeric(difftime(max(messzeitpunkt),min(messzeitpunkt),units = "weeks")/52.25)),by=.(mkz)]
    }
    

    series <- ts_mat %>%
        group_split() 
    
    if (!input$trend_last){
      series_size <- lapply(series,function(x){
        mkz2date(x,period_len)
      })
      
    } else {
      series_size <- lapply(series,function(x){
        mkz2date(x,period_len,filter = T,start_year = 2011)
      })
      
    }
    
    too_few <- which(sapply(series_size,function(x) any(is.null(x))| any(is.nan(x)))==T)
    
    filtered_series <- series_size[-too_few]
    
    mk_ls <- sapply(filtered_series,function(x) mk.test(x)$p.value)
    sens_slope_ls <- sapply(filtered_series,function(x) sens.slope(x)$estimates)
    
    sign_df <- tibble(mkz=ts_mat$mkz[-too_few],p_value=mk_ls,slope=unname(sens_slope_ls))
    
    
    if (input$marker_loc=="Sachsen"){
      sf_sachsen() %>%
        left_join(sign_df,by="mkz")
    } else if (input$marker_loc=="Sachsen-Anhalt"){
      sf_sachsen_anhalt() %>%
        left_join(sign_df,by="mkz")
    } else {
      sf_all() %>%
        left_join(sign_df,by="mkz")
    }
    
  })
  
  
  output$cluster_plot <- renderPlot({
    
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    
    kmean_obj <- kmeans(df_var()[complete.cases(df_var()),.(min_var=min_var*-1,max_var)],input$k,nstart=25)

    fviz_cluster(kmean_obj,df_var()[complete.cases(df_var()),.(min_var=min_var*-1,max_var)],geom = "point",
                 ellipse.type = "convex",ggtheme = theme_bw(),palette=viridis::viridis(input$k)) +
      labs(x="Maximale negative Abweichung vom mittleren Grundwasserpegel [m]",y="Maximale positive Abweichung vom mittleren Grundwasserpegel [m]",title = "")

  })
  
  output$cluster_info <- renderPlot({
    
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    clust_list <- vector("list",6)
    for (i in 1:6){
      clust_list[[i]] <- data.frame(clust=i+1,ss=sum(kmeans(scale(df_var()[complete.cases(df_var()),.(min_var,max_var)]),i+1,nstart=25)$withinss))
    }
    bind_rows(clust_list) %>%
      ggplot(aes(clust,ss,fill=as.factor(clust)))+
      geom_col()+
      scale_fill_viridis_d("Cluster")+
      labs(x="Cluster",y="Varianz innerhalb der Cluster")
  })
  
  output$gw_surface <- renderRglwidget({
    shiny::validate(
      need(input$df_sachsen!="" & input$df_sachsen_anhalt!="","Die Datensaetze muessen erst geuploadet werden")
    )
    
    try(rgl.close())
    
    gw_surface <- raster_to_matrix(kriged())
    gw_surface %>%
      sphere_shade(zscale = 1) %>%
      plot_3d(gw_surface, zscale = 1, fov = 0, theta = -45, phi = 45,
              windowsize = c(1200, 1000), zoom = 0.75,
              water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = c("#785E3C"),
              waterlinecolor = "grey", waterlinealpha = 0.8,solid=F)
    rglwidget()
  })
  

  output$download_selected <- downloadHandler(
    filename = function(){
      paste("selected_shapefile","zip",sep = ".")
    },
    content = function(file){
      
      shp_data <- selected_shp$data %>%
        st_transform(crs=orig_crs) %>%
        as("Spatial")
      
      temp_shp <- tempdir()
      # write shp files

      writeOGR(shp_data, temp_shp, "selected_features", "ESRI Shapefile",
               overwrite_layer = TRUE)
      
      #zip all the shp files
      zip_file <- file.path(temp_shp, "selected_features.zip")
      shp_files <- list.files(temp_shp,
                              "selected_features",
                              full.names = TRUE)
      # the following zip method works for me in linux but substitute with whatever method working in your OS 
      zip_command <- paste("zip -j", 
                           zip_file, 
                           paste(shp_files, collapse = " "))
      system(zip_command)
      # copy the zip file to the file argument
      file.copy(zip_file, file)
      # remove all the files created
      file.remove(zip_file, shp_files)
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
