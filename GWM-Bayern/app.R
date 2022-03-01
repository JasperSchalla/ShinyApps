library(shiny)
library(data.table)
library(shinydashboard)
library(leaflet)
library(sf)
library(tidyverse)
library(scales)
library(dygraphs)
library(xts)
library(factoextra)

addLegend <- leaflet::addLegend

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = span(tagList(icon("tint"),"GWM"))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Analysis",tabName = "analysis",icon = icon("chart-line")),
                        menuItem("Map",tabName = "wells_data",icon = icon("layer-group"))
                      )
                    ),
                    dashboardBody(tags$head(
                      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
                    ),
                    tabItems(
                      tabItem(tabName = "wells_data",
                              fluidRow(
                                leafletOutput("map"),
                                absolutePanel(fixed = T,draggable = F,
                                              top = 200, left="auto" ,right = "auto", bottom = "auto",
                                              width = 350, height = "auto", style = "opacity: 1; z-index: 10;",
                                              box(id="option_panel",width = 12,
                                                  h3("Map options"),
                                                  br(),
                                                  checkboxInput("show_time_series",label=strong("Show time series")))),
                                conditionalPanel("input.show_time_series==true",
                                                 absolutePanel(fixed = T,
                                                               draggable = F, top = 75, left=600 ,right = "auto", bottom = "auto",
                                                               width = 1500, height = "auto", style = "opacity: 1; z-index: 10;",
                                                               box(dygraphOutput("time_series")))))
                              
                      ),
                      tabItem(tabName = "analysis",
                        fluidRow(
                          box(width = 3,
                              selectInput("cluster_var",label = strong("Measuring Depth"),choices = c("flach","tief"),selected = "flach"),
                              sliderInput("cluster_k",label = strong("K"),min=2,max=15,value = 2),
                              numericInput("cluster_xlim",label = strong("X-axis Limit"),min = 1,max = 15,value = 1),
                              numericInput("cluster_ylim",label = strong("Y-axis Limit"),min = 1,max = 15,value = 1)),
                          box(width = 9,plotOutput("cluster"))
                        ),
                        fluidRow(
                          box(width = 3,
                              selectInput("osm_legend",choices = list("CLC Class 1" = "lu_alt",
                                                                      "CLC Class 2" = "lu"),
                                          selected = "lu",label = strong("CLC Classification"))),
                          box(width = 9,plotOutput("osm"))
                        )
                      )
                    )
                    )
)


server <- function(input, output, session){
  
  theme_set(theme_bw())
  unit_values <- "m ue. NN"

  lu <- reactive({
    fread("./data/lu.csv") %>%
      filter(type==input$cluster_var)
  })
  export_data <-  fread("gzip -dc './data/bayern.csv.gz'") %>%
    mutate(messzeitpunkt=as.Date(datum))
  
  # Update xlim and ylim input
  
  observe({
    updateNumericInput(session,"cluster_xlim",min=0,max = max(cluster_data()$min_var*-1)+0.05*max(cluster_data()$min_var*-1),
                       value = round(max(cluster_data()$min_var*-1)+0.05*max(cluster_data()$min_var*-1),0))
    updateNumericInput(session,"cluster_ylim",min=0,max = max(cluster_data()$max_var)+0.05*max(cluster_data()$max_var),
                       value=round(max(cluster_data()$max_var)+0.05*max(cluster_data()$max_var),0))
  })
  
  # Filtering
  
  length_criterion <- 10 # years
  max_criterion <- as.Date("1991-01-01")
  
  mkz_filtered_flach <- export_data[type=="flach"][,.(period=as.numeric(difftime(max(datum),min(datum),units = "weeks")/52.25),
                                 end_date=max(datum)),by=.(mkz,type)][period>=length_criterion & end_date>=max_criterion][,.(mkz,type)][,mkz]
  
  mkz_filtered_tief <- export_data[type=="tief"][,.(period=as.numeric(difftime(max(datum),min(datum),units = "weeks")/52.25),
                                                      end_date=max(datum)),by=.(mkz,type)][period>=length_criterion & end_date>=max_criterion][,.(mkz,type)][,mkz]
  
  df_bayern <- export_data[(mkz %in% mkz_filtered_flach & type=="flach") | (mkz %in% mkz_filtered_tief & type=="tief")]
  
  # Spatial data 
  
  sf_bayern<- st_as_sf(as.data.frame(unique(df_bayern,by="mkz")),coords = c("ostwert","nordwert"),crs=31468) %>%
    st_transform(crs=4326)
  
  
  classes <- reactive({
    sf_bayern %>%
        left_join(cluster_data(),by="mkz") %>%
        mutate(class=factor(class)) %>%
        filter(type==input$cluster_var)
  })
  
  pal_alt <- colorFactor("viridis",NULL,na.color = NA)
  
  classes_popup <- reactive({
    paste("<strong>Station number:</strong>",classes()$mkz)
  })
  
  output$map <- renderLeaflet({
    classes() %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 2,
        stroke = T,
        opacity = 1,
        color = ~pal_alt(class),
        layerId = classes()$mkz,
        popup = classes_popup()
      ) %>%
      addLegend("bottomright",pal=pal_alt,values=~class,opacity = 1,title = "Classes")
  })
  
  # Get ic from points which got clicked on
  
  mkz_clicked <- eventReactive(input$map_marker_click,{
    click <- input$map_marker_click
    if (is.null(click)){
      return()
    }
    
    click$id
  })
  
  cluster <- reactive({
    set.seed(100)
    
    cluster_data <- df_bayern[type==input$cluster_var][,.(mw,gw,mkz)][,var:=gw-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)]
    
    cluster_input <- cluster_data[,.(min_var,max_var)][complete.cases(cluster_data[,.(min_var,max_var)])]
    kmeans(cluster_input,input$cluster_k,nstart=25)
  })
  
  cluster_data <- reactive({
    cluster_df <- df_bayern[type==input$cluster_var][,.(mw,gw,mkz)][,var:=gw-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)]
    cluster_df[complete.cases(cluster_df)][,.(mkz,class=cluster()$cluster,min_var,max_var)]
  })
  
  output$time_series <- renderDygraph({
    
    
    # df_bayern[mkz==mkz_clicked(),.(datum,gw,type)] %>%
    #   filter(type==input$cluster_var) %>%
    #   ggplot(aes(datum,gw))+
    #   geom_line(col="darkblue")+
    #   geom_point(col="darkblue")+
    #   labs(x="",y= unit_values,title = mkz_clicked())+
    #   scale_y_continuous(labels = comma)
    #scale_y_reverse(labels=comma)
    
    data <- df_bayern[mkz==mkz_clicked(),.(datum,gw,type,station_name)] %>%
      filter(type==input$cluster_var) %>%
      mutate(datum=as.Date(as.integer(datum)))
    
    xts_data <- xts(data[,gw],order.by = data[,datum])
    
    dygraph(xts_data,main = glue::glue("{unique(data$station_name)}: Station {mkz_clicked()} ({input$cluster_var})")) %>%
      dyAxis("y",label = glue::glue("Groundwater level {unit_values}")) %>%
      dyRangeSelector()
    
  })
  
  output$cluster <- renderPlot({
    cluster_data <- df_bayern[type==input$cluster_var][,.(mw,gw,mkz)][,var:=gw-mw][,.(min_var=min(var),max_var=max(var)),by=.(mkz)]
    
    cluster_input <- cluster_data[,.(min_var,max_var)][complete.cases(cluster_data[,.(min_var,max_var)])]
    
    fviz_cluster(cluster(),cluster_input[,.(min_var=min_var*-1,max_var)],geom = "point",
                 ellipse.type = "convex",ggtheme = theme_bw(),palette=viridis::viridis(input$cluster_k),stand = F) +
      labs(x="Maximum negative deviation from mean groundwater level [m]",y="Maximum positive deviation from mean groundwater level [m]",title = "")+
      xlim(c(0,input$cluster_xlim))+
      ylim(c(0,input$cluster_ylim))
  })

  
  output$osm <- renderPlot({
    
    lu_classes <- cluster_data() %>%
      left_join(lu(),by="mkz") %>%
      mutate(class=as.factor(class))
    
    lu_classes[,count:=.N,by=.(class)] %>%
      rename(label=input$osm_legend) %>%
      ggplot(aes(x=class,fill=label))+
      geom_bar(position = "fill")+
      geom_text(aes(class,1.1,label=count))+
      scale_y_continuous(labels = scales::percent,breaks = seq(0,1,0.25))+
      scale_fill_brewer("Land Use Category",palette = "Paired",na.value="black")+
      labs(x="Class",y="")
  })
  
}

shinyApp(ui = ui, server = server)

