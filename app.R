# MH-shiny
# create by Valentina Costa
# 2021

# library -----------------------------------------------------------------

library(htmltools)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(RColorBrewer)
library(reactable)
library(wordcloud)

# dataset -----------------------------------------------------------------

# database-systematic map
data <- read.csv("data/database_selected.csv")

# national MPA
MPA_med_national <-
  st_read("data/MPAnational_5m.gpkg", quiet = T) %>%
  st_transform('+proj=longlat +datum=WGS84')

# Nature 2000 site
MPA_med_NATURA2000 <-
  st_read("data/Nature2000_5m.gpkg", quiet = T) %>%
  st_transform('+proj=longlat +datum=WGS84')

# Proposed Natura 2000 site
MPA_med_pNATURA2000 <-
  st_read("data/PropNature2000_5m.gpkg", quiet = T) %>%
  st_transform('+proj=longlat +datum=WGS84')


# shiny-ui ----------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  
  navbarPage(
    "Marine Hazard",

    ## tabpanel HOME ----
    tabPanel("Home",
             div(
               class = "outer",
               
               hr(),
               includeMarkdown("inst/Rmarkdown/home-MH.Rmd")               

             )),
    
    ## tabpanel Interactive map ----
    tabPanel(
      "Interactive map",
      div(
        class = "outer",
        
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 280,
          height = "auto",
          
          selectInput(
            "area",
            "Select by Area",
            choices = levels(factor(data$area)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "site",
            "Select by Site Name",
            choices = levels(factor(data$site)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "site_type",
            "Select by Site Type",
            choices = levels(factor(data$site_type)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "country",
            "Select by Country",
            choices = levels(factor(data$country)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "target_category",
            "Select by Target Category",
            choices = levels(factor(data$target_cat)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "bio_target",
            "Select by Biological Target Category",
            choices = levels(factor(data$bio_target)),
            selected = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            "bio_response",
            "Select by Biological Response",
            choices = levels(factor(data$bio_response)),
            selected = NULL,
            multiple = TRUE
          ),
          
          sliderInput(
            "int_depth",
            label = "Depth range",
            min = min(data$avg_depth),
            max = max(data$avg_depth),
            value = c(min(data$avg_depth), max(data$avg_depth))
          ),
          
          actionButton("int_clear", "Clear selection")
        )
      )
    ),
    
    ## tabpanel Data explorer ----
    tabPanel(
      "Data Explorer",
      div(
        class = "outer",
        
        hr(),
        fluidRow(
          column(3, htmlOutput("selection_1"), offset = 1),
        ),
        hr(),
        h2("List of scientific items"),
        
        fluidRow(
          column(9, reactableOutput("table01"),
                 downloadButton("download")),
          column(3, plotOutput("target_cat", height = "40vh"),
                      plotOutput("bio_target", height = "40vh"))
        )
        
      )
    ),
    
    ## tabpanel Keywords analysis ----
    tabPanel(
      "Keywords analysis",
      div(
        class = "outer",
        
        hr(),
        fluidRow(
          column(3, htmlOutput("selection_2"), offset = 1),
        ),
        hr(),
        h2("Word Cloud "),
        
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            selectInput(
              "selection",
              "Select:",
              choices = c("", "abstract", "title",
                          "author_keywords", "index_keywords"),
              selected = NULL,
              multiple = FALSE
            ),
            actionButton("update", "Update"),
            hr(),
            sliderInput(
              "freq",
              "Minimum Frequency:",
              min = 1,
              max = 50,
              value = 5
            ),
            sliderInput(
              "max",
              "Maximum Number of Words:",
              min = 1,
              max = 300,
              value = 300
            ),
            width = 4
          ),
          
          # Show Word Cloud
          mainPanel(plotOutput("cloud"),
                    plotOutput("frequencies"))
        )
      )
    ),
    
    ## tabpanel About ----
    tabPanel("About",
             div(
               class = "outer",
               
               hr(),
               includeMarkdown("inst/Rmarkdown/about-MH.Rmd")               
             ))
    
  )
)

# shiny-server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # prepare the text for map labels
    mylabels_MPA <- paste(
    "<b>", "National Marine Protected Area", "</b>", "<br/>",
    "<b>", "Country: ", "</b>", MPA_med_national$ISO3,"<br/>",
    "<b>", "Name: ", "</b>", MPA_med_national$NAME, "<br/>") %>%
    lapply(htmltools::HTML)

  mylabels_Nat2000 <- paste(
    "<b>", "Nature2000 area", "</b>", "<br/>",
    "<b>", "Country: ", "</b>", MPA_med_NATURA2000$ISO3,"<br/>",
    "<b>", "Name: ", "</b>", MPA_med_NATURA2000$NAME, "<br/>") %>%
    lapply(htmltools::HTML)

  mylabels_pNat2000 <- paste(
    "<b>", "Proposed Nature2000 area", "</b>", "<br/>",
    "<b>", "Country: ", "</b>", MPA_med_pNATURA2000$ISO3,"<br/>",
    "<b>", "Name: ", "</b>", MPA_med_pNATURA2000$NAME, "<br/>") %>%
    lapply(htmltools::HTML)

  
  # Create the base-map
  output$map <-
    renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(15, 37, zoom = 4.5) %>%
        addMeasure(position = "bottomleft") %>%
        
        ## add MPA polygons
        addPolygons(data = MPA_med_national,
                  color = "green",
                  stroke = T,
                  weight = 1,
                  label = mylabels_MPA,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "11px",
                    direction = "auto"),
                  group = "MPA (green)") %>%
      
        ## add Nature2000 polygons
      addPolygons(data = MPA_med_NATURA2000,
                  color = "orange",
                  stroke = T,
                  weight = 1,
                  label = mylabels_Nat2000,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "3px 8px"),
                    textsize = "11px",
                    direction = "auto"),
                  group = "Nature 2000 (orange)") %>%
      
        ## add ProposedNature2000 polygons
      addPolygons(data = MPA_med_pNATURA2000,
                  color = "grey",
                  stroke = T,
                  weight = 1,
                  label = mylabels_pNat2000,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "11px",
                    direction = "auto"),
                  group = "Proposed Nature 2000 (grey)") %>%
      
        ## add Layers control
      addLayersControl(
        overlayGroups = c(
          "MPA (green)",
          "Nature 2000 (orange)",
          "Proposed Nature 2000 (grey)"
        ),
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      )
    })
  
  ## Selection
  
  output$selection_1 <- output$selection_2 <-
    
    renderPrint({
      HTML(paste0("Selection: ", "<b>", 
                  input$area, 
                  ", ", input$site, 
                  ", ", input$site_type,  
                  "</b>"))
    })
  
  ## create a filtered dataset
  
  data_selected <- reactive({
    
    
    
    data_selected <-
      data %>%
      filter(site %in% input$site |
               area %in% input$area |
               site_type %in% input$site_type |
               country %in% input$country |
               target_cat %in% input$target_cat |
               bio_target %in% input$bio_target |
               bio_response %in% input$bio_response)
    
    data_selected
    
  })
  
  # create a table of filtered dataset
  
  output$table01 <- renderReactable({
    table_selected <-
      data_selected() %>%
      distinct(doi, .keep_all = T)
    
    reactable(
      table_selected[c(1:4, 6)],
      columns = list(
        author = colDef(name = "Authors"),
        title = colDef(name = "Title"),
        year = colDef(name = "Year", minWidth = 40) ,
        source_title = colDef(name = "Source Title"),
        link = colDef(name = "Link")
      ),
      height = 700,
      outlined = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      showPagination = TRUE
    )
  })
  
  
  # graph-01
  
  output$target_cat <- renderPlot({
    data_selected() %>%
      ggplot() +
      geom_histogram(aes(x = avg_depth, fill = target_cat),
                     color = "black") +
      scale_fill_brewer(palette = "Dark2") +
      labs(x = NULL,
           y = "# of observations",
           title = "Observations per depth") +
      guides(
        fill = guide_legend(
          title = "Target category",
          ncol = 2,
          title.position = "top",
          title.theme = element_text(face = "bold",
                                     size = 11)
        )
      ) +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size = 11))
    
  })
  
  # graph-02
  
  output$bio_target <- renderPlot({
    data_selected() %>%
      filter(target_cat == "biological") %>%
      ggplot() +
      geom_histogram(aes(x = avg_depth,
                         fill = bio_target),
                     color = "black") +
      scale_fill_brewer(palette = "PiYG") +
      labs(x = NULL,
           y = "# of observations",
           title = "Observations per biological target") +
      guides(
        fill = guide_legend(
          title = "Biological target",
          ncol = 4,
          title.position = "top",
          title.theme = element_text(face = "bold",
                                     size = 11)
        )
      ) +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size = 11))
    
    
  })
  
  ## create the tidy_words dataset
  
  tidy_words <- reactive({
    
    req(input$selection)
    
    tidy_words <-
      data_selected() %>%
      distinct(doi, .keep_all = T) %>%
      tidytext::unnest_tokens(
        output = word,
        input = input$selection,
        token = "regex",
        pattern = ";"
      ) %>%
      filter(!is.na(word)) %>%
      mutate(word = str_squish(word)) %>%
      count(word, sort = T)
    
    tidy_words
    
  })
  
  # wordcloud
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <-
    renderPlot({
      tidy_words() %>%
        with(
          wordcloud_rep(
            word,
            n,
            min.freq = input$freq,
            max.words = input$max,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2")
          )
        )
    })
  
  
  # Number of Words occurrence
  
  output$frequencies <-
    renderPlot({
      tidy_words() %>%
        filter(n > 50) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = n, label = n)) +
        geom_bar(stat = "identity") +
        geom_label(aes(fill = n), colour = "white", fontface = "bold") +
        scale_fill_gradientn(colours = brewer.pal(8, "Accent")) +
        coord_flip() +
        labs(y = "N. of Occurrence",
             x = "Keywords") +
        theme_bw() +
        theme(legend.position = "none")
    })
  
  ## observeEvent - input$area
  
  observeEvent(input$area, {
    
    if (input$area != "")
    
      {
      colorData_area <- data[data$area %in% input$area, ]
      pal_area <- colorFactor("plasma", levels = colorData_area)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          lng = ~ Longitude,
          lat = ~ Latitude,
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_area(colorData_area),
          popup = ~htmlEscape(Longitude),
          popupOptions = popupOptions(maxWidth = "100%", closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  # observeEvent - input$site
  
  observeEvent(input$site, {
    if (input$site != "")
    {
      # leafletProxy("map") %>% clearShapes()
      
      colorData_site <- data[data$site %in% input$site, ]
      pal_site <- colorFactor("magma", levels = colorData_site)
      
      leafletProxy("map", session) %>%
        addCircleMarkers(
          data = data_selected(),
          lng = ~ Longitude,
          lat = ~ Latitude,
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_site(colorData_site),
          label = ~ as.character(site),
          popup = ~htmlEscape(c(area,site)),
          popupOptions = popupOptions(maxWidth = "100%", closeOnClick = TRUE),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  # observeEvent - input$site_type
  
  observeEvent(input$site_type, {
    if (input$site_type != "")
    {
      colorData_site_type <- data[data$site_type %in% input$site_type, ]
      pal_type <-
        colorFactor("inferno", levels = colorData_site_type)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          lng = ~ Longitude,
          lat = ~ Latitude,
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_type(colorData_site_type),
          popup = ~htmlEscape(Longitude),
          popupOptions = popupOptions(maxWidth = "100%", closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  # observeEvent - input$country
  
  observeEvent(input$country, {
    if (input$country != "")
    {
      colorData_country <- data[data$country %in% input$country, ]
      pal_type <-
        colorFactor("inferno", levels = colorData_country)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          lng = ~ Longitude,
          lat = ~ Latitude,
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_type(colorData_country),
          popup = ~htmlEscape(country),
          popupOptions = popupOptions(maxWidth = "100%", closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  # observeEvent - input$int_clear
  
  observeEvent(input$int_clear, {
    
    updateSelectInput(session, "site", selected = "")
    updateSelectInput(session, "site_type", selected = "")
    updateSelectInput(session, "area", selected = "")
    updateSelectInput(session, "country", selected = "")
    updateSelectInput(session, "target_cat", selected = "")
    updateSelectInput(session, "bio_target", selected = "")
    updateSelectInput(session, "bio_response", selected = "")
    
    leafletProxy("map") %>% clearMarkers()
    
  })
  
  # observeEvent - input$update
  
  observeEvent(input$update, {
    updateSelectInput(session, "selection", selected = "")
    
  })
  
  
}


# run the application -----------------------------------------------------

shinyApp(ui, server)
