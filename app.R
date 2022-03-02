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

# # national MPA
# MPA_med_national <-
#   st_read("data/MPAnational_5m.gpkg", quiet = T) %>%
#   st_transform('+proj=longlat +datum=WGS84')
# 
# # Nature 2000 site
# MPA_med_NATURA2000 <-
#   st_read("data/Nature2000_5m.gpkg", quiet = T) %>%
#   st_transform('+proj=longlat +datum=WGS84')
# 
# # Proposed Natura 2000 site
# MPA_med_pNATURA2000 <-
#   st_read("data/PropNature2000_5m.gpkg", quiet = T) %>%
#   st_transform('+proj=longlat +datum=WGS84')


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
               fluidRow(
                 column(5, includeMarkdown("inst/Rmarkdown/home-MH.Rmd"), 
                        img(src = "SZN.png",
                            height = "100px",
                            width = "250px"), offset = 1),
                 column(6, img(src = "vulcano.png",
                               height = "480px",
                               width = "640px"))),
               hr(),
               helpText("Info from http://www.marinehazard.cnr.it.", align = "center")

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
            "country",
            "Select by Country",
            choices = levels(factor(data$country)),
            selected = NULL,
            multiple = TRUE
          ),
          
          conditionalPanel(
            condition = "input.country != ''", 
            selectInput(
            "area",
            "Select by Area",
            choices = levels(factor(data$area)),
            selected = NULL,
            multiple = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.area != ''",
            selectInput("site",
            "Select by Site Name",
            choices = levels(factor(data$site)),
            selected = NULL,
            multiple = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.site != ''",
            selectInput(
            "site_type",
            "Select by Site Type",
            choices = levels(factor(data$site_type)),
            selected = NULL,
            multiple = TRUE)
          ),
          

          # sliderInput(
          #   "int_depth",
          #   label = "Depth range",
          #   min = min(data$avg_depth),
          #   max = max(data$avg_depth),
          #   value = c(min(data$avg_depth), max(data$avg_depth))
          # ),
          
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
          column(3, selectInput("var1", NULL , 
                                choices = c("", names(data)[c(3, 4, 10:12, 15:25)]),
                                selected = "")),
          column(3, selectInput("var2", NULL , 
                                choices = c("", names(data)[c(3, 4, 10:12, 15:25)]),
                                selected = ""))
        ),
        h2("List of scientific items"),
        
        fluidRow(
          column(9, reactableOutput("table01"),
                 downloadButton("download_filtered")),
          column(3, plotOutput("graph_01", height = "40vh"),
                      plotOutput("graph_02", height = "40vh"))
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
            hr(),
            h4("Wordcloud"), 
            numericInput(
              "cloud_min_freq",
              "Minimum Frequency:",
              value = 5
            ),
            sliderInput(
              "max",
              "Maximum Number of Words:",
              min = 1,
              max = 300,
              value = 300
            ),
            hr(),
            h4("Words frequency"),
            numericInput("words_min_freq", 
                         "Minimum frequency:",
                         value = 1),   
            width = 3
          ),
          
          # Show Word Cloud
          mainPanel(column(6, plotOutput("cloud")),
                    column(6, plotOutput("frequencies")))
        )
      )
    ),
    
    ## tabpanel About ----
    tabPanel("About",
             div(
               class = "outer",
               
               hr(),
               column(10, includeMarkdown("inst/Rmarkdown/about-MH.Rmd"), offset = 1)               
             ))
    
  )
)

# shiny-server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ## create the text for map labels ----
  #   mylabels_MPA <- paste(
  #   "<b>", "National Marine Protected Area", "</b>", "<br/>",
  #   "<b>", "Country: ", "</b>", MPA_med_national$ISO3,"<br/>",
  #   "<b>", "Name: ", "</b>", MPA_med_national$NAME, "<br/>") %>%
  #   lapply(htmltools::HTML)
  # 
  # mylabels_Nat2000 <- paste(
  #   "<b>", "Nature2000 area", "</b>", "<br/>",
  #   "<b>", "Country: ", "</b>", MPA_med_NATURA2000$ISO3,"<br/>",
  #   "<b>", "Name: ", "</b>", MPA_med_NATURA2000$NAME, "<br/>") %>%
  #   lapply(htmltools::HTML)
  # 
  # mylabels_pNat2000 <- paste(
  #   "<b>", "Proposed Nature2000 area", "</b>", "<br/>",
  #   "<b>", "Country: ", "</b>", MPA_med_pNATURA2000$ISO3,"<br/>",
  #   "<b>", "Name: ", "</b>", MPA_med_pNATURA2000$NAME, "<br/>") %>%
  #   lapply(htmltools::HTML)

  
  ## create the base-map ----
  
  output$map <-
    renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(15, 37, zoom = 4.5) %>%
        addMeasure(position = "bottomleft") 
      
      # %>%
        
      #   ### add MPA polygons ----
      #   addPolygons(data = MPA_med_national,
      #             color = "green",
      #             stroke = T,
      #             weight = 1,
      #             label = mylabels_MPA,
      #             labelOptions = labelOptions(
      #               style = list("font-weight" = "normal", padding = "3px 8px"),
      #               textsize = "11px",
      #               direction = "auto"),
      #             group = "MPA (green)") %>%
      # 
      #   ### add Nature2000 polygons ----
      # addPolygons(data = MPA_med_NATURA2000,
      #             color = "orange",
      #             stroke = T,
      #             weight = 1,
      #             label = mylabels_Nat2000,
      #             labelOptions = labelOptions(
      #               style = list("font-weight" = "normal", 
      #                            padding = "3px 8px"),
      #               textsize = "11px",
      #               direction = "auto"),
      #             group = "Nature 2000 (orange)") %>%
      # 
      #   ### add ProposedNature2000 polygons ----
      # addPolygons(data = MPA_med_pNATURA2000,
      #             color = "grey",
      #             stroke = T,
      #             weight = 1,
      #             label = mylabels_pNat2000,
      #             labelOptions = labelOptions(
      #               style = list("font-weight" = "normal", padding = "3px 8px"),
      #               textsize = "11px",
      #               direction = "auto"),
      #             group = "Proposed Nature 2000 (grey)") %>%
      
      #   ### add Layers control ----
      # addLayersControl(
      #   overlayGroups = c(
      #     "MPA (green)",
      #     "Nature 2000 (orange)",
      #     "Proposed Nature 2000 (grey)"
      #   ),
      #   position = "bottomright",
      #   options = layersControlOptions(collapsed = FALSE)
      # )
      
    })
  
  ## Selection ----
  
  output$selection_1 <- output$selection_2 <-
    
    renderPrint({
      HTML(paste0("Selection: ", "<b>", 
                  input$area, 
                  ", ", input$site, 
                  ", ", input$site_type,
                  ", ", input$country,
                  "</b>"))
    })
  
  
  ## create a selected dataset ----

  data_selected <- reactive({

  data_selected <-

      data %>%
      filter(area %in% input$area |
               site %in% input$site |
               site_type %in% input$site_type |
               country %in% input$country )

    validate(need(nrow(data_selected)!=0,
    "There are no matches in the dataset. Try removing one or more filters."))

    data_selected

  })
  
  
  ## create a table of filtered data ----
  
  output$table01 <- renderReactable({
    
    req(data_selected)
    
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
  
  
  ## download the filtered data ----
  output$download_filtered <- downloadHandler(
    
    filename = "MH-shiny-datatable-filtered.csv",
    
    content = function(file) {
      
      s <- data_selected() %>% 
        distinct(doi, .keep_all = T)
      
      write.csv(s[c(1:4, 6)], file)
    }
  )
  
  ## graph-01 ----

  output$graph_01 <- renderPlot({

    req(input$var1)
    
    data_selected() %>%
      ggplot(aes_string(x = input$var1)) +
      geom_bar(aes_string(x = input$var1, fill = input$var1),
                     color = "black") +
      labs(y = "# of observations") +
      guides(
        fill = guide_legend(
          ncol = 3,
          title.position = "top",
          title.theme = element_text(face = "bold",
                                     size = 11)
        )
      ) +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size = 11))

  })
  
  ## graph-02 ----

  output$graph_02 <- renderPlot({

    req(input$var2)
    
    data_selected() %>%
      ggplot(aes_string(x = input$var2)) +
      geom_bar(aes_string(x = input$var2, fill = input$var2),
               color = "black") +
      labs(y = "# of observations") +
      guides(
        fill = guide_legend(
          ncol = 3,
          title.position = "top",
          title.theme = element_text(face = "bold",
                                     size = 11)
        )
      ) +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size = 11))


  })
  
  ## create the tidy_words dataset ----
  
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
  
  ## wordcloud ----
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <-
    renderPlot({
      tidy_words() %>%
        with(
          wordcloud_rep(
            word,
            n,
            min.freq = input$cloud_min_freq,
            max.words = input$max,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2")
          )
        )
    })
  
  
  ## words frequency ----
  
  output$frequencies <- renderPlot({
      
    tidy_words() %>%
        filter(n > input$words_min_freq) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = n, label = n)) +
        geom_bar(stat = "identity") +
        geom_label(aes(fill = n), colour = "white", fontface = "bold") +
        scale_fill_gradientn(colours = brewer.pal(8, "Accent")) +
        coord_flip() +
        labs(title = "Words Frequency",
             x = "", y = "N. of Occurrence") +
        theme_bw() +
        theme(legend.position = "none")
    })
  
  ## observeEvent ----
  
  ### input$area ----
  
  observeEvent(input$area, {
    
    if (input$area != "")
    
      {
      colorData_area <- data[data$area %in% input$area, ]
      pal_area <- colorFactor("plasma", levels = colorData_area)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_area(colorData_area),
          popup = ~ paste0(
            "<b>Country: </b>", country, "<br>",
            "<b>Name: </b>", site, "<br>",
            "<b>Range depth: </b>", min(avg_depth), "-", max(data$avg_depth), 
            " m", "<br>"), 
          popupOptions = popupOptions(closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  ### input$site ----
  
  observeEvent(input$site, {
    if (input$site != "")
    {
      colorData_site <- data[data$site %in% input$site, ]
      pal_site <- colorFactor("magma", levels = colorData_site)
      
      leafletProxy("map", session) %>%
        addCircleMarkers(
          data = data_selected(),
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_site(colorData_site),
          popup = ~ paste0(
            "<b>Country: </b>", country, "<br>",
            "<b>Name: </b>", site, "<br>",
            "<b>Range depth: </b>", min(avg_depth), "-", max(avg_depth), 
            " m", "<br>"), 
          popupOptions = popupOptions(closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  
  ### input$site_type ----
  
  observeEvent(input$site_type, {
    if (input$site_type != "")
    {
      colorData_site_type <- data[data$site_type %in% input$site_type, ]
      pal_type <-
        colorFactor("inferno", levels = colorData_site_type)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_type(colorData_site_type),
          popup = ~ paste0(
            "<b>Country: </b>", country, "<br>",
            "<b>Name: </b>", site, "<br>",
            "<b>Range depth: </b>", min(avg_depth), "-", max(data$avg_depth), 
            " m", "<br>"), 
          popupOptions = popupOptions(closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  ### input$country ----
  
  observeEvent(input$country, {
    if (input$country != "")
    {
      colorData_country <- data[data$country %in% input$country, ]
      pal_type <-
        colorFactor("inferno", levels = colorData_country)
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_selected(),
          stroke = T,
          color = "white",
          weight = 0.2,
          fillColor = ~ pal_type(colorData_country),
          popup = ~ paste0(
            "<b>Country: </b>", country, "<br>",
            "<b>Name: </b>", site, "<br>",
            "<b>Range depth: </b>", min(avg_depth), "-", max(data$avg_depth), 
            " m", "<br>"), 
          popupOptions = popupOptions(closeOnClick = TRUE),
          label = ~ as.character(site),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "11px",
            direction = "auto"
          )
        )
    }
  })
  
  ### input$int_clear ----
  
  observeEvent(input$int_clear, {
    
    updateSelectInput(session, "site", selected = "")
    updateSelectInput(session, "site_type", selected = "")
    updateSelectInput(session, "area", selected = "")
    updateSelectInput(session, "country", selected = "")

    leafletProxy("map") %>% clearMarkers()
    
  })
  
  
}


# run the application -----------------------------------------------------

shinyApp(ui, server)
