# MH-shiny: Marine Hazard Web Application

# created by Valentina Costa
# 2021

# MH-shiny was published as part of the publication: 
# Costa V, Sciutteri V, Consoli P, Manea E, Menini E, 
# Andaloro F, Romeo T, Danovaro R. 
# Volcanic-associated ecosystems of the Mediterranean Sea: 
# a Systematic Map and an Interactive Tool to support their conservation. 
# PeerJ Life & Environment
# DOI: 10.7717/peerj.15162
# 2023


# library -----------------------------------------------------------------

library(htmltools)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(shiny)
library(DT)
library(RColorBrewer)
library(reactable)
library(wordcloud)
library(ggraph)
library(shinyWidgets)

# dataset -----------------------------------------------------------------

# database-systematic map
data <- read.csv("data/database_selected.csv")


# The layers related to the National MPA, Natura 2000 sites and Proposed
# Natura 2000 sites can be visualized in the Shiny-app

# # national MPA
# MPA_med_national <-
#   st_read("data/MPAnational_5m.gpkg", quiet = T) %>%
#   st_transform('+proj=longlat +datum=WGS84')
# 
# # Natura 2000 site
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
  
  theme = bslib::bs_theme(version = 3, "simplex"),
  
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
                               width = "640px"),
                        fluidRow(
                        column(12, "Shallow Hydrothermal vent emissions in Vulcano 
                        Island (Aeolian Archipelago, Italy).")))),
               hr(),
               helpText("MH-shiny was published as part of the publication:",
                        br(),
                        "Costa V, Sciutteri V, Consoli P, Manea E, Menini E, 
                        Andaloro F, Romeo T, Danovaro R. 2023.",
                        br(),
                        "Volcanic-associated ecosystems of the Mediterranean Sea: 
                        a Systematic Map and an Interactive Tool to support their 
                        conservation.",
                        br(),
                        "PeerJ 11:e15162 DOI: 10.7717/peerj.15162",
                        br(),
                        "DOI: 10.5281/zenodo.7537047",
                        align = "center")

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
          width = 320,
          height = "auto",
          
          h4(HTML("<b> Volcanic-Associated Ecosystems </b>")),
          hr(),
        
          selectizeGroupUI(
            id = "my-filters",
            inline = FALSE,
            params = list(
              country = list(inputId = "country", 
                             title = "Select a Country", placeholder = 'select'),
              area = list(inputId = "area", 
                             title = "Select a Region", placeholder = 'select'),
              site = list(inputId = "site", 
                               title = "Select a Site", placeholder = 'select'),
              site_type = list(inputId = "site_type", 
                              title = "Select the Site Type", placeholder = 'select')
            ), 
            btn_label = "Reset all filters"
          ),
          
          actionButton("int_clear", "Clear map")
        )
      )
    ),
    
    ## tabpanel Data explorer ----
    tabPanel(
      "Data Explorer",
      div(
        class = "",
        
        hr(),
        fluidRow(
          column(3, htmlOutput("country_1"), 
                 htmlOutput("area_1"), 
                 htmlOutput("site_name_1"),
                 htmlOutput("site_type_1")),
          column(3, selectInput("var1", "Graph 1: Variable to visualize", 
                                choices = c("", names(data)[c(3, 4, 10:12, 15:25)]),
                                selected = "")),
          column(3, selectInput("var2", "Graph 2: Variable to visualize:", 
                                choices = c("", names(data)[c(3, 4, 10:12, 15:25)]),
                                selected = ""))
        ),
        hr(),
        h3("List of scientific items"),
        fluidRow(
          column(9, reactableOutput("table01", height = "auto"),
                 downloadButton("download_filtered", 
                                label = "Download the selected data"), 
                 downloadButton("SaveGraph01", 
                         label = "Download Graph 01"),
                 downloadButton("SaveGraph02", 
                                label = "Download Graph 02")),
          column(3, shinycssloaders::withSpinner(plotOutput("graph_01", height = "45vh")),
                 shinycssloaders::withSpinner(plotOutput("graph_02", height = "45vh")))
        )
        
      )
    ),
    
    ## tabpanel Keywords analysis ----
    tabPanel(
      "Keywords analysis",
      div(
        class = "",
        
        hr(),
        fluidRow(
          column(3, htmlOutput("country_2")), 
          column(3, htmlOutput("area_2")), 
          column(3, htmlOutput("site_name_2")),
          column(3, htmlOutput("site_type_2"))
        ),
        hr(),

        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            selectInput(
              "selection",
              "Select:",
              choices = c("author_keywords", "index_keywords"),
              selected = "author_keywords",
              multiple = FALSE
            ),
            hr(),
            h4("Wordcloud"), 
            numericInput(
              "cloud_min_freq",
              "Minimum Frequency:",
              value = 1
            ),
            numericInput(
              "max",
              "Maximum Number of Words:",
              value = 150
            ),
            hr(),
            h4("Words frequency"),
            numericInput("words_min_freq", 
                         "Minimum frequency:",
                         value = 1),
            hr(),
            h4("Network analysis"),
            numericInput("net_min", 
                         "Minimum frequency:",
                         value = 0),
            hr(),
            actionButton("int_clear_2", "Clear selection"),
            width = 3
          ),
          
          # Show Word Cloud
          mainPanel(fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("cloud"))),
            column(6, shinycssloaders::withSpinner(plotOutput("frequencies")))
          ),
          fluidRow(column(
            12,
            shinycssloaders::withSpinner(plotOutput("network", height = "60vh"))
          )))
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
      # 
      # #   ### add MPA polygons ----
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
      # #   ### add Nature2000 polygons ----
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
      # #   ### add ProposedNature2000 polygons ----
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
      # 
      # #   ### add Layers control ----
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
  
  # ## Selection ----
 
  output$country_1 <- output$country_2 <- 

    renderPrint({
      HTML(paste(
        "<b>", "Country: ",
        "</b>",
        input[["my-filters-country"]]
      ))
    })

  
  output$area_1 <- output$area_2 <- 

    renderPrint({
      HTML(paste(
        "<b>", "Region: ",
        "</b>",
        input[["my-filters-area"]]
        # input$area
      ))
    })

  output$site_name_1 <- output$site_name_2 <- 

    renderPrint({
      HTML(paste(
        "<b>", "Site: ",
        "</b>",
        input[["my-filters-site"]]
      ))
    })

  output$site_type_1 <- output$site_type_2 <- 

    renderPrint({
      HTML(paste(
        "<b>", "Site Type: ",
        "</b>",
        input[["my-filters-site_type"]]
      ))
    })

  
  ## create the filtered dataset ----

  data_selected <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data,
    vars = c("country", "area", "site", "site_type"),
    inline = FALSE
  )
  
  
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
        link = colDef(name = "Link", html = TRUE,
                      cell = function(value, index) {
                        sprintf('<a href="%s" target="_blank">%s</a>', 
                                table_selected$link[index], value)
                      })
      ),
      height = 700,
      filterable = TRUE,
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

  ## download the Graph 01 ----
  output$SaveGraph01 <- downloadHandler(
    filename = function(file) {
      "graph_01.png"
    },
    content = function(file) {
      ggsave(file, plot = graph_01(), device = "png")
    }
  )

  ## download the Graph 02 ----
  output$SaveGraph02 <- downloadHandler(
    filename = function(file) {
      "graph_02.png"
    },
    content = function(file) {
      ggsave(file, plot = graph_02(), device = "png")
    }
  )
    
  ## render graph-01 ----

  graph_01 <- reactive({

    req(input$var1)
    req(data_selected)
    
    data_selected() %>%
      filter(data_selected()[input$var1] != "") %>%
      ggplot() +
    geom_bar(aes_string(
      x = input$var1,
      group = input$var1,
      fill = input$var1),
      color = "black") +
      labs(y = "# of observations") +
      theme_bw() +
      theme(legend.position = "none",
            text = element_text(size = 12),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 35,
                                       vjust = 1,
                                       hjust = 1))

   
  })
  
  output$graph_01 <- renderPlot({
    graph_01()  
  })
  
  ## render graph-02 ----

  graph_02 <- reactive({

    req(input$var2)
    req(data_selected)
    
    data_selected() %>%
      filter(data_selected()[input$var2] != "") %>%
      ggplot() +
      geom_bar(aes_string(
        x = input$var2,
        group = input$var2,
        fill = input$var2),
        color = "black") +
      labs(y = "# of observations") +
      theme_bw() +
      theme(legend.position = "none",
            text = element_text(size = 12),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 35,
                                       vjust = 1,
                                       hjust = 1))
    
  })
  
  output$graph_02 <- renderPlot({
    graph_02()  
  })
  
  ## create the tidy_words dataset ----
  
  tidy_words <- reactive({
    
    if (input$selection == "author_keywords") {
    
    tidy_words <-
      data_selected() %>%
      distinct(doi, .keep_all = T) %>%
      tidytext::unnest_tokens(
        output = word,
        input = author_keywords,
        token = "regex",
        pattern = ";"
      ) %>%
      filter(!is.na(word)) %>%
      mutate(word = str_squish(word)) %>%
      count(word, sort = T)
    
    }
    
    if (input$selection == "index_keywords") {
      
      tidy_words <-
        data_selected() %>%
        distinct(doi, .keep_all = T) %>%
        tidytext::unnest_tokens(
          output = word,
          input = index_keywords,
          token = "regex",
          pattern = ";"
        ) %>%
        filter(!is.na(word)) %>%
        mutate(word = str_squish(word)) %>%
        count(word, sort = T)
      
    }
    
    tidy_words
    
  })
  

  ## wordcloud ----
  
  wordcloud_rep <- repeatable(wordcloud)
  

  output$cloud <- renderPlot({
      
    layout(matrix(c(1, 2), nrow=2), heights=c(0.3, 4))
    par(mar = rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, cex = 1.5, "Wordcloud")

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
  
  
  ## keywords co-occurrence ----
  
  keywords_cooccurences <-  reactive({
    
    req(data_selected)
    
    keywords_cooccurences <- 
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
      widyr::pairwise_count(item = word, 
                            feature = doi, 
                            sort = TRUE)
    
    })
    
    
  ## graph of network co-occurrence ----
  
  output$network <- renderPlot({
    
    keywords_cooccurences() %>% 
      filter(n > input$net_min) %>%
      igraph::graph_from_data_frame() %>%
      ggraph(layout = 'fr') +
      geom_edge_link(aes(edge_alpha = n, edge_color = n, edge_width = n)) +
      geom_node_point(color = "darkslategray4", alpha = 0.5 , size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      scale_edge_color_viridis(name ="N. of co-occurrence", option = "A", guide = "none") +
      scale_edge_width_continuous(range = c(1, 3), guide = "none") +
      scale_edge_alpha(guide = "none") +
      theme_void() +
      theme(legend.position = "bottom")
      
      })


  
  ## observeEvent ----
  
  observeEvent(data_selected(), {
    
    leafletProxy("map") %>%
      clearMarkers() %>%   
      addCircleMarkers(
        data = data_selected(),
        stroke = T,
        color = "white",
        weight = 0.2,
        fillColor = "blue",
        popup = ~ paste(
          "<h5>", site, "</h5>",
          "<b>Country: </b>", country, "<br>",
          "<b>Region: </b>", area, "<br>",
          "<b>Site Type: </b>", site_type, "<br>",
          "<b>Depth: </b>", avg_depth, " m", "<br>"), 
        popupOptions = popupOptions(closeOnClick = TRUE),
        label = ~ as.character(site),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "11px",
          direction = "auto"
        )
      )
  }
  )
  
  
  
  ### input$int_clear ----
  
  observeEvent(
    input$int_clear & input$int_clear_2, {
    
    updateSelectizeInput(session, inputId = "my-filters",
                           selected = "")
      
    leafletProxy("map") %>% clearMarkers()
    
  })
  
  
}


# run the application -----------------------------------------------------

shinyApp(ui, server)

