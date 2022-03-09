

library(tidyverse)

data <- 
  read.csv("data/database_selected.csv")


graphs <- 
  data %>%
  ggplot() +
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
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 30))



# ## create a selected dataset ----
# 
data_selected <- reactive({
  
  req(input$area)
  req(input$site)
  req(input$site_type)
  req(input$country)
  
  data_selected <-
    data %>%
    filter(
      if (input$area != "") {
        area %in% input$area
      } else { TRUE }) %>%
    filter(
      if (input$site != "") {
        site %in% input$site
      } else { TRUE }) %>% 
    filter(
      if (input$site_type != "") {
        site %in% input$site_type
      } else { TRUE }) %>% 
    filter(
      if (input$country != "") {
        site %in% input$country
      } else { TRUE })
  
  # validate(need(nrow(data_selected)!=0, "There are no matches in the dataset.
  #               Try removing one or more filters."))
  
  data_selected
  
})



data_selected <- reactive({
  
  input <- vector("list", 5) 
  
  input[1] = 
    if (input$area != "") {
    input[1] = input$area 
  } else {
    input[1] = list(NULL)
  }
  
  input[2] = 
    if (input$site != "") {
      input[2] = input$site 
    } else {
      input[2] = list(NULL)
    }
  
  input[3] = 
    if (input$site_type != "") {
      input[3] = input$site_type 
    } else {
      input[3] = list(NULL)
    }
  
  input[4] = 
    if (input$country != "") {
      input[4] = input$country 
    } else {
      input[4] = list(NULL)
    }
  
  input[5] = 
    if (input$int_depth != "") {
      input[5] = input$int_depth 
    } else {
      input[5] = list(NULL)
    }
  
  data_selected <-
    data %>% 
    filter(
      if(!is.null(input[[1]])) 
        area %in% input[[1]] 
      else TRUE) %>% 
    filter(
      if(!is.null(input[[2]])) 
        site %in% input[[2]] 
      else TRUE) %>% 
    filter(
      if(!is.null(input[[3]])) 
        site_type %in% input[[3]] 
      else TRUE) %>% 
    filter(
      if(!is.null(input[[4]])) 
        country %in% input[[4]] 
      else TRUE) 
  
  validate(need(nrow(data_selected)!=0, 
                "There are no matches in the dataset. Try removing one or more filters."))
  
  data_selected
  
  
})

a = "Aeolian Arc"
b = ""


data_2 <- 
  data %>%
  filter(
    if (a != "") {
      area %in% a
    } else { TRUE }) %>% 
  filter(
    if (b != "") {
      site %in% b
    } else { TRUE })  





input <- vector("list", 5) 

if (b != "") {
  input[2] = b 
  } else {
  input[2] = list(NULL)
}


input[1] = "Aeolian Arc"
input[2] = list(NULL)
input[3] = list(NULL)
input[4] = list(NULL)
input[5] = list(NULL)


data_2 <-
  data %>% 
  filter(
    if(!is.null(input[[1]])) 
      area %in% input[[1]] 
    else TRUE) %>% 
  filter(
    if(!is.null(input[[2]])) 
      site %in% input[[2]] 
    else TRUE) %>% 
  filter(
    if(!is.null(input[[3]])) 
      site_type %in% input[[3]] 
    else TRUE) %>% 
  filter(
    if(!is.null(input[[4]])) 
      country %in% input[[4]] 
    else TRUE) 




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


filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else if (is.character(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

data_2 <-
  data %>% 
  if (area != "") {
    filter(area %in% area)
  } else {TRUE 
  } &
  if (site != "") {
    filter(data, site %in% input$site)
  } else {TRUE
  } 

filter_var(data$avg_depth, c(1, 100)) 

data_2 <- data[selected, ]
data_2

input$area = "Aeolian Arc"



data_2 <-
  data %>% 
  filter(
    if (input$area != "") {
  area %in% input$area
} else
  data) %>%
  filter(
    if (input$site != "") {
      site %in% input$site
    })
  


tidy_words <-
  data %>%
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


keywords_cooccurences <- 
  data %>%
  distinct(doi, .keep_all = T) %>% 
  tidytext::unnest_tokens(output = word,
                input = author_keywords,
                token = "regex",
                pattern=";") %>%
  filter(!is.na(word))  %>% 
  widyr::pairwise_count(item = word, 
                 feature = label, sort = TRUE)

  