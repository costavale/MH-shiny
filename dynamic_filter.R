##### begin dynamic filter #####

fields <- reactive({
  c(colnames(data_internal$raw))
})

# filter_by <- function (df, ...) {
#   filter_conditions <- quos(...)
#   df %>% dplyr::filter(!!!filter_conditions)
# }

# filter on 1 column
filter1_by <- function(df, fcol1, fv1) {
  
  filter_var1 <- dplyr::quo(fcol1)
  
  df %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var1), dplyr::all_vars(. == fv1)
    )
}

# filter on 2 columns
filter2_by <- function(df, fcol1, fv1, fcol2, fv2) {
  filter_var1 <- dplyr::quo(fcol1)
  filter_var2 <- dplyr::quo(fcol2)
  
  df %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var1), dplyr::all_vars(. == fv1)
    ) %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var2), dplyr::all_vars(. == fv2)
    )
}

# filter on 3 columns
filter3_by <- function(df, fcol1, fv1, fcol2, fv2, fcol3, fv3) {
  filter_var1 <- dplyr::quo(fcol1)
  filter_var2 <- dplyr::quo(fcol2)
  filter_var3 <- dplyr::quo(fcol3)
  
  df %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var1), dplyr::all_vars(. == fv1)
    ) %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var2), dplyr::all_vars(. == fv2)
    ) %>%
    dplyr::filter_at(
      dplyr::vars(!!filter_var3), dplyr::all_vars(. == fv3)
    )
}

filtered_df <- reactive({
  
  # case when all three filters are used
  if (input$filter3req & input$filter2req) {
    
    filter3_by(
      data_internal$raw, input$filter1, input$filter1val,
      input$filter2, input$filter2val,
      input$filter3, input$filter3val
    )
  } else if (input$filter2req) {
    
    # case when two filters are used
    filter2_by(
      data_internal$raw, input$filter1, input$filter1val,
      input$filter2, input$filter2val
    )
  } else {
    # case when only one filter is used
    filter1_by(data_internal$raw, input$filter1, input$filter1val)
  }
})

# vector of picklist values for the first selected filter
choicevec1 <- reactive({
  req(data_internal$raw)
  
  if (any(class(data_internal$raw) == "sf")) {
    data_internal$raw %>%
      sf::st_drop_geometry() %>%
      dplyr::select(input$filter1) %>%
      unique()
  } else {
    data_internal$raw %>%
      dplyr::select(input$filter1) %>%
      unique()
  }
})


# select first filter column from fields vector
output$filter1eval <- renderUI({
  selectInput("filter1", "Select filter criteria 1:", choices = fields())
})
# renders the picklist for the first selected filter
output$filter1choice <- renderUI(
  selectizeInput(
    "filter1val",
    "Select filter 1 condition:",
    choices = choicevec1(),
    multiple = TRUE
  )
)
# second column chosen from all remaining fields
output$filter2eval <- renderUI({
  selectInput("filter2", "Select filter criteria 2:",
              choices = fields()[fields() != input$filter1]
  )
})
# vector of picklist values for the second selected filter
choicevec2 <- reactive({
  req(data_internal$raw)
  
  if (any(class(data_internal$raw) == "sf")) {
    filter1_by(
      sf::st_drop_geometry(data_internal$raw), input$filter1, input$filter1val
    ) %>%
      dplyr::select(input$filter2) %>%
      unique()
  } else {
    filter1_by(
      data_internal$raw, input$filter1, input$filter1val
    ) %>%
      dplyr::select(input$filter2) %>%
      unique()
  }
})
# renders picklist for filter 2
output$filter2choice <- renderUI(
  selectizeInput(
    "filter2val",
    "Select filter 2 condition:",
    choices = choicevec2(),
    multiple = TRUE
  )
)
# third column selected from remaining fields
output$filter3eval <- renderUI({
  selectInput("filter3",
              "Select filter criteria 3:",
              choices = fields()[!fields() %in% c(input$filter1, input$filter2)]
  )
})
# vector of picklist values for third selected column
choicevec3 <- reactive({
  req(data_internal$raw)
  
  if (any(class(data_internal$raw) == "sf")) {
    filter2_by(
      sf::st_drop_geometry(data_internal$raw),
      input$filter1, input$filter1val,
      input$filter2, input$filter2val
    ) %>%
      dplyr::select(input$filter3) %>%
      unique()
  } else {
    filter2_by(
      data_internal$raw, input$filter1,
      input$filter1val, input$filter2,
      input$filter2val
    ) %>%
      dplyr::select(input$filter3) %>%
      unique()
  }
})

# render picklist for filter 3
output$filter3choice <- renderUI(
  selectizeInput(
    "filter3val",
    "Select filter 3 condition:",
    choices = choicevec3(),
    multiple = TRUE
  )
)

##### end dynamic filter ####