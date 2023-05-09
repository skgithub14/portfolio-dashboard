library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(zoo)
library(tidyverse)
library(sf)
library(spData)
library(tmap)
library(tmaptools)
library(echarts4r)

# interactive map
tmap_mode("view")

# load data
loc.dat <- readRDS("./data/locations_complete.rds")
kpi.dat <- readRDS("./data/kpis.rds")

# UI
ui <- fixedPage(
  
  # load stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
  ),
  
  # window title
  titlePanel(title = NULL, windowTitle = "K&S Franchise Sales & Marketing Dashboard"),
  
  # add space
  fluidRow(br()),
  
  # page wellPanel
  wellPanel(class = "data-box", style = "padding-bottom: 5px",
  
    #### Header ####
    fluidRow(
      
      # TITLE
      column(width = 5,
        div(img(src = "place-setting.png", height = "60px"), style = "display: inline-block; transform: translate(50%, -15%);"),
        h1("K&S Franchise", class = "page-title"),
      ),
      
      ## conditionally set inputs widgets in the top right corner
      # month/year selector for main dashboard
      conditionalPanel("input.Tabs == 'Dashboard'",
        column(width = 7,
          div(style = "display: inline-block; vertical-align:top; margin-top:25px", 
            h4("Month")
          ),
          div(style = "display: inline-block; margin-right:25px; margin-top:25px", 
            selectInput("selMonth", NULL, 
                        choices = unique(month(kpi.dat$date, label = TRUE, abbr = FALSE)),
                        selected = "January",
                        width = "100px")
          ),
          div(style = "display: inline-block; vertical-align:top; margin-top:25px",
            h4("Year")
          ),
          div(style = "display: inline-block; margin-right:25px; margin-top:25px", 
            selectInput("selYear", NULL, 
                        choices = unique(year(kpi.dat$date)),
                        selected = 2023,
                        width = "100px"),
          ),
          div(style = "display: inline-block; vertical-align:top; margin-top:25px",
            h4("Compare to")
          ),
          div(style = "display: inline-block; margin-top:25px", 
            selectInput("selCompareTo", NULL, 
                        choices = c("Previous Year", "Previous Month"),
                        width = "150px")
          )
        )
      ),
      
      # location selector
      conditionalPanel("input.Tabs == 'Locations'",
        column(width = 5, offset = 2,
          div(style = "display: inline-block; vertical-align:top; margin-top:25px", 
            h4("Location")
          ),
          div(style = "display: inline-block; margin-right:25px; margin-top:25px", 
            selectInput("selLocation", NULL, 
                        choices = unique(kpi.dat$city),
                        width = "200px"),
          )
        )
      )
    ),
    hr(style = "margin-top:0px"),
    
    tabsetPanel(id = "Tabs",
                
      #### Dashboard tab ####
      tabPanel(title = "Dashboard", icon = icon('dashboard'),
        br(),
        fluidRow(
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("Revenue", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dRev"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dRevDiff"), class = "box-change-data")
              ),
              column(width = 2,
                div(img(src = "money.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            ),
          )),
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("Revenue per Seat", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dRevPS"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dRevPSDiff"), class = "box-change-data")
              ),
              column(width = 2,
                div(img(src = "table.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            )
          )),
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("% Strategic Category", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dStrat"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dStratDiff"), class = "box-change-data")
              ),
              column(width = 2,
                     div(img(src = "targeting.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            )
          ))
        ),
        fluidRow(
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("Transactions", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dTrans"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dTransDiff"), class = "box-change-data")
              ),
              column(width = 2,
                     div(img(src = "cash-register.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            )
          )),
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("Ave. Check Size", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dAveCheck"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dAveCheckDiff"), class = "box-change-data")
              ),
              column(width = 2,
                     div(img(src = "receipt.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            )
          )),
          column(width = 4, wellPanel(class = "data-box",
            fluidRow(
              column(width = 10,
                h4("% Online Orders", class = "box-title"), 
                hr(class = "title-hr"),
                tagAppendAttributes(textOutput("dOnline"), class = "box-data"),
                p("  ", style = "display: inline-block"),
                tagAppendAttributes(textOutput("dOnlineDiff"), class = "box-change-data")
              ),
              column(width = 2,
                     div(img(src = "online-shop.png", height = "35px"), style = "transform: translate(-35%, 25%)"),
              )
            )
          ))
        ),
        
        # 2nd data row
        fluidRow(
          
          # revenue by day
          column(width = 6, 
            wellPanel(class = "data-box", style = "padding-left: 10px; padding-bottom: 0px",
              h4("Revenue by Day", class = "box-title", style = "margin-left:10px; margin-bottom: -15px"),
              hr(class = "title-hr"),
              plotlyOutput("revPlotly", height = "300px")
            ),
            
            # breakdown
            wellPanel(class = "data-box", style = "padding-left: 10px; padding-bottom: 0px",
              h4("Breakdown", class = "box-title", style = "margin-left:10px; margin-bottom: -15px"),
              hr(class = "title-hr"),
              plotlyOutput("breakdownPlotly", height = "180px")
            )
          ),
          
          # map
          column(width = 6, wellPanel(class = "data-box", style = "padding-bottom:10px",
            h4("Revenue by Location", class = "box-title", style = "margin-bottom: -15px"),
            hr(class = "title-hr", style = "margin-bottom: 10px"),
            tmapOutput("map", height = "500px"),
            p("Click a location to see details", style = "margin-bottom: 0px")
          ))
        )
      ), # end of dashboard tabPanel
      
      #### Locations tab ####
      tabPanel(title = "Locations", icon = icon('map'),
       br(),
       fluidRow(
         column(width = 6, wellPanel(class = "data-box",
           fluidRow(
             column(width = 11,
                h4("Revenue", class = "box-title"), 
                hr(class = "title-hr")
             ),
             column(width = 1,
                div(img(src = "money.png", height = "35px"), style = "transform: translate(-75%, -25%)"),
             )
           ),
           fluidRow(
              echarts4rOutput("lRev", height = "300%")
           )
         )),
         column(width = 6, wellPanel(class = "data-box",
           fluidRow(
             column(width = 11,
                    h4("% Strategic Category", class = "box-title"), 
                    hr(class = "title-hr")
             ),
             column(width = 1,
                    div(img(src = "targeting.png", height = "35px"), style = "transform: translate(-75%, -25%)"),
             )
           ),
           fluidRow(
             echarts4rOutput("lStrat", height = "300%")
           )
         ))
       ),
       fluidRow(
         column(width = 6, wellPanel(class = "data-box",
           fluidRow(
             column(width = 11,
                    h4("Transactions & Ave. Check Size", class = "box-title"), 
                    hr(class = "title-hr"),
             ),
             column(width = 1,
                    div(img(src = "cash-register.png", height = "35px"), style = "transform: translate(-75%, -25%)"),
             )
           ),
           fluidRow(
             echarts4rOutput("lTrans", height = "300%")
           )
         )),
         column(width = 6, wellPanel(class = "data-box",
           fluidRow(
             column(width = 11,
                    h4("% Online Orders", class = "box-title"), 
                    hr(class = "title-hr"),

             ),
             column(width = 1,
                    div(img(src = "online-shop.png", height = "35px"), style = "transform: translate(-75%, -25%)"),
             )
           ),
           fluidRow(
             echarts4rOutput("lOnline", height = "300%")
           )
         ))
       ),
      )
    ) # end of tabsetPanel
  )
)

# Server
server <- function(input, output, session) {
  
  #### Selectors ####
  comparisons <- reactiveValues(month = "January", year = 2022)
  observeEvent(list(input$selCompareTo, input$selMonth, input$selYear), {
    if(input$selCompareTo == "Previous Year"){
      comparisons$month <- input$selMonth
      comparisons$year <- as.numeric(input$selYear) - 1
    } else {
      ordered.months <- c(
        "January","February","March","April","May","June","July",
        "August","September","October","November","December"
      )
      if(input$selMonth == "January"){
        comparisons$month <- "December"
      } else {
        comparisons$month <- ordered.months[which(ordered.months == input$selMonth)-1]
      }
      comparisons$year <- input$selYear
    }
  })
  
  #### Dashboard Tab Outputs ####
  ##### Numeric Values ####
  # monthy total revenue
  output$dRev <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    scales::dollar(round(sum(
      kpi.dat$revenue[which(kpi.dat$month == input$selMonth & 
                              kpi.dat$year == input$selYear)], 
      na.rm = T), 2))
  })
  output$dRevDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(
        kpi.dat[which(kpi.dat$month == comparisons$month & 
                        kpi.dat$year == comparisons$year),]) > 0,
        "No data"))
    this.rev <- sum(
      kpi.dat$revenue[which(kpi.dat$month == input$selMonth & 
                              kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.rev <- sum(
      kpi.dat$revenue[which(kpi.dat$month == comparisons$month & 
                              kpi.dat$year == comparisons$year)],
      na.rm = T)
    perc.change <- round((this.rev - last.rev) / last.rev * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  # monthly total revenue per seat
  output$dRevPS <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    scales::dollar(round(sum(
      kpi.dat$RevPS[which(kpi.dat$month == input$selMonth & 
                            kpi.dat$year == input$selYear)],
      na.rm = T), 2))
  })
  output$dRevPSDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(
        kpi.dat[which(kpi.dat$month == comparisons$month & 
                        kpi.dat$year == comparisons$year),]) > 0,
        "No data"))
    this.rev <- sum(
      kpi.dat$RevPS[which(kpi.dat$month == input$selMonth & 
                            kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.rev <- sum(
      kpi.dat$RevPS[which(kpi.dat$month == comparisons$month & 
                            kpi.dat$year == comparisons$year)], 
      na.rm = T)
    perc.change <- round((this.rev - last.rev) / last.rev * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  # monthly mean % strategic category
  output$dStrat <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    round(mean(
      kpi.dat$perc.strategic[which(kpi.dat$month == input$selMonth & 
                                     kpi.dat$year == input$selYear)],
      na.rm = T), 2)
  })
  output$dStratDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(
        kpi.dat[which(kpi.dat$month == comparisons$month & 
                        kpi.dat$year == comparisons$year),]) > 0,
        "No data"))
    this.perc <- mean(
      kpi.dat$perc.strategic[which(kpi.dat$month == input$selMonth & 
                                     kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.perc <- mean(
      kpi.dat$perc.strategic[which(kpi.dat$month == comparisons$month & 
                                     kpi.dat$year == comparisons$year)], 
      na.rm = T)
    perc.change <- round((this.perc - last.perc) / last.perc * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  # monthly total transactions
  output$dTrans <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    scales::comma(round(sum(
      kpi.dat$transactions[which(kpi.dat$month == input$selMonth & 
                                   kpi.dat$year == input$selYear)], 
      na.rm = T), 2))
  })
  output$dTransDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(
        kpi.dat[which(kpi.dat$month == comparisons$month & 
                        kpi.dat$year == comparisons$year),]) > 0,
        "No data"))
    this.rev <- sum(
      kpi.dat$transactions[which(kpi.dat$month == input$selMonth & 
                                   kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.rev <- sum(
      kpi.dat$transactions[which(kpi.dat$month == comparisons$month & 
                                   kpi.dat$year == comparisons$year)], 
      na.rm = T)
    perc.change <- round((this.rev - last.rev) / last.rev * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  # monthly ave check
  output$dAveCheck <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    scales::dollar(round(mean(
      kpi.dat$ave.check[which(kpi.dat$month == input$selMonth & 
                                kpi.dat$year == input$selYear)]), 
      2))
  })
  output$dAveCheckDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(kpi.dat[which(kpi.dat$month == comparisons$month & 
                                kpi.dat$year == comparisons$year),]) > 0,
           "No data"))
    this.check <- mean(
      kpi.dat$ave.check[which(kpi.dat$month == input$selMonth & 
                                kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.check <- mean(
      kpi.dat$ave.check[which(kpi.dat$month == comparisons$month & 
                                kpi.dat$year == comparisons$year)], 
      na.rm = T)
    perc.change <- round((this.check - last.check) / last.check * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  # monthly % online orders
  output$dOnline <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    round(mean(
      kpi.dat$perc.online[which(kpi.dat$month == input$selMonth & 
                                  kpi.dat$year == input$selYear)]), 
      2)
  })
  output$dOnlineDiff <- renderText({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"),
      need(nrow(
        kpi.dat[which(kpi.dat$month == comparisons$month & 
                        kpi.dat$year == comparisons$year),]) > 0,
        "No data"))
    this.perc <- mean(
      kpi.dat$perc.online[which(kpi.dat$month == input$selMonth & 
                                  kpi.dat$year == input$selYear)], 
      na.rm = T)
    last.perc <- mean(
      kpi.dat$perc.online[which(kpi.dat$month == comparisons$month & 
                                  kpi.dat$year == comparisons$year)], 
      na.rm = T)
    perc.change <- round((this.perc - last.perc) / last.perc * 100, 1)
    sign <- ifelse(perc.change < 0, "", "+")
    perc.change <- paste0("(", sign, perc.change , "%)")
    perc.change
  })
  
  ##### Plots ####
  # revenue plot
  output$revPlotly <- renderPlotly({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    
    # pre the data
    p.dat <-
      kpi.dat %>%
      select(date, month, year, revenue) %>%
      filter(month == input$selMonth & year == input$selYear)
    p.dat.last <-
      kpi.dat %>%
      select(date, month, year, revenue) %>%
      filter(month == comparisons$month & year == comparisons$year)
    
    # make data frames the same length if months have different days
    if(nrow(p.dat) > nrow(p.dat.last)){
      for(dr in 1:(nrow(p.dat)-nrow(p.dat.last))){
        p.dat.last <- rbind(p.dat.last, p.dat.last[nrow(p.dat.last),])
        p.dat.last$revenue[nrow(p.dat.last)] <- NA
      }
    }
    if(nrow(p.dat.last) > nrow(p.dat)){
      p.dat.last <- p.dat.last[1:nrow(p.dat),]
    }
    
    # create the plot
    fig <- plot_ly(
        x = ~ p.dat$date,
        y = ~ p.dat$revenue,
        name = paste0(input$selMonth," ", input$selYear),
        type = "bar")
    
    # add comparison only if data is available
    if(nrow(kpi.dat[which(kpi.dat$month == comparisons$month & 
                          kpi.dat$year == comparisons$year),]) > 0){
      fig <- fig %>% 
        add_trace(y = ~ p.dat.last$revenue,
                  name = paste0(comparisons$month," ", comparisons$year))
    }
    fig <-
      fig %>%
      layout(yaxis = list(title = "",
                          hoverformat = "$,f"),
             xaxis = list(title = ""),
             legend = list(x = 0.5,
                           xanchor = "center",
                           orientation = "h"),
             hovermode = "x unified") %>%
      config(displayModeBar = F)
    fig
  })
  
  # revenue breakdown by profit and cost
  output$breakdownPlotly <- renderPlotly({
    shiny::validate(need(nrow(
      kpi.dat[which(kpi.dat$month == input$selMonth & 
                      kpi.dat$year == input$selYear),]) > 0,
      "No data"))
    
    # prep the data
    p.dat <-
      kpi.dat %>%
      select(date, month, year, contains("cost"), profit) %>%
      pivot_longer(cols = -c(date, month, year), values_to = "amount", names_to = "type") %>%
      filter(month == input$selMonth & year == as.numeric(input$selYear)) %>%
      mutate(type = case_when(type == "profit" ~ "Profit",
                              type == "food.cost" ~ "Food Cost",
                              type == "labor.cost" ~ "Labor Cost",
                              type == "facility.cost" ~ "Facility Cost")) %>%
      group_by(month, year, type) %>%
      summarise(sum.value = sum(amount, na.rm = T)) %>%
      ungroup() %>%
      mutate(month.year = zoo::as.yearmon(paste0(month, " ", year), "%B %Y"))
    
    # create the plot
    plot_ly(
        y = ~ p.dat$type,
        x = ~ p.dat$sum.value,
        type = "bar") %>%
      layout(xaxis = list(title = "",
                          hoverformat = "$,f"),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
  })
  
  ##### Map ####
  # revenue by location map
  output$map <- renderTmap({
    new.england <- 
      spData::us_states %>%
      filter(NAME %in% c("Maine","New Hampshire","Vermont","Massachusetts",
                         "Rhode Island","Connecticut","New York"))
    
    rev.df <-
      kpi.dat %>%
      select(city, revenue, month, year) %>%
      filter(month == input$selMonth & year == as.numeric(input$selYear)) %>%
      group_by(city) %>%
      summarise(month_rev = sum(revenue, na.rm = T)) %>%
      ungroup()
    
    last.rev.df <-
      kpi.dat %>%
      select(city, revenue, month, year) %>%
      filter(month == comparisons$month & year == comparisons$year) %>%
      group_by(city) %>%
      summarise(month_rev_last = sum(revenue, na.rm = T)) %>%
      ungroup() %>%
      select(city, month_rev_last)
    
    loc.sf <-
      loc.dat %>%
      select(city, state, long, lat, seats) %>%
      mutate(city.state = paste0(city, ", ", state)) %>%
      left_join(rev.df, by = "city")
    if(nrow(last.rev.df) > 0){
      loc.sf <-
        loc.sf %>%
        left_join(last.rev.df, by = "city") %>%
        mutate(perc.change = round((month_rev - month_rev_last) / month_rev_last * 100, 2)) %>%
        mutate(perc.change = paste0(ifelse(perc.change < 0, perc.change, paste0("+", perc.change)), "%"))
    } else {
      loc.sf <-
        loc.sf %>%
        mutate(month_rev_last = NA,
               perc.change = NA)
    }
    loc.sf <-
      loc.sf %>%
      st_as_sf(coords = c("long","lat"))
    
    map <- 
      tm_shape(new.england,
            projection = "WGS84") +
      tm_fill(alpha = 0.5,
              id = "NAME",
              interactive = FALSE) +
      tm_borders(col = "white") +
      tm_view(set.view = c(6.25)) +
      tm_shape(loc.sf,
               projection = "WGS84") +
      tm_text(
        text = "city",
        size = 1.5,
        just = "left",
        xmod = 1.5) +
      tm_bubbles(
        size = "month_rev", 
        col = "month_rev", 
        midpoint = 400000,
        palette = "Blues",
        legend.size.show = FALSE,
        title.col = "Monthly Revenue",
        legend.col.reverse = TRUE,
        id = "city.state",
        legend.format = list(fun = function(x) scales::dollar(x)),
        popup.vars = c("Revenue, this period: " = "month_rev",
                       "Revenue, last period: " = "month_rev_last",
                       "% Change: " = "perc.change"),
        popup.format = list(month_rev = list(fun = function(x) scales::dollar(x)),
                            month_rev_last = list(fun = function(x) scales::dollar(x)))) +
      tm_layout(
        frame = FALSE,
        outer.margins = c(0,0,0,0),
        inner.margins = c(0,0,0,0))
    
    map
  })
  
  #### Location Tab Outputs ####
  # revenue
  output$lRev <- renderEcharts4r({
    kpi.dat |>
      filter(city == input$selLocation) |>
      group_by(month, year) |>
      mutate(month_rev = sum(revenue, na.rm = T) / 1000) |>
      filter(day(date) == "1") |>
      ungroup() |>
      e_charts(date) |>
      e_line(month_rev) |>
      e_datazoom(type = "slider") |>
      e_legend(FALSE) |>
      e_y_axis(
        formatter = e_axis_formatter("currency", digits = 0,currency = "USD")
      ) |>
      e_axis_labels(
        y = "Revenue ($1,000)"
      )
  })
  
  # transactions and ave check size
  output$lTrans <- renderEcharts4r({
    kpi.dat |>
      filter(city == input$selLocation) |>
      group_by(month, year) |>
      mutate(month_trans = sum(transactions, na.rm = T),
             month_ave.check = mean(ave.check, na.rm = T)) |>
      filter(day(date) == "1") |>
      ungroup() |>
      e_charts(date) |>
      e_bar(month_trans, name = "Transactions") |>
      e_step(month_ave.check, name = "Ave. Check ($)", y_index = 1) |>
      e_axis_labels(
        y = "Transactions", 
      ) |>
      e_datazoom(type = "slider")
  })
  
  # % strategic
  output$lStrat <- renderEcharts4r({
    p.dat <-
      kpi.dat |>
      filter(city == input$selLocation) |>
      group_by(month, year) |>
      mutate(month_strat = mean(perc.strategic, na.rm = T) / 100) |>
      filter(day(date) == "1") |>
      ungroup() |>
      mutate(fitted = NA)
    
    # fitted line
    p.dat$fitted <- lm(month_strat ~ date, data = p.dat)$fitted.values
    
    p.dat |>
      e_charts(date) |>
      e_scatter(month_strat, name = "% Strategic", symbol_size = 5) |>
      e_line(fitted, symbol = "none", name = "Fitted Line") |>
      e_datazoom(type = "slider") |>
      e_y_axis(
        formatter = e_axis_formatter("percent"),
        min = 0,
        max = 1)
  })
  
  # % online
  output$lOnline <- renderEcharts4r({
    p.dat <-
      kpi.dat |>
      filter(city == input$selLocation) %>%
      mutate(perc.online = perc.online / 100,
             perc.drivethru = sample(seq(0.2, 0.5, 0.01), 1) * (1 - perc.online),
             perc.eatin = 1 - perc.online - perc.drivethru)
      
    p.dat1 <- data.frame(type = c("Online","Drive-thru","Eat-in"),
                         perc = c(mean(p.dat$perc.online, na.rm = T),
                                  mean(p.dat$perc.drivethru, na.rm = T),
                                  mean(p.dat$perc.eatin, na.rm = T)))
      
    p.dat1 |>
      e_charts(type) |>
      e_pie(perc, radius = c("50%","70%"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
