# Module UI

#' @title   mod_temporal_ui and mod_temporal_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_temporal
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_temporal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    style = "background-color: black;",
    fluidRow(
      column(
        12,
        plotlyOutput(
          ns("bar"),
          height = "250px"
        ),
        absolutePanel(
          top = 10,
          left = 20,
          selectInput(
            ns("barselect"),
            "Select Column to be displayed",
            c(
              "basisOfRecord",
              "kingdom",
              "phylum",
              "order",
              "family",
              "genus",
              "species"
            ),
            selected = "basisOfRecord"
          )
        )
      )
    ),
    fluidRow(
      column(
        6,
        style = "padding-right:0px;",
        plotOutput(
          ns("roseplot")
        )
      ),
      column(
        6,
        style = "padding-left:0px;",
        plotlyOutput(
          ns("pie")
        ),
        absolutePanel(
          top = 10,
          left = 20,
          selectInput(
            ns("pieselect"),
            "Select Column to be displayed",
            c(
              "basisOfRecord",
              "kingdom",
              "phylum",
              "order",
              "family",
              "genus",
              "species"
            ),
            selected = "basisOfRecord"
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_temporal
#' @export
#' @keywords internal

mod_temporal_server <-
  function(input, output, session, dataTemporal) {
    ns <- session$ns
    formattedData <- reactive({
      dataForBar <- format_bdvis(dataTemporal(), source = 'rgbif')
      
      
      names(dataForBar) = gsub("\\.", "_", names(dataForBar))
      if ("Date_collected" %in% colnames(dataForBar)) {
        if (length(which(!is.na(dataForBar$Date_collected))) == 0) {
          stop("Date_collected has no data")
        }
        dayofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%d"
          )
        )
        weekofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%U"
          )
        )
        monthofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%m"
          )
        )
        Year_ = as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%Y"
          )
        )
        dataForBar <-
          cbind(dataForBar[c("basisOfRecord",
                             "kingdom",
                             "phylum",
                             "order",
                             "family",
                             "genus",
                             "species")], dayofYear, weekofYear, monthofYear, Year_)
        
      } else {
        stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
      }
      return(dataForBar)
    })
    
    #Plot bar graph
    output$bar <- renderPlotly({
      dataForBar <-
        arrange(formattedData(), as.numeric(formattedData()$Year_))
      dataForBar <- dataForBar[c(input$barselect, "Year_")]
      
      dataForBar <-
        data.frame(table(dataForBar)) %>% 
        rename(group = input$barselect,
               variable = Year_,
               value = Freq)
        plot_ly(
          dataForBar,
          source = "barselected",
          x = ~ variable,
          y = ~ value,
          color = ~ group
          ) %>%
          add_bars() %>%
          layout(
          barmode = 'stack',
          paper_bgcolor = '#000000',
          plot_bgcolor = '#000000',
          xaxis = list(color = '#ffffff'),
          yaxis = list(color = '#ffffff'),
          leagend = list(color = '#ffffff')
        )
    })
    
    observe({
      select <- event_data("plotly_click", source = "barselected")
      if (is.null(select)) {
        output$pie <- renderPlotly({
          if (input$pieselect == "kingdom") {
            label <- ~ kingdom
          } else if (input$pieselect == "phylum") {
            label <- ~ phylum
          } else if (input$pieselect == "family") {
            label <- ~ family
          } else if (input$pieselect == "genus") {
            label <- ~ genus
          } else if (input$pieselect == "species") {
            label <- ~ species
          } else if (input$pieselect == "order") {
            label <- ~ order
          } else {
            label <- ~ basisOfRecord
          }
          if (!nrow(dataTemporal()[-which(dataTemporal()[, input$pieselect] == ""), ]) == 0) {
            dataa <-
              dataTemporal()[-which(dataTemporal()[, input$pieselect] == ""), ]
          } else {
            dataa <- dataTemporal()
          }
          
          plot_ly(
            data = na.omit(dataa[c("basisOfRecord",
                                   "kingdom",
                                   "phylum",
                                   "order",
                                   "family",
                                   "genus",
                                   "species")]),
            labels = label,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text'
          ) %>% 
            layout(
            showlegend = FALSE,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
        })
      } else {
        #create new dataset based on where user clicked on bar graph
        newData <-
          dataTemporal() %>%
          filter(year %in% as.numeric(select))
        output$pie <- renderPlotly({
          if (input$pieselect == "kingdom") {
            label <- ~ kingdom
          } else if (input$pieselect == "phylum") {
            label <- ~ phylum
          } else if (input$pieselect == "family") {
            label <- ~ family
          } else if (input$pieselect == "genus") {
            label <- ~ genus
          } else if (input$pieselect == "species") {
            label <- ~ species
          } else if (input$pieselect == "order") {
            label <- ~ order
          } else {
            label <- ~ basisOfRecord
          }
          #Remove blank data from column(Blank! Not NA)
          if (!nrow(newData[-which(newData[, input$pieselect] == ""), ]) == 0) {
            newData <- newData[-which(newData[, input$pieselect] == ""), ]
          }
          
          plot_ly(
            data = na.omit(newData[c("basisOfRecord",
                                     "kingdom",
                                     "phylum",
                                     "order",
                                     "family",
                                     "genus",
                                     "species")]),
            labels = label,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text'
          ) %>%
            layout(
            showlegend = FALSE,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
        })
      }
      
    })
    
    #redraw roseplot when any change made in barplot
    observe({
      select <- event_data("plotly_click", source = "barselected")
      if (is.null(select)) {
        output$roseplot <- renderPlot({
          dataForRose <-
            arrange(formattedData(),
                    as.numeric(formattedData()$monthofYear))
          dataForRose <-
            dataForRose[c("basisOfRecord", "monthofYear")]
          if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]) == 0) {
            dataForRose <-
              dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]
          }
          dataForRose <-
            data.frame(table(dataForRose)) %>% 
            rename(group = basisOfRecord,
                   variable = monthofYear,
                   value = Freq
            )
            ggplot(data = dataForRose,
                   aes(
                    x = variable,
                    y = value,
                    fill = group
                  )
            ) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("") + theme(
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#000000", colour = "#000000"),
              plot.background = element_rect(fill = "#000000"),
              axis.text.x = element_text(color = "#ffffff", size = 17),
              axis.text.y = element_text(color = "#ffffff", size = 17),
              # Change legend
              legend.background = element_rect(fill = "black", color = NA),
              legend.key = element_rect(color = "gray", fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white")
              
            )
        },  bg = "transparent")
      } else {
        output$roseplot <- renderPlot({
          dataForRose <-
            formattedData() %>% 
            filter(Year_ %in% as.numeric(select))
            dataForRose <-
              arrange(dataForRose, as.numeric(dataForRose$monthofYear))
            dataForRose <-
              dataForRose[c("basisOfRecord", "monthofYear")]
            if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]) == 0) {
              dataForRose <-
                dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]
            }
            dataForRose <-
              data.frame(table(dataForRose)) %>%
              rename(group = basisOfRecord,
                     variable = monthofYear,
                     value = Freq
              )
            ggplot(
              data = dataForRose,
              aes(
                x = variable,
                y = value,
                fill = group
              )
            ) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("") + theme(
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#000000", colour = "#000000"),
              plot.background = element_rect(fill = "#000000"),
              axis.text.x = element_text(color = "#ffffff", size = 17),
              axis.text.y = element_text(color = "#ffffff", size = 17),
              # Change legend
              legend.background = element_rect(fill = "black", color = NA),
              legend.key = element_rect(color = "gray", fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white")
              
            )
        })
      }
    })
  }

## To be copied in the UI
# mod_temporal_ui("temporal_ui_1")

## To be copied in the server
# callModule(mod_temporal_server, "temporal_ui_1")
