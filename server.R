library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(cluster)

allzips <- test

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$households, breaks = 20)$breaks

  output$hist <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

#    hist(zipsInBounds()$households)
 #   hist(zipsInBounds()$households, freq=FALSE, xlab=”Households”, main=”Distribution of Houses”, col=”lightgreen”, xlim=c(15,35),  ylim=c(0, .20))
 #   curve(dnorm(zipsInBounds()$households, mean=mean(allzips$households), sd=sd(allzips$households)), add=TRUE, col=”darkblue”, lwd=2)

    hist(zipsInBounds()$households,
      breaks = centileBreaks,
      main = "Households (visible zips)",
      xlab = "Percentile",
      xlim = range(allzips$households),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterTIVexp_count <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(TIV ~ exp_count, data = zipsInBounds(), xlim = range(allzips$TIV), ylim = range(allzips$exp_count)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

      colorData <- zipdata[[colorBy]]
      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)


      if (sizeBy == "exp_count")
      {
        # Radius is treated specially in the "superzip" case.
        radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 500000
      }
      else if (sizeBy == "households")
      {
        radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 50000
      }
      else
      {
        #radius <- ((zipdata[[sizeBy]]*10000) / max(zipdata[[sizeBy]])) * 50000
        radius <- (abs(mean(zipdata[[sizeBy]], na.rm=TRUE) - zipdata[[sizeBy]]) / sd(zipdata[[sizeBy]],na.rm=TRUE)) * 10000

        #radius <- 50000
        #radius <- ~ifelse((zipdata[[sizeBy]]  / max(zipdata[[sizeBy]])) > .001, 6, 10)
      }


    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
     # clusterOptions = markerClusterOptions()
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zip,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zip == zipcode,]
    content <- as.character(tagList(
      tags$h4(HTML(sprintf("%s, %s %s",
              selectedZip$city, selectedZip$state, selectedZip$zip
      ))),
      tags$strong("Expsosures:", as.integer(selectedZip$exp_count)),tags$br(),
      sprintf("Total Insured Value: %s", dollar(selectedZip$TIV)), tags$br(),
      sprintf("AAL: %s", dollar(selectedZip$AAL)), tags$br(),
      sprintf("CTE: %s", dollar(selectedZip$CTE_1))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
})
