
output$world_map <- renderLeaflet({

  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   zoomDelta = 1,
                                   zoomSnap = 0,
                                   minZoom = 2.3,
                                   maxZoom = 10,
                                   preferCanvas = TRUE)) %>%

    # leafem::addMouseCoordinates() %>%

    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'bottomright' }).addTo(this)
          }") %>%

    fitBounds(lat1 = map_initialview_data$target_zone_bound1.lat,
              lng1 = map_initialview_data$target_zone_bound1.long,
              lat2 = map_initialview_data$target_zone_bound2.lat,
              lng2 = map_initialview_data$target_zone_bound2.long) %>%

    addMapPane(name = "Minimal_layer",
               zIndex = 400) %>%

    addMapPane(name = "Satellite_layer",
               zIndex = 405) %>%

    addMapPane(name = "P50_bw_layer",
               zIndex = 410) %>%

    addMapPane(name = "P50_red_livestock_map",
               zIndex = 415) %>%

    addMapPane(name = "P50_coloured_freshwater_map",
               zIndex = 415) %>%

    addMapPane(name = "P50_coloured_marine_map",
               zIndex = 415) %>%

    addMapPane(name = "boundaries_labels_layer",
               zIndex = 420) %>%

    addMapPane(name = "PPS_livestock_layer",
               zIndex = 500) %>%

    addMapPane(name = "PPS_freshwater_layer",
               zIndex = 500) %>%

    addMapPane(name = "PPS_marine_layer",
               zIndex = 500) %>%

    addMapPane(name = "PPS_filtered_layer",
               zIndex = 515) %>%

    addScaleBar(position = "bottomleft",
                scaleBarOptions(maxWidth = 200)) %>%

    # Minimal gray base layer
    addProviderTiles(
      provider = providers$Esri.WorldShadedRelief,
      group = "Minimal",
      options = c(pathOptions(pane = "Minimal_layer"),
                  providerTileOptions(updateWhenZooming = FALSE,
                                      updateWhenIdle = FALSE))) %>%

    addProviderTiles(
      provider = providers$Esri.WorldPhysical,
      group = "Satellite",
      layerId = "satellite_layer",
      options = c(pathOptions(pane = "Satellite_layer"),
                  providerTileOptions(updateWhenZooming = FALSE,
                                      updateWhenIdle = FALSE))) %>%

    hideGroup(group = "Satellite") %>%

    # Black and white livestock hotspots map
    addTiles(
      urlTemplate = paste0("https://nicocriscuolo.github.io/P50_bw_livestock_tiles/",
                           version,
                           "/{z}/{x}/{-y}.png"),
      group = "AMR Hotspots bw",
      options = c(pathOptions(pane = "P50_bw_layer"),
                  providerTileOptions(updateWhenZooming = FALSE,
                                      updateWhenIdle = FALSE))) %>%

    hideGroup(group = "AMR Hotspots bw") %>%

    # States and regions borders and places names
    addProviderTiles(
      provider = providers$Stamen.TonerHybrid,
      options = c(pathOptions(pane = "boundaries_labels_layer"),
                  providerTileOptions(updateWhenZooming = FALSE,
                                      updateWhenIdle = FALSE))) %>%

    # Red livestock hotspots map - # TO REMOVE AFTER MOZILLA FIXED THE BUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    addTiles(
      urlTemplate = paste0("https://nicocriscuolo.github.io/P50_red_livestock_tiles/",
                           version,
                           "/{z}/{x}/{y}.png"),
      group = "AMR Hotspots",
      layerId = "P50_red_livestock_layer",
      options = c(pathOptions(pane = "P50_red_livestock_map"),
                  providerTileOptions(updateWhenZooming = FALSE,
                                      updateWhenIdle = FALSE))) %>%

    # Livestock surveys - # TO REMOVE AFTER MOZILLA FIXED THE BUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    addMarkers(
      data = resistancebank_surveys,
      lng = ~XCoord,
      lat = ~YCoord,
      icon = PPS_markers["blue"],
      group = "AMR Surveys",
      options = pathOptions(pane = "PPS_livestock_layer"),
      popup = ~paste("<div style='max-height:810px' align='justify'>",
                     "<h3>",
                     paste0(ifelse(test = grepl(pattern = ",",
                                                x = Author),
                                   yes = sub(pattern = ",.*$",
                                             replacement = "",
                                             x = Author),
                                   no = Author),
                            " et al.",
                            ifelse(test = is.na(PubDate),
                                   yes = "",
                                   no = paste0(", ", PubDate))), "<br>",
                     "</h3>",
                     "<br/>",
                     "<span style='font-size:12.7px'>",
                     "<strong>Title</strong>", ": ",
                     Title, "<br>",
                     "<br/>",
                     "<strong>Authors</strong>", ": ",
                     Authors_name, "<br>",
                     "<br/>",
                     "<strong>Journal</strong>", ": ",
                     paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                     is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                     !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                   yes = paste0(Journal),
                                   no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                     "<br/>",
                     Plot, "<br>",
                     "<br/>",
                     "<div style='line-height:13px'>",
                     "<strong>Antibiotics</strong>", ": ",
                     "</span>",
                     "<span style='color:#a8a4a4; font-size:11px'>",
                     Plot_caption,
                     "</span>",
                     "</div>",
                     "</div>",
                     sep = ""),
      label = ~as.character(paste0(Author,
                                   ifelse(test = is.na(PubDate),
                                          yes = "",
                                          no = paste0(", ", PubDate)))),
      labelOptions = labelOptions(direction = "bottom",
                                  style = list("font-size" = "12.7px")),
      clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[1]],
                                            zoomToBoundsOnClick = FALSE,
                                            removeOutsideVisibleBounds = FALSE,
                                            spiderfyDistanceMultiplier = 2,
                                            freezeAtZoom = 7)
    )

})

# 2.1 #####===== Add control layers after closing initial panel =====#####
observeEvent(input$explore_amr_map |
              input$add_your_survey1 |
              input$display_download_RESBANK_panel2 |
              input$close_initial_panel |
               any(!is.null(c(input$filterdata_ISO3,
                              input$filterdata_species,
                              input$filterdata_sample_type,
                              input$filterdata_pathogen,
                              input$filterdata_compound,
                              input$filterdata_compound_class,
                              input$filterdata_compound_agisar))), {

                                leafletProxy(mapId = "world_map") %>%
                                  addLayersControl(overlayGroups = list("AMR Surveys",
                                                                        "AMR Hotspots",
                                                                        "Satellite"),
                                                   options = layersControlOptions(collapsed = FALSE))

                              }, ignoreInit = TRUE)

# 2.2 #####===== Add and remove maps and survey when switching between livestock, freshwater, and marine layer =====#####
observe({

  if (input$switch_farming_type == "livestock") {

    leafletProxy(mapId = "world_map") %>%

      removeTiles(layerId = "P50_coloured_freshwater_layer") %>%
      removeTiles(layerId = "P50_coloured_marine_layer") %>%

      clearMarkerClusters() %>%

      # Red livestock hotspots map
      addTiles(
        urlTemplate = paste0("https://nicocriscuolo.github.io/P50_red_livestock_tiles/",
                             version,
                             "/{z}/{x}/{y}.png"),
        group = "AMR Hotspots",
        layerId = "P50_red_livestock_layer",
        options = c(pathOptions(pane = "P50_red_livestock_map"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      addMarkers(
        data = resistancebank_surveys,
        lng = ~XCoord,
        lat = ~YCoord,
        icon = PPS_markers["blue"],
        group = "AMR Surveys",
        options = pathOptions(pane = "PPS_livestock_layer"),
        popup = ~paste("<div style='max-height:810px' align='justify'>",
                       "<h3>",
                       paste0(ifelse(test = grepl(pattern = ",",
                                                  x = Author),
                                     yes = sub(pattern = ",.*$",
                                               replacement = "",
                                               x = Author),
                                     no = Author),
                              " et al.",
                              ifelse(test = is.na(PubDate),
                                     yes = "",
                                     no = paste0(", ", PubDate))), "<br>",
                       "</h3>",
                       "<br/>",
                       "<span style='font-size:12.7px'>",
                       "<strong>Title</strong>", ": ",
                       Title, "<br>",
                       "<br/>",
                       "<strong>Authors</strong>", ": ",
                       Authors_name, "<br>",
                       "<br/>",
                       "<strong>Journal</strong>", ": ",
                       paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                       is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                       !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                     yes = paste0(Journal),
                                     no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                       "<br/>",
                       Plot, "<br>",
                       "<br/>",
                       "<div style='line-height:13px'>",
                       "<strong>Antibiotics</strong>", ": ",
                       "</span>",
                       "<span style='color:#a8a4a4; font-size:11px'>",
                       Plot_caption,
                       "</span>",
                       "</div>",
                       "</div>",
                       sep = ""),
        label = ~as.character(paste0(Author,
                                     ifelse(test = is.na(PubDate),
                                            yes = "",
                                            no = paste0(", ", PubDate)))),
        labelOptions = labelOptions(direction = "bottom",
                                    style = list("font-size" = "12.7px")),
        clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[1]],
                                              zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = FALSE,
                                              spiderfyDistanceMultiplier = 2,
                                              freezeAtZoom = 7)
      )

  } else if (input$switch_farming_type == "freshwater") {

    leafletProxy(mapId = "world_map") %>%

      removeTiles(layerId = "P50_red_livestock_layer") %>%
      removeTiles(layerId = "P50_coloured_marine_layer") %>%

      clearMarkerClusters() %>%

      # Coloured freshwater P50 map
      addTiles(
        urlTemplate = "https://nicocriscuolo.github.io/P50_coloured_aquaculture_tiles/2021/{z}/{x}/{-y}.png",
        group = "AMR Hotspots",
        layerId = "P50_coloured_freshwater_layer",
        options = c(pathOptions(pane = "P50_coloured_freshwater_map"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      addMarkers(
        data = resistancebank_freshwater_surveys,
        lng = ~XCoord,
        lat = ~YCoord,
        icon = PPS_markers["blue"],
        group = "AMR Surveys",
        options = pathOptions(pane = "PPS_freshwater_layer"),
        popup = ~paste("<div style='max-height:810px' align='justify'>",
                       "<h3>",
                       paste0(ifelse(test = grepl(pattern = ",",
                                                  x = Author),
                                     yes = sub(pattern = ",.*$",
                                               replacement = "",
                                               x = Author),
                                     no = Author),
                              " et al.",
                              ifelse(test = is.na(PubDate),
                                     yes = "",
                                     no = paste0(", ", PubDate))), "<br>",
                       "</h3>",
                       "<br/>",
                       "<span style='font-size:12.7px'>",
                       "<strong>Title</strong>", ": ",
                       Title, "<br>",
                       "<br/>",
                       "<strong>Authors</strong>", ": ",
                       Authors_name, "<br>",
                       "<br/>",
                       "<strong>Journal</strong>", ": ",
                       paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                       is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                       !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                     yes = paste0(Journal),
                                     no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                       "<br/>",
                       Plot, "<br>",
                       "<br/>",
                       "<div style='line-height:13px'>",
                       "<strong>Antibiotics</strong>", ": ",
                       "</span>",
                       "<span style='color:#a8a4a4; font-size:11px'>",
                       Plot_caption,
                       "</span>",
                       "</div>",
                       "</div>",
                       sep = ""),
        label = ~as.character(paste0(Author,
                                     ifelse(test = is.na(PubDate),
                                            yes = "",
                                            no = paste0(", ", PubDate)))),
        labelOptions = labelOptions(direction = "bottom",
                                    style = list("font-size" = "12.7px")),
        clusterOptions = markerClusterOptions(iconCreateFunction = JS(marker_cluster[[1]]),
                                              zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = FALSE,
                                              spiderfyDistanceMultiplier = 2,
                                              freezeAtZoom = 7)
      )

  } else if (input$switch_farming_type == "marine") {

    leafletProxy(mapId = "world_map") %>%

      removeTiles(layerId = "P50_red_livestock_layer") %>%
      removeTiles(layerId = "P50_coloured_freshwater_layer") %>%

      clearMarkerClusters() %>%

      # Coloured marine P50 map
      addTiles(
        urlTemplate = "https://nicocriscuolo.github.io/P50_coloured_fisheries_tiles/2021/{z}/{x}/{-y}.png",
        group = "AMR Hotspots",
        layerId = "P50_coloured_marine_layer",
        options = c(pathOptions(pane = "P50_coloured_marine_map"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      #
      addMarkers(
        data = resistancebank_marine_surveys,
        lng = ~XCoord,
        lat = ~YCoord,
        icon = PPS_markers["blue"],
        group = "AMR Surveys",
        options = pathOptions(pane = "PPS_marine_layer"),
        popup = ~paste("<div style='max-height:810px' align='justify'>",
                       "<h3>",
                       paste0(ifelse(test = grepl(pattern = ",",
                                                  x = Author),
                                     yes = sub(pattern = ",.*$",
                                               replacement = "",
                                               x = Author),
                                     no = Author),
                              " et al.",
                              ifelse(test = is.na(PubDate),
                                     yes = "",
                                     no = paste0(", ", PubDate))), "<br>",
                       "</h3>",
                       "<br/>",
                       "<span style='font-size:12.7px'>",
                       "<strong>Title</strong>", ": ",
                       Title, "<br>",
                       "<br/>",
                       "<strong>Authors</strong>", ": ",
                       Authors_name, "<br>",
                       "<br/>",
                       "<strong>Journal</strong>", ": ",
                       paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                       is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                       !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                     yes = paste0(Journal),
                                     no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                       "<br/>",
                       Plot, "<br>",
                       "<br/>",
                       "<div style='line-height:13px'>",
                       "<strong>Antibiotics</strong>", ": ",
                       "</span>",
                       "<span style='color:#a8a4a4; font-size:11px'>",
                       Plot_caption,
                       "</span>",
                       "</div>",
                       "</div>",
                       sep = ""),
        label = ~as.character(paste0(Author,
                                     ifelse(test = is.na(PubDate),
                                            yes = "",
                                            no = paste0(", ", PubDate)))),
        labelOptions = labelOptions(direction = "bottom",
                                    style = list("font-size" = "12.7px")),
        clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[1]],
                                              zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = FALSE,
                                              spiderfyDistanceMultiplier = 2,
                                              freezeAtZoom = 7)
      )

  }

})

#
observe({

  if (input$switch_farming_type == "livestock" &
      any(input$world_map_groups %in% "AMR Hotspots")) {

    leafletProxy(mapId = "world_map") %>%

      removeControl(layerId = "P50_coloured_freshwater_legend") %>%
      removeControl(layerId = "P50_coloured_marine_legend") %>%

      # Coloured livestock hotspots legend
      addLegend(pal = P50_red_livestock_palette,
                values = 80:0,
                opacity = 1,
                title = "AMR [%]",
                group = "AMR Hotspots",
                layerId = "P50_red_livestock_legend",
                position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x = x,
                                                                     decreasing = TRUE)))

  } else if (input$switch_farming_type == "freshwater" &
             any(input$world_map_groups %in% "AMR Hotspots")) {

    leafletProxy(mapId = "world_map") %>%

      removeControl(layerId = "P50_red_livestock_legend") %>%
      removeControl(layerId = "P50_coloured_marine_legend") %>%

      # Coloured freshwater hotspots legend
      addLegend(pal = P50_coloured_freshwater_palette,
                values = 50:0,
                opacity = 1,
                bins = 8,
                title = "AMR [%]",
                group = "AMR Hotspots",
                layerId = "P50_coloured_freshwater_legend",
                position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x = x,
                                                                     decreasing = TRUE)))

  } else if (input$switch_farming_type == "marine" &
             any(input$world_map_groups %in% "AMR Hotspots")) {

    leafletProxy(mapId = "world_map") %>%

      removeControl(layerId = "P50_red_livestock_legend") %>%
      removeControl(layerId = "P50_coloured_freshwater_legend") %>%

      addLegend(pal = P50_coloured_marine_palette,
                values = 50:0,
                opacity = 1,
                bins = 8,
                title = "AMR [%]",
                group = "AMR Hotspots",
                layerId = "P50_coloured_marine_legend",
                position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x = x,
                                                                     decreasing = TRUE)))

  }

})

# 2.3 #####===== Display and hide the black and white P50 map when filtering data =====#####
observeEvent(input$display_filtered_data, {

  # Disable the switch between livestock, freshwater, and marine data
  disable(id = "switch_farming_type")

  hideElement(id = "filterdata_panel_inputs",
              anim = TRUE,
              animType = "slide")

  hideElement(id = "hide_filterdata_panel_inputs")

  showElement(id = "show_filterdata_panel_inputs")

  if (!is.null(input$filterdata_compound_class)) {

    resistancebank_surveys_filtered <- mutate(.data = resistancebank_surveys_filtered(),
                                              group = cut(x = resistancebank_surveys_filtered()[, unique(resistancebank$Class[resistancebank$Class_WHO == input$filterdata_compound_class])],
                                                          breaks = c(seq(from = 0,
                                                                         to = 100,
                                                                         by = 5)),
                                                          labels = c("239951",
                                                                     "57B05B",
                                                                     "6EBE60",
                                                                     "83C865",
                                                                     "97D16C",
                                                                     "ADDB77",
                                                                     "BEE382",
                                                                     "D4EE8F",
                                                                     "E4F194",
                                                                     "ECEE94",
                                                                     "F2EA94",
                                                                     "F9E895",
                                                                     "FFDA8D",
                                                                     "FDC67B",
                                                                     "FBAF6C",
                                                                     "F99A59",
                                                                     "F5864E",
                                                                     "EF7445",
                                                                     "E55C3B",
                                                                     "D93429"),
                                                          include.lowest = TRUE))

  } else {

    resistancebank_surveys_filtered <- resistancebank_surveys_filtered()

  }

  leafletProxy(mapId = "world_map") %>%

    fitBounds(lat1 = map_initialview_data$target_zone_bound1.lat,
              lng1 = map_initialview_data$target_zone_bound1.long,
              lat2 = map_initialview_data$target_zone_bound2.lat,
              lng2 = map_initialview_data$target_zone_bound2.long) %>%

    clearMarkerClusters() %>%

    hideGroup(group = "AMR Hotspots") %>%

    hideGroup(group = "AMR Surveys") %>%

    showGroup(group = "AMR Hotspots bw") %>%

    addMarkers(data = resistancebank_surveys_filtered,
               lng = ~XCoord,
               lat = ~YCoord,
               icon = {if (is.null(input$filterdata_compound_class)) {

                        PPS_markers["blue_black"]

                       } else {

                        ~PPS_markers[group]

                       }
                      },
               group = "AMR Hotspots bw",
               options = pathOptions(pane = "PPS_filtered_layer"),
               popup = ~paste("<div style='max-height:810px' align='justify'>",
                              "<h3>",
                              paste0(ifelse(test = grepl(pattern = ",",
                                                         x = Author),
                                            yes = sub(pattern = ",.*$",
                                                      replacement = "",
                                                      x = Author),
                                            no = Author),
                                     " et al.",
                                     ifelse(test = is.na(PubDate),
                                            yes = "",
                                            no = paste0(", ", PubDate))), "<br>",
                              "</h3>",
                              "<br/>",
                              "<span style='font-size:12.7px'>",
                              "<strong>Title</strong>", ": ",
                              Title, "<br>",
                              "<br/>",
                              "<strong>Authors</strong>", ": ",
                              Authors_name, "<br>",
                              "<br/>",
                              "<strong>Journal</strong>", ": ",
                              paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                              is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                              !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                            yes = paste0(Journal),
                                            no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                              "<br/>",
                              Plot, "<br>",
                              "<br/>",
                              "<div style='line-height:13px'>",
                              "<strong>Antibiotics</strong>", ": ",
                              "</span>",
                              "<span style='color:#a8a4a4; font-size:11px'>",
                              Plot_caption,
                              "</span>",
                              "</div>",
                              "</div>",
                              sep = ""),
               label = ~as.character(paste0(Author,
                                            ifelse(test = is.na(PubDate),
                                                   yes = "",
                                                   no = paste0(", ", PubDate)))),
               labelOptions = labelOptions(direction = "bottom",
                                           style = list("font-size" = "12.7px")),
               clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[3]],
                                                     zoomToBoundsOnClick = FALSE,
                                                     removeOutsideVisibleBounds = FALSE,
                                                     spiderfyDistanceMultiplier = 2,
                                                     freezeAtZoom = 7)
    ) %>%

    addLegend(pal = P50_bw_livestock_palette,
              values = 80:0,
              opacity = 1,
              title = "AMR [%]",
              group = "AMR Hotspots bw",
              layerId = "P50_bw_livestock_legend",
              position = "bottomleft",
              labFormat = labelFormat(transform = function(x) sort(x = x,
                                                                   decreasing = TRUE)))

})

# 2.3.1 ###= Conditionally add and remove the antibiotics class legend =#####
observeEvent(input$display_filtered_data, {

  if (!is.null(input$filterdata_compound_class)) {

    leafletProxy(mapId = "world_map") %>%

      addLegend(pal = class_palette_GnYlRd,
                values = 100:0,
                opacity = 1,
                title = paste0("AMR to ",
                               gsub(pattern = " ",
                                    replacement = "<br>",
                                    x = unique(resistancebank$Class[resistancebank$Class_WHO == input$filterdata_compound_class])),
                               " [%]"),
                group = "AMR Hotspots bw",
                layerId = "class_legend",
                position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

  } else {

    leafletProxy(mapId = "world_map") %>%

      removeControl(layerId = "class_legend")

  }

})

# 2.3.2 ###= Remove "dark" AMR map and markers and go back to initial AMR map =#####
observeEvent(input$close_filterdata_panel |
             input$add_your_survey2 |
             input$close_filterdata_panel_controls |
             input$display_Country_report_panel |
             input$display_about_panel |
             input$display_limitations_panel, {

                 if (any(input$world_map_groups %in% "AMR Hotspots bw")) {

                   leafletProxy(mapId = "world_map") %>%

                     hideGroup(group = "AMR Hotspots bw") %>%

                     removeControl(layerId = "P50_bw_livestock_legend") %>%

                     removeControl(layerId = "class_legend") %>%

                     addMarkers(
                       data = resistancebank_surveys,
                       lng = ~XCoord,
                       lat = ~YCoord,
                       icon = PPS_markers["blue"],
                       group = "AMR Surveys",
                       options = pathOptions(pane = "PPS_livestock_layer"),
                       popup = ~paste("<div style='max-height:810px' align='justify'>",
                                      "<h3>",
                                      paste0(ifelse(test = grepl(pattern = ",",
                                                                 x = Author),
                                                    yes = sub(pattern = ",.*$",
                                                              replacement = "",
                                                              x = Author),
                                                    no = Author),
                                             " et al.",
                                             ifelse(test = is.na(PubDate),
                                                    yes = "",
                                                    no = paste0(", ", PubDate))), "<br>",
                                      "</h3>",
                                      "<br/>",
                                      "<span style='font-size:12.7px'>",
                                      "<strong>Title</strong>", ": ",
                                      Title, "<br>",
                                      "<br/>",
                                      "<strong>Authors</strong>", ": ",
                                      Authors_name, "<br>",
                                      "<br/>",
                                      "<strong>Journal</strong>", ": ",
                                      paste0(ifelse(test = is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication." |
                                                      is.na(URL) & Journal != "Report could not be linked to a peer-reviewed publication." |
                                                      !is.na(URL) & Journal == "Report could not be linked to a peer-reviewed publication.",
                                                    yes = paste0(Journal),
                                                    no = paste0("<a href='", URL, "'", " target='_blank'>", Journal, "</a>"))), "<br>",
                                      "<br/>",
                                      Plot, "<br>",
                                      "<br/>",
                                      "<div style='line-height:13px'>",
                                      "<strong>Antibiotics</strong>", ": ",
                                      "</span>",
                                      "<span style='color:#a8a4a4; font-size:11px'>",
                                      Plot_caption,
                                      "</span>",
                                      "</div>",
                                      "</div>",
                                      sep = ""),
                       label = ~as.character(paste0(Author,
                                                    ifelse(test = is.na(PubDate),
                                                           yes = "",
                                                           no = paste0(", ", PubDate)))),
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list("font-size" = "12.7px")),
                       clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[1]],
                                                             zoomToBoundsOnClick = FALSE,
                                                             removeOutsideVisibleBounds = FALSE,
                                                             spiderfyDistanceMultiplier = 2,
                                                             freezeAtZoom = 7)
                     ) %>%

                     showGroup(group = "AMR Hotspots") %>%

                     showGroup(group = "AMR Surveys")

                   # Empty every field of the filted data panel
                   reset(id = "filterdata_ISO3")

                   reset(id = "filterdata_species")

                   reset(id = "filterdata_sample_type")

                   reset(id = "filterdata_pathogen")

                   reset(id = "filterdata_compound")

                   reset(id = "filterdata_compound_class")

                   reset(id = "filterdata_compound_agisar")

                   # Re-enable the switch between livestock, freshwater, and marine data
                   enable(id = "switch_farming_type")

                 }

               }, ignoreInit = TRUE)

# 2.4 #####===== Reset location target input and update the map view and bounds with target input =====#####
observeEvent(input$clear_location_text, {

  reset(id = "target")

})

observeEvent(input$zoom_to_location, {

  target_zone <- ggmap::geocode(location = input$target)

  target_zone_center.lat <- target_zone$lat

  target_zone_center.long <- target_zone$lon

  target_zone.zoom <- 10

  target_zone_bound1.lat <- target_zone_center.lat + 0.6

  target_zone_bound1.long <- target_zone_center.long - 1.3

  target_zone_bound2.lat <- target_zone_center.lat - 0.6

  target_zone_bound2.long <- target_zone_center.long + 1.3

  leafletProxy(mapId = "world_map") %>%

    fitBounds(lat1 = target_zone_bound1.lat,
              lng1 = target_zone_bound1.long,
              lat2 = target_zone_bound2.lat,
              lng2 = target_zone_bound2.long)

})

# 2.5 #####===== Back to initial world map view =====#####
observeEvent(input$initial_map_view, {

  if (input$world_map_zoom >= 2.3) {

    leafletProxy(mapId = "world_map") %>%

      fitBounds(lat1 = map_initialview_data$target_zone_bound1.lat,
                lng1 = map_initialview_data$target_zone_bound1.long,
                lat2 = map_initialview_data$target_zone_bound2.lat,
                lng2 = map_initialview_data$target_zone_bound2.long)

  }

})

# 2.6 #####===== New markers in case of files inside the folder =====#####
observe({

  aws3_surveys_name <- get_bucket(bucket = "amr-hegep-bucket",
                                  prefix = "temporary_responses/",
                                  max = Inf)

  aws3_surveys_name <- data.table::rbindlist(aws3_surveys_name)[["Key"]][-1]

  if (input$switch_farming_type == "livestock") {

    if (length(aws3_surveys_name) != 0 &
        any(input$world_map_groups %in% "AMR Surveys")) {

      aws3_surveys <- lapply(aws3_surveys_name, function(x) {

        object <- get_object(object = x,
                             bucket = "amr-hegep-bucket")

        object_data <- readBin(con = object,
                               what = "character")

        read.csv(text = object_data,
                 header = TRUE,
                 stringsAsFactors = FALSE)

      })

      # Concatenate all data together into one data.frame
      aws3_surveys_data <- do.call(what = rbind,
                                   args = aws3_surveys)

      leafletProxy(mapId = "world_map") %>%

        addMapPane(name = "PPS_temporary",
                   zIndex = 600) %>%

        addMarkers(
          lat = aws3_surveys_data$YCoord,
          lng = aws3_surveys_data$XCoord,
          icon = PPS_markers["lightblue_temp"],
          group = "AMR Surveys",
          options = pathOptions(pane = "PPS_temporary"),
          popup = paste("<div style='max-height:810px' align='justify'>",
                        "<h3>",
                        paste0(ifelse(test = grepl(pattern = ",",
                                                   x = aws3_surveys_data$Author),
                                      yes = sub(pattern = ",.*$",
                                                replacement = "",
                                                x = aws3_surveys_data$Author),
                                      no = aws3_surveys_data$Author),
                               " et al.",
                               ifelse(test = !is.na(aws3_surveys_data$PubDate),
                                      yes = paste0(", ",
                                                   aws3_surveys_data$PubDate),
                                      no = "")), "<br>",
                        "</h3>",
                        "<br/>",
                        "<span style='font-size:12.7px'>",
                        ifelse(test = aws3_surveys_data$Title == "Report could not be linked to a peer-reviewed publication.",
                               yes = paste("<em>Personal contribution from", paste0(aws3_surveys_data$Author,
                                                                                    "."), "</em>"),
                               no = paste("<strong>Title</strong>", ": ", aws3_surveys_data$Title,
                                          sep = "")), "<br>",
                        "<br/>",
                        ifelse(test = grepl(pattern = "et al.",
                                            x = aws3_surveys_data$Authors_name,
                                            fixed = TRUE) == FALSE,
                               yes = paste("<strong>Authors</strong>", ": ", aws3_surveys_data$Authors_name, "<br>", "<br/>",
                                           sep = ""),
                               no = paste("<span style='font-size:0.5px'>",
                                          " ",
                                          "</span>")),
                        ifelse(test = aws3_surveys_data$Journal != "Report could not be linked to a peer-reviewed publication." &
                                 aws3_surveys_data$Title != "Report could not be linked to a peer-reviewed publication.",
                               yes = paste("<strong>Journal</strong>", ": ",
                                           ifelse(test = is.na(aws3_surveys_data$URL),
                                                  yes = aws3_surveys_data$Journal,
                                                  no = paste0("<a href = '",
                                                              aws3_surveys_data$URL,
                                                              "'",
                                                              " target='_blank'>",
                                                              aws3_surveys_data$Journal,
                                                              "</a>")), "<br>", "<br/>",
                                           sep = ""),
                               no = paste("<span style='font-size:0.5px'>",
                                          " ",
                                          "</span>")),
                        aws3_surveys_data$Plot, "<br>",
                        "<br/>",
                        "<div style='line-height:13px'>",
                        "<strong>Antibiotics</strong>", ": ",
                        "</span>",
                        "<span style='color:#a8a4a4; font-size:11px'>",
                        aws3_surveys_data$Plot_caption,
                        "</span>",
                        "</div>",
                        "</div>",
                        sep = ""),
          label = paste0(aws3_surveys_data$Author,
                         ifelse(is.na(aws3_surveys_data$PubDate),
                                yes = "",
                                no = paste0(", ", aws3_surveys_data$PubDate))),
          labelOptions = labelOptions(direction = "bottom",
                                      style = list(
                                        "font-size" = "12.7px"
                                      )),
          clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[2]],
                                                zoomToBoundsOnClick = FALSE,
                                                removeOutsideVisibleBounds = FALSE,
                                                spiderfyDistanceMultiplier = 2,
                                                freezeAtZoom = 7)
        ) %>%

        addControl(html = "<strong>Surveys</strong></br>
                             <img height='20' src='https://nicocriscuolo.github.io/resistancebank_plots/markers/marker_3D.png'> Reviewed</br>
                             <img height='20' src='https://nicocriscuolo.github.io/resistancebank_plots/markers/marker_3D_lightblue.png'> Submitted",
                   layerId = "temporary_legend",
                   position = "bottomleft")

    } else {

      leafletProxy(mapId = "world_map") %>%

        removeControl(layerId = "temporary_legend")

    }

  } else {

    leafletProxy(mapId = "world_map") %>%

      removeControl(layerId = "temporary_legend")

  }

})
