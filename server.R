
server <- function(input, output, session) {

  #########################=========================#########################
  # 1. ##########========== DISPLAY AND HIDE PANELS ==========##########

  # 1.1 #####===== Initial panel =====#####
  download_RESBANK2_reactive_values <- reactiveValues(download_number = 0)

  observeEvent(input$explore_amr_map |
               input$add_your_survey1 |
               download_RESBANK2_reactive_values$download_number |
               input$close_initial_panel, {

    hideElement(id = "initial_panel_division",
                anim = TRUE,
                animType = "fade")

    showElement(id = "controls_panel",
                anim = TRUE,
                animType = "fade")

    showElement(id = "target_panel",
                anim = TRUE,
                animType = "fade")

    showElement(id = "initial_map_view_panel",
                anim = TRUE,
                animType = "fade")

    showElement(id = "name_Maps_panel",
                anim = TRUE,
                animType = "fade")

  }, ignoreInit = TRUE)

  # 1.2 #####===== Controls panel =====#####

  # 1.2.1 ###= Add your survey panel =#####
  observeEvent(input$add_your_survey1, {

    hideElement(id = "add_your_survey2",
                anim = FALSE)

    showElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

  })

  observeEvent(input$close_add_your_survey_division, {

    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    showElement(id = "add_your_survey2",
                anim = FALSE)

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

  })

  observeEvent(input$close_add_your_survey_controls, {

    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

  })

  observeEvent(input$add_your_survey2, {

    showElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    showElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    hideElement(id = "add_your_survey2",
                anim = FALSE)

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)

    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)

  })

  # 1.2.1.1 ###= Fill form panel =#####
  observeEvent(input$fill_form, {

    showElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

  })

  observeEvent(input$close_add_your_survey_controls2, {

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    showElement(id = "add_your_survey2",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

  })

  observeEvent(input$close_form, {

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

    showElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    showElement(id = "close_add_your_survey_controls",
                anim = FALSE)

  })

  # 1.2.2 ###= Country report panel =#####
  observeEvent(input$display_Country_report_panel, {

    showElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "display_Country_report_panel",
                anim = FALSE)

    showElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

  })

  observeEvent(input$close_country_report_panel, {

    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)

  })

  observeEvent(input$close_country_report_panel_controls, {

    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)

  })

  country_report_disabled_choices <- c(Countries$Country_ISO3) %in% c(setdiff(x = Countries$Country_ISO3,
                                                                              y = Countries_PPS$Country_ISO3))

  shinyWidgets::updatePickerInput(session = session,
                                  inputId = "Country_report_nation",
                                  choices = c(Countries$Country_ISO3),
                                  choicesOpt = list(
                                    disabled = country_report_disabled_choices,
                                    style = ifelse(test = country_report_disabled_choices,
                                                   yes = "font-size: 82%; line-height: 1.2; color: #DEDEDE;",
                                                   no = "font-size: 82%; line-height: 1.2;")),
                                  selected = Countries$Country_ISO3[Countries$Country_ISO3 == "India (IND)"])

  # 1.2.3 ###= About panel =#####
  observeEvent(input$display_about_panel, {

    showElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "display_about_panel",
                anim = FALSE)

    showElement(id = "close_about_panel_controls",
                anim = FALSE)

    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

  })

  observeEvent(input$close_about_panel, {

    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)

  })

  observeEvent(input$close_about_panel_controls, {

    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)

  })

  # 1.2.4 ###= GitHub repository =#####
  toggleState(id = "open_GitHub_repository")

  # 2. ##########========== WORLD MAP =========##########
  output$world_map <- renderLeaflet({

    P50_palette <- colorNumeric(
      palette = c("#ffdbdb",
                  "#fed1bb",
                  "#fdc1aa",
                  "#fdac93",
                  "#fd927b",
                  "#f4746a",
                  "#ec5c60",
                  "#ee3d4c",
                  "#d61100"),
      domain = 80:0,
      reverse = TRUE
    )

    leaflet_object <- leaflet(data = resistancebank_surveys,
                              options = leafletOptions(zoomControl = FALSE,
                                                       zoomDelta = 1,
                                                       zoomSnap = 0,
                                                       minZoom = 2.7,
                                                       maxZoom = 10,
                                                       preferCanvas = TRUE)) %>%

      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
        }") %>%

      addMapPane(name = "Minimal_layer",
                 zIndex = 400) %>%

      addMapPane(name = "Satellite_layer",
                 zIndex = 405) %>%

      addMapPane(name = "P50_layer",
                 zIndex = 410) %>%

      addMapPane(name = "boundaries_labels_layer",
                 zIndex = 415) %>%

      addMapPane(name = "PPS_layer",
                 zIndex = 500) %>%

      addProviderTiles(
        provider = providers$Esri.WorldShadedRelief,
        group = "Minimal",
        options = c(pathOptions(pane = "Minimal_layer"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      addProviderTiles(
        provider = providers$Esri.WorldPhysical,
        group = "Satellite",
        options = c(pathOptions(pane = "Satellite_layer"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      hideGroup("Satellite") %>%

      addTiles(
        urlTemplate = "https://nicocriscuolo.github.io/resbank_tiles/P50_red/{z}/{x}/{y}.png",
        group = "AMR Hotspots",
        options = c(pathOptions(pane = "P50_layer"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      addProviderTiles(
        provider = providers$Stamen.TonerHybrid,
        options = c(pathOptions(pane = "boundaries_labels_layer"),
                    providerTileOptions(updateWhenZooming = FALSE,
                                        updateWhenIdle = FALSE))) %>%

      addMarkers(
        lng = ~XCoord,
        lat = ~YCoord,
        icon = marker_PPS,
        group = "AMR Surveys",
        clusterId = "reviewed_markers_cluster",
        popup = ~paste("<div style='max-height:790px' align='justify'>",
                       "<h3>",
                       paste0(resistancebank_surveys$Author,
                              " et al.",
                              ifelse(test = is.na(resistancebank_surveys$PubDate),
                                     yes = "",
                                     no = paste0(", ", resistancebank_surveys$PubDate))), "<br>",
                       "</h3>",
                       "<br/>",
                       "<span style='font-size:12.7px'>",
                       "<strong>Title</strong>", ": ",
                       resistancebank_surveys$Title, "<br>",
                       "<br/>",
                       "<strong>Authors</strong>", ": ",
                       resistancebank_surveys$Authors_name, "<br>",
                       "<br/>",
                       "<strong>Journal</strong>", ": ",
                       paste0(ifelse(test = is.na(resistancebank_surveys$URL) & resistancebank_surveys$Journal == "Report could not be linked to a peer-reviewed publication." |
                                            is.na(resistancebank_surveys$URL) & resistancebank_surveys$Journal != "Report could not be linked to a peer-reviewed publication." |
                                            !is.na(resistancebank_surveys$URL) & resistancebank_surveys$Journal == "Report could not be linked to a peer-reviewed publication.",
                                     yes = paste0(resistancebank_surveys$Journal),
                                     no = paste0("<a href='", resistancebank_surveys$URL, "'", " target='_blank'>", resistancebank_surveys$Journal, "</a>"))), "<br>",
                       "<br/>",
                       resistancebank_surveys$Plot, "<br>",
                       "<br/>",
                       "<div style='line-height:13px'>",
                       "<strong>Antibiotics</strong>", ": ",
                       "</span>",
                       "<span style='color:#a8a4a4; font-size:11px'>",
                       resistancebank_surveys$Plot_caption,
                       "</span>",
                       "</div>",
                       "</div>",
                       sep = ""),
        label = ~as.character(paste0(resistancebank_surveys$Author,
                                     ifelse(test = is.na(resistancebank_surveys$PubDate),
                                            yes = "",
                                            no = paste0(", ", resistancebank_surveys$PubDate)))),
        labelOptions = labelOptions(direction = "bottom",
                                    style = list(
                                      "font-size" = "12.7px"
                                    )),
        options = pathOptions(pane = "PPS_layer"),
        clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[1]],
                                              zoomToBoundsOnClick = FALSE,
                                              removeOutsideVisibleBounds = FALSE,
                                              spiderfyDistanceMultiplier = 2,
                                              freezeAtZoom = 7) # !!!!!!!!!!!!!!
      ) %>%

      fitBounds(lat1 = 61.6,
                lng1 = -121.55,
                lat2 = -62.6,
                lng2 = 155.1) %>%

      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 200)) %>%

      addLegend(pal = P50_palette,
                values = 80:0,
                opacity = 1,
                title = "AMR [%]",
                group = "AMR Hotspots",
                position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

      # setMaxBounds(lng1 = -170,
      #              lat1 = 88,
      #              lng2 = 192,
      #              lat2 = -88)

  })

  # 2.1 #####===== Add control layers after closing initial panel =====#####
  observeEvent(input$explore_amr_map |
               input$add_your_survey1 |
               download_RESBANK2_reactive_values$download_number |
               input$close_initial_panel, {

    leafletProxy(mapId = "world_map") %>%

      addLayersControl(overlayGroups = c("AMR Surveys",
                                         "AMR Hotspots",
                                         "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) # TRUE to show all the layers in one group

  }, ignoreInit = TRUE)

  # 2.2 #####===== Updates of map view and bounds with target input =====#####
  observeEvent(input$zoom_to_location, {

  ###= Geocoding-based view
    target_zone <- geocode(location = input$target)

    target_zone_center.lat <- target_zone$lat

    target_zone_center.long <- target_zone$lon

    target_zone.zoom <- 10 # !!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  # 2.3 #####===== Back to initial world map view =====#####
  observeEvent(input$initial_map_view, {

    if (input$world_map_zoom >= 2.7) {

      target_zone_bound1.lat <- 61.6

      target_zone_bound1.long <- -121.55

      target_zone_bound2.lat <- -62.6

      target_zone_bound2.long <- 155.1

      leafletProxy(mapId = "world_map") %>%

        fitBounds(lat1 = target_zone_bound1.lat,
                  lng1 = target_zone_bound1.long,
                  lat2 = target_zone_bound2.lat,
                  lng2 = target_zone_bound2.long)

    }

  })

  # 2.4 #####===== New markers in case of files inside the folder =====#####
  observe({

    aws3_surveys_name <- get_bucket(bucket = "amr-hegep-bucket",
                                    prefix = "temporary_responses/",
                                    max = Inf)

    aws3_surveys_name <- data.table::rbindlist(aws3_surveys_name)[["Key"]][-1]

    if (length(aws3_surveys_name) != 0 & any(input$world_map_groups %in% "AMR Surveys")) {
                                             # this input call works with {mapId}_groups, hence "world_map_groups"

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
          icon = marker_temporary,
          group = "AMR Surveys",
          clusterId = "temporary_markers_cluster",
          popup = paste("<div style='max-height:790px' align='justify'>",
                        "<h3>",
                        paste0(aws3_surveys_data$Author,
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
          options = pathOptions(pane = "PPS_temporary")
          # , clusterOptions = markerClusterOptions(iconCreateFunction = marker_cluster[[2]],
          #                                         zoomToBoundsOnClick = FALSE,
          #                                         removeOutsideVisibleBounds = FALSE,
          #                                         spiderfyDistanceMultiplier = 2,
          #                                         freezeAtZoom = 7)

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

  })

  # 2.5 #####===== Reactive dataset for international P50 level =====#####
  source(file = "modules/server_international-P50-level.R",
         local = TRUE)$value

  # 3. ##########========== DATA SUBMISSION ==========####################
  # 3.1 ##########=== User information panel ===##########
  observeEvent(input$show_user_information_panel, {

    showElement(id = "user_information_panel")

    hideElement(id = "show_user_information_panel")

    showElement(id = "hide_user_information_panel")

  })

  observeEvent(input$hide_user_information_panel, {

    hideElement(id = "user_information_panel")

    showElement(id = "show_user_information_panel")

    hideElement(id = "hide_user_information_panel")

  })

  # 3.2 ##########=== Study information panel ===##########
  observeEvent(input$show_study_information_panel, {

    showElement(id = "study_information_panel")

    hideElement(id = "show_study_information_panel")

    showElement(id = "hide_study_information_panel")

  })

  observeEvent(input$hide_study_information_panel, {

    hideElement(id = "study_information_panel")

    showElement(id = "show_study_information_panel")

    hideElement(id = "hide_study_information_panel")

  })

  # 3.2.1 #####=== Operation on DOIs ===#####

  # 3.2.1.1 ###= Replacement of wrong parts of DOIs =#####
  DOI <- reactive({

    DOI <- input$doi

    DOI <- DOI %>%
      str_replace(pattern = "https://dx.doi.org/",
                  replacement = "") %>%
      str_replace(pattern = "http://dx.doi.org/",
                  replacement = "") %>%
      str_replace(pattern = "https://dx.doi/",
                  replacement = "") %>%
      str_replace(pattern = "https://doi.org/",
                  replacement = "") %>%
      str_replace(pattern = "https://dx.doi/",
                  replacement = "") %>%
      str_replace(pattern = "http://dx.doi/",
                  replacement = "") %>%
      str_replace(pattern = "HTTPS://DX.DOI.ORG/",
                  replacement = "") %>%
      str_replace(pattern = "HTTP://DX.DOI.ORG/",
                  replacement = "") %>%
      str_replace(pattern = "DX.DOI.ORG/",
                  replacement = "") %>%
      str_replace(pattern = "DX.DOI/",
                  replacement = "") %>%
      str_replace(pattern = "https://",
                  replacement = "") %>%
      str_replace(pattern = "http://",
                  replacement = "") %>%
      str_replace(pattern = "dx.doi.org/",
                  replacement = "") %>%
      str_replace(pattern = "dx.doi/",
                  replacement = "") %>%
      str_replace(pattern = "hps://doi.org/",
                  replacement = "") %>%
      str_replace(pattern = "hps://",
                  replacement = "") %>%
      str_replace(pattern = "doi.org/",
                  replacement = "") %>%
      str_replace_all(pattern = " ",
                      replacement = "")

    return(DOI)

  })

  # 3.2.1.2 ###= Bibliographic research =#####
  observe({

    if (DOI() != "" & nchar(DOI()) > 9) {

      EPMC_SUMMARY <- europepmc::epmc_search(query = DOI())

      if (is.null(EPMC_SUMMARY)) {

        journal_name.form <- ""
        paper_title.form <- ""
        publication_year.form <- c(1985:2022)

        updateTextInput(session = session,
                        inputId = "journal",
                        value = "")

        updateTextInput(session = session,
                        inputId = "paper_title",
                        value = "")

        updateSelectizeInput(session = session,
                             inputId = "PubDate",
                             choices = publication_year.form,
                             selected = "")

      } else {

        journal_name.form <- EPMC_SUMMARY$journalTitle[[1]]
        paper_title.form <- EPMC_SUMMARY$title[[1]]
        publication_year.form <- EPMC_SUMMARY$pubYear[[1]]

        updateTextInput(session = session,
                        inputId = "journal",
                        value = journal_name.form)

        updateTextInput(session = session,
                        inputId = "paper_title",
                        value = paper_title.form)

        updateSelectizeInput(session = session,
                             inputId = "PubDate",
                             choices = c(1985:2022),
                             selected = publication_year.form)

      }

    } else {

      updateTextInput(session = session,
                      inputId = "journal",
                      value = "")

      updateTextInput(session = session,
                      inputId = "paper_title",
                      value = "")

      updateSelectizeInput(session = session,
                           inputId = "PubDate",
                           choices = c("",
                                       1985:2022),
                           selected = "")

    }

  })

  # 3.3 ##########=== AMR panel ===##########
  observeEvent(input$show_amr_panel, {

    showElement(id = "amr_panel")

    hideElement(id = "show_amr_panel")

    showElement(id = "hide_amr_panel")

  })

  observeEvent(input$hide_amr_panel, {

    hideElement(id = "amr_panel")

    showElement(id = "show_amr_panel")

    hideElement(id = "hide_amr_panel")

  })

  # 3.3.1 ###= Dynamic UI for AMR ####
  source(file = "modules/server_antimicrobial-resistance-form.R",
         local = TRUE)$value

  # 3.4 ##########=== Remark panel ===##########
  observeEvent(input$show_remark_panel, {

    showElement(id = "remark_panel")

    hideElement(id = "show_remark_panel")

    showElement(id = "hide_remark_panel")

  })

  observeEvent(input$hide_remark_panel, {

    hideElement(id = "remark_panel")

    showElement(id = "show_remark_panel")

    hideElement(id = "hide_remark_panel")

  })


  # 3.3 ###############===== Form and Template datasets =====###############

  # 3.3.1 #####=== Data input from Form ===#####
  formData <- reactive({

    ###= Reverse geocoding to obtain coordinates =#####
    coordinates <- as.data.frame(geocode(location = input$coordinates))

    if (is.na(coordinates$lat) | is.na(coordinates$lon)) {

      coordinates$lat <- 47.367

      coordinates$lon <- 8.551

    }

    ###= Main dataframe =#####
    response_info <- data.frame("author_name" = rep(x = gsub(pattern = ",",
                                                         replacement = "",
                                                         x = input$author_name),
                                                    times = nrow(response_amr())),
                                "Author" = rep(x = gsub(pattern = ",",
                                                        replacement = "",
                                                        x = input$Author),
                                               times = nrow(response_amr())),
                                "Institution" = rep(x = gsub(pattern = ",",
                                                             replacement = "",
                                                             x = input$institution),
                                                    times = nrow(response_amr())),
                                "Contact" = rep(x = gsub(pattern = ",",
                                                         replacement = "",
                                                         x = input$author_email),
                                                     times = nrow(response_amr())),
                                "DOI" = rep(x = gsub(pattern = ",",
                                                     replacement = "",
                                                     x = DOI()),
                                            times = nrow(response_amr())),
                                "PubDate" = rep(x = input$PubDate,
                                                times = nrow(response_amr())),
                                "Title" = rep(x = input$paper_title,
                                              times = nrow(response_amr())),
                                "Journal" = rep(x = input$journal,
                                                times = nrow(response_amr())),
                                "country_study" = rep(x = input$country_study,
                                                      times = nrow(response_amr())),
                                "study_address" = rep(x = gsub(pattern = ",",
                                                               replacement = "",
                                                               x = input$coordinates),
                                                      times = nrow(response_amr())),
                                "YCoord" = rep(x = coordinates$lat,
                                               times = nrow(response_amr())),
                                "XCoord" = rep(x = coordinates$lon,
                                               times = nrow(response_amr()))
                                )

    ###= Remark message if there are wrong coordinates =#####
    if (unique(response_info$YCoord) == "47.367" |
        unique(response_info$XCoord) == "8.551") {

      response_info$Remark <- rep(x = gsub(pattern = ",",
                                           replacement = "",
                                           x = paste0("Wrong coordinates. ", input$remark)),
                                  times = nrow(response_amr()))

    } else {

      response_info$Remark <- rep(x = gsub(pattern = ",",
                                      replacement = "",
                                      x = input$remark),
                             times = nrow(response_amr()))

    }

    ###= Complete form dataset =#####
    response <- cbind(response_info,
                      response_amr())

    response$Rescom <- as.numeric(as.character(response$Rescom))

    response$Nsamples <- as.numeric(as.character(response$Nsamples))

    response$NIsolates <- as.numeric(as.character(response$NIsolates))

    response$Prev <- as.numeric(as.character(response$Prev))

    response$Title <- as.character(response$Title)

    response$Journal <- as.character(response$Journal)

    response[response == ""] <- NA

    return(response)

  })

  # 3.3.2 #####=== Data input from Template ===#####
  templateData <- reactive({

    req(input$upload_resistancebank_template)

    withProgress(message = "Uploading data",
                 style = "notification",
                 value = 0.1, {

                   template <- input$upload_resistancebank_template

                   resistancebank_template <- as.data.frame(readxl::read_excel(path = template$datapath,
                                                                               sheet = 1,
                                                                               col_names = TRUE,
                                                                               na  = c("", " ", "NA")))

                   setProgress(1)

                   Sys.sleep(time = 1)

    })

    return(resistancebank_template)

  })

  # 3.4 ##########===== Mandatory fields =====##########
  source(file = "modules/server_mandatory-fields.R",
         local = TRUE)$value

  # 3.5 #####===== Functions to save the data and upload them on Dropbox =====#####

  # 3.5.1 #####=== Save form data ===#####

  # Form unique filename
  source(file = "modules/server_unique-formname.R",
         local = TRUE)$value

  # Save form function
  save_form <- function(data) {

    filePath <- file.path(tempdir(), unique_formname())

    write.table(x = data,
                file = filePath,
                row.names = FALSE,
                quote = TRUE,
                sep = ",")

    drop_upload(file = filePath,
                path = "temporary_directory",
                mode = "overwrite")

  }

  # 3.5.2 #####=== Save template data ===#####

  # Template unique filename
  source(file = "modules/server_unique-templatename.R",
         local = TRUE)$value

  # Save template function
  save_template <- function(data) {

    filePath <- file.path(tempdir(), unique_templatename())

    write.table(x = data,
                file = filePath,
                row.names = FALSE,
                quote = TRUE,
                sep = ",")

    drop_upload(file = filePath,
                path = "temporary_directory",
                mode = "overwrite")

  }

  # 3.6 #####===== Save form data with submit button and thanks =====#####
  observeEvent(input$submit, {

  # 3.6.1 ###= Preliminary operations =#####
    toggleState(id = "submit")

    reset(id = "form")

    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

    form_csv <- formData()

    save_form(form_csv)

    # form_csv <- drop_read_csv(file = "temporary_directory/fxbxfb_xcvbxcvb_Albania(ALB)_1576594848c2fd05ee7216f7fad7666f29a1e6a2cf19b33c10.xlsx",
    #                           header = TRUE,
    #                           na.string = c("", " ", "NA"))

  # 3.6.2 ###= "Thank you" notification =#####
    showNotification(ui = paste("Thank you, your contribution has been submitted successfully! Your data will be added to Resistance bank once approved by the curator."),
                     duration = 10,
                     closeButton = FALSE,
                     type = "default")

  # 3.6.3 ###= Bibliographic research =#####

  # Title
    if (is.na(unique(form_csv$Title))) {

      form_csv$Title <- "Report could not be linked to a peer-reviewed publication."

    } else {

      form_csv$Title <- gsub(pattern = "&lt;i&gt;",
                             replacement = "",
                             x = form_csv$Title)

      form_csv$Title <- gsub(pattern = "&lt;/i&gt;",
                             replacement = "",
                             x = form_csv$Title)

    }

  # Journal
    if (is.na(unique(form_csv$Journal))) {

      form_csv$Journal <- "Report could not be linked to a peer-reviewed publication."

    }

  # Authors name
    if (!is.na(unique(form_csv$DOI)) & nchar(as.character(unique(form_csv$DOI))) > 9) {

      EPMC_SUMMARY <- europepmc::epmc_search(query = unique(form_csv$DOI))

      if (is.null(EPMC_SUMMARY)) {

        form_csv$Authors_name <- paste0(unique(form_csv$author_name),
                                        " ",
                                        unique(form_csv$Author),
                                        " et al.")

      } else {

        form_csv$Authors_name <- EPMC_SUMMARY$authorString[[1]]

      }

    } else {

      form_csv$Authors_name <- paste0(unique(form_csv$author_name),
                                      " ",
                                      unique(form_csv$Author),
                                      " et al.")

    }

  # URL
    if (!is.na(unique(form_csv$DOI)) & nchar(as.character(unique(form_csv$DOI))) > 9) {

      DOI_URL <- try(crminer::crm_links(doi = unique(as.character(form_csv$DOI)),
                                        type = "all"))

      if (class(DOI_URL) != "try-error") {

        DOI_URL <- crminer::crm_links(doi = unique(as.character(form_csv$DOI)),
                                      type = "all")

        form_csv$URL <- ifelse(test = length(DOI_URL$html) == 1,
                               yes = DOI_URL$html$html,
                               no = ifelse(test = length(DOI_URL$pdf) == 1,
                                           yes = DOI_URL$pdf$pdf,
                                           no = ifelse(test = length(DOI_URL$unspecified) == 1,
                                                       yes = DOI_URL$unspecified$unspecified,
                                                       no = NA)))

      } else {

        form_csv$URL <- NA

      }

    } else {

      form_csv$URL <- NA

    }

  # 3.6.4 ###= Aggregation for Rescom field to generate the plot =#####
    study <- aggregate(x = list("Rescom" = form_csv$Rescom),
                       by = list("Compound" = form_csv$Compound,
                                 "Pathogens" = form_csv$Pathogens,
                                 "Species" = form_csv$Species),
                       FUN = mean,
                       na.rm = TRUE)

  # 3.6.5 ###= Plot of the user submitted data =#####
    source(file = "modules/server_unique-formplot.R",
           local = TRUE)$value

  # Temporary plot caption of unique antiobiotic names + codes
    Antibiotics_df_unique <- subset(x = Antibiotics_df,
                                    subset = Code %in% study$Compound)

  # Save the plot inside the user temporary folder
    imagePath <- file.path(tempdir(),
                           paste0(substr(x = unique_formname(),
                                         start = 1 ,
                                         stop = nchar(x = unique_formname()) - 4),
                                  ".svg"))

    ggsave(filename = imagePath,
           plot = study_plot,
           dpi = 106,
           width = 11,
           height = 12.83)

  # Upload the temporary plot inside the Amazon S3 bucket
    put_object(file = imagePath,
               object = paste0(substr(x = unique_formname(),
                                      start = 1 ,
                                      stop = nchar(x = unique_formname()) - 4),
                               ".svg"),
               bucket = "amr-hegep-bucket/temporary_amr_plots",
               acl = c("public-read"))

  # 3.6.6 ###= Display new marker in leaflet =#####
    form_csv_center.lat <- unique(form_csv$YCoord)

    form_csv_center.lon <- unique(form_csv$XCoord)

    form_csv.zoom <- 7

    form_csv_bound1.lat <- form_csv_center.lat + 0.6

    form_csv_bound1.lon <- form_csv_center.lon - 1.3

    form_csv_bound2.lat <- form_csv_center.lat - 0.6

    form_csv_bound2.lon <- form_csv_center.lon + 1.3

  # Popup content based on bibliographic informations available
    popup_content.form <- paste("<div style='max-height:790px' align='justify'>",
                                "<h3>",
                                paste0(unique(form_csv$Author),
                                       " et al.",
                                       ifelse(test = !is.na(unique(form_csv$PubDate)),
                                              yes = paste0(", ",
                                                           unique(form_csv$PubDate)),
                                              no = "")), "<br>",
                                "</h3>",
                                "<br/>",
                                "<span style='font-size:12.7px'>",
                                ifelse(test = unique(form_csv$Title) == "Report could not be linked to a peer-reviewed publication.",
                                       yes = paste("<em>Personal contribution from", paste0(unique(form_csv$Author),
                                                                                            ifelse(test = !is.na(unique(form_csv$Institution)),
                                                                                                   yes = paste0(", ",
                                                                                                                unique(form_csv$Institution),
                                                                                                                "."),
                                                                                                   no = "."), "</em>")),
                                       no = paste("<strong>Title</strong>", ": ", unique(form_csv$Title),
                                                  sep = "")), "<br>",
                                "<br/>",
                                {if (grepl(pattern = "et al.",
                                           x = unique(form_csv$Authors_name),
                                           fixed = TRUE) == FALSE) {
                                  paste("<strong>Authors</strong>", ": ", unique(form_csv$Authors_name), "<br>", "<br/>",
                                        sep = "")
                                  }
                                },
                                {if (unique(form_csv$Journal) != "Report could not be linked to a peer-reviewed publication." &
                                     unique(form_csv$Title) != "Report could not be linked to a peer-reviewed publication.") {
                                  paste("<strong>Journal</strong>", ": ",
                                        ifelse(test = is.na(unique(form_csv$URL)),
                                               yes = unique(form_csv$Journal),
                                               no = paste0("<a href = '",
                                                           unique(form_csv$URL),
                                                           "'",
                                                           " target='_blank'>",
                                                           unique(form_csv$Journal),
                                                           "</a>")), "<br>", "<br/>",
                                        sep = "")
                                  }
                                },
                                paste0("<img height='350' src='",
                                       "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/temporary_amr_plots/",
                                       substr(x = unique_formname(),
                                              start = 1 ,
                                              stop = nchar(x = unique_formname()) - 4),
                                       ".svg",
                                       "'/>"), "<br>",
                                "<br/>",
                                "<div style='line-height:13px'>",
                                "<strong>Antibiotics</strong>", ": ",
                                "</span>",
                                "<span style='color:#a8a4a4; font-size:11px'>",
                                paste(Antibiotics_df_unique$Combination, collapse = ", "),
                                "</span>",
                                "</div>",
                                "</div>",
                                sep = "")

  # Update leaflet map
    leafletProxy(mapId = "world_map") %>%

      addMapPane(name = "PPS_submission",
                 zIndex = 620) %>%

      addMarkers(
        lat = unique(form_csv$YCoord),
        lng = unique(form_csv$XCoord),
        icon = marker_submission,
        label = paste0(unique(form_csv$Author),
                       ifelse(is.na(unique(form_csv$PubDate)),
                                          yes = "",
                                          no = paste0(", ", unique(form_csv$PubDate)))),
        labelOptions = labelOptions(direction = "bottom",
                                    style = list(
                                      "font-size" = "12.7px"
                                    )),
        popup = popup_content.form,
        options = pathOptions(pane = "PPS_submission")) %>%

      setView(lat = form_csv_center.lat,
              lng = form_csv_center.lon,
              zoom = form_csv.zoom) %>%

      fitBounds(lat1 = form_csv_bound1.lat,
                lng1 = form_csv_bound1.lon,
                lat2 = form_csv_bound2.lat,
                lng2 = form_csv_bound2.lon)

  # 3.6.7 ###= Send authomatic e-mail with form link =#####
    unique_formurl <- drop_share(path = paste0("temporary_directory/",
                                               unique_formname()))

    email_form <- gmailr::gm_mime() %>%
      gmailr::gm_subject(paste0("NEW RESBANK SUBMISSION - ",
                                substr(x = unique_formname(),
                                       start = 1 ,
                                       stop = nchar(x = unique_formname()) - 4))) %>%
      gmailr::gm_from("nico.criscuolo1618@gmail.com") %>%
      # gmailr::gm_to(c("nico.criscuolo981@gmail.com",
      #                 "joao.pires@env.ethz.ch")) %>%
      gmailr::gm_to("nico.criscuolo981@gmail.com") %>%
      gmailr::gm_text_body(paste0(substr(x = unique_formname(),
                                         start = 1 ,
                                         stop = nchar(x = unique_formname()) - 4),
                                  " ",
                                  unique_formurl$url))

    gmailr::gm_send_message(email_form)

  # 3.6.8 ###= Notification in case of wrong coordinates =#####
    if (unique(form_csv$YCoord) == "47.367" |
        unique(form_csv$XCoord) == "8.551") {

      showNotification(ui = paste("The geocode function could not retrieve the correct coordinates of your study, the curator will get in contact with you soon."),
                       duration = 12,
                       closeButton = FALSE,
                       type = "error")

    }

  # 3.6.9 ###= Updates the 1 row file from the form in AWS3 for displaying of temporary markers =#####
    survey_aws3 <- data.frame("Author" = unique(form_csv$Author),
                              "YCoord" = unique(form_csv$YCoord),
                              "XCoord" = unique(form_csv$XCoord),
                              "PubDate" = unique(form_csv$PubDate),
                              "Plot" = paste0("<img height='350' src='",
                                              "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/temporary_amr_plots/",
                                              substr(x = unique_formname(),
                                                     start = 1,
                                                     stop = nchar(x = unique_formname()) - 4),
                                              ".svg",
                                              "'/>"),
                              "Plot_caption" = paste(Antibiotics_df_unique$Combination,
                                                     collapse = ", "),
                              "Journal" = unique(form_csv$Journal),
                              "Title" = unique(form_csv$Title),
                              "Authors_name" = unique(form_csv$Authors_name),
                              "URL" = unique(form_csv$URL))

    formPath <- file.path(tempdir(), unique_formname())

    write.table(x = survey_aws3,
                file = formPath,
                row.names = FALSE,
                quote = TRUE,
                sep = ",")

    put_object(file = formPath,
               object = unique_formname(),
               bucket = "amr-hegep-bucket/temporary_responses",
               acl = c("public-read"))

  })

  # 3.7 #####===== Save template data with submit button and thanks =====#####
  observeEvent(input$upload_resistancebank_template, {

  # 3.7.1 ###= Preliminary operations =#####
    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

    template_csv <- templateData()

    # template_csv <- as.data.frame(readxl::read_excel("/Users/ncriscuolo/Desktop/resistancebank_template_1/resistancebank_template_1.xlsx",
    #                                                  col_names = TRUE,
    #                                                  na = c("", " ", "NA")))

    if (all(c(correct_names$template_csv_colnames) %in% colnames(template_csv)) == FALSE) {

      showNotification(ui = "Please upload a .xlsx file with the same number and column names of the template we provided.",
                       duration = 15,
                       closeButton = FALSE,
                       type = "error")

    } else {

      if (is.numeric(template_csv$YCoord) == FALSE) {

        template_csv$YCoord <- as.character(template_csv$YCoord)

        template_csv$YCoord <- gsubfn(pattern = ".",
                                      replacement = list("," = ".",
                                                         " " = "",
                                                         "'" = "",
                                                         "_" = "",
                                                         "%" = "",
                                                         "/" = "",
                                                         "&" = "",
                                                         "=" = "",
                                                         "+" = "",
                                                         "*" = ""),
                                      x = template_csv$YCoord)

        template_csv$YCoord <- as.numeric(template_csv$YCoord)

      }

      if (any(unique(template_csv$YCoord) < -90) == TRUE |
          any(unique(template_csv$YCoord) > 90) == TRUE) {

        template_csv$YCoord <- 47.367

      }

      if (is.numeric(template_csv$XCoord) == FALSE) {

        template_csv$XCoord <- as.character(template_csv$XCoord)

        template_csv$XCoord <- gsubfn(pattern = ".",
                                      replacement = list("," = ".",
                                                         " " = "",
                                                         "'" = "",
                                                         "_" = "",
                                                         "%" = "",
                                                         "/" = "",
                                                         "&" = "",
                                                         "=" = "",
                                                         "+" = "",
                                                         "*" = ""),
                                      x = template_csv$XCoord)

        template_csv$XCoord <- as.numeric(template_csv$XCoord)

      }

      if (any(unique(template_csv$XCoord) < -180) == TRUE |
          any(unique(template_csv$XCoord) > 180) == TRUE) {

        template_csv$XCoord <- 8.551

      }

      if (is.numeric(template_csv$Rescom) == FALSE) {

        template_csv$Rescom <- as.character(template_csv$Rescom)

        template_csv$Rescom <- gsubfn(pattern = ".",
                                      replacement = list("," = ".",
                                                         " " = "",
                                                         "'" = "",
                                                         "-" = "",
                                                         "_" = "",
                                                         "%" = "",
                                                         "/" = "",
                                                         "&" = "",
                                                         "=" = "",
                                                         "+" = "",
                                                         "*" = ""),
                                      x = template_csv$Rescom)

        template_csv$Rescom <- as.numeric(template_csv$Rescom)

      }

      template_csv$Compound <- as.character(template_csv$Compound)

      template_csv$Compound <- template_csv$Compound %>%
        str_replace_all(pattern = " ",
                        replacement = "")

      if (any(is.na(template_csv$DOI)) == TRUE |
          any(is.na(template_csv$Author)) == TRUE |
          any(is.na(template_csv$ISO3)) == TRUE |
          any(is.na(template_csv$YCoord)) == TRUE |
          any(is.na(template_csv$XCoord)) == TRUE |
          any(is.na(template_csv$Species)) == TRUE |
          any(is.na(template_csv$SampleType)) == TRUE |
          any(is.na(template_csv$Method)) == TRUE |
          any(is.na(template_csv$Pathogens)) == TRUE |
          any(is.na(template_csv$Compound)) == TRUE |
          any(is.na(template_csv$Rescom)) == TRUE |
          any(is.na(template_csv$Contact)) == TRUE) {

        template_csv_subset <- subset(x = template_csv,
                                      select = c("DOI",
                                                 "Author",
                                                 "ISO3",
                                                 "YCoord",
                                                 "XCoord",
                                                 "Species",
                                                 "SampleType",
                                                 "Method",
                                                 "Pathogens",
                                                 "Compound",
                                                 "Rescom",
                                                 "Contact"))

        showNotification(ui = paste("Missing values found inside the following ",
                                    ifelse(test = length(colnames(template_csv_subset)[!complete.cases(t(template_csv_subset))]) == 1,
                                           yes = "column: ",
                                           no = "columns: "),
                                    paste(colnames(template_csv_subset)[!complete.cases(t(template_csv_subset))],
                                          collapse = ", "),
                                    ". Please upload a .xlsx file with all the mandatory fields completed and numeric values for coordinates and antimicrobial resistance.",
                                    sep = ""),
                         duration = 15,
                         closeButton = FALSE,
                         type = "error")

      } else if (length(unique(template_csv$DOI)) != 1) {

        showNotification(ui = paste("The value of the DOI column (DOI, unique string or link to research item) should be the same for every row of the template."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else if (length(unique(template_csv$ISO3)) != 1) {

        showNotification(ui = paste("The value of the ISO3 column should be the same for every row of the template."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else if (any(template_csv$Rescom > 100) == TRUE) {

        showNotification(ui = paste("The numbers inside the Rescom column should have values that range from 0 to 100."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else {

        if (length(colnames(template_csv)) > length(correct_names$template_csv_colnames)) {

          template_csv <- subset(x = template_csv,
                                 select = c(correct_names$template_csv_colnames))

        }

        showNotification(ui = "Thank you, your contribution has been submitted successfully! Your data will be added to Resistance bank once approved by the curator.",
                         duration = 10,
                         closeButton = FALSE,
                         type = "default")

  # 3.7.2 ###= Clean and rename fields and save template in Dropbox =#####

  # DOI
        template_csv$DOI <- template_csv$DOI %>%
          str_replace(pattern = "https://dx.doi.org/",
                      replacement = "") %>%
          str_replace(pattern = "http://dx.doi.org/",
                      replacement = "") %>%
          str_replace(pattern = "https://dx.doi/",
                      replacement = "") %>%
          str_replace(pattern = "https://doi.org/",
                      replacement = "") %>%
          str_replace(pattern = "https://dx.doi/",
                      replacement = "") %>%
          str_replace(pattern = "http://dx.doi/",
                      replacement = "") %>%
          str_replace(pattern = "HTTPS://DX.DOI.ORG/",
                      replacement = "") %>%
          str_replace(pattern = "HTTP://DX.DOI.ORG/",
                      replacement = "") %>%
          str_replace(pattern = "DX.DOI.ORG/",
                      replacement = "") %>%
          str_replace(pattern = "DX.DOI/",
                      replacement = "") %>%
          str_replace(pattern = "https://",
                      replacement = "") %>%
          str_replace(pattern = "http://",
                      replacement = "") %>%
          str_replace(pattern = "dx.doi.org/",
                      replacement = "") %>%
          str_replace(pattern = "dx.doi/",
                      replacement = "") %>%
          str_replace(pattern = "hps://doi.org/",
                      replacement = "") %>%
          str_replace(pattern = "hps://",
                      replacement = "") %>%
          str_replace(pattern = "doi.org/",
                      replacement = "") %>%
          str_replace_all(pattern = " ",
                          replacement = "")

  # Species
        template_csv$Species <- template_csv$Species %>%
          str_replace_all(pattern = " ",
                          replacement = "")

        template_csv$Species <- correct_names$Species[apply(X = adist(x = template_csv$Species,
                                                                      y = correct_names$Species),
                                                            MARGIN = 1,
                                                            FUN = which.min)]

  # SampleType
        template_csv$SampleType <- template_csv$SampleType %>%
          str_replace_all(pattern = ",",
                          replacement = "") %>%
          str_replace_all(pattern = "\\.",
                          replacement = "") %>%
          str_replace_all(pattern = " ",
                          replacement = "")

        template_csv$SampleType <- correct_names$SampleType[apply(X = adist(x = template_csv$SampleType,
                                                                            y = correct_names$SampleType),
                                                                  MARGIN = 1,
                                                                  FUN = which.min)]

  # Method
        template_csv$Method <- template_csv$Method %>%
          str_replace_all(pattern = " ",
                          replacement = "") %>%
          str_replace(pattern = "ad",
                      replacement = "AD") %>%
          str_replace(pattern = "Ad",
                      replacement = "AD") %>%
          str_replace(pattern = "aD",
                      replacement = "AD") %>%
          str_replace(pattern = "bd",
                      replacement = "BD") %>%
          str_replace(pattern = "Bd",
                      replacement = "BD") %>%
          str_replace(pattern = "bD",
                      replacement = "BD") %>%
          str_replace(pattern = "dd",
                      replacement = "DD") %>%
          str_replace(pattern = "Dd",
                      replacement = "DD") %>%
          str_replace(pattern = "dD",
                      replacement = "DD")

        template_csv$Method <- correct_names$Method[apply(X = adist(x = template_csv$Method,
                                                                    y = correct_names$Method),
                                                          MARGIN = 1,
                                                          FUN = which.min)]

  # Pathogens
        template_csv$Pathogens <- template_csv$Pathogens %>%
          str_replace_all(pattern = " ",
                          replacement = "")

        template_csv$Pathogens <- correct_names$Pathogens[apply(X = adist(x = template_csv$Pathogens,
                                                                          y = correct_names$Pathogens),
                                                                MARGIN = 1,
                                                                FUN = which.min)]

  # Compound
        template_csv$Compound <- sapply(X = 1:length(template_csv$Compound),
                                        FUN = function(i) {

                                          temporary_Antibiotics_df <- bind_rows(mutate_all(.tbl = Antibiotics_df,
                                                                                           .funs = as.character),
                                                                                mutate_all(.tbl = Antibiotics_df_common_names,
                                                                                           .funs = as.character))

                                          temporary_Compound_name <- temporary_Antibiotics_df$Name[apply(X = adist(x = template_csv$Compound[i],
                                                                                                                   y = temporary_Antibiotics_df$Name),
                                                                                                         MARGIN = 1,
                                                                                                         FUN = which.min)]

                                          if (temporary_Compound_name %in% c(Antibiotics_df_common_names$Name)) {

                                            temporary_Antibiotics_df$Code[temporary_Antibiotics_df$Name == temporary_Compound_name]

                                          } else {

                                            lev_distance <- adist(x = template_csv$Compound[i],
                                                                  y = correct_names$Compound)

                                            if (any(lev_distance < 12) == TRUE) { # Check the correct number!

                                              Antibiotics_df$Code[match(correct_names$Compound[apply(X = adist(x = template_csv$Compound[i],
                                                                                                               y = correct_names$Compound),
                                                                                                     MARGIN = 1,
                                                                                                     FUN = which.min)],
                                                                        Antibiotics_df$Name)]

                                            } else {

                                              template_csv$Compound[i]

                                            }

                                          }

                                        })

  # Save template in Dropbox
        save_template(template_csv)

  # 3.7.3 ###= Bibliographic research =#####
        if (nchar(unique(template_csv$DOI)) > 9) {

          EPMC_SUMMARY <- europepmc::epmc_search(query = unique(template_csv$DOI))

        } else {

          EPMC_SUMMARY <- NULL

        }

  # Title
        if (is.null(EPMC_SUMMARY)) {

          template_csv$Title <- "Report could not be linked to a peer-reviewed publication."

        } else {

          template_csv$Title <- EPMC_SUMMARY$title[[1]]

          template_csv$Title <- gsub(pattern = "&lt;i&gt;",
                                     replacement = "",
                                     x = template_csv$Title)

          template_csv$Title <- gsub(pattern = "&lt;/i&gt;",
                                     replacement = "",
                                     x = template_csv$Title)

        }

  # Journal
        if (is.null(EPMC_SUMMARY)) {

          template_csv$Journal <- "Report could not be linked to a peer-reviewed publication."

        } else {

          template_csv$Journal <- EPMC_SUMMARY$journalTitle[[1]]

        }

  # Author's name
        if (is.null(EPMC_SUMMARY)) {

          template_csv$Authors_name <- paste0(unique(template_csv$Author),
                                              " et al.")

        } else {

          template_csv$Authors_name <- EPMC_SUMMARY$authorString[[1]]

        }

  # URL
        DOI_URL <- try(crminer::crm_links(doi = unique(as.character(template_csv$DOI)),
                                          type = "all"))

        if (class(DOI_URL) != "try-error") {

          DOI_URL <- crminer::crm_links(doi = unique(as.character(template_csv$DOI)),
                                        type = "all")

          template_csv$URL <- ifelse(test = length(DOI_URL$html) == 1,
                                     yes = DOI_URL$html$html,
                                     no = ifelse(test = length(DOI_URL$pdf) == 1,
                                                 yes = DOI_URL$pdf$pdf,
                                                 no = ifelse(test = length(DOI_URL$unspecified) == 1,
                                                             yes = DOI_URL$unspecified$unspecified,
                                                             no = NA)))

        } else {

          template_csv$URL <- NA

        }

  # 3.7.4 ###= Aggregation for Rescom field to generate the plot =#####
        if (any(nchar(template_csv$Compound) > 3) == TRUE) {

          template_csv <- template_csv[nchar(template_csv$Compound) == 3, ]

          rownames(template_csv) <- seq(length = nrow(template_csv))

        }

        template_csv_list <- split(x = template_csv,
                                   f = list(template_csv$YCoord,
                                            template_csv$XCoord),
                                   drop = TRUE,
                                   lex.order = FALSE)

        temp_aws3_list <- list()

        for (i in 1:length(template_csv_list)) {

          template_csv_temp <- template_csv_list[[i]]

          study <- aggregate(x = list("Rescom" = template_csv_temp$Rescom),
                             by = list("Compound" = template_csv_temp$Compound,
                                       "Pathogens" = template_csv_temp$Pathogens,
                                       "Species" = template_csv_temp$Species),
                             FUN = mean,
                             na.rm = TRUE)

  # 3.7.5 ###= Plot of the user submitted data through template =#####
          source(file = "modules/server_unique-formplot.R",
                 local = TRUE)$value

  # Temporary plot caption with unique antibiotic codes and names
          Antibiotics_df_unique <- subset(x = Antibiotics_df,
                                          subset = Code %in% study$Compound)

  # Save the plot inside the user temporary folder
          imagePath <- file.path(tempdir(),
                                 paste0(substr(x = unique_templatename(),
                                               start = 1 ,
                                               stop = nchar(x = unique_templatename()) - 4),
                                        "_",
                                        i,
                                        ".svg"))

          ggsave(filename = imagePath,
                 plot = study_plot,
                 dpi = 106,
                 width = 11,
                 height = 12.83)

  # Upload the temporary plot inside the Amazon S3 bucket
          put_object(file = imagePath,
                     object = paste0(substr(x = unique_templatename(),
                                            start = 1 ,
                                            stop = nchar(x = unique_templatename()) - 4),
                                     "_",
                                     i,
                                     ".svg"),
                     bucket = "amr-hegep-bucket/temporary_amr_plots",
                     acl = c("public-read"))

  # 3.7.6 ###= Updates the 1 row file(s) from the form in AWS3 for displaying of temporary markers =#####
          survey_aws3 <- data.frame("Author" = unique(template_csv_temp$Author),
                                    "YCoord" = unique(template_csv_temp$YCoord),
                                    "XCoord" = unique(template_csv_temp$XCoord),
                                    "PubDate" = unique(template_csv_temp$PubDate),
                                    "Plot" = paste0("<img height='350' src='",
                                                    "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/temporary_amr_plots/",
                                                    substr(x = unique_templatename(),
                                                           start = 1,
                                                           stop = nchar(x = unique_templatename()) - 4),
                                                    "_",
                                                    i,
                                                    ".svg",
                                                    "'/>"),
                                    "Plot_caption" = paste(Antibiotics_df_unique$Combination,
                                                           collapse = ", "),
                                    "Title" = unique(template_csv_temp$Title),
                                    "Journal" = unique(template_csv_temp$Journal),
                                    "Authors_name" = unique(template_csv_temp$Authors_name),
                                    "URL" = unique(template_csv_temp$URL))

          templatePath <- file.path(tempdir(),
                                    paste0(substr(x = unique_templatename(),
                                                  start = 1 ,
                                                  stop = nchar(x = unique_templatename()) - 4),
                                           "_",
                                           i,
                                           ".csv"))

          write.table(x = survey_aws3,
                      file = templatePath,
                      row.names = FALSE,
                      quote = TRUE,
                      sep = ",")

          put_object(file = templatePath,
                     object = paste0(substr(x = unique_templatename(),
                                            start = 1 ,
                                            stop = nchar(x = unique_templatename()) - 4),
                                     "_",
                                     i,
                                     ".csv"),
                     bucket = "amr-hegep-bucket/temporary_responses",
                     acl = c("public-read"))

          temp_aws3_list[[i]] <- survey_aws3

        }

        temp_aws3_data <- do.call(what = rbind,
                                  args = temp_aws3_list)

  # 3.7.7 ###= Display new marker for template in leaflet =#####
        if (nrow(temp_aws3_data) == 1) {

          template_csv_center.lat <- unique(template_csv$YCoord)

          template_csv_center.lon <- unique(template_csv$XCoord)

          template_csv.zoom <- 7

          template_csv_bound1.lat <- template_csv_center.lat + 0.6

          template_csv_bound1.lon <- template_csv_center.lon - 1.3

          template_csv_bound2.lat <- template_csv_center.lat - 0.6

          template_csv_bound2.lon <- template_csv_center.lon + 1.3

        } else {

          template_csv_center.lat <- mean(template_csv$YCoord)

          template_csv_center.lon <- mean(template_csv$XCoord)

          template_csv.zoom <- 3

        }

  # Popup content based on bibliographic informations available
  popup_content.template <- paste("<div style='max-height:790px' align='justify'>",
                                  "<h3>",
                                  paste0(temp_aws3_data$Author,
                                         " et al.",
                                         ifelse(test = !is.na(temp_aws3_data$PubDate),
                                                yes = paste0(", ",
                                                             temp_aws3_data$PubDate),
                                                no = "")), "<br>",
                                  "</h3>",
                                  "<br/>",
                                  "<span style='font-size:12.7px'>",
                                  ifelse(test = temp_aws3_data$Title == "Report could not be linked to a peer-reviewed publication.",
                                         yes = paste("<em>Personal contribution from", paste0(temp_aws3_data$Author,
                                                                                              "."), "</em>"),
                                         no = paste("<strong>Title</strong>", ": ", temp_aws3_data$Title,
                                                    sep = "")), "<br>",
                                  "<br/>",
                                  ifelse(test = grepl(pattern = "et al.",
                                                      x = temp_aws3_data$Authors_name,
                                                      fixed = TRUE) == FALSE,
                                         yes = paste("<strong>Authors</strong>", ": ", temp_aws3_data$Authors_name, "<br>", "<br/>",
                                                     sep = ""),
                                         no = paste("<span style='font-size:0.5px'>",
                                                    " ",
                                                    "</span>")),
                                  ifelse(test = temp_aws3_data$Journal != "Report could not be linked to a peer-reviewed publication." &
                                           temp_aws3_data$Title != "Report could not be linked to a peer-reviewed publication.",
                                         yes = paste("<strong>Journal</strong>", ": ",
                                                     ifelse(test = is.na(temp_aws3_data$URL),
                                                            yes = temp_aws3_data$Journal,
                                                            no = paste0("<a href = '",
                                                                        temp_aws3_data$URL,
                                                                        "'",
                                                                        " target='_blank'>",
                                                                        temp_aws3_data$Journal,
                                                                        "</a>")), "<br>", "<br/>",
                                                     sep = ""),
                                         no = paste("<span style='font-size:0.5px'>",
                                                    " ",
                                                    "</span>")),
                                  temp_aws3_data$Plot, "<br>",
                                  "<br/>",
                                  "<div style='line-height:13px'>",
                                  "<strong>Antibiotics</strong>", ": ",
                                  "</span>",
                                  "<span style='color:#a8a4a4; font-size:11px'>",
                                  temp_aws3_data$Plot_caption,
                                  "</span>",
                                  "</div>",
                                  "</div>",
                                  sep = "")

  # Update leaflet map
        leaflet_template <- leafletProxy(mapId = "world_map") %>%

          addMapPane(name = "PPS_template",
                     zIndex = 620) %>%

          addMarkers(
            lat = temp_aws3_data$YCoord,
            lng = temp_aws3_data$XCoord,
            icon = marker_submission,
            label = paste0(temp_aws3_data$Author,
                           ifelse(is.na(temp_aws3_data$PubDate),
                                  yes = "",
                                  no = paste0(", ", temp_aws3_data$PubDate))),
            labelOptions = labelOptions(direction = "bottom",
                                        style = list(
                                          "font-size" = "12.7px"
                                        )),
            popup = popup_content.template,
            options = pathOptions(pane = "PPS_template"))

        if (nrow(temp_aws3_data) == 1) {

          leaflet_template %>%

            setView(lat = template_csv_center.lat,
                    lng = template_csv_center.lon,
                    zoom = template_csv.zoom) %>%

            fitBounds(lat1 = template_csv_bound1.lat,
                      lng1 = template_csv_bound1.lon,
                      lat2 = template_csv_bound2.lat,
                      lng2 = template_csv_bound2.lon)

        } else {

          leaflet_template %>%

            setView(lat = template_csv_center.lat,
                    lng = template_csv_center.lon,
                    zoom = template_csv.zoom)

        }

  # 3.7.8 ###= Notification in case of wrong coordinates =#####
        if (all(unique(template_csv$YCoord) == "47.367") == TRUE &
            all(unique(template_csv$XCoord) == "8.551") == TRUE) {

          showNotification(ui = paste("The geocode function could not retrieve the correct coordinates of your study, the curator will get in contact with you soon."),
                           duration = 12,
                           closeButton = FALSE,
                           type = "error")

        }

  # 3.7.9 ###= Send authomatic e-mail with template link =#####
        unique_templateurl <- drop_share(path = paste0("temporary_directory/",
                                                       unique_templatename()))

        email_template <- gmailr::gm_mime() %>%
          gmailr::gm_subject(paste0("NEW RESBANK SUBMISSION - ",
                                    substr(x = unique_templatename(),
                                           start = 1 ,
                                           stop = nchar(x = unique_templatename()) - 4))) %>%
          gmailr::gm_from("nico.criscuolo1618@gmail.com") %>%
          # gmailr::gm_to(c("nico.criscuolo981@gmail.com",
          #                 "joao.pires@env.ethz.ch")) %>%
          gmailr::gm_to("nico.criscuolo981@gmail.com") %>%
          gmailr::gm_text_body(paste0(substr(x = unique_templatename(),
                                             start = 1 ,
                                             stop = nchar(x = unique_templatename()) - 4),
                                      " ",
                                      unique_templateurl$url))

        gmailr::gm_send_message(email_template)

      }

    }

  })

  # 4. ##########========== DOWNLOAD DATA ==========####################

  # 4.1 ###= Download resistancebank from "initial panel" =#####
  output$download_RESBANK2 <- downloadHandler(

    filename <- function() {

      paste0("resistancebank",
             ".zip")

    },

    content <- function(file) {

      download.file(url = "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Downloads/resistancebank.zip",
                    destfile = file)

    download_RESBANK2_reactive_values$download_number <- download_RESBANK2_reactive_values$download_number + 1

  }, contentType = "application/zip")

  # 4.2 ###= Download resistancebank from "controls panel" =#####
  output$download_RESBANK <- downloadHandler(

    filename <- function() {

      paste0("resistancebank",
             ".zip")

    },

    content <- function(file) {

      download.file(url = "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Downloads/resistancebank.zip",
                    destfile = file)

    }, contentType = "application/zip")

  # 4.3 ###= Download P50 layer =#####
  output$download_P50 <- downloadHandler(

     filename <- function() {

       paste0("P50_raster",
              ".zip")

     },

     content <- function(file) {

       download.file(url = "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Downloads/P50_raster.zip",
                     destfile = file)

     }, contentType = "application/zip")

  # 4.4 ###= Download Country Report =#####

  # Toggle state for Download button that generates the country report
  observe({

    toggleState(id = "download_Country_report",
                condition = input$Country_report_nation != "")

  })

  # Functions to download the country report
  output$download_Country_report <- downloadHandler(

    filename <- function() {

      paste0("AMR report - ",
             subset(x = Countries_PPS,
                    subset = is.element(el = Country_ISO3,
                                        set = input$Country_report_nation))$Country,
             ".pdf")

    },

    content <- function(file) {

      withBusyIndicatorServer(buttonId = "download_Country_report", {

        tempdir_Country_report <- file.path(tempdir(), "Country_report.Rmd")

        tempdir_TimesNormal <- file.path(tempdir(), "Times.otf")

        tempdir_TimesBold <- file.path(tempdir(), "TimesBold.otf")

        file.copy(from = "Country_report.Rmd",
                  to = tempdir_Country_report,
                  overwrite = TRUE)

        file.copy(from = "Times.otf",
                  to = tempdir_TimesNormal,
                  overwrite = TRUE)

        file.copy(from = "TimesBold.otf",
                  to = tempdir_TimesBold,
                  overwrite = TRUE)

        params <- list(ISO3 = subset(x = Countries_PPS,
                                     subset = is.element(el = Country_ISO3,
                                                         set = input$Country_report_nation))$ISO3,
                       Country = subset(x = Countries_PPS,
                                        subset = is.element(el = Country_ISO3,
                                                            set = input$Country_report_nation))$Country,
                       Country_ISO3 = subset(x = Countries_PPS,
                                             subset = is.element(el = Country_ISO3,
                                                                 set = input$Country_report_nation))$Country_ISO3,
                       Continent = subset(x = Countries_PPS,
                                          subset = is.element(el = Country_ISO3,
                                                              set = input$Country_report_nation))$Continent,
                       Population =  subset(x = Countries_PPS,
                                            subset = is.element(el = Country_ISO3,
                                                                set = input$Country_report_nation))$Population_2018,
                       Density = subset(x = Countries_PPS,
                                        subset = is.element(el = Country_ISO3,
                                                            set = input$Country_report_nation))$Pop_density,
                       GDP = subset(x = Countries_PPS,
                                    subset = is.element(el = Country_ISO3,
                                                        set = input$Country_report_nation))$GDP_2016,
                       Antibiotics = subset(x = Countries_PPS,
                                            subset = is.element(el = Country_ISO3,
                                                                set = input$Country_report_nation))$Antibiotics_2013,
                       PAG_2030 = subset(x = Countries_PPS,
                                         subset = is.element(el = Country_ISO3,
                                                             set = input$Country_report_nation))$PAG_2030,
                       Cattle = subset(x = Countries_PPS,
                                       subset = is.element(el = Country_ISO3,
                                                           set = input$Country_report_nation))$Cattle,
                       Chickens = subset(x = Countries_PPS,
                                         subset = is.element(el = Country_ISO3,
                                                             set = input$Country_report_nation))$Chickens,
                       Pigs = subset(x = Countries_PPS,
                                     subset = is.element(el = Country_ISO3,
                                                         set = input$Country_report_nation))$Pigs,
                       resistancebank_surveys = subset(x = resistancebank_surveys,
                                                       subset = is.element(el = ISO3,
                                                                           set = subset(x = Countries_PPS,
                                                                                        subset = is.element(el = Country_ISO3,
                                                                                                            set = input$Country_report_nation))$ISO3)),
                       AMR_Exposure_data_rm = AMR_Exposure_data_rm(),
                       resistancebank_amr = subset(x = resistancebank_amr,
                                                   subset = is.element(el = ISO3,
                                                                       set = subset(x = Countries_PPS,
                                                                                    subset = is.element(el = Country_ISO3,
                                                                                                        set = input$Country_report_nation))$ISO3))
                       )

        rmarkdown::render(tempdir_Country_report,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))

      })

    })

  # Display the preview of the country report
  output$country_report_images <- renderUI({

    src = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Countries_information/Report_images/",
                 gsub(pattern = " ",
                      replacement = "+",
                      x = subset(x = Countries_PPS,
                                 subset = is.element(el = Country_ISO3,
                                                     set = input$Country_report_nation))$Country),
                 ".png")

    tags$img(src = src,
             width = "100%")

  })

  # 4.5 ###= Download subset of resistancebank data based on Country =#####
  output$download_Country_data <- downloadHandler(

    filename <- function() {

      paste0("resistancebank_",
             gsub(pattern = " ",
                  replacement = "",
                  subset(x = Countries_PPS,
                         subset = is.element(el = Country_ISO3,
                                             set = input$Country_report_nation))$Country),
             ".zip")

    },

    content <- function(file) {

      withBusyIndicatorServer(buttonId = "download_Country_data", {

        s3load(object = "resistancebank.rda",
               bucket = "amr-hegep-bucket")

        resistancebank_subset <- subset(x = resistancebank,
                                        subset = is.element(el = ISO3,
                                                            set = subset(x = Countries_PPS,
                                                                         subset = is.element(el = Country_ISO3,
                                                                                             set = input$Country_report_nation))$ISO3))

        country_name <- gsub(pattern = " ",
                             replacement = "",
                             x = subset(x = Countries_PPS,
                                        subset = is.element(el = Country_ISO3,
                                                            set = input$Country_report_nation))$Country)

        save_object(object = "Downloads/legend_resistancebank.docx",
                    bucket = "amr-hegep-bucket",
                    file = paste0(tempdir(),
                                  "/legend_resistancebank.docx"),
                    overwrite = TRUE)

        write.csv(x = resistancebank_subset,
                  file = paste0(tempdir(),
                                "/resistancebank_",
                                country_name,
                                ".csv"),
                  quote = TRUE,
                  row.names = FALSE)

        zip(zipfile = paste0(tempdir(),
                             "/resistancebank_",
                             country_name,
                             ".zip"),
            files = c(paste0(tempdir(),
                             "/legend_resistancebank.docx"),
                      paste0(tempdir(),
                             "/resistancebank_",
                             country_name,
                             ".csv")),
            extras = "-j")

        put_object(file = paste0(tempdir(),
                                 "/resistancebank_",
                                 country_name,
                                 ".zip"),
                   object = paste0("resistancebank_",
                                   country_name,
                                   ".zip"),
                   bucket = "amr-hegep-bucket/Downloads",
                   acl = c("public-read"))

        download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Downloads/",
                                   "resistancebank_",
                                   country_name,
                                   ".zip"),
                      destfile = file)

        delete_object(object = paste0("Downloads/",
                                      "resistancebank_",
                                      country_name,
                                      ".zip"),
                      bucket = "amr-hegep-bucket")

      })

    }, contentType = "application/zip")

  # 4.6 ###= Download resistancebank template =#####
  output$download_resistancebank_template <- downloadHandler(

    filename <- function() {

      paste0("resistancebank_template",
             ".zip")

    },

    content <- function(file) {

      download.file(url = "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/Downloads/resistancebank_template.zip",
                    destfile = file)

    }, contentType = "application/zip")

  #########################=========================#########################

} # Closes SERVER

