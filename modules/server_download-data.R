
  # 4.1 #####==== Download resistancebank database and AMR map from the "Initial" and "Controls" panel =====#####

  # 4.1.2 ###= Activate the download button and display feedback in case of wrong email =#####

  # 4.1.2.1 ###= Resistancebank databases =#####
  observe({

    if (input$RESBANK_downloader_name != "" &
        input$RESBANK_downloader_country != "" &
        input$RESBANK_downloader_institution != "" &
        input$RESBANK_downloader_email != "") {

      if (email_validation_function(email = input$RESBANK_downloader_email)) {enable(id = "download_RESBANK")} else {disable(id = "download_RESBANK")}
      if (input$RESBANK_downloader_name == "NA") {disable(id = "download_RESBANK")}
      if (input$RESBANK_downloader_institution == "NA") {disable(id = "download_RESBANK")}

    } else {

      disable(id = "download_RESBANK")

    }

  })

  output$wrong_RESBANK_downloader_email <- renderText({

    req(input$RESBANK_downloader_email)

    feedbackWarning(inputId = "RESBANK_downloader_email",
                    show = !email_validation_function(email = input$RESBANK_downloader_email),
                    text = "Please enter a valid email address",
                    color = "red",
                    icon = NULL)

  })

  # 4.1.2.2 ###= P50 maps =#####
  observe({

    if (input$P50_downloader_name != "" &
        input$P50_downloader_country != "" &
        input$P50_downloader_institution != "" &
        input$P50_downloader_email != "") {

      if (email_validation_function(email = input$P50_downloader_email)) {enable(id = "download_P50")} else {disable(id = "download_P50")}
      if (input$P50_downloader_name == "NA") {disable(id = "download_P50")}
      if (input$P50_downloader_institution == "NA") {disable(id = "download_P50")}

    } else {

      disable(id = "download_P50")

    }

  })

  output$wrong_P50_downloader_email <- renderText({

    req(input$P50_downloader_email)

    feedbackWarning(inputId = "P50_downloader_email",
                    show = !email_validation_function(email = input$P50_downloader_email),
                    text = "Please enter a valid email address",
                    color = "red",
                    icon = NULL)

  })

  # 4.1.3 ###= Download the databases and maps and upload user's information in AWS S3 =#####

  # 4.1.3.1 ###= Resistancebank databases =#####
  output$download_RESBANK <- downloadHandler(

    filename = function() {

        if (input$switch_farming_type == "livestock") {

          paste0("resistancebank_livestock.zip")

        } else if (input$switch_farming_type == "freshwater") {

          paste0("resistancebank_freshwater.zip")

        } else if (input$switch_farming_type == "marine") {

          paste0("resistancebank_marine.zip")

        }

    },

    content = function(file) {

      withBusyIndicatorServer(buttonId = "download_RESBANK", {

        #
        if (input$switch_farming_type == "livestock") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/datasets/",
                                     version,
                                     "/resistancebank_livestock.zip"),
                        destfile = file)

        } else if (input$switch_farming_type == "freshwater") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/datasets/",
                                     version,
                                     "/resistancebank_freshwater.zip"),
                        destfile = file)

        } else if (input$switch_farming_type == "marine") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/datasets/",
                                     version,
                                     "/resistancebank_marine.zip"),
                        destfile = file)

        }

        #
        RESBANK_downloader_data <- data.frame("time" = format(x = Sys.time(),
                                                              format = "%Y%m%d_%H%M"),
                                              "name" = as.character(input$RESBANK_downloader_name),
                                              "country" = as.character(input$RESBANK_downloader_country),
                                              "institution" = as.character(input$RESBANK_downloader_institution),
                                              "email" = as.character(input$RESBANK_downloader_email),
                                              "type" = "database",
                                              "animals" = input$switch_farming_type)

        RESBANK_downloader_data_string <- paste0(paste(names(RESBANK_downloader_data),
                                                       collapse = ","),
                                                 "\n",
                                                 paste(unname(RESBANK_downloader_data),
                                                       collapse = ","))

        put_object(file = charToRaw(RESBANK_downloader_data_string),
                   object = paste0(RESBANK_downloader_data$time,
                                   "_",
                                   substr(x = digest::digest(RESBANK_downloader_data_string,
                                                             algo = "md5"),
                                          start = 1,
                                          stop = 15),
                                   ".csv"),
                   bucket = "amr-hegep-bucket/resistancebank_downloaders",
                   acl = c("public-read"))

      })

      # Reset input fields and hide the "download_RESBANK_panel" element
      Sys.sleep(time = 1)

      reset(id = "RESBANK_downloader_name")
      reset(id = "RESBANK_downloader_country")
      reset(id = "RESBANK_downloader_institution")
      reset(id = "RESBANK_downloader_email")

      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)

    },

    contentType = "application/zip")

  # 4.1.3.2 ###= P50 maps =#####
  output$download_P50 <- downloadHandler(

    filename = function() {

      if (input$switch_farming_type == "livestock") {

        paste0("P50_livestock_raster.zip")

      } else if (input$switch_farming_type == "freshwater") {

        paste0("P50_freshwater_raster.zip")

      } else if (input$switch_farming_type == "marine") {

        paste0("P50_marine_raster.zip")

      }

    },

    content = function(file) {

      withBusyIndicatorServer(buttonId = "download_P50", {

        if (input$switch_farming_type == "livestock") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                     version,
                                     "/P50_livestock_raster.zip"),
                        destfile = file)

        } else if (input$switch_farming_type == "freshwater") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                     version,
                                     "/P50_freshwater_raster.zip"),
                        destfile = file)

        } else if (input$switch_farming_type == "marine") {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                     version,
                                     "/P50_marine_raster.zip"),
                        destfile = file)

        }

        #
        P50_downloader_data <- data.frame("time" = format(x = Sys.time(),
                                                          format = "%Y%m%d_%H%M"),
                                          "name" = as.character(input$P50_downloader_name),
                                          "country" = as.character(input$P50_downloader_country),
                                          "institution" = as.character(input$P50_downloader_institution),
                                          "email" = as.character(input$P50_downloader_email),
                                          "type" = "raster",
                                          "animals" = input$switch_farming_type)

        P50_downloader_data_string <- paste0(paste(names(P50_downloader_data),
                                                   collapse = ","),
                                             "\n",
                                             paste(unname(P50_downloader_data),
                                                   collapse = ","))

        put_object(file = charToRaw(P50_downloader_data_string),
                   object = paste0(P50_downloader_data$time,
                                   "_",
                                   substr(x = digest::digest(P50_downloader_data_string,
                                                             algo = "md5"),
                                          start = 1,
                                          stop = 15),
                                   ".csv"),
                   bucket = "amr-hegep-bucket/resistancebank_downloaders",
                   acl = c("public-read"))

    })

    # Reset input fields and hide the "download_P50_panel" element
    Sys.sleep(time = 1)

    reset(id = "P50_downloader_name")
    reset(id = "P50_downloader_country")
    reset(id = "P50_downloader_institution")
    reset(id = "P50_downloader_email")

    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)

    },

    contentType = "application/zip")

  # 4.2 #####==== Download the resistancebank template =====#####
  output$download_template <- downloadHandler(

    filename <- function() {

      paste0("resistancebank_template.zip")

    },

    content <- function(file) {

      download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                 version,
                                 "/resistancebank_template.zip"),
                    destfile = file)

    }, contentType = "application/zip")

  # 4.3 #####==== Download and visualize resistancebank data from the "Filter data" panel =====#####

  # 4.3.1 ###= Display download buttons, informations on filtered data, and error message =#####
  output$filterdata_buttons_and_info <- renderUI({

    if (nrow(resistancebank_filtered()) == nrow(resistancebank) &
        all(is.null(c(input$filterdata_ISO3,
                      input$filterdata_species,
                      input$filterdata_sample_type,
                      input$filterdata_pathogen,
                      input$filterdata_compound,
                      input$filterdata_compound_class,
                      input$filterdata_compound_agisar)))) {

      return(NULL)

    } else if (!all(c(input$filterdata_ISO3) %in% resistancebank_filtered()$ISO3) |
               !all(c(input$filterdata_species) %in% resistancebank_filtered()$Species) |
               !all(c(input$filterdata_sample_type) %in% resistancebank_filtered()$SampleType) |
               !all(c(input$filterdata_pathogen) %in% resistancebank_filtered()$Pathogens) |
               !all(c(input$filterdata_compound) %in% resistancebank_filtered()$Compound) |
               !all(c(input$filterdata_compound_class) %in% resistancebank_filtered()$Class_WHO) |
               !all(c(input$filterdata_compound_agisar) %in% resistancebank_filtered()$pathogen_compound)) {

      tags$div(style = "text-align: center;
                          padding: 10px 0px 0px 0px;",
               tags$img(src = "filter-figures/error_message.png",
                        width = "53.5%")
      )


    } else {

      tags$table(style = "width: 100%;",
                 tags$tr(tags$td(style = "width: 54%;
                                            padding-top: 10px;",
                                 align = "left",
                                 tags$div(p("Database records: ",
                                            style = "display: inline-block;"),
                                          h5(textOutput(outputId = "resistancebank_filtered_rows"),
                                             style = "display: inline-block;"))
                 ),
                 tags$td(style = "width: 46%;
                                            padding-top: 10px;",
                         align = "center",
                         withBusyIndicatorUI(
                           downloadButton(outputId = "download_RESBANK_filtered",
                                          label = h5({if (nrow(resistancebank_filtered()) == nrow(resistancebank) &
                                                          any(!is.null(c(input$filterdata_ISO3,
                                                                         input$filterdata_species,
                                                                         input$filterdata_sample_type,
                                                                         input$filterdata_pathogen,
                                                                         input$filterdata_compound,
                                                                         input$filterdata_compound_class,
                                                                         input$filterdata_compound_agisar)))) {

                                            "Resistancebank"

                                          } else {

                                            "Download data"

                                          }
                                          },
                                          style = "display: inline-block;"),
                                          style = "box-shadow: none!important;
                                                             outline: 0;
                                                             border-width: 0px;
                                                             padding: 0px 5px 0px 5px;"))
                 )
                 ),
                 tags$tr(tags$td(style = "width: 54%;",
                                 align = "left",
                                 tags$div(p("Peer-reviewed studies: ",
                                            style = "display: inline-block;"),
                                          h5(textOutput(outputId = "resistancebank_filtered_studies"),
                                             style = "display: inline-block;"))
                 ),
                 tags$td(style = "width: 46%;
                                  padding-right: 12px;",
                         align = "center",
                         actionButton(inputId = "display_filtered_data",
                                      label = tags$div(icon(name = "map-location-dot",
                                                            lib = "font-awesome"),
                                                       h5("Show on map",
                                                          style = "display: inline-block;")),
                                      style = "box-shadow: none!important;
                                                         outline: 0;
                                                         border-width: 0px;
                                                         padding: 0px 5px 0px 5px;")
                 )
                 )
      )

    }

  })

  # 4.3.2 ###= Functions to download resistancebank after filtering the data =#####
  output$download_RESBANK_filtered <- downloadHandler(

    filename <- function() {

      if (nrow(resistancebank_filtered()) == nrow(resistancebank)) {

        paste0("resistancebank.zip")

      } else {

        paste0("resistancebank_filtered.zip")

      }

    },

    content <- function(file) {

      withBusyIndicatorServer(buttonId = "download_RESBANK_filtered", {

        resistancebank_filtered <- resistancebank_filtered()

        save_object(object = paste0("downloads/",
                                    version,
                                    "/legend_resistancebank_livestock.docx"),
                    bucket = "amr-hegep-bucket",
                    file = paste0(tempdir(),
                                  "/legend_resistancebank_livestock.docx"),
                    overwrite = TRUE)

        resistancebank_filtered$Class_WHO <- NULL
        resistancebank_filtered$pathogen_compound <- NULL

        if (nrow(resistancebank_filtered()) == nrow(resistancebank)) {

          download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/datasets/",
                                     version,
                                     "/resistancebank.zip"),
                        destfile = file)

        } else {

          readr::write_excel_csv(x = resistancebank_filtered,
                                 file = paste0(tempdir(),
                                               "/resistancebank_filtered.csv"))

          zip(zipfile = paste0(tempdir(),
                               "/resistancebank_filtered.zip"),
              files = c(paste0(tempdir(),
                               "/legend_resistancebank_livestock.docx"),
                        paste0(tempdir(),
                               "/resistancebank_filtered.csv")),
              extras = "-j")

          file.remove(paste0(tempdir(),
                             "/resistancebank_filtered.csv"))

          put_object(file = paste0(tempdir(),
                                   "/resistancebank_filtered.zip"),
                     object = paste0("resistancebank_filtered.zip"),
                     bucket = "amr-hegep-bucket/downloads",
                     acl = c("public-read"))

          download.file(url = "https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/resistancebank_filtered.zip",
                        destfile = file)

          delete_object(object = paste0("downloads/",
                                        "resistancebank_filtered.zip"),
                        bucket = "amr-hegep-bucket")

          file.remove(paste0(tempdir(),
                             "/legend_resistancebank_livestock.docx"))

        }

      })

    }, contentType = "application/zip")

  # 4.4 #####===== Download Country Report =====#####

  # Reactive AMR_Exposure dataset based on coutry selection when producing the country report
  source(file = "modules/server_international-P50-level.R",
         local = TRUE)$value

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

        download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                   version,
                                   "/country_reports/PDFs/",
                                   gsub(pattern = " ",
                                        replacement = "+",
                                        x = subset(x = Countries_PPS,
                                                   subset = is.element(el = Country_ISO3,
                                                                       set = input$Country_report_nation))$Country),
                                   ".pdf"),
                      destfile = file)

      })

    })

  # Display previews of the country reports
  output$country_report_previews <- renderUI({

    src = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                 version,
                 "/country_reports/previews/",
                 gsub(pattern = " ",
                      replacement = "+",
                      x = subset(x = Countries_PPS,
                                 subset = is.element(el = Country_ISO3,
                                                     set = input$Country_report_nation))$Country),
                 ".png")

    tags$img(src = src,
             width = "100%")

  })

  # 4.6.1 ###= Download subset of resistancebank data based on Country =###
  output$download_Country_data <- downloadHandler(

    filename <- function() {

      paste0("resistancebank_livestock_",
             gsub(pattern = " ",
                  replacement = "",
                  subset(x = Countries_PPS,
                         subset = is.element(el = Country_ISO3,
                                             set = input$Country_report_nation))$Country),
             ".zip")

    },

    content <- function(file) {

      withBusyIndicatorServer(buttonId = "download_Country_data", {

        resistancebank_subset <- subset(x = resistancebank,
                                        subset = is.element(el = ISO3,
                                                            set = subset(x = Countries_PPS,
                                                                         subset = is.element(el = Country_ISO3,
                                                                                             set = input$Country_report_nation))$ISO3))

        resistancebank_subset$Class_WHO <- NULL
        resistancebank_subset$pathogen_compound <- NULL

        country_name <- gsub(pattern = " ",
                             replacement = "",
                             x = subset(x = Countries_PPS,
                                        subset = is.element(el = Country_ISO3,
                                                            set = input$Country_report_nation))$Country)

        save_object(object = paste0("downloads/",
                                    version,
                                    "/legend_resistancebank_livestock.docx"),
                    bucket = "amr-hegep-bucket",
                    file = paste0(tempdir(),
                                  "/legend_resistancebank_livestock.docx"),
                    overwrite = TRUE)

        readr::write_excel_csv(x = resistancebank_subset,
                               file = paste0(tempdir(),
                                             "/resistancebank_livestock_",
                                             country_name,
                                             ".csv"))

        zip(zipfile = paste0(tempdir(),
                             "/resistancebank_livestock_",
                             country_name,
                             ".zip"),
            files = c(paste0(tempdir(),
                             "/legend_resistancebank_livestock.docx"),
                      paste0(tempdir(),
                             "/resistancebank_livestock_",
                             country_name,
                             ".csv")),
            extras = "-j")

        put_object(file = paste0(tempdir(),
                                 "/resistancebank_livestock_",
                                 country_name,
                                 ".zip"),
                   object = paste0("resistancebank_livestock_",
                                   country_name,
                                   ".zip"),
                   bucket = paste0("amr-hegep-bucket/downloads/",
                                   version),
                   acl = c("public-read"))

        download.file(url = paste0("https://amr-hegep-bucket.s3.eu-central-1.amazonaws.com/downloads/",
                                   version,
                                   "/resistancebank_livestock_",
                                   country_name,
                                   ".zip"),
                      destfile = file)

        delete_object(object = paste0("downloads/",
                                      version,
                                      "/resistancebank_livestock_",
                                      country_name,
                                      ".zip"),
                      bucket = "amr-hegep-bucket")

        file.remove(paste0(tempdir(),
                           "/legend_resistancebank_livestock.docx"))

      })

    }, contentType = "application/zip")
