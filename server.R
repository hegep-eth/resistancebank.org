
server <- function(input, output, session) {

  # If the application is in testing mode, once the app is launched it stops the RStudio console by simply closing the browser
  if (testing_mode == TRUE) {source(file = "modules/stop_through_browser.R", local = TRUE)$value}

  # 1. ##########========== OPERATIONS ON PANELS AND DATASETS ==========##########

  # 1.1 #####===== Display and hide panels =====#####
  source(file = "modules/server_display-hide-panels.R",
         local = TRUE)$value

  # 1.2 #####===== Create the reactive resistancebank filtered datasets =====#####





  #############################################################################
  # s3load("resistancebank.rda",
  #        bucket = "amr-hegep-bucket")
  #
  # s3load("resistancebank_surveys.rda",
  #        bucket = "amr-hegep-bucket")
  #
  # resistancebank_filtered <- resistancebank %>%
  #   filter(Class_WHO %in% "Penicillins (Critically important)") %>%
  #   as.data.frame()
  #
  # str(resistancebank_filtered)
  #
  # resistancebank_surveys_filtered <- resistancebank_filtered[!duplicated(resistancebank_filtered[c("DOI",
  #                                                                                                  "XCoord",
  #                                                                                                  "YCoord")]), ]
  #
  # resistancebank_surveys_filtered <- merge(x = resistancebank_surveys_filtered[, c("DOI",
  #                                                                                  "XCoord",
  #                                                                                  "YCoord",
  #                                                                                  "PubDate")],
  #                                          y = resistancebank_surveys,
  #                                          by = c("DOI",
  #                                                 "XCoord",
  #                                                 "YCoord",
  #                                                 "PubDate"),
  #                                          all.x = TRUE,
  #                                          all.y = FALSE,
  #                                          sort = FALSE)
  #
  # unique(resistancebank$Class_WHO)
  #
  # if (all(as.character(resistancebank_surveys_filtered$Journal) == "Report could not be linked to a peer-reviewed publication.") == TRUE) {
  #
  #   print("Yes")
  #
  # } else {
  #
  #   print ("NO")
  # }
  #############################################################################





  # 1.2.1 ###= resistancebank filtered dataset =#####
  resistancebank_filtered <- reactive({

    resistancebank_filtered <- resistancebank %>%
      filter(ISO3 %in% {if (is.null(input$filterdata_ISO3)) {unique(resistancebank$ISO3)} else {c(input$filterdata_ISO3)}} &
             Species %in% {if (is.null(input$filterdata_species)) {unique(resistancebank$Species)} else {c(input$filterdata_species)}} &
             SampleType %in% {if (is.null(input$filterdata_sample_type)) {unique(resistancebank$SampleType)} else {c(input$filterdata_sample_type)}} &
             Pathogens %in% {if (is.null(input$filterdata_pathogen)) {unique(resistancebank$Pathogens)} else {c(input$filterdata_pathogen)}} &
             Compound %in% {if (is.null(input$filterdata_compound)) {unique(resistancebank$Compound)} else {c(input$filterdata_compound)}} &
             Class_WHO %in% {if (is.null(input$filterdata_compound_class)) {unique(resistancebank$Class_WHO)} else {c(input$filterdata_compound_class)}} &
             pathogen_compound %in% {if (is.null(input$filterdata_compound_agisar)) {unique(resistancebank$pathogen_compound)} else {c(input$filterdata_compound_agisar)}})

    if (nrow(resistancebank_filtered) != 0) {

      rownames(resistancebank_filtered) <- seq(from = 1,
                                               to = nrow(resistancebank_filtered),
                                               by = 1)

    }

    return(resistancebank_filtered)

  })

  # 1.2.2 ###= resistancebank surveys filtered dataset =#####
  resistancebank_surveys_filtered <- reactive({

    resistancebank_surveys_filtered <- resistancebank_filtered()[!duplicated(resistancebank_filtered()[c("DOI",
                                                                                                         "XCoord",
                                                                                                         "YCoord")]), ]

    resistancebank_surveys_filtered <- merge(x = resistancebank_surveys_filtered[, c("DOI",
                                                                                     "XCoord",
                                                                                     "YCoord",
                                                                                     "PubDate")],
                                             y = resistancebank_surveys,
                                             by = c("DOI",
                                                    "XCoord",
                                                    "YCoord",
                                                    "PubDate"),
                                             all.x = TRUE,
                                             all.y = FALSE,
                                             sort = FALSE)

    return(resistancebank_surveys_filtered)

  })

  # 1.2.3 ###= resistancebank bibliography filtered dataset =#####
  resistancebank_bibliography <- reactive({

    resistancebank_bibliography <- resistancebank_surveys_filtered()[resistancebank_surveys_filtered()$Journal != "Report could not be linked to a peer-reviewed publication.", ]

    if (nrow(resistancebank_bibliography) == 0) {

      resistancebank_bibliography <- data.frame()

    } else {

      resistancebank_bibliography <- data.frame("Author" = paste0(resistancebank_bibliography$Author,
                                                                  " et al."),
                                                "Title" = resistancebank_bibliography$Title,
                                                "Journal" = paste0('<a href="',
                                                                   resistancebank_bibliography$URL,
                                                                   '" target="_blank">',
                                                                   resistancebank_bibliography$Journal,
                                                                   '</a>'),
                                                "Year" = resistancebank_bibliography$PubDate)

      resistancebank_bibliography <- resistancebank_bibliography[!duplicated(resistancebank_bibliography[c("Author",
                                                                                                           "Title",
                                                                                                           "Journal")]), ]

      resistancebank_bibliography <- resistancebank_bibliography[order(resistancebank_bibliography$Year,
                                                                       decreasing = TRUE), ]

      rownames(resistancebank_bibliography) <- seq(from = 1,
                                                   to = nrow(resistancebank_bibliography),
                                                   by = 1)

    }

    return(resistancebank_bibliography)

  })

  # 1.2.4 ###= Display the reactive table of every unique peer-reviewed paper available after filtering of resistancebank =#####
  output$filterdata_papers <- DT::renderDataTable({

    if (nrow(resistancebank_filtered()) == nrow(resistancebank) &
        all(is.null(c(input$filterdata_ISO3,
                      input$filterdata_species,
                      input$filterdata_sample_type,
                      input$filterdata_pathogen,
                      input$filterdata_compound,
                      input$filterdata_compound_class,
                      input$filterdata_compound_agisar)))) {

      DT::datatable(data.frame("Message" = "Filter the resistancebank.org database to display peer-reviewed studies associated with your selection."),
                    rownames = FALSE,
                    colnames = " ",
                    selection = "none",
                    options = list(dom = "t"))

    } else if (nrow(resistancebank_filtered()) > 0 &
               nrow(resistancebank_bibliography()) == 0) {

      DT::datatable(data.frame("Message" = "There are no peer-reviewed studies in the resistancebank.org database associated with your selection."),
                    rownames = FALSE,
                    colnames = " ",
                    selection = "none",
                    options = list(dom = "t")) %>%
        DT::formatStyle(columns = "Message",
                        color = "#A85458",
                        backgroundColor = "#E9E0E1")

    } else if (!all(c(input$filterdata_ISO3) %in% resistancebank_filtered()$ISO3) |
               !all(c(input$filterdata_species) %in% resistancebank_filtered()$Species) |
               !all(c(input$filterdata_sample_type) %in% resistancebank_filtered()$SampleType) |
               !all(c(input$filterdata_pathogen) %in% resistancebank_filtered()$Pathogens) |
               !all(c(input$filterdata_compound) %in% resistancebank_filtered()$Compound) |
               !all(c(input$filterdata_compound_class) %in% resistancebank_filtered()$Class_WHO) |
               !all(c(input$filterdata_compound_agisar) %in% resistancebank_filtered()$pathogen_compound)) {

      DT::datatable(data.frame())

    } else {

      DT::datatable(resistancebank_bibliography(),
                    escape = FALSE,
                    rownames = FALSE,
                    selection = "none",
                    options = list(scrollY = "187px",
                                   paging = FALSE,
                                   lengthChange = FALSE,
                                   info = FALSE,
                                   deferRender = TRUE,
                                   autoWidth = TRUE
                                   # columnDefs = list(list(width = "500px",
                                   #                        targets = c("Title")))
                                   )
                    ) %>%
        DT::formatStyle(columns = c("Author",
                                    "Title",
                                    "Journal",
                                    "Year"),
                        color = "black",
                        backgroundColor = "white",
                        fontWeight = "normal",
                        fontSize = "80%")

    }

  })

  # 1.2.5 ###= Display information about the reactive resistancebank datasets =#####
  output$resistancebank_filtered_rows <- renderText({

    format(x = nrow(resistancebank_filtered()),
           big.mark = ",")

  })

  output$resistancebank_filtered_studies <- renderText({

    format(x = nrow(resistancebank_bibliography()),
           big.mark = ",")

  })

  # 2. ##########========== WORLD MAP =========##########
  source(file = "modules/server_world-map.R",
         local = TRUE)$value

  # 3. ##########========== DATA SUBMISSION ==========##########

  # 3.1.1 #####=== Operation on DOIs ===#####

  # 3.1.1.1 ###= Replacement of wrong parts of DOIs =#####
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

  # 3.1.1.2 ###= Bibliographic research =#####
  observe({

    if (DOI() != "" & nchar(DOI()) > 9) {

      refmanager_summary <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = DOI())))

      if (class(refmanager_summary) != "try-error") {

        if (is.null(refmanager_summary)) {

          journal_name.form <- ""
          paper_title.form <- ""
          publication_year.form <- c(1984:as.numeric(format(Sys.Date(), "%Y")) + 1)

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

          journal_name.form <- refmanager_summary$journal
          paper_title.form <- refmanager_summary$title
          publication_year.form <- refmanager_summary$year

          updateTextInput(session = session,
                          inputId = "journal",
                          value = journal_name.form)

          updateTextInput(session = session,
                          inputId = "paper_title",
                          value = paper_title.form)

          updateSelectizeInput(session = session,
                               inputId = "PubDate",
                               choices = c(1984:as.numeric(format(Sys.Date(), "%Y")) + 1),
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
                                         1984:as.numeric(format(Sys.Date(), "%Y")) + 1),
                             selected = "")

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
                                       1984:as.numeric(format(Sys.Date(), "%Y")) + 1),
                           selected = "")

    }

  })

  # 3.2.1 ###= Dynamic UI for AMR ####
  source(file = "modules/server_antimicrobial-resistance-form.R",
         local = TRUE)$value

  # 3.3 ###############===== Form and Template datasets =====###############

  # 3.3.1 #####=== Data input from Form ===#####
  formData <- reactive({

  # Reverse geocoding to obtain coordinates
    coordinates <- as.data.frame(ggmap::geocode(location = input$coordinates))

    if (is.na(coordinates$lat) |
        is.na(coordinates$lon)) {

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
                                "ISO3" = rep(x = input$ISO3,
                                             times = nrow(response_amr())),
                                "study_address" = rep(x = gsub(pattern = ",",
                                                               replacement = "",
                                                               x = input$coordinates),
                                                      times = nrow(response_amr())),
                                "YCoord" = rep(x = coordinates$lat,
                                               times = nrow(response_amr())),
                                "XCoord" = rep(x = coordinates$lon,
                                               times = nrow(response_amr())),
                                "SamplingScheme" = rep(x = input$sampling_scheme,
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
                                                                               sheet = "Template",
                                                                               col_names = TRUE,
                                                                               na  = c("",
                                                                                       " ",
                                                                                       "NA")))

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

  # Define function to save the form in Dropbox
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

  # Define function to save the template in Dropbox
  save_template <- function(data) {

    filePath <- file.path(tempdir(),
                          unique_templatename())

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

  # 3.6.1 #####===== Check if author's email is valid =====#####
    if (grepl(pattern = "@",
              x = input$author_email,
              fixed = TRUE) == FALSE |
        str_count(string = input$author_email,
                  pattern = "@") != 1) {

      toggleState(id = "submit")

      showNotification(ui = paste("The e-mail address you have inserted is not valid."),
                       duration = 12,
                       closeButton = FALSE,
                       type = "error")

      Sys.sleep(time = 5)

      reset(id = "author_email")

    } else {

  # 3.6.2 ###= Preliminary operations =#####
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

      # Add URL if DOI is available
      refmanager_summary <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = unique(form_csv$DOI))))

      if (class(refmanager_summary) != "try-error") {

        if (is.null(refmanager_summary$url)) {

          form_csv$URL <- as.character(NA)

        } else {

          form_csv$URL <- as.character(refmanager_summary$url)

        }

      } else {

        form_csv$URL <- as.character(NA)

      }

      # Add the DataEntryPerson variable to the form
      form_csv$DataEntryPerson <- paste0(unique(form_csv$Author),
                                         "_",
                                         gsub(pattern = "-",
                                              replacement = "",
                                              x = Sys.Date()),
                                         "_",
                                         "FORM_SUBMISSION")

      # Add the variables "name_of_location", "level_of_uncertainty", and "radius" as (NA)
      form_csv$name_of_location <- as.character(NA)
      form_csv$level_of_uncertainty <- as.character(NA)
      form_csv$radius <- as.numeric(NA)

      # Add the variables "StartDate", "EndDate", "Strain", and "Concg" as NA (for now)
      form_csv$StartDate <- as.Date(NA)
      form_csv$EndDate <- as.Date(NA)
      form_csv$Strain <- as.character(NA)
      form_csv$Concg <- as.character(NA)

      # Save form to Dropbox
      save_form(form_csv)

  # 3.8.3 ###= "Thank you" notification =#####
      showNotification(ui = paste("Thank you! Your contribution has been submitted successfully. Your data will be added to resistancebank.org once approved by the curator."),
                       duration = 10,
                       closeButton = FALSE,
                       type = "default")

  # 3.8.4 ###= Bibliographic research =#####

  # Title
      if (is.na(unique(form_csv$Title))) {

        form_csv$Title <- "Report could not be linked to a peer-reviewed publication."

      } else {

        form_csv$Title <- form_csv$Title %>%
          gsub(pattern = "&lt;i&gt;",
               replacement = "") %>%
          gsub(pattern = "&lt;/i&gt;",
               replacement = "")

      }

  # Journal
      if (is.na(unique(form_csv$Journal))) {

        form_csv$Journal <- "Report could not be linked to a peer-reviewed publication."

      }

  # Authors name
      if (!is.na(unique(form_csv$DOI)) & nchar(as.character(unique(form_csv$DOI))) > 9) {

        refmanager_summary <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = unique(form_csv$DOI))))

        if (class(refmanager_summary) != "try-error") {

          if (is.null(refmanager_summary)) {

            form_csv$Authors_name <- paste0(unique(form_csv$author_name),
                                            " ",
                                            unique(form_csv$Author),
                                            " et al.")

          } else {

            form_csv$Authors_name <- paste0(refmanager_summary$author,
                                            collapse = ", ")

          }

        } else {

          form_csv$Authors_name <- paste0(unique(form_csv$author_name),
                                          " ",
                                          unique(form_csv$Author),
                                          " et al.")

        }

      } else {

        form_csv$Authors_name <- paste0(unique(form_csv$author_name),
                                        " ",
                                        unique(form_csv$Author),
                                        " et al.")

      }

  # URL
      if (!is.na(unique(form_csv$DOI)) & nchar(as.character(unique(form_csv$DOI))) > 9) {

        DOI_URL <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = unique(x = form_csv$DOI)))$url)

        if (class(DOI_URL) != "try-error") {

          if (!is.null(DOI_URL)) {

            form_csv$URL <- DOI_URL

          } else {

            form_csv$URL <- NA

          }

        } else {

          form_csv$URL <- NA

        }

      } else {

        form_csv$URL <- NA

      }

  # 3.8.5 ###= Aggregation for Rescom field to generate the plot =#####
      study <- aggregate(x = list("Rescom" = form_csv$Rescom),
                         by = list("Compound" = form_csv$Compound,
                                   "Pathogens" = form_csv$Pathogens,
                                   "Species" = form_csv$Species),
                         FUN = mean,
                         na.rm = TRUE)

  # 3.8.6 ###= Plot of the user submitted data =#####
      source(file = "modules/server_unique-surveyplot.R",
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

  # 3.8.7 ###= Display new marker in leaflet =#####
      form_csv_center.lat <- unique(form_csv$YCoord)

      form_csv_center.lon <- unique(form_csv$XCoord)

      form_csv.zoom <- 7

      form_csv_bound1.lat <- form_csv_center.lat + 0.6

      form_csv_bound1.lon <- form_csv_center.lon - 1.3

      form_csv_bound2.lat <- form_csv_center.lat - 0.6

      form_csv_bound2.lon <- form_csv_center.lon + 1.3

  # Popup content based on bibliographic informations available
      popup_content.form <- paste("<div style='max-height:810px' align='justify'>",
                                  "<h3>",
                                  paste0(ifelse(test = grepl(pattern = ",",
                                                             x = unique(form_csv$Author)),
                                                yes = sub(pattern = ",.*$",
                                                          replacement = "",
                                                          x = unique(form_csv$Author)),
                                                no = unique(form_csv$Author)),
                                         " et al., ",
                                         unique(form_csv$PubDate)), "<br>",
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
                                  paste(Antibiotics_df_unique$Combination,
                                        collapse = ", "),
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
          icon = PPS_markers["lightblue"],
          label = paste0(unique(form_csv$Author),
                         ", ",
                         unique(form_csv$PubDate)),
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

  # 3.8.8 ###= Updates the 1 row file from the form in AWS3 for displaying of temporary markers =#####
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

  # 3.8.9 ###= Notification in case of wrong coordinates =#####
      if (unique(form_csv$YCoord) == "47.367" |
          unique(form_csv$XCoord) == "8.551") {

        showNotification(ui = paste("The geocode function could not retrieve the correct coordinates of your study, the curator will get in contact with you soon."),
                         duration = 12,
                         closeButton = FALSE,
                         type = "error")

      }

  # 3.8.10 ###= Send authomatic e-mail with form link =#####
      unique_formurl <- drop_share(path = paste0("temporary_directory/",
                                                 unique_formname()))

      # E-mail for the resistancebank.org curators
      email_form <- gmailr::gm_mime() %>%
        gmailr::gm_subject(paste0("NEW RESBANK SUBMISSION - ",
                                  substr(x = unique_formname(),
                                         start = 1 ,
                                         stop = nchar(x = unique_formname()) - 4))) %>%
        gmailr::gm_from("healthgeographyandpolicy@gmail.com") %>%
        gmailr::gm_to(c(
          # "thomas.vanboeckel@env.ethz.ch",
          "nico.criscuolo1618@gmail.com"
        )) %>%
        gmailr::gm_text_body(paste0(substr(x = unique_formname(),
                                           start = 1 ,
                                           stop = nchar(x = unique_formname()) - 4),
                                    " ",
                                    unique_formurl$url))

      try(expr = gmailr::gm_send_message(email_form),
          silent = FALSE)

      # E-mail for users that submitted the form
      email_for_user <- gmailr::gm_mime() %>%
        gmailr::gm_subject("Your submission to resistancebank.org") %>%
        gmailr::gm_from("healthgeographyandpolicy@gmail.com") %>%
        gmailr::gm_to(c(unique(form_csv$Contact))) %>%
        gmailr::gm_text_body(paste(paste0("Dear Dr. ",
                                          unique(form_csv$Author),
                                          ","),
                                   "Thank you very much for submitting your data to resistancebank.org. Our curator will check your data shortly.",
                                   "Don't hesitate to contact us for further questions or feedback.",
                                   "Best regards,",
                                   "The HEGEP team",
                                   sep = "\n\n"))

      try(expr = gmailr::gm_send_message(email_for_user),
          silent = FALSE)

    }

  })

  # 3.9 #####===== Save template data with submit button and thanks =====#####
  observeEvent(input$upload_resistancebank_template, {

  # 3.9.1 ###= Preliminary operations =#####
    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)

    template_csv <- templateData()

    # Add the DataEntryPerson variable
    if (is.na(unique(template_csv$DataEntryPerson))) {

      template_csv$DataEntryPerson <- paste0(unique(template_csv$Author),
                                             "_",
                                             gsub(pattern = "-",
                                                  replacement = "",
                                                  x = Sys.Date()),
                                             "_",
                                             "TEMPLATE_SUBMISSION")

    }

    # template_csv <- as.data.frame(readxl::read_excel("~/Dropbox/nico/work/research/2019/PhD/resistancebank/datasets/template/resistancebank_template.xlsx",
    #                                                  col_names = TRUE,
    #                                                  sheet = "Example",
    #                                                  na = c("",
    #                                                         " ",
    #                                                         "NA")))

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

      if (length(which(is.na(template_csv$Contact))) > 0) {

        template_csv$Contact[is.na(template_csv$Contact)] <- "E-mail contact not available"

      }

      template_csv$Compound <- template_csv$Compound %>%
        as.character() %>%
        str_replace_all(pattern = " ",
                        replacement = "")

      if (any(is.na(template_csv$Author)) == TRUE |
          any(is.na(template_csv$PubDate)) == TRUE |
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
                                      select = c("Author",
                                                 "PubDate",
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

      } else if (length(unique(template_csv$PubDate)) != 1) {

        showNotification(ui = paste("The value of the PubDate column should be the same for every row of the template."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else if (length(unique(template_csv$ISO3)) != 1) {

        showNotification(ui = paste("The value of the ISO3 column should be the same for every row of the template."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else if (any(template_csv$Rescom > 100) == TRUE) {

        showNotification(ui = paste("The values inside the Rescom column should range from 0 to 100."),
                         duration = 10,
                         closeButton = FALSE,
                         type = "error")

      } else if (all(grepl(pattern = "@",
                           x = template_csv$Contact,
                           fixed = TRUE)) == FALSE |
                 all(str_count(string = template_csv$Contact,
                               pattern = "@") == 1) == FALSE) {

        showNotification(ui = paste("The e-mail address you have inserted is not valid."),
                         duration = 12,
                         closeButton = FALSE,
                         type = "error")

      } else {

        if (length(colnames(template_csv)) > length(correct_names$template_csv_colnames)) {

          template_csv <- subset(x = template_csv,
                                 select = c(correct_names$template_csv_colnames))

        }

        showNotification(ui = "Thank you! Your contribution has been submitted successfully. Your data will be added to resistancebank.org once approved by the curator.",
                         duration = 10,
                         closeButton = FALSE,
                         type = "default")

  # 3.9.2 ###= Clean and rename fields and save template in Dropbox =#####

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
                          replacement = "") %>%
          trimws()

  # Species
        template_csv$Species <- template_csv$Species %>%
          str_replace_all(pattern = " ",
                          replacement = "") %>%
          trimws()

        # template_csv$Species <- correct_names$Species[apply(X = adist(x = template_csv$Species,
        #                                                               y = correct_names$Species),
        #                                                     MARGIN = 1,
        #                                                     FUN = which.min)]

  # Pathogens
        template_csv$Pathogens <- template_csv$Pathogens %>%
          trimws()

        template_csv$Pathogens <- correct_names$Pathogens[apply(X = adist(x = template_csv$Pathogens,
                                                                          y = correct_names$Pathogens),
                                                                MARGIN = 1,
                                                                FUN = which.min)]

  # Compound
        # template_csv$Compound <- sapply(X = 1:length(template_csv$Compound),
        #                                 FUN = function(i) {
        #
        #                                   temporary_Antibiotics_df <- bind_rows(mutate_all(.tbl = Antibiotics_df,
        #                                                                                    .funs = as.character),
        #                                                                         mutate_all(.tbl = Antibiotics_df_common_names,
        #                                                                                    .funs = as.character))
        #
        #                                   temporary_Compound_name <- temporary_Antibiotics_df$Name[apply(X = adist(x = template_csv$Compound[i],
        #                                                                                                            y = temporary_Antibiotics_df$Name),
        #                                                                                                  MARGIN = 1,
        #                                                                                                  FUN = which.min)]
        #
        #                                   if (temporary_Compound_name %in% c(Antibiotics_df_common_names$Name)) {
        #
        #                                     temporary_Antibiotics_df$Code[temporary_Antibiotics_df$Name == temporary_Compound_name]
        #
        #                                   } else {
        #
        #                                     lev_distance <- adist(x = template_csv$Compound[i],
        #                                                           y = correct_names$Compound)
        #
        #                                     if (any(lev_distance < 12) == TRUE) { # Check the correct number!
        #
        #                                       Antibiotics_df$Code[match(correct_names$Compound[apply(X = adist(x = template_csv$Compound[i],
        #                                                                                                        y = correct_names$Compound),
        #                                                                                              MARGIN = 1,
        #                                                                                              FUN = which.min)],
        #                                                                 Antibiotics_df$Name)]
        #
        #                                     } else {
        #
        #                                       template_csv$Compound[i]
        #
        #                                     }
        #
        #                                   }
        #
        #                                 })

  # Save template in Dropbox
        save_template(template_csv)

  # 3.9.3 ###= Bibliographic research =#####
        if (!is.na(unique(template_csv$DOI)) &
            nchar(unique(template_csv$DOI)) > 9) {

          refmanager_summary <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = unique(template_csv$DOI))))

        } else {

          refmanager_summary <- NULL

        }

        # Title
        if (is.na(unique(template_csv$Title))) {

          if (class(refmanager_summary) != "try-error") {

            if (is.null(refmanager_summary)) {

              template_csv$Title <- "Report could not be linked to a peer-reviewed publication."

            } else {

              template_csv$Title <- refmanager_summary$title %>%
                gsub(pattern = "&lt;i&gt;",
                     replacement = "") %>%
                gsub(pattern = "&lt;/i&gt;",
                     replacement = "") %>%
                gsub(pattern = "&lt;sub&gt;",
                     replacement = "") %>%
                gsub(pattern = "&lt;/sub&gt;",
                     replacement = "")

            }

          } else {

            template_csv$Title <- "Report could not be linked to a peer-reviewed publication."

          }

        }

        # Jorunal
        if (is.na(unique(template_csv$Journal))) {

          if (class(refmanager_summary != "try-error")) {

            if (is.null(refmanager_summary)) {

              template_csv$Journal <- "Report could not be linked to a peer-reviewed publication."

            } else {

              template_csv$Journal <- refmanager_summary$journal

            }

          } else {

            template_csv$Journal <- "Report could not be linked to a peer-reviewed publication."

          }

        }

        # Authors name
        if (class(refmanager_summary) != "try-error") {

          if (is.null(refmanager_summary)) {

            paste0(unique(template_csv$Author),
                   " et al.")

          } else {

            paste0(refmanager_summary$author,
                   collapse = ", ")

          }

        } else {

          paste0(unique(template_csv$Author),
                 " et al.")

        }

        # URL
        if (is.na(unique(template_csv$URL))) {

          DOI_URL <- try(unlist(RefManageR::GetBibEntryWithDOI(doi = unique(template_csv$DOI)))$url)

          if (class(DOI_URL != "try-error")) {

            if (is.null(refmanager_summary)) {

              template_csv$URL <- NA

            } else {

              template_csv$URL <- DOI_URL

            }

          } else {

            template_csv$URL <- NA

          }

        }

  # 3.9.4 ###= Aggregation for Rescom field to generate the plot =#####
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

  # 3.9.5 ###= Plot of the user submitted data through template =#####
          source(file = "modules/server_unique-surveyplot.R",
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

  # 3.9.6 ###= Updates the 1 row file(s) from the form in AWS3 for displaying of temporary markers =#####
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

  # 3.9.7 ###= Display new marker for template in leaflet =#####
        if (nrow(temp_aws3_data) == 1) {

          template_csv_center.lat <- unique(template_csv$YCoord)

          template_csv_center.lon <- unique(template_csv$XCoord)

          template_csv.zoom <- 7

          template_csv_bound1.lat <- template_csv_center.lat + 0.6

          template_csv_bound1.lon <- template_csv_center.lon - 1.3

          template_csv_bound2.lat <- template_csv_center.lat - 0.6

          template_csv_bound2.lon <- template_csv_center.lon + 1.3

        }

  # Popup content based on bibliographic informations available
  popup_content.template <- paste("<div style='max-height:810px' align='justify'>",
                                  "<h3>",
                                  paste0(ifelse(test = grepl(pattern = ",",
                                                             x = temp_aws3_data$Author),
                                                yes = sub(pattern = ",.*$",
                                                          replacement = "",
                                                          x = temp_aws3_data$Author),
                                                no = temp_aws3_data$Author),
                                         " et al., ",
                                         temp_aws3_data$PubDate), "<br>",
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
            icon = PPS_markers["lightblue"],
            label = paste0(temp_aws3_data$Author,
                           ", ",
                           temp_aws3_data$PubDate),
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

            fitBounds(lat1 = map_initialview_data$target_zone_bound1.lat,
                      lng1 = map_initialview_data$target_zone_bound1.long,
                      lat2 = map_initialview_data$target_zone_bound2.lat,
                      lng2 = map_initialview_data$target_zone_bound2.long)

        }

  # 3.9.8 ###= Notification in case of wrong coordinates =#####
        if (all(unique(template_csv$YCoord) == "47.367") == TRUE &
            all(unique(template_csv$XCoord) == "8.551") == TRUE) {

          showNotification(ui = paste("The geocode function could not retrieve the correct coordinates of your study, the curator will get in contact with you soon."),
                           duration = 12,
                           closeButton = FALSE,
                           type = "error")

        }

  # 3.9.9 ###= Send authomatic e-mail with template link =#####
        unique_templateurl <- drop_share(path = paste0("temporary_directory/",
                                                       unique_templatename()))

        # E-mail for the resistancebank.org curators
        email_template <- gmailr::gm_mime() %>%
          gmailr::gm_subject(paste0("NEW RESBANK SUBMISSION - ",
                                    substr(x = unique_templatename(),
                                           start = 1 ,
                                           stop = nchar(x = unique_templatename()) - 4))) %>%
          gmailr::gm_from("healthgeographyandpolicy@gmail.com") %>%
          gmailr::gm_to(c(
            # "thomas.vanboeckel@env.ethz.ch",
            "nico.criscuolo1618@gmail.com"
            )) %>%
          gmailr::gm_text_body(paste0(substr(x = unique_templatename(),
                                             start = 1 ,
                                             stop = nchar(x = unique_templatename()) - 4),
                                      " ",
                                      unique_templateurl$url))

        try(expr = gmailr::gm_send_message(email_template),
            silent = FALSE)

        # E-mail for users that submitted the form
        email_for_user <- gmailr::gm_mime() %>%
          gmailr::gm_subject("Your submission to resistancebank.org") %>%
          gmailr::gm_from("healthgeographyandpolicy@gmail.com") %>%
          gmailr::gm_to(c(unique(template_csv$Contact))) %>%
          gmailr::gm_text_body(paste(paste0("Dear Dr. ",
                                            unique(template_csv$Author),
                                            ","),
                                     "Thank you very much for submitting your data to resistancebank.org. Our curator will check your data shortly.",
                                     "Don't hesitate to contact us for further questions or feedback.",
                                     "Best regards,",
                                     "The HEGEP team",
                                     sep = "\n\n"))

        try(expr = gmailr::gm_send_message(email_for_user),
            silent = FALSE)

      }

    }

  })

  # 4. ##########========== DOWNLOAD DATA ==========####################
  source(file = "modules/server_download-data.R",
         local = TRUE)$value

}
