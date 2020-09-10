
ui <- fluidPage(

  #########################=========================#########################
  # 1. ##########========== HTML AND JAVA CODE ==========##########
  useShinyjs(),

  tags$head(includeScript("Hotjar.js")),

  tags$head(includeScript("zoom_to_location_button.js")),

  tags$head(includeScript("disable_backspace.js")),

  tags$head(includeCSS("page_style.css")),

  ###= General style for every widget
  tags$style(".selectize-control.single .selectize-input:after {right: 0px; margin-right: 3px;}"),
  tags$style(".popover {max-width: 650px;}"),
  tags$style(".selectize-input {min-height: 0px; height: 20px; padding: 0px 0px 20px 5px; font-size: 10px; margin-top: 7px; text-align: left;}"),
  tags$style(".selectize-dropdown {font-size: 10px; text-align: left;}"),
  tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
  tags$head(tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;} .box-body {padding-bottom: 0px;}"))),
  tags$head(tags$style(HTML(".newfileinput {width: 0.1px; height: 0.1px; opacity: 0; overflow: hidden; position: absolute; z-index: -1;}"))),
  tags$head(tags$style(HTML("#initial_map_view_div .tooltip {width: 145px;}"))),
  tags$head(
    tags$style(
      HTML("
        #author_name {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #Author {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #institution {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #author_email {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #doi {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #doi::placeholder {color: #DEDEDE;}
        #paper_title {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #journal {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #coordinates {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #coordinates::placeholder {color: #DEDEDE;}
        #remark {height: 22px; margin-top: 7px; font-size: 10px; padding: 0px 0px 0px 5px;}
        #remark::placeholder {color: #DEDEDE;}"))),

  HTML('<meta name="viewport" content="width=1024">'),

  # 2. ##########========== WORLD MAP ==========##########
  div(id = "map",

                 leafletOutput(outputId = "world_map",
                               width = "100%",
                               height = "100%"), # percentage height can be set thanks to the .css file
  hidden(
    absolutePanel(id = "name_Maps_panel",
                  fixed = TRUE,
                  top = 12,
                  left = "auto",
                  right = 96,
                  bottom = "auto",
                  width = "auto",
                  height = "auto",
                  h4(strong("Maps"))
    )
  ),

  hidden(
    absolutePanel(id = "target_panel",
                  fixed = TRUE,
                  top = 21,
                  left = 10,
                  right = "auto",
                  bottom = "auto",
                  width = "auto",
                  height = "auto",
                  clearableTextInput(inputId = "target",
                                     label = NULL,
                                     value = "",
                                     placeholder = "Country, city, address, or latitude and longitude.")
    )
  ),

  conditionalPanel(condition = "input.target != ''",

    absolutePanel(id = "zoom_to_location_panel",
                  fixed = TRUE,
                  top = 21.4,
                  left = 360,
                  right = "auto",
                  bottom = "auto",
                  div(id = "zoom_to_location_div",
                      actionButton(inputId = "zoom_to_location",
                                   label = strong("Go!")))
    )

  ),

  hidden(
    absolutePanel(id = "initial_map_view_panel",
                  fixed = TRUE,
                  top = 21.4,
                  left = 410,
                  right = "auto",
                  bottom = "auto",
                  div(id = "initial_map_view_div",
                      actionButton(inputId = "initial_map_view",
                                   label = icon(name = "globe-americas",
                                                lib = "font-awesome")),
                      bsTooltip(id = "initial_map_view",
                                title = "World map initial view",
                                placement = "right",
                                trigger = "hover"))
    )
  ),

  # 3. ##########========== PANELS ==========##########

  # 3.1 #####===== Initial panel =====#####
  tags$div(id = "initial_panel_division",

    absolutePanel(id = "initial_panel",
                  fixed = TRUE,
                  top = "32%",
                  right = 0,
                  left = 0,
                  bottom = "auto",
                  width = "33.5%",
                  height = "auto",

                  actionButton(inputId = "close_initial_panel",
                               label = icon(name = "times",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),
                                        # font-size: 1.5vmin;

    tags$div(style = "height: 3px;"),

                  tags$p(strong("Welcome to resistancebank.org"),
                         style = "font-size: 2.6vmin;
                                  text-align: center;"),

    tags$div(style = "height: 13px;"),

                  tags$p("The open access repository for surveys and maps of antimicrobial resistance in animals",
                         style = "font-size: 1.9vmin;
                                  text-align: center;"),

    tags$div(style = "height: 32px;"),

                  fluidRow(
                    column(width = 4,
                           align = "center",
                           actionButton(inputId = "explore_amr_map",
                                        label = tags$img(src = "world_icon.png",
                                                         width = "96%",
                                                         height = "95%",
                                                         align = "center")),
                           tags$p(strong("Maps of antimicrobial resistance"),
                                  style = "font-size: 1.3vmin;
                                           text-align: center;"),
                    ),
                    column(width = 4,
                           align = "center",
                           actionButton(inputId = "add_your_survey1",
                                        label = tags$img(src = "location-map_icon2.png",
                                                         height = "45%",
                                                         width = "45%",
                                                         align = "center")),
                           tags$p(strong("Add your survey"),
                                  style = "font-size: 1.3vmin;
                                           text-align: center;"),
                    ),
                    column(width = 4,
                           align = "center",
                           NEWdownloadButton(outputId = "download_RESBANK2",
                                             label = tags$img(src = "download_icon.png",
                                                              height = "54%",
                                                              width = "54%",
                                                              align = "center")),
                           tags$p(strong("Download antimicrobial resistance data"),
                                  style = "font-size: 1.3vmin;
                                           text-align: center;")

                    )
                  )
      )

  ),

  # 3.2 #####===== Controls panel =====#####
  hidden(

    absolutePanel(id = "controls_panel",
                  fixed = TRUE,
                  top = 160,
                  left = "auto",
                  right = 10,
                  bottom = "auto",
                  width = 142,
                  height = "auto",
        fluidRow(
          column(width = 12,
                 align = "left",
                 div(id = "controls_panel_Data_text",
                   h4(strong("Data"))
                 ),
                 actionButton(inputId = "add_your_survey2",
                              label = div(icon(name = "map-marker-alt",
                                               lib = "font-awesome"),
                                          strong("Add your survey")),
                              align = "left"),

  hidden(
                actionButton(inputId = "close_add_your_survey_controls",
                             label = div(icon(name = "map-marker-alt",
                                              lib = "font-awesome"),
                                         strong("Add your survey")),
                             align = "left")
  ),

  hidden(
                actionButton(inputId = "close_add_your_survey_controls2",
                             label = div(icon(name = "map-marker-alt",
                                              lib = "font-awesome"),
                                         strong("Add your survey")),
                             align = "left")
  ),

                 actionButton(inputId = "display_Country_report_panel",
                              label = div(icon(name = "file-contract",
                                               lib = "font-awesome"),
                                          strong("Country report")),
                              align = "left"),

  hidden(
                 actionButton(inputId = "close_country_report_panel_controls",
                              label = div(icon(name = "file-contract",
                                               lib = "font-awesome"),
                                          strong("Country report")),
                              align = "left")
  ),

br(),

                 downloadButton(outputId = "download_RESBANK",
                                label = strong("Resistance bank"),
                                align = "left"),

br(),

                 downloadButton(outputId = "download_P50",
                                label = strong("AMR hotspots map"),
                                align = "left"),

br(),
br(),
br(),

  div(id = "controls_panel_Data_text",
      h4(strong("Info"))
  ),

                 actionButton(inputId = "display_about_panel",
                              label = div(icon(name = "info-circle",
                                               lib = "font-awesome"),
                                          strong("About")),
                              align = "left"),
  hidden(
                 actionButton(inputId = "close_about_panel_controls",
                              label = div(icon(name = "info-circle",
                                               lib = "font-awesome"),
                                          strong("About")),
                              align = "left")
  ),
                 actionButton(inputId = "open_GitHub_repository",
                              label = div(icon(name = "github",
                                               lib = "font-awesome"),
                                          strong("GitHub")),
                              onclick ="window.open('https://github.com/hegep-eth/resistancebank.org', '_blank')",
                              align = "left"),
                 actionButton(inputId = "open_Youtube_video",
                              label = div(icon(name = "youtube",
                                               lib = "font-awesome"),
                                          strong("Tutorial")),
                              onclick ="window.open('https://www.youtube.com/watch?v=WoegZDXTjH4', '_blank')",
                              align = "left")
          )
        )
    )
  ),

  # 3.3 #####===== Add your survey panel =====#####
  hidden(

    div(id = "add_your_survey_division",

        absolutePanel(id = "add_your_survey_panel",
                      fixed = TRUE,
                      draggable = FALSE,
                      top = 11,
                      right = 160,
                      left = "auto",
                      bottom = "auto",
                      width = "30%",
                      height = "auto",

                      actionButton(inputId = "close_add_your_survey_division",
                                   label = icon(name = "times",
                                                lib = "font-awesome"),
                                   style = "position: absolute;
                                            top: 0;
                                            right: 0;
                                            background-color: rgba(0, 0, 0, 0);
                                            border-width: 0px;
                                            padding: 0px 5px 0px 0px;"),

  tags$div(style = "height: 20px;"),

                      fluidRow(
                        column(width = 4,
                               actionButton(inputId = "fill_form",
                                            label = img(src = "form.png",
                                                        height = "85.5%",
                                                        width = "85.5%",
                                                        align = "center"),
                                            style = "border-width: 0px;"),
                               tags$p(strong("Fill a form"),
                                      style = "font-size: 1.17vmin;
                                               text-align: center;")
                        ),
                        column(width = 4,
                               align = "center",
                               NEWfileInput(inputId = "upload_resistancebank_template",
                                            label = tags$img(src = "upload_template.png",
                                                             height = "81.3%",
                                                             width = "81.3%",
                                                             align = "center"),
                                            labelIcon = NULL,
                                            accept = c(".xlsx"),
                                            progress = FALSE,
                                            style = "border-width: 0px;"),
                               tags$div(style = "display: inline-block;
                                                 vertical-align: top;
                                                 font-size: 1.17vmin;",
                                        strong("Use this ")),
                               downloadLink(outputId = "download_resistancebank_template",
                                            label = tags$strong(".xlsx template",
                                                                style = "vertical-align: top;
                                                                         font-size: 1.17vmin;")) #! After resizing window there is too much space between two lines
                        ),
                        column(width = 4,
                               a(actionButton(inputId = "contact_us_for_survey",
                                              label = img(src = "contact_us.png",
                                                        height = "85.5%",
                                                        width = "85.5%",
                                                        align = "center"),
                                              style = "border-width: 0px;"),
                                 href = "mailto:thomasvanboeckel@resistancebank.org"),
                               tags$p(strong("Contact us"),
                                      style = "font-size: 1.17vmin;
                                               text-align: center;")
                        )
                      )

        )

    )

  ),

  # 3.3.1 ###= Form panel =#####
  hidden(

  div(id = "form",

      absolutePanel(id = "form_panel", ###!!!!!! ricordati che deve essere lo stesso su ogni schermo!!!
                    fixed = TRUE,
                    top = 11,
                    left = 130,
                    bottom = 30,
                    right = 160,
                    width = "auto",
                    height = "auto",

                    actionButton(inputId = "close_form",
                                 label = icon(name = "times",
                                              lib = "font-awesome"), # fixed !!!! ! ! ! !!!! ! !!! ! ### #  #à## ##!!
                                 style = "position: absolute;
                                          top: 0;
                                          right: 0;
                                          background-color: rgba(0, 0, 0, 0);
                                          border-width: 0px;
                                          padding: 0px 5px 0px 0px;
                                          z-index: 100000;"),

  # 3.3.1.1 ###= User information =#####
                  fluidRow(
                    column(width = 6,
                           actionButton(inputId = "show_user_information_panel",
                                        label = icon(name = "plus",
                                                     class = "plus_investigator",
                                                     lib = "font-awesome"),
                                        style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                           tags$style(".plus_investigator {color: #FFFFFF;}"),
                           hidden(
                             actionButton(inputId = "hide_user_information_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_investigator",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_investigator {color: #FFFFFF;}")
                           ),
                           div(style = "display: inline-block; margin-left: 5px;",
                               h3(strong(" Investigator")))
                    )
                  ),

  hidden(

  div(id = "user_information_panel",

                  fluidRow(
                    br(),
                    column(width = 2,
                           textInput(inputId = "author_name",
                                     label = h6(mandatory_asterisk("First name")),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "Author",
                                     label = h6(mandatory_asterisk("Last name")),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "institution",
                                     label = h6("Institution"),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "author_email",
                                     label = h6(mandatory_asterisk("e-mail")),
                                     value = "")
                    )
                  )
  )

  ),

br(),

  # 3.3.1.2 ###= Survey information =#####
                  fluidRow(
                    column(width = 6,
                           actionButton(inputId = "show_study_information_panel",
                                        label = icon(name = "plus",
                                                     class = "plus_study_information",
                                                     lib = "font-awesome"),
                                        style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                           tags$style(".plus_study_information {color: #FFFFFF;}"),
  hidden(
                             actionButton(inputId = "hide_study_information_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_study_information",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_study_information {color: #FFFFFF;}")
  ),
  div(style = "display: inline-block; margin-left: 5px;",
      h3(strong(" Survey information")))
  )
                  ),

  hidden(

  div(id = "study_information_panel",
  # DOI
                  fluidRow(
                    br(),
                    column(width = 2,
                           textInput(inputId = "doi",
                                     label = h6("Digital Object Identifier"),
                                     placeholder = "10.1126/science.aao1495",
                                     value = "")
                    ),
  # PUBLICATION YEAR
                    column(width = 2,
                           selectizeInput(inputId = "PubDate",
                                          label = h6("Publication year"),
                                          choices = "",
                                          selected = "",
                                          multiple = FALSE,
                                          options = list(create = FALSE))
                    ),
 # Study Title
                    column(width = 2,
                           textInput(inputId = "paper_title",
                                     label = h6("Title"),
                                     value = "")
                    ),
  # Study Journal
                    column(width = 2,
                           textInput(inputId = "journal",
                                     label = h6("Journal"),
                                     value = "")
                    ),
 # Country of study
                    column(width = 2,
                           selectizeInput(inputId = "country_study",
                                          label = h6(mandatory_asterisk("Country")),
                                          choices = c("China (CHN)" = "",
                                                      Countries$Country_ISO3),
                                          selected = "",
                                          multiple = TRUE,
                                          options = list(maxItems = 1,
                                                         create = FALSE,
                                                         closeAfterSelect = FALSE))
                    ),
 # Coordinates
                    column(width = 2,
                           textInput(inputId = "coordinates",
                                     label = h6(mandatory_asterisk("Location")),
                                     value = "",
                                     placeholder = "Place name or latitude/longitude")
                    )
                  )

  )

  ),

br(),

  # 3.3.1.3 ###= Antimicrobial resistance =#####
                  fluidRow(
                    column(width = 6,
                           actionButton(inputId = "show_amr_panel",
                                        label = icon(name = "plus",
                                                     class = "plus_antimicrobial_resistance",
                                                     lib = "font-awesome"),
                                        style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                           tags$style(".plus_antimicrobial_resistance {color: #FFFFFF;}"),
  hidden(
                             actionButton(inputId = "hide_amr_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_antimicrobial_resistance",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_antimicrobial_resistance {color: #FFFFFF;}")
  ),
                           div(style = "display: inline-block; margin-left: 5px;",
                               h3(strong(" Antimicrobial resistance")))
                    )
                  ),

  hidden(

    source(file = "modules/ui_antimicrobial-resistance-form.R",
           local = TRUE)$value

  ),

br(),

  # 3.3.1.3 ###= Remark =#####
                      fluidRow(
                        column(width = 6,
                               actionButton(inputId = "show_remark_panel",
                                            label = icon(name = "plus",
                                                         class = "plus_remark",
                                                         lib = "font-awesome"),
                                            style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                               tags$style(".plus_remark {color: #FFFFFF;}"),
  hidden(
                                 actionButton(inputId = "hide_remark_panel",
                                              label = icon(name = "minus",
                                                           class = "minus_remark",
                                                           lib = "font-awesome"),
                                              style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                                 tags$style(".minus_remark {color: #FFFFFF;}")
  ),
                               div(style = "display: inline-block; margin-left: 5px;",
                                   h3(strong(" Comments")))
                        )
                      ),
  hidden(

  div(id = "remark_panel",

                      fluidRow(
                        column(width = 12,
                               textInput(inputId = "remark",
                                         label = "",
                                         value = "",
                                         placeholder = "General comments about the samples, methodologies, guidelines and/or results",
                                         width = "100%")
                        )
                      )
  )

  ),

br(),
br(),

  # 3.3.1.4 ###= Submit =#####
                      fluidRow(id = "submit_button",
                        column(width = 12,
                               align = "center",
                               actionButton(inputId = "submit",
                                            label = "Add your survey",
                                            icon(name = "upload",
                                                 lib = "font-awesome"))
                        )
                      )
        )
    )
  ),

  # 3.4 #####===== Country report panel =====#####
  hidden(

    absolutePanel(id = "download_Country_report_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 11,
                  right = 160,
                  left = "auto",
                  bottom = "auto",
                  width = "55%",
                  height = "auto",

                  actionButton(inputId = "close_country_report_panel",
                               label = icon(name = "times",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                            top: 0;
                                            right: 0;
                                            background-color: rgba(0, 0, 0, 0);
                                            border-width: 0px;
                                            padding: 0px 5px 0px 0px;"),

                  fluidRow(
                    column(width = 3,
                           tags$p(tags$br(),
                                  strong("Select country"),
                                  style = "font-size: 1.17vmin;"),
                           shinyWidgets::pickerInput(inputId = "Country_report_nation",
                                                     label = NULL,
                                                     choices = c(Countries$Country_ISO3),
                                                     multiple = FALSE,
                                                     options = list(
                                                       size = 10,
                                                       `live-search` = TRUE
                                                     ))
                    ),
                    column(width = 4,
                           align = "center",
br(),
br(),
                           withBusyIndicatorUI(
                             downloadButton(outputId = "download_Country_report",
                                            label = "Download as PDF",
                                            style = "box-shadow: none!important;
                                                     outline: 0;
                                                     background-color: transparent;
                                                     border-width: 0px;"))
                    ),
                    column(width = 4,
                           align = "center",
br(),
br(),
                           withBusyIndicatorUI(
                             downloadButton(outputId = "download_Country_data",
                                            label = "Download data",
                                            style = "box-shadow: none!important;
                                                     outline: 0;
                                                     background-color: transparent;
                                                     border-width: 0px;"))
                    )
                  ),

div(style = "height: 0.5vh;"),

                  fluidRow(
                    column(width = 12,
                           uiOutput(outputId = "country_report_images")
                    )
                  )

    )

  ),
  # 3.5 #####===== About panel =====#####
  hidden(

    absolutePanel(id = "about_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 11,
                  right = 160,
                  left = "auto",
                  bottom = "auto",
                  width = "31%",
                  height = "auto",

                  actionButton(inputId = "close_about_panel",
                               label = icon(name = "times",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                            top: 0;
                                            right: 0;
                                            background-color: rgba(0, 0, 0, 0);
                                            border-width: 0px;
                                            padding: 0px 5px 0px 0px;"),

                  tags$p(strong("Resistance bank"),
                         style = "font-size: 1.6vmin;
                                  padding: 0px 0px 0px 4px;"),

                  fluidRow(
                    column(width = 12,
                      source(file = "modules/ui_text-about-panel.R",
                             local = TRUE)$value
                    )
                  )

    )

  )

  #########################=========================#########################
  ), # Closes the "World Map" division

) # Closes fluidPage





