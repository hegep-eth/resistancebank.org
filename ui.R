
ui <- fluidPage(

  #########################=========================#########################
  # 1. ##########========== HTML AND JAVA CODE ==========##########
  useShinyjs(),
  useShinyFeedback(),

  # extendShinyjs(text = full_screen_function,
  #               functions = c("toggleFullScreen")),

  # tags$head(includeScript("Hotjar.js")),

  tags$head(includeScript("zoom_to_location_button.js")),

  tags$head(includeScript("disable_backspace.js")),

  tags$head(includeCSS("page_style.css")),

  # 1.1 #####===== General style for every widget =====#####
  tags$style(".popover {max-width: 650px;}"),
  tags$style(".selectize-control.single .selectize-input:after {right: 0px; margin-right: 3px;}"),
  tags$style(".selectize-input {min-height: 0px; height: 20px; padding: 0px 0px 20px 5px; font-size: 10px; margin-top: 7px; text-align: left;}"),
  tags$style(".selectize-dropdown {font-size: 10px; text-align: left;}"),
  tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
  tags$head(tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;} .box-body {padding-bottom: 0px;}"))),
  tags$head(tags$style(HTML(".newfileinput {width: 0.1px; height: 0.1px; opacity: 0; overflow: hidden; position: absolute; z-index: -1;}"))),
  tags$head(tags$style(HTML("#initial_map_view_div .tooltip {width: 135px;}"))),
  # tags$head(tags$style(HTML("#full_screen_view_div .tooltip {width: 90px;}"))),
  tags$head(tags$style(HTML(".shiny-input-container > label {margin-bottom: -30px;}"))),
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
        #remark::placeholder {color: #DEDEDE;}
        #filterdata_ISO3+ div>.selectize-input{height: 55px !important}
        #filterdata_species+ div>.selectize-input{height: 55px !important}
        #filterdata_sample_type+ div>.selectize-input{height: 55px !important}
        #filterdata_pathogen+ div>.selectize-input{height: 55px !important}
        #filterdata_compound+ div>.selectize-input{height: 55px !important}
        #filterdata_compound_class+ div>.selectize-input{height: 55px !important}
        #filterdata_compound_class+ .selectize-control.single .selectize-input:after{content: none;}
        #filterdata_compound_agisar+ div>.selectize-input{height: 55px !important}"))),

  # 2. ##########========== WORLD MAP ==========##########
  div(id = "map",

      leafletOutput(outputId = "world_map",
                   width = "100%",
                   height = "100%"), # percentage height can be set thanks to the .css file

  hidden(
    absolutePanel(id = "switch_farming_type_panel",
                  fixed = TRUE,
                  top = 9,
                  left = "auto",
                  right = -157.5,
                  bottom = "auto",
                  width = "auto",
                  height = "auto",
                  style = "z-index: 1000;",
                  h4(strong("Animals"),
                     style = "margin-bottom: 20px;"),
                  radioButtons(inputId = "switch_farming_type",
                               label = NULL,
                               choiceNames = list(
                                 tags$span(style = "font-size: 12px;
                                                    font-weight: bold;",
                                           "Livestock"),
                                 tags$span(style = "font-size: 12px;
                                                    font-weight: bold;",
                                           "Freshwater"),
                                 tags$span(style = "font-size: 12px;
                                                    font-weight: bold;",
                                           "Marine")),
                               choiceValues = c("livestock",
                                                "freshwater",
                                                "marine"),
                               selected = "livestock")
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
                  textInput(inputId = "target",
                            label = NULL,
                            value = "",
                            placeholder = "Country, city, address, or latitude and longitude",
                            width = "395px")
    )
  ),

  conditionalPanel(condition = "input.target != ''",

     absolutePanel(id = "clear_target_panel",
                   fixed = TRUE,
                   top = 21.4,
                   left = 325,
                   right = "auto",
                   bottom = "auto",
                   div(id = "clear_target_panel_div",
                       actionButton(inputId = "clear_location_text",
                                    label = icon(name = "xmark",
                                                 lib = "font-awesome")))
     ),

    absolutePanel(id = "zoom_to_location_panel",
                  fixed = TRUE,
                  top = 21.4,
                  left = 355,
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
                                   label = icon(name = "earth-americas",
                                                lib = "font-awesome"),
                                   style = "font-size: 16px;
                                            border: none;
                                            box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);"),
                      bsTooltip(id = "initial_map_view",
                                title = "World map initial view",
                                placement = "bottom",
                                trigger = "hover"))
    )
  ),

  # hidden(
  #   absolutePanel(id = "full_screen_view_panel",
  #                 fixed = TRUE,
  #                 top = 21.4,
  #                 left = 455,
  #                 right = "auto",
  #                 bottom = "auto",
  #                 div(id = "full_screen_view_div",
  #                     actionButton(inputId = "full_screen_view_button",
  #                                  label = icon(name = "expand",
  #                                               lib = "font-awesome"),
  #                                  onclick = "shinyjs.toggleFullScreen();"),
  #                     bsTooltip(id = "full_screen_view_button",
  #                               title = "Full screen",
  #                               placement = "right",
  #                               trigger = "hover"))
  #   )
  # ),

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
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),

    tags$div(style = "height: 3px;"),

                  tags$p(strong("Welcome to resistancebank.org"),
                         style = "font-size: 2.6vmin;
                                  text-align: center;"),

    tags$div(style = "height: 13px;"),

                  tags$p("The open access repository for surveys and maps of antimicrobial resistance in animals",
                         style = "font-size: 1.9vmin;
                                  text-align: center;"),

    tags$div(style = "height: 32px;"),

                  tags$table(style = "width: 100%;",
                             tags$tr(tags$td(style = "width: 33.3%;",
                                             align = "center",
                                             actionButton(inputId = "explore_amr_map",
                                                          label = tags$img(src = "world_icon.png",
                                                                           width = "50%",
                                                                           height = "50%",
                                                                           align = "center"),
                                                          style = "border-width: 0px;
                                                                   box-shadow: none!important;
                                                                   outline: 0;
                                                                   background-color: transparent;")
                                    ),
                                    tags$td(style = "width: 33.3%;",
                                            align = "center",
                                            actionButton(inputId = "add_your_survey1",
                                                         label = tags$img(src = "location-map_icon2.png",
                                                                          height = "45%",
                                                                          width = "45%",
                                                                          align = "center"),
                                                         style = "border-width: 0px;
                                                                  box-shadow: none!important;
                                                                  outline: 0;
                                                                  background-color: transparent;")
                                    ),
                                    tags$td(style = "width: 33.3%;",
                                              align = "center",
                                              actionButton(inputId = "display_download_RESBANK_panel2",
                                                           label = tags$img(src = "download_icon.png",
                                                                            height = "52%",
                                                                            width = "52%",
                                                                            align = "center"),
                                                           style = "border-width: 0px;
                                                                    box-shadow: none!important;
                                                                    outline: 0;
                                                                    background-color: transparent;")
                                    )
                             ),
                             tags$tr(tags$td(style = "width: 33.3%;",
                                             align = "center",
                                             tags$p(strong("Maps of antimicrobial resistance"),
                                                    style = "font-size: 1.3vmin;
                                                             text-align: center;")
                                     ),
                                     tags$td(style = "width: 33.3%;",
                                             align = "center",
                                             tags$p(strong("Add your survey"),
                                                    style = "font-size: 1.3vmin;
                                                             text-align: center;")
                                     ),
                                     tags$td(style = "width: 33.3%;",
                                             align = "center",
                                             tags$p(strong("Download antimicrobial resistance data"),
                                                    style = "font-size: 1.3vmin;
                                                             text-align: center;")
                                     )
                             )
                  )
      )

  ),

  # 3.2 #####===== Controls panel =====#####
  hidden(
    absolutePanel(id = "name_Maps_panel",
                  fixed = TRUE,
                  top = 139,
                  left = "auto",
                  right = 96,
                  bottom = "auto",
                  width = "auto",
                  height = "auto",
                  style = "background-color: transparent;
                           padding: 0px 0px 0px 0px;
                           margin: auto;
                           z-index: 1000;",
                  h4(strong("Maps")),

    )
  ),

  hidden(
    absolutePanel(id = "controls_panel",
                  fixed = TRUE,
                  top = 10,
                  left = "auto",
                  right = 10,
                  bottom = "auto",
                  width = 142,
                  height = "auto",
                  style = "z-index: 900;
                           background-color: white;
                           opacity: 1;
                           border-radius: 5pt;
                           box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                           padding: 5px 5px 5px 0px;
                           margin: auto;
                           padding-bottom: 0px;
                           padding-top: 259px;",

        fluidRow(
          column(width = 12,
                 align = "left",
                 div(id = "controls_panel_Data_text",
                     h4(strong("Data"))
                 ),
                 actionButton(inputId = "add_your_survey2",
                              label = div(icon(name = "location-dot",
                                               lib = "font-awesome"),
                                          strong("Add your survey")),
                              align = "left"),

  hidden(
                actionButton(inputId = "close_add_your_survey_controls",
                             label = div(icon(name = "location-dot",
                                              lib = "font-awesome"),
                                         strong("Add your survey")),
                             align = "left")
  ),

  hidden(
                actionButton(inputId = "close_add_your_survey_controls2",
                             label = div(icon(name = "location-dot",
                                              lib = "font-awesome"),
                                         strong("Add your survey")),
                             align = "left")
  ),

                 actionButton(inputId = "display_filterdata_panel",
                              label = div(icon(name = "filter",
                                               lib = "font-awesome"),
                                          strong("Filter data")),
                              align = "left"),

  hidden(

                  actionButton(inputId = "close_filterdata_panel_controls",
                               label = div(icon(name = "filter",
                                                lib = "font-awesome"),
                                           strong("Remove filter")),
                               align = "left")

  ),

  # Download country report panel
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

  # Download resistancebank database panel
  actionButton(inputId = "display_download_RESBANK_panel",
               label = div(icon(name = "download",
                                lib = "font-awesome"),
                           strong("Resistancebank")),
               align = "left"),

  hidden(
    actionButton(inputId = "close_download_RESBANK_panel_controls",
                 label = div(icon(name = "download",
                                  lib = "font-awesome"),
                             strong("Resistancebank")),
                 align = "left")
  ),

  # Download P50 maps panel
  actionButton(inputId = "display_download_P50_panel",
               label = div(icon(name = "download",
                                lib = "font-awesome"),
                           strong("AMR hotspots map")),
               align = "left"),

  hidden(
    actionButton(inputId = "close_download_P50_panel_controls",
                 label = div(icon(name = "download",
                                  lib = "font-awesome"),
                             strong("AMR hotspots map")),
                 align = "left")
  ),

br(),
br(),

  div(id = "controls_panel_Data_text",
      h4(strong("Info"))
  ),

                 actionButton(inputId = "display_about_panel",
                              label = div(icon(name = "circle-info",
                                               lib = "font-awesome"),
                                          strong("About")),
                              align = "left"),
  hidden(
                 actionButton(inputId = "close_about_panel_controls",
                              label = div(icon(name = "circle-info",
                                               lib = "font-awesome"),
                                          strong("About")),
                              align = "left")
  ),

                actionButton(inputId = "display_limitations_panel",
                             label = div(icon(name = "triangle-exclamation",
                                              lib = "font-awesome"),
                                         strong("Limitations")),
                             align = "left"),

  hidden(
                actionButton(inputId = "close_limitations_panel_controls",
                             label = div(icon(name = "triangle-exclamation",
                                              lib = "font-awesome"),
                                         strong("Limitations")),
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
                              onclick ="window.open('https://youtu.be/TpMQ_3JLJ2I', '_blank')",
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
                                   label = icon(name = "xmark",
                                                lib = "font-awesome"),
                                   style = "position: absolute;
                                            top: 0;
                                            right: 0;
                                            background-color: rgba(0, 0, 0, 0);
                                            border-width: 0px;
                                            padding: 0px 5px 0px 0px;"),

  tags$div(style = "height: 20px;"),

                      tags$table(style = "width: 100%",
                                 tags$tr(tags$td(style = "width: 33.3%;",
                                                 align = "center",
                                                 actionButton(inputId = "fill_form",
                                                              label = img(src = "form.png",
                                                                          height = "85.5%",
                                                                          width = "85.5%",
                                                                          align = "center"),
                                                              style = "border-width: 0px;
                                                                       box-shadow: none!important;
                                                                       outline: 0;
                                                                       background-color: transparent;")
                                         ),
                                         tags$td(style = "width: 33.3%;",
                                                 align = "center",
                                                 NEWfileInput(inputId = "upload_resistancebank_template",
                                                              label = tags$img(src = "upload_template.png",
                                                                               height = "81.3%",
                                                                               width = "81.3%",
                                                                               align = "center"),
                                                              labelIcon = NULL,
                                                              accept = c(".xlsx"),
                                                              progress = FALSE)
                                         ),
                                         tags$td(style = "width: 33.3%;",
                                                 align = "center",
                                                 a(actionButton(inputId = "contact_us_for_survey",
                                                                label = img(src = "contact_us.png",
                                                                            height = "85.5%",
                                                                            width = "85.5%",
                                                                            align = "center"),
                                                                style = "border-width: 0px;
                                                                         box-shadow: none!important;
                                                                         outline: 0;
                                                                         background-color: transparent;"),
                                                   href = "mailto:thomas.vanboeckel@env.ethz.ch")
                                         )
                                 ),
                                 tags$tr(tags$td(style = "width: 33.3%",
                                                 align = "center",
                                                 h5("Fill a form")
                                         ),
                                         tags$td(style = "width: 33.3%",
                                                 align = "center",
                                                 h5("Use this ",
                                                    downloadLink(outputId = "download_template",
                                                                 label = " .xlsx template"))
                                         ),
                                         tags$td(style = "width: 33.3%",
                                                 align = "center",
                                                 h5("Contact us")
                                         )
                                 )
                      )
        )

    )

  ),

  # 3.3.1 ###= Form panel =#####
  hidden(

  tags$div(id = "form",

      absolutePanel(id = "form_panel", ###!!!!!! ricordati che deve essere lo stesso su ogni schermo!!!
                    fixed = TRUE,
                    top = 11,
                    left = 130,
                    bottom = 30,
                    right = 160,
                    width = "auto",
                    height = "auto",

                    actionButton(inputId = "close_form",
                                 label = icon(name = "xmark",
                                              lib = "font-awesome"),
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
                           hidden(
                             actionButton(inputId = "show_user_information_panel",
                                          label = icon(name = "plus",
                                                       class = "plus_investigator",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".plus_investigator {color: #FFFFFF;}")
                           ),
                             actionButton(inputId = "hide_user_information_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_investigator",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_investigator {color: #FFFFFF;}"),
                             div(style = "display: inline-block; margin-left: 5px;",
                                 h3(strong(" Investigator")))
                    )
                  ),

  div(id = "user_information_panel",

                  fluidRow(
                    column(width = 2,
                           textInput(inputId = "author_name",
                                     label = h6(mandatory_asterisk("Name")),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "Author",
                                     label = h6(mandatory_asterisk("Surname")),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "institution",
                                     label = h6("Institution"),
                                     value = "")
                    ),
                    column(width = 2,
                           textInput(inputId = "author_email",
                                     label = h6(mandatory_asterisk("E-mail")),
                                     value = "")
                    )
                  )
  ),

br(),

  # 3.3.1.2 ###= Survey information =#####
                  fluidRow(
                    column(width = 6,
                           hidden(
                             actionButton(inputId = "show_study_information_panel",
                                          label = icon(name = "plus",
                                                       class = "plus_study_information",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".plus_study_information {color: #FFFFFF;}")
                           ),
                             actionButton(inputId = "hide_study_information_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_study_information",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_study_information {color: #FFFFFF;}"),
                             div(style = "display: inline-block; margin-left: 5px;",
                                 h3(strong(" Survey information")))
                    )
                  ),

    tags$div(id = "study_information_panel",

  # DOI
             tags$table(style = "width: 100%;
                                 table-layout: fixed;",
                        tags$tr(tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        textInput(inputId = "doi",
                                                  label = h6("Digital Object Identifier"),
                                                  placeholder = "10.1038/s41597-021-00978-9",
                                                  value = "",
                                                  width = "95%")
                                ),

  # Publication Year
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        selectizeInput(inputId = "PubDate",
                                                       label = h6(mandatory_asterisk("Publication year")),
                                                       choices = "",
                                                       selected = "",
                                                       multiple = FALSE,
                                                       width = "95%",
                                                       options = list(maxItems = 1,
                                                                      create = FALSE,
                                                                      closeAfterSelect = TRUE))
                                ),

 # Study Title
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        textInput(inputId = "paper_title",
                                                  label = h6("Title"),
                                                  value = "",
                                                  width = "95%")
                                ),

  # Study Journal
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        textInput(inputId = "journal",
                                                  label = h6("Journal"),
                                                  value = "",
                                                  width = "95%")
                                ),

 # Country of study
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        selectizeInput(inputId = "ISO3",
                                                       label = h6(mandatory_asterisk("Country")),
                                                       choices = c("China" = "",
                                                                   countries_list),
                                                       selected = "",
                                                       multiple = TRUE,
                                                       width = "95%",
                                                       options = list(maxItems = 1,
                                                                      create = FALSE,
                                                                      closeAfterSelect = TRUE))
                                ),
 # Coordinates
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        textInput(inputId = "coordinates",
                                                  label = h6(mandatory_asterisk("Location")),
                                                  value = "",
                                                  placeholder = "Place name or latitude/longitude",
                                                  width = "95%")
                                ),
 # Sampling scheme
                                tags$td(style = "width: 14.28%;",
                                        align = "left",
                                        selectizeInput(inputId = "sampling_scheme",
                                                       label = h6("Sampling scheme"),
                                                       choices = c("Random (simple)" = "",
                                                                   "Connvenience-based",
                                                                   "Random (simple)",
                                                                   "Statistically-based",
                                                                   "Systematic"),
                                                       selected = "",
                                                       multiple = FALSE,
                                                       width = "95%",
                                                       options = list(maxItems = 1,
                                                                      create = FALSE,
                                                                      closeAfterSelect = TRUE))
                                )
                  )
           )
    ),

br(),

  # 3.3.1.3 ###= Antimicrobial resistance =#####
                  fluidRow(
                    column(width = 6,
                           hidden(
                             actionButton(inputId = "show_amr_panel",
                                          label = icon(name = "plus",
                                                       class = "plus_antimicrobial_resistance",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".plus_antimicrobial_resistance {color: #FFFFFF;}")
                           ),
                             actionButton(inputId = "hide_amr_panel",
                                          label = icon(name = "minus",
                                                       class = "minus_antimicrobial_resistance",
                                                       lib = "font-awesome"),
                                          style = "border-radius: 50%; border: 1.5px solid #2D2D2D; background-color:#2D2D2D; padding: 0.5px 3px 0.5px 3px; margin-bottom: 7px; font-size: 65%;"),
                             tags$style(".minus_antimicrobial_resistance {color: #FFFFFF;}"),
                             div(style = "display: inline-block; margin-left: 5px;",
                                 h3(strong(" Antimicrobial resistance")))
                    )
                  ),

    source(file = "modules/ui_antimicrobial-resistance-form.R",
           local = TRUE)$value,

br(),

  # 3.3.1.4 ###= Comments =#####
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

  # 3.3.1.5 ###= Submit =#####
                      fluidRow(id = "submit_button",
                        column(width = 12,
                               align = "center",
                               actionButton(inputId = "submit",
                                            label = h5("Add your survey",
                                                       style = "display: inline-block;"),
                                            icon(name = "upload",
                                                 lib = "font-awesome"),
                                            style = "box-shadow: none!important;
                                                     outline: 0;
                                                     border-width: 0px;
                                                     padding: 0px 5px 0px 5px;")
                        )
                      )
        )
    )
  ),

  # 3.4 #####===== Filter data panel =====#####
  hidden(

    absolutePanel(id = "filterdata_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 11,
                  right = 160,
                  left = "auto",
                  bottom = "auto",
                  width = "49%",
                  height = "auto",

                  tags$p(".",
                         style = "color: white;"),

    hidden(

                  actionButton(inputId = "show_filterdata_panel_inputs",
                               label = icon(name = "plus",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 30px 0px 0px;")
    ),

                  actionButton(inputId = "hide_filterdata_panel_inputs",
                               label = icon(name = "minus",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);;
                                        border-width: 0px;
                                        padding: 0px 30px 40px 0px;"),

                  actionButton(inputId = "close_filterdata_panel",
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),

                  tags$div(id = "filterdata_panel_inputs",

                           tags$table(style = "width: 100%;",
                                      tags$tr(tags$td(style = "width: 10%;",
                                                      align = "center",
                                                      img(src = "filter-figures/world-americas.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_ISO3",
                                                                     label = h5("Countries"),
                                                                     choices = c("India" = "",
                                                                                 as.list(setNames(object = Countries_PPS$ISO3,
                                                                                                  nm = Countries_PPS$Country))),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE))
                                              ),
                                              tags$td(style = "width: 10%;",
                                                      align = "center",
                                                      tags$img(src = "filter-figures/compound.png",
                                                               width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_compound",
                                                                     label = h5("Antibiotics"),
                                                                     choices = c("Penicillin" = "",
                                                                                 antibiotics_available_list),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE))
                                              )
                                      ),
                                      tags$tr(tags$td(style = "width: 10%;
                                                               padding-top: 10px;",
                                                      align = "center",
                                                      img(src = "filter-figures/species.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;
                                                               padding-top: 10px;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_species",
                                                                     label = h5("Species"),
                                                                     choices = c("Chicken" = "",
                                                                                 sort(unique(resistancebank$Species))),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE))
                                              ),
                                              tags$td(style = "width: 10%
                                                               padding-top: 10px;",
                                                      align = "center",
                                                      img(src = "filter-figures/compound_class.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;
                                                               padding-top: 10px;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_compound_class",
                                                                     label = h5("Antibiotics class (WHO)"),
                                                                     choices = c(sort(unique(resistancebank$Class_WHO[resistancebank$Class_WHO != "NA (NA)"]))),
                                                                     selected = NULL,
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(placeholder = "3rd Generation Cephalosporin (Critically important)",
                                                                                    create = FALSE,
                                                                                    closeAfterSelect = TRUE,
                                                                                    maxItems = 1))
                                              )
                                      ),
                                      tags$tr(tags$td(style = "width: 10%;
                                                               padding-top: 10px;",
                                                      align = "center",
                                                      img(src = "filter-figures/sample_type.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;
                                                               padding-top: 10px;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_sample_type",
                                                                     label = h5("Samples type"),
                                                                     choices = list("Product" = "",
                                                                                    "Fecal" = "Fecal",
                                                                                    "Killed animal" = "KilledAnimal",
                                                                                    "Living animal" = "LivingAnimal",
                                                                                    "Meat" = "Meat",
                                                                                    "Product" = "Product"),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE))

                                              ),
                                              tags$td(style = "width: 10%;
                                                               padding-top: 10px;",
                                                      align = "center",
                                                      img(src = "filter-figures/compound_agisar.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;
                                                               padding-top: 10px;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_compound_agisar",
                                                                     label = h5("AGISAR combination"),
                                                                     choices = c("E. coli - Tetracycline" = "",
                                                                                 sort(unique(resistancebank_amr$pathogen_compound))),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE)))
                                      ),
                                      tags$tr(tags$td(style = "width: 10%;
                                                               padding-top: 10px;",
                                                      align = "center",
                                                      img(src = "filter-figures/pathogen.png",
                                                          width = "75%")
                                              ),
                                              tags$td(style = "width: 40%;
                                                               padding-top: 10px;",
                                                      align = "left",
                                                      selectizeInput(inputId = "filterdata_pathogen",
                                                                     label = h5("Pathogens"),
                                                                     choices = c("E. coli" = "",
                                                                                 if ("Enterococcus" %in% unique(sort(resistancebank$Pathogens))) {

                                                                                   setNames(object = as.list(unique(sort(resistancebank$Pathogens))),
                                                                                            nm = c("Campylobacter spp.",
                                                                                                   "E. coli",
                                                                                                   "Enterococcus spp.",
                                                                                                   "S. aureus",
                                                                                                   "Salmonella spp. (non-typhoidal)"))

                                                                                 } else {

                                                                                   setNames(object = as.list(unique(sort(resistancebank$Pathogens))),
                                                                                            nm = c("Campylobacter spp.",
                                                                                                   "E. coli",
                                                                                                   "S. aureus",
                                                                                                   "Salmonella spp. (non-typhoidal)"))

                                                                                 }),
                                                                     selected = "",
                                                                     multiple = TRUE,
                                                                     width = "94%",
                                                                     options = list(create = FALSE,
                                                                                    closeAfterSelect = FALSE))
                                              ),
                                              tags$td(colspan = 2,
                                                      style = "width: 50%;",
                                                      uiOutput(outputId = "filterdata_buttons_and_info")
                                              )
                                      )
                            ),

tags$br(),

                            fluidRow(
                              column(width = 12,
                                     DT::dataTableOutput(outputId = "filterdata_papers",
                                                         width = "100%")
                              )
                            )
                )
    )

  ),

  # 3.5 #####===== Country report panel =====#####
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
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),

br(),

                  tags$table(style = "width: 100%;",
                             tags$tr(tags$td(style = "width: 30%",
                                             align = "left",
                                             h5("Country"),
                                             shinyWidgets::pickerInput(inputId = "Country_report_nation",
                                                                       label = NULL,
                                                                       choices = c(Countries$Country_ISO3),
                                                                       multiple = FALSE,
                                                                       options = list(
                                                                         size = 10,
                                                                         `live-search` = TRUE
                                                                       ))
                                    ),
                                    tags$td(style = "width: 26%;"),
                                    tags$td(style = "width: 21%;",
                                            align = "right",
                                            withBusyIndicatorUI(
                                              downloadButton(outputId = "download_Country_report",
                                                             label = h5("Download as PDF",
                                                                        style = "display: inline-block;"),
                                                             style = "box-shadow: none!important;
                                                                      outline: 0;
                                                                      border-width: 0px;
                                                                      padding: 0px 5px 0px 5px;")
                                            )
                                    ),
                                    tags$td(style = "width: 2%;"),
                                    tags$td(style = "width: 21%",
                                            align = "left",
                                            withBusyIndicatorUI(
                                              downloadButton(outputId = "download_Country_data",
                                                             label = h5("Download data",
                                                                        style = "display: inline-block;"),
                                                             style = "box-shadow: none!important;
                                                                      outline: 0;
                                                                      border-width: 0px;
                                                                      padding: 0px 5px 0px 5px;"))
                                    )
                             )

                  ),

div(style = "height: 0.5vh;"),

                  fluidRow(
                    column(width = 12,
                           uiOutput(outputId = "country_report_previews")
                    )
                  )

    )

  ),

  # 3.6 #####===== Download resistancebank and AMR map panel =====#####
  hidden(

    absolutePanel(id = "download_RESBANK_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = "21%",
                  right = 0,
                  left = 0,
                  bottom = "auto",
                  width = "20%",
                  height = "auto",

                  actionButton(inputId = "close_download_RESBANK_panel",
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),

                  tags$br(),

                  tags$div(style = "padding: 0px 10px 0px 10px;",

                    tags$table(style = "width: 100%;
                                        table-layout: fixed;",
                               tags$tr(tags$td(align = "left",
                                               tags$p("Our database is freely available, but we are interested to know about you",
                                                      style = "font-size: 16px;
                                                               font-weight: bold;
                                                               text-align: center;")
                               )),
                               tags$tr(tags$td(align = "left",
                                               textInput(inputId = "RESBANK_downloader_name",
                                                         label = h5("Name"),
                                                         value = "",
                                                         width = "100%")
                               )),
                               tags$tr(tags$td(align = "left",
                                               tags$style(".bs-placeholder {color: #FFFFFF !important;}"),
                                               shinyWidgets::pickerInput(inputId = "RESBANK_downloader_country",
                                                                         label = h5("Country"),
                                                                         choices = c("",
                                                                                     Countries$Country),
                                                                         selected = "",
                                                                         multiple = FALSE,
                                                                         width = "100%",
                                                                         options = shinyWidgets::pickerOptions(
                                                                           size = 10,
                                                                           `live-search` = FALSE,
                                                                           noneSelectedText = "Select country"
                                                                         )),
                               )),
                               tags$tr(tags$td(align = "left",
                                               textInput(inputId = "RESBANK_downloader_institution",
                                                         label = h5("Institution"),
                                                         value = "",
                                                         width = "100%")
                               )),
                               tags$tr(tags$td(align = "left",
                                               textInput(inputId = "RESBANK_downloader_email",
                                                         label = h5("E-mail"),
                                                         value = "",
                                                         width = "100%"),
                                               textOutput(outputId = "wrong_RESBANK_downloader_email")
                               ))
                    ),

                    tags$br(),

                    fluidRow(
                      column(width = 12,
                             align = "center",
                             withBusyIndicatorUI(
                                downloadButton(outputId = "download_RESBANK",
                                               label = h5("Download database",
                                                          style = "display: inline-block;"),
                                               style = "box-shadow: none!important;
                                                        outline: 0;
                                                        border-width: 0px;
                                                        padding: 0px 5px 0px 5px;")),

                             tags$br(),

                             tags$p("Using ",
                                    tags$a("resistancebank.org",
                                           href = "https://resistancebank.org",
                                           target = "_blank",
                                           .noWS = "outside"),
                                    " for your work? Thank you for citing ",
                                    tags$a("Criscuolo et al., 2021. resistancebank.org, an open-access repository
                                              for surveys of antimicrobial resistance in animals, Scientific Data 8 (189)",
                                           href = "https://www.nature.com/articles/s41597-021-00978-9",
                                           target = "_blank",
                                           .noWS = "outside"),
                                    ".",
                                    style = "color: #A8A4A4;
                                               font-size: 11px;
                                               text-align: justify;")

                      ),

                    )

                  )

    )
  ),

  hidden(

    absolutePanel(id = "download_P50_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = "21%",
                  right = 0,
                  left = 0,
                  bottom = "auto",
                  width = "20%",
                  height = "auto",

                  actionButton(inputId = "close_download_P50_panel",
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                          top: 0;
                                          right: 0;
                                          background-color: rgba(0, 0, 0, 0);
                                          border-width: 0px;
                                          padding: 0px 5px 0px 0px;"),

                  tags$br(),

                  tags$div(style = "padding: 0px 10px 0px 10px;",

                           tags$table(style = "width: 100%;
                                          table-layout: fixed;",
                                      tags$tr(tags$td(align = "left",
                                                      tags$p("Our database is freely available, but we are interested to know about you",
                                                             style = "font-size: 16px;
                                                                 font-weight: bold;
                                                                 text-align: center;")
                                      )),
                                      tags$tr(tags$td(align = "left",
                                                      textInput(inputId = "P50_downloader_name",
                                                                label = h5("Name"),
                                                                value = "",
                                                                width = "100%")
                                      )),
                                      tags$tr(tags$td(align = "left",
                                                      tags$style(".bs-placeholder {color: #FFFFFF !important;}"),
                                                      shinyWidgets::pickerInput(inputId = "P50_downloader_country",
                                                                                label = h5("Country"),
                                                                                choices = c("",
                                                                                            Countries$Country),
                                                                                selected = "",
                                                                                multiple = FALSE,
                                                                                width = "100%",
                                                                                options = shinyWidgets::pickerOptions(
                                                                                  size = 10,
                                                                                  `live-search` = FALSE,
                                                                                  noneSelectedText = "Select country"
                                                                                )),
                                      )),
                                      tags$tr(tags$td(align = "left",
                                                      textInput(inputId = "P50_downloader_institution",
                                                                label = h5("Institution"),
                                                                value = "",
                                                                width = "100%")
                                      )),
                                      tags$tr(tags$td(align = "left",
                                                      textInput(inputId = "P50_downloader_email",
                                                                label = h5("E-mail"),
                                                                value = "",
                                                                width = "100%"),
                                                      textOutput(outputId = "wrong_P50_downloader_email")
                                      ))
                           ),

                           tags$br(),

                           fluidRow(
                             column(width = 12,
                                    align = "center",
                                    withBusyIndicatorUI(
                                      downloadButton(outputId = "download_P50",
                                                     label = h5("Download map",
                                                                style = "display: inline-block;"),
                                                     style = "box-shadow: none!important;
                                                              outline: 0;
                                                              border-width: 0px;
                                                              padding: 0px 5px 0px 5px;")),

                                    tags$br(),

                                    tags$p("Using ",
                                           tags$a("resistancebank.org",
                                                  href = "https://resistancebank.org",
                                                  target = "_blank",
                                                  .noWS = "outside"),
                                           " for your work? Thank you for citing ",
                                           tags$a("Criscuolo et al., 2021. resistancebank.org, an open-access repository
                                              for surveys of antimicrobial resistance in animals, Scientific Data 8 (189)",
                                                  href = "https://www.nature.com/articles/s41597-021-00978-9",
                                                  target = "_blank",
                                                  .noWS = "outside"),
                                           ".",
                                           style = "color: #A8A4A4;
                                               font-size: 11px;
                                               text-align: justify;")

                             ),

                           )

                  )

    )
  ),

  # 3.7 #####===== About panel =====#####
  hidden(

    absolutePanel(id = "about_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 11,
                  right = 160,
                  left = "auto",
                  bottom = "auto",
                  width = "32%",
                  height = "auto",

                  actionButton(inputId = "close_about_panel",
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                                        padding: 0px 5px 0px 0px;"),

                  tags$p(strong("resistancebank.org"),
                         style = "font-size: 1.6vmin;
                                  padding: 0px 0px 0px 4px;"),

                  fluidRow(
                    column(width = 12,
                      source(file = "modules/ui_text-about-panel.R",
                             local = TRUE)$value
                    )
                  )

    )

  ),

  # 3.8 #####===== Limitations panel =====#####
  hidden(

    absolutePanel(id = "limitations_panel",
                  fixed = TRUE,
                  draggable = FALSE,
                  top = 11,
                  right = 160,
                  left = "auto",
                  bottom = "auto",
                  width = "32%",
                  height = "auto",

                  actionButton(inputId = "close_limitations_panel",
                               label = icon(name = "xmark",
                                            lib = "font-awesome"),
                               style = "position: absolute;
                                        top: 0;
                                        right: 0;
                                        background-color: rgba(0, 0, 0, 0);
                                        border-width: 0px;
                               padding: 0px 5px 0px 0px;"),

                  tags$p(strong("Limitations"),
                         style = "font-size: 1.6vmin;
                                  padding: 0px 0px 0px 4px;"),

                  fluidRow(
                    column(width = 12,
                           source(file = "modules/ui_text-limitations-panel.R",
                                  local = TRUE)$value
                    )
                  )

    )

  )

  #########################=========================#########################
  ), # Closes the "World Map" division

) # Closes fluidPage
