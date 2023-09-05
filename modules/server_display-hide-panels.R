
  # 1.1.1 ###= Initial panel =#####
  observeEvent(input$explore_amr_map |
               input$add_your_survey1 |
               input$display_download_RESBANK_panel2 |
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

                 showElement(id = "full_screen_view_panel",
                             anim = TRUE,
                             animType = "fade")

                 showElement(id = "switch_farming_type_panel",
                             anim = TRUE,
                             animType = "fade")

                 showElement(id = "name_Maps_panel",
                             anim = TRUE,
                             animType = "fade")

               }, ignoreInit = TRUE)

  observeEvent(input$display_download_RESBANK_panel2, {

    showElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "display_download_RESBANK_panel",
                anim = FALSE)

    showElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

  })



  # 1.1.2 ###= Controls panel =#####

  # Add your survey panel
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



    hideElement(id = "filterdata_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_filterdata_panel_controls",
                anim = FALSE)

    showElement(id = "display_filterdata_panel",
                anim = FALSE)



    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)



    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)



    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)



    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)



    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
                anim = FALSE)

  })

  # Fill form panel
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

  # Form's user fields
  observeEvent(input$show_user_information_panel, {

    showElement(id = "user_information_panel",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "show_user_information_panel")

    showElement(id = "hide_user_information_panel")

  })

  observeEvent(input$hide_user_information_panel, {

    hideElement(id = "user_information_panel",
                anim = TRUE,
                animType = "slide")

    showElement(id = "show_user_information_panel")

    hideElement(id = "hide_user_information_panel")

  })

  # Form's study fields
  observeEvent(input$show_study_information_panel, {

    showElement(id = "study_information_panel",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "show_study_information_panel")

    showElement(id = "hide_study_information_panel")

  })

  observeEvent(input$hide_study_information_panel, {

    hideElement(id = "study_information_panel",
                anim = TRUE,
                animType = "slide")

    showElement(id = "show_study_information_panel")

    hideElement(id = "hide_study_information_panel")

  })

  # Form's AMR fields
  observeEvent(input$show_amr_panel, {

    showElement(id = "amr_panel",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "show_amr_panel")

    showElement(id = "hide_amr_panel")

  })

  observeEvent(input$hide_amr_panel, {

    hideElement(id = "amr_panel",
                anim = TRUE,
                animType = "slide")

    showElement(id = "show_amr_panel")

    hideElement(id = "hide_amr_panel")

  })

  # Form's remark field
  observeEvent(input$show_remark_panel, {

    showElement(id = "remark_panel",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "show_remark_panel")

    showElement(id = "hide_remark_panel")

  })

  observeEvent(input$hide_remark_panel, {

    hideElement(id = "remark_panel",
                anim = TRUE,
                animType = "slide")

    showElement(id = "show_remark_panel")

    hideElement(id = "hide_remark_panel")

  })

  # Show limited controls button available in "Freshwater" and "Marine" mode
  observe({

    if (input$switch_farming_type == "freshwater" |
        input$switch_farming_type == "marine") {

      hideElement(id = "add_your_survey2",
                  anim = FALSE)

      hideElement(id = "close_add_your_survey_controls",
                  anim = FALSE)

      hideElement(id = "close_add_your_survey_controls2",
                  anim = FALSE)

      hideElement(id = "add_your_survey_division",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "form",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_filterdata_panel",
                  anim = FALSE)

      hideElement(id = "close_filterdata_panel_controls",
                  anim = FALSE)

      hideElement(id = "filterdata_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_Country_report_panel",
                  anim = FALSE)

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      hideElement(id = "download_Country_report_panel",
                  anim = FALSE)

    } else {

      showElement(id = "add_your_survey2",
                  anim = FALSE)

      showElement(id = "display_filterdata_panel",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)

    }

  })

  # 1.1.3 ###= Filter data panel =#####
  observeEvent(input$display_filterdata_panel, {



    showElement(id = "filterdata_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "display_filterdata_panel",
                anim = FALSE)

    showElement(id = "close_filterdata_panel_controls",
                anim = FALSE)



    hideElement(id = "add_your_survey_division",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls",
                anim = FALSE)

    showElement(id = "add_your_survey2",
                anim = FALSE)



    hideElement(id = "download_Country_report_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_country_report_panel_controls",
                anim = FALSE)

    showElement(id = "display_Country_report_panel",
                anim = FALSE)



    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)



    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)



    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)



    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
                anim = FALSE)



    hideElement(id = "form",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_add_your_survey_controls2",
                anim = FALSE)

    # Filter panel inputs
    showElement(id = "filterdata_panel_inputs",
                anim = FALSE)

    hideElement(id = "show_filterdata_panel_inputs",
                anim = FALSE)

    showElement(id = "hide_filterdata_panel_inputs",
                anim = FALSE)

  })

  observeEvent(input$close_filterdata_panel, {

    hideElement(id = "filterdata_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_filterdata_panel_controls",
                anim = FALSE)

    showElement(id = "display_filterdata_panel",
                anim = FALSE)

  })

  observeEvent(input$close_filterdata_panel_controls, {

    hideElement(id = "filterdata_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_filterdata_panel_controls",
                anim = FALSE)

    showElement(id = "display_filterdata_panel",
                anim = FALSE)

    # Filter panel inputs
    Sys.sleep(time = 0.5)

    showElement(id = "filterdata_panel_inputs",
                anim = FALSE)

    hideElement(id = "show_filterdata_panel_inputs",
                anim = FALSE)

    showElement(id = "hide_filterdata_panel_inputs",
                anim = FALSE)

  })

  observeEvent(input$hide_filterdata_panel_inputs, {

    hideElement(id = "filterdata_panel_inputs",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "hide_filterdata_panel_inputs")

    showElement(id = "show_filterdata_panel_inputs")

  })

  observeEvent(input$show_filterdata_panel_inputs, {

    showElement(id = "filterdata_panel_inputs",
                anim = TRUE,
                animType = "slide")

    hideElement(id = "show_filterdata_panel_inputs")

    showElement(id = "hide_filterdata_panel_inputs")

  })

  # 1.1.4 ###= Country report panel =#####
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



    hideElement(id = "filterdata_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_filterdata_panel_controls",
                anim = FALSE)

    showElement(id = "display_filterdata_panel",
                anim = FALSE)



    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)



    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)



    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)



    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
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

  # 1.1.5 ###= Download resistancebank panel =#####
  observeEvent(input$display_download_RESBANK_panel, {

    if (input$switch_farming_type != "livestock") {



      showElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)

      showElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
                  anim = FALSE)



      hideElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_about_panel_controls",
                  anim = FALSE)

      showElement(id = "display_about_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)

    } else {



      showElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)

      showElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)



      hideElement(id = "add_your_survey_division",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls",
                  anim = FALSE)

      showElement(id = "add_your_survey2",
                  anim = FALSE)



      hideElement(id = "filterdata_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_filterdata_panel_controls",
                  anim = FALSE)

      showElement(id = "display_filterdata_panel",
                  anim = FALSE)



      hideElement(id = "download_Country_report_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
                  anim = FALSE)



      hideElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_about_panel_controls",
                  anim = FALSE)

      showElement(id = "display_about_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)



      hideElement(id = "form",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls2",
                  anim = FALSE)

    }

  })

  observeEvent(input$close_download_RESBANK_panel, {

    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)

  })

  observeEvent(input$close_download_RESBANK_panel_controls, {

    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)

  })



  # 1.1.6 ###= Download P50 map panel =#####
  observeEvent(input$display_download_P50_panel, {

    if (input$switch_farming_type != "livestock") {



      showElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_download_P50_panel",
                  anim = FALSE)

      showElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)



      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_about_panel_controls",
                  anim = FALSE)

      showElement(id = "display_about_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)

    } else {



      showElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_download_P50_panel",
                  anim = FALSE)

      showElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)



      hideElement(id = "add_your_survey_division",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls",
                  anim = FALSE)

      showElement(id = "add_your_survey2",
                  anim = FALSE)



      hideElement(id = "filterdata_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_filterdata_panel_controls",
                  anim = FALSE)

      showElement(id = "display_filterdata_panel",
                  anim = FALSE)



      hideElement(id = "download_Country_report_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)



      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_about_panel_controls",
                  anim = FALSE)

      showElement(id = "display_about_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)



      hideElement(id = "form",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls2",
                  anim = FALSE)

    }

  })

  observeEvent(input$close_download_P50_panel, {

    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)

  })

  observeEvent(input$close_download_P50_panel_controls, {

    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)

  })



  # 1.1.7 ###= About panel =#####
  observeEvent(input$display_about_panel, {

    if (input$switch_farming_type != "livestock") {



      showElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_about_panel",
                  anim = FALSE)

      showElement(id = "close_about_panel_controls",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
                  anim = FALSE)



      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)

    } else {

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



      hideElement(id = "filterdata_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_filterdata_panel_controls",
                  anim = FALSE)

      showElement(id = "display_filterdata_panel",
                  anim = FALSE)



      hideElement(id = "download_Country_report_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)



      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
                  anim = FALSE)



      hideElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_limitations_panel_controls",
                  anim = FALSE)

      showElement(id = "display_limitations_panel",
                  anim = FALSE)



      hideElement(id = "form",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls2",
                  anim = FALSE)

    }

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

  # 1.1.8 ###= Limitations panel =#####
  observeEvent(input$display_limitations_panel, {

    if (input$switch_farming_type != "livestock") {



      showElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_limitations_panel",
                  anim = FALSE)

      showElement(id = "close_limitations_panel_controls",
                  anim = FALSE)




      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
                  anim = FALSE)



      hideElement(id = "about_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_about_panel_controls",
                  anim = FALSE)

      showElement(id = "display_about_panel",
                  anim = FALSE)

    } else {



      showElement(id = "limitations_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "display_limitations_panel",
                  anim = FALSE)

      showElement(id = "close_limitations_panel_controls",
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



      hideElement(id = "filterdata_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_filterdata_panel_controls",
                  anim = FALSE)

      showElement(id = "display_filterdata_panel",
                  anim = FALSE)



      hideElement(id = "download_Country_report_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)



      hideElement(id = "download_RESBANK_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_RESBANK_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_RESBANK_panel",
                  anim = FALSE)



      hideElement(id = "download_P50_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_download_P50_panel_controls",
                  anim = FALSE)

      showElement(id = "display_download_P50_panel",
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

    }

  })

  observeEvent(input$close_limitations_panel, {

    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
                anim = FALSE)

  })

  observeEvent(input$close_limitations_panel_controls, {

    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
                anim = FALSE)

  })

  # 1.1.7 #####===== Close or hide panels when clicking on map background =====#####
  observeEvent(input$world_map_click, {



    hideElement(id = "download_RESBANK_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_RESBANK_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_RESBANK_panel",
                anim = FALSE)



    hideElement(id = "download_P50_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_download_P50_panel_controls",
                anim = FALSE)

    showElement(id = "display_download_P50_panel",
                anim = FALSE)



    hideElement(id = "about_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_about_panel_controls",
                anim = FALSE)

    showElement(id = "display_about_panel",
                anim = FALSE)



    hideElement(id = "limitations_panel",
                anim = TRUE,
                animType = "fade")

    hideElement(id = "close_limitations_panel_controls",
                anim = FALSE)

    showElement(id = "display_limitations_panel",
                anim = FALSE)



    if (input$switch_farming_type == "livestock") {

      hideElement(id = "add_your_survey_division",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "form",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_add_your_survey_controls",
                  anim = FALSE)

      hideElement(id = "close_add_your_survey_controls2",
                  anim = FALSE)

      showElement(id = "add_your_survey2",
                  anim = FALSE)



      hideElement(id = "filterdata_panel_inputs",
                  anim = TRUE,
                  animType = "slide")

      hideElement(id = "hide_filterdata_panel_inputs")

      showElement(id = "show_filterdata_panel_inputs")



      hideElement(id = "download_Country_report_panel",
                  anim = TRUE,
                  animType = "fade")

      hideElement(id = "close_country_report_panel_controls",
                  anim = FALSE)

      showElement(id = "display_Country_report_panel",
                  anim = FALSE)

    }

  })
