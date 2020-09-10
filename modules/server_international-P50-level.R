
  ###= Reactive International P50 index for R Markdown Country report
  AMR_Exposure_data_rm <- reactive({

    AMR_Exposure_rm <- AMR_Exposure

    if (is.null(input$Country_report_nation)) {

      AMR_Exposure_rm$Selection <- rep(x = FALSE,
                                       times = nrow(AMR_Exposure_rm))

      AMR_Exposure_data_rm <- AMR_Exposure_rm

    } else {

      AMR_Exposure_rm$Selection <- sapply(X = 1:nrow(AMR_Exposure_rm),
                                          FUN = function(i) {

                                            subset(x = Countries_PPS,
                                                   subset = is.element(el = Country_ISO3,
                                                                       set = input$Country_report_nation))$Country %in% AMR_Exposure_rm$Country[i]

                                          })

      AMR_Exposure_data_rm <- AMR_Exposure_rm

    }

    return(AMR_Exposure_data_rm)

  })
