
amr_test_values <- reactiveValues(val = 0,
                                  ignore = 0)


#####===== Define the input number to count every row =====#####
input_number <- reactive({

  input_number <- input$add_amr_test + 1

  return(input_number)

})


observeEvent(input$add_amr_test, {

  amr_test_divId <- max(amr_test_values$val) + 1

  #####==== Define variables to store previous widget values =====#####

  # Species
  if (!is.null(eval(parse(text = paste0("input$species_", input_number() - 1))))) {

    species_value = eval(parse(text = paste0("input$species_", input_number() - 1)))

  } else {

    species_value = ""

  }

  # Animal sample type
  if (!is.null(eval(parse(text = paste0("input$sample_type_", input_number() - 1))))) {

    sample_type_value = eval(parse(text = paste0("input$sample_type_", input_number() - 1)))

  } else {

    sample_type_value = ""

  }

  # Pathogen
  if (!is.null(eval(parse(text = paste0("input$pathogen_", input_number() - 1))))) {

    pathogen_value = eval(parse(text = paste0("input$pathogen_", input_number() - 1)))

  } else {

    pathogen_value = ""

  }

  # Method
  if (!is.null(eval(parse(text = paste0("input$method_", input_number() - 1))))) {

    method_value = eval(parse(text = paste0("input$method_", input_number() - 1)))

  } else {

    method_value = ""

  }

  # Number of samples
  if (!is.null(eval(parse(text = paste0("input$nsamples_", input_number() - 1))))) {

    nsamples_value = eval(parse(text = paste0("input$nsamples_", input_number() - 1)))

  } else {

    nsamples_value = ""

  }

  # Isolates
  if (!is.null(eval(parse(text = paste0("input$isolates_", input_number() - 1))))) {

    isolates_value = eval(parse(text = paste0("input$isolates_", input_number() - 1)))

  } else {

    isolates_value = ""

  }

  # Prevalence
  if (!is.null(eval(parse(text = paste0("input$prevalence_", input_number() - 1))))) {

    prevalence_value = eval(parse(text = paste0("input$prevalence_", input_number() - 1)))

  } else {

    prevalence_value = ""

  }

  # Guidelines
  if (!is.null(eval(parse(text = paste0("input$guidelines_", input_number() - 1))))) {

    guidelines_value = eval(parse(text = paste0("input$guidelines_", input_number() - 1)))

  } else {

    guidelines_value = ""

  }

  # Document - EUCAST
  if (!is.null(eval(parse(text = paste0("input$document_EUCAST_", input_number() - 1))))) {

    document_EUCAST_value = eval(parse(text = paste0("input$document_EUCAST_", input_number() - 1)))

  } else {

    document_EUCAST_value = ""

  }

  # Document - CLSI
  if (!is.null(eval(parse(text = paste0("input$document_CLSI_", input_number() - 1))))) {

    document_CLSI_value = eval(parse(text = paste0("input$document_CLSI_", input_number() - 1)))

  } else {

    document_CLSI_value = ""

  }

  # Document - SFM
  if (!is.null(eval(parse(text = paste0("input$document_SFM_", input_number() - 1))))) {

    document_SFM_value = eval(parse(text = paste0("input$document_SFM_", input_number() - 1)))

  } else {

    document_SFM_value = ""

  }

  #####===== Insert dynamic UI =====#####
  insertUI(
    selector = "#amr_test_placeholder",
    where = "beforeBegin",
    ui = tags$div(id = amr_test_divId,
                  fluidRow(
                    column(width = 1,
                           style = "padding: 0px 5px 0px 5px;",
                           splitLayout(cellWidths = c("15%", "85%"), # 8%
                                       actionButton(inputId = paste0("delete_row_",
                                                                     input_number()),
                                                    label = div(icon(name = "times",
                                                                     lib = "font-awesome")),
                                                    style = "color: #FFFFFF; border-radius: 50%; border: 1px solid #2D2D2D; background-color: #2D2D2D; font-color: red; padding: 0.6px 2.8px 0.3px 3px; font-size:48%; margin-top: 8px;"),
                                       # h6(input_number()),
                                       selectizeInput(inputId = paste0("species_",
                                                                       input_number()),
                                                      label =  NULL,
                                                      choices = c("Chicken" = "",
                                                                  "Chicken",
                                                                  "Pig",
                                                                  "Cattle",
                                                                  "Sheep",
                                                                  "Duck",
                                                                  "Buffalo",
                                                                  "Horse"),
                                                      selected = species_value,
                                                      width = "100%",
                                                      options = list(create = TRUE))
                           )
                    ),
                    column(width = 1,
                           style = "padding: 0px 2px 0px 2px;",
                           selectizeInput(inputId = paste0("sample_type_",
                                                           input_number()),
                                          label = NULL,
                                          choices = list("Live animal" = "",
                                                         "Live animal" = "LivingAnimal",
                                                         "Food products" = "Product",
                                                         "Fecal" = "Fecal",
                                                         "Slaughtered" = "KilledAnimal"),
                                          selected = sample_type_value,
                                          width = "100%")
                    ),
                    column(width = 1,
                           style = "padding: 0px 2px 0px 0px;",
                           selectizeInput(inputId = paste0("pathogen_",
                                                           input_number()),
                                          label = NULL,
                                          choices = list("E. coli" = "",
                                                         "E. coli" = "E. coli",
                                                         "Salmonella spp. (non-typhoidal)" = "Salmonella",
                                                         "Campylobacter spp." = "Campylobacter",
                                                         "Enterococcus spp." = "Enterococcus",
                                                         "S. aureus" = "S. aureus"),
                                          selected = pathogen_value,
                                          width = "100%",
                                          options = list(create = FALSE))
                    ),
                    column(width = 1,
                           style = "padding: 0px 2px 0px 2px;",
                           selectizeInput(inputId = paste0("method_",
                                                           input_number()),
                                          label = NULL,
                                          choices = list("Disk diffusion" = "",
                                                         "Disk diffusion" = "DD",
                                                         "Agar diluition" = "AD",
                                                         "Broth diluition" = "BD",
                                                         "Etest" = "Etest",
                                                         "VITEK" = "VITEK",
                                                         "SIRscan" = "SIRscan"),
                                          selected = method_value,
                                          width = "100%",
                                          multiple = FALSE,
                                          options = list(create = TRUE))
                    ),
                    column(width = 2,
                           style = "padding: 0px 0px 0px 5px;",
                           selectizeInput(inputId = paste0("compound_",
                                                           input_number()),
                                          label = NULL,
                                          choices = c("Penicillin" = "",
                                                      lapply(X = Antibiotics_list,
                                                             FUN = function(i) {

                                                               i[-c(1, 3, 4, 5)]

                                                             })),
                                          selected = "",
                                          width = "100%")
                    ),
                    column(width = 3,
                           align = "center",
                           splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                                       selectizeInput(inputId = paste0("rescom_",
                                                                       input_number()),
                                                      label = NULL,
                                                      choices = c("100" = "",
                                                                  seq(from = 0,
                                                                      to = 100,
                                                                      by = 1)),
                                                      selected = "",
                                                      width = "90%"),
                                       selectizeInput(inputId = paste0("nsamples_",
                                                                       input_number()),
                                                      label = NULL,
                                                      choices = c("10" = "",
                                                                  1:200,
                                                                  nsamples_value),
                                                      selected = nsamples_value,
                                                      width = "90%",
                                                      options = list(create = TRUE)),
                                       selectizeInput(inputId = paste0("isolates_",
                                                                       input_number()),
                                                      label = NULL,
                                                      choices = c("20" = "",
                                                                  1:200,
                                                                  isolates_value),
                                                      selected = isolates_value,
                                                      width = "90%",
                                                      options = list(create = TRUE)),
                                       selectizeInput(inputId = paste0("prevalence_",
                                                                       input_number()),
                                                      label = NULL,
                                                      choices = c("50" = "",
                                                                  seq(from = 0,
                                                                      to = 100,
                                                                      by = 1)),
                                                      selected = prevalence_value,
                                                      width = "90%")
                           )
                    ),
                    column(width = 1,
                           style = "padding: 0px 2px 0px 2px;",
                           selectizeInput(inputId = paste0("guidelines_",
                                                           input_number()),
                                          label = NULL,
                                          choices = c("EUCAST" = "",
                                                      "EUCAST",
                                                      "CLSI",
                                                      "SFM"),
                                          selected = guidelines_value)
                    ),
                    column(width = 1,
                           style = "padding: 0px 0px 0px 0px;",
                           div(id = "document_div",
                               tags$style("#document_div .selectize-dropdown .optgroup-header {font-size: 10px;}"),
                               conditionalPanel(condition = parse(text = paste0("input.guidelines_", input_number(), " == ''")),
                                                selectizeInput(inputId = paste0("document_NULL_",
                                                                                input_number()),
                                                               label = NULL,
                                                               choices = c("Guidelines?" = ""),
                                                               selected = "")
                               ),
                               conditionalPanel(condition = parse(text = paste0("input.guidelines_", input_number(), " == 'EUCAST'")),
                               selectizeInput(inputId = paste0("document_EUCAST_",
                                                               input_number()),
                                              label = NULL,
                                              choices = list("Table 2019" = "",
                                                             "EUCAST - ECOFF" = c(`ECOFF` = "ECOFF"),
                                                             "EUCAST - Human" = c(`Table 2019` = "EUCAST_2019",
                                                                                  `Table 2018` = "EUCAST_2018",
                                                                                  `Table 2017` = "EUCAST_2017",
                                                                                  `Table 2016` = "EUCAST_2016",
                                                                                  `Table 2015` = "EUCAST_2015",
                                                                                  `Table 2014` = "EUCAST_2014",
                                                                                  `Table 2013` = "EUCAST_2013",
                                                                                  `Table 2012` = "EUCAST_2012",
                                                                                  `Table 2011` = "EUCAST_2011",
                                                                                  `Table 2010` = "EUCAST_2010",
                                                                                  `Table 2009` = "EUCAST_2009")),
                                              selected = document_EUCAST_value,
                                              width = "100%")
                               ),
                               conditionalPanel(condition = parse(text = paste0("input.guidelines_", input_number(), " == 'CLSI'")),
                                                selectizeInput(inputId = paste0("document_CLSI_",
                                                                                input_number()),
                                                               label = NULL,
                                                               choices = list("M100_S29_2019" = "",
                                                                              "CLSI - Human" = c(`M100-S29 2019` = "M100_S29_2019",
                                                                                                 `M100-S28 2018` = "M100_S28_2018",
                                                                                                 `M100-S27 2017` = "M100_S27_2017",
                                                                                                 `M100-S26 2016` = "M100_S26_2016",
                                                                                                 `M100-S25 2015` = "M100_S25_2015",
                                                                                                 `M100-S24 2014` = "M100_S24_2014",
                                                                                                 `M100-S23 2013` = "M100_S23_2013",
                                                                                                 `M100-S22 2012` = "M100_S22_2012",
                                                                                                 `M100-S21 2011` = "M100_S21_2011",
                                                                                                 `M100-S20 2010` = "M100_S20_2010",
                                                                                                 `M100-S19 2009` = "M100_S19_2009",
                                                                                                 `M100-S18 2008` = "M100_S18_2008",
                                                                                                 `M100-S17 2007` = "M100_S17_2007",
                                                                                                 `M100-S16 2006` = "M100_S16_2006",
                                                                                                 `M100-S15 2005` = "M100_S15_2005",
                                                                                                 `M100-S14 2004` = "M100_S14_2004",
                                                                                                 `M100-S13 2003` = "M100_S13_2003",
                                                                                                 `M100-S12 2002` = "M100_S12_2002",
                                                                                                 `M100-S11 2001` = "M100_S11_2001",
                                                                                                 `M100-S10 2000` = "M100_S10_2000"),
                                                                              "CLSI - Campylobacter" = c(`M45-A3 2016` = "M45_A3_2016",
                                                                                                         `M45-A2 2010` = "M45_A2_2010",
                                                                                                         `M45-A 2006` = "M45_A_2006",
                                                                                                         `M45-P 2000` = "M45_P_2000"),
                                                                              "CLSI - Veterinary" = c(`VET01-A5 2018` = "VET01_A5_2018",
                                                                                                      `VET01-A4 2013` = "VET01_A4_2013",
                                                                                                      `M31-A3 2008` = "M31_A3_2008",
                                                                                                      `M31-A2 2002` = "M31_A2_2002",
                                                                                                      `M31-A 1999` = "M31_A_1999",
                                                                                                      `M31-P 1997` = "M31_P_1997")),
                                                               selected = document_CLSI_value,
                                                               width = "100%")
                                                ),
                               conditionalPanel(condition = parse(text = paste0("input.guidelines_", input_number(), " == 'SFM'")),
                                                selectizeInput(inputId = paste0("document_SFM_",
                                                                                input_number()),
                                                               label = NULL,
                                                               choices = list("CA_SFM_2019" = "",
                                                                              "SFM - Human" = c(`CA-SFM 2019` = "CA_SFM_2019",
                                                                                                `CA-SFM 2018` = "CA_SFM_2018",
                                                                                                `CA-SFM 2017` = "CA_SFM_2017",
                                                                                                `CA-SFM 2016` = "CA_SFM_2016",
                                                                                                `CA-SFM 2015` = "CA_SFM_2015",
                                                                                                `CA-SFM 2014` = "CA_SFM_2014",
                                                                                                `CA-SFM 2013` = "CA_SFM_2013",
                                                                                                `CA-SFM 2012` = "CA_SFM_2012",
                                                                                                `CA-SFM 2011` = "CA_SFM_2011",
                                                                                                `CA-SFM 2010` = "CA_SFM_2010",
                                                                                                `CA-SFM 2009` = "CA_SFM_2009",
                                                                                                `CA-SFM 2008` = "CA_SFM_2008",
                                                                                                `CA-SFM 2007` = "CA_SFM_2007",
                                                                                                `CA-SFM 2006` = "CA_SFM_2006",
                                                                                                `CA-SFM 2005` = "CA_SFM_2005",
                                                                                                `CA-SFM 2004` = "CA_SFM_2004",
                                                                                                `CA-SFM 2003` = "CA_SFM_2003",
                                                                                                `CA-SFM 2002` = "CA_SFM_2002"),
                                                                              "SFM - Veterinary" = c(`CA-SFM VET 2018` = "CA_SFM_VET_2018",
                                                                                                     `CA-SFM VET 2017` = "CA_SFM_VET_2017",
                                                                                                     `CA-SFM VET 2016` = "CA_SFM_VET_2016",
                                                                                                     `CA-SFM VET 2015` = "CA_SFM_VET_2015",
                                                                                                     `CA-SFM VET 2013` = "CA_SFM_VET_2013",
                                                                                                     `CA-SFM VET 2012` = "CA_SFM_VET_2012",
                                                                                                     `CA-SFM VET 2010` = "CA_SFM_VET_2010")),
                                                               selected = document_SFM_value,
                                                               width = "100%")
                               )
                           )
                    ),
                    column(width = 1,
                           align = "center",
                           style = "padding: 0px 0px 0px 2px;",
                           div(id = "breakpoint_div",
                               # tags$style("#breakpoint_div .selectize-input {width: 80px;}"),
                               conditionalPanel(condition = parse(text = paste0("input.method_", input_number(), " == ''")),
                               selectizeInput(inputId = paste0("breakpoint_NULL_",
                                                               input_number()),
                                              label = NULL,
                                              choices = c("Method?" = ""),
                                              selected = "",
                                              width = "100%")
                               ),
                               conditionalPanel(condition = parse(text = paste0("input.method_", input_number(), " == 'DD' ||
                                                                                input.method_", input_number(), " == 'SIRscan'")),
                                                selectizeInput(inputId = paste0("breakpoint_Disk_",
                                                                                input_number()),
                                                               label = NULL,
                                                               choices = c("e. g. 15 mm" = "",
                                                                           paste0(seq(from = 6,
                                                                                      to = 50,
                                                                                      by = 1),
                                                                                  " mm")),
                                                               selected = "",
                                                               width = "100%")
                               ),
                               conditionalPanel(condition = parse(text = paste0("input.method_", input_number(), " == 'AD' ||
                                                                                 input.method_", input_number(), " == 'BD' ||
                                                                                 input.method_", input_number(), " == 'Etest' ||
                                                                                 input.method_", input_number(), " == 'VITEK'")),
                                                selectizeInput(inputId = paste0("breakpoint_MIC_",
                                                                                input_number()),
                                                               label = NULL,
                                                               choices = c("e. g. 8 mg/L" = "",
                                                                           paste0(c(0.002,
                                                                                    0.004,
                                                                                    0.008,
                                                                                    0.016,
                                                                                    0.032,
                                                                                    0.064,
                                                                                    0.125,
                                                                                    0.25,
                                                                                    0.50,
                                                                                    1,
                                                                                    2,
                                                                                    4,
                                                                                    8,
                                                                                    16,
                                                                                    32,
                                                                                    64,
                                                                                    128,
                                                                                    256,
                                                                                    512),
                                                                                  " mg/L")),
                                                               selected = "",
                                                               width = "100%")
                               )
                           )
                    )
                  )
    )
  )

  #####===== Update amr_test_values =====#####
  amr_test_values$val <- c(amr_test_values$val,
                           amr_test_divId)

}, ignoreNULL = FALSE)



#####===== Remove dynamic UI by rows =====#####
observeEvent(lapply(paste0("delete_row_", amr_test_values$val), function(x) input[[x]]), {
  value <- grep(1, lapply(paste0("delete_row_", amr_test_values$val), function(x) input[[x]]))
  value <- value[!value%in%amr_test_values$ignore]

  if (length(value) != 0) {
    removeUI(

      selector = paste0('#', value - 1)

    )

    amr_test_values$ignore <- c(amr_test_values$ignore,
                                value)
  }

})



#####===== Keep track of deleted rows =====#####
deleted_rows <- reactive({

  deleted_rows <- grep(1, lapply(paste0("delete_row_", amr_test_values$val), function(x) input[[x]])) - 1

  return(deleted_rows)

})



subset_input_number <- reactive({

  if (length(deleted_rows()) > 0) {

    subset_input_number <- setdiff(x = seq(from = 1,
                                           to = input_number(),
                                           by = 1),
                                   y = deleted_rows())

  }

  return(subset_input_number)

})



#####===== Create datasets for the amr responses =====#####
response_amr <- reactive({

  if (input_number() != 0) {

    response_amr <- data.frame("Species" = sapply(X = 1:input_number(),
                                                  FUN = function(i) {

                                                    input[[paste0("species_", i)]]

                                                    }),
                               "SampleType" = sapply(X = 1:input_number(),
                                                      FUN = function(i) {

                                                        input[[paste0("sample_type_", i)]]

                                                      }),
                               "Pathogens" = sapply(X = 1:input_number(),
                                                   FUN = function(i) {

                                                     input[[paste0("pathogen_", i)]]

                                                   }),
                               "Method" = sapply(X = 1:input_number(),
                                                 FUN = function(i) {

                                                   input[[paste0("method_", i)]]

                                                 }),
                               "Compound" = sapply(X = 1:input_number(),
                                                   FUN = function(i) {
                                                     input[[paste0("compound_", i)]]
                                                   }),
                               "Rescom" = sapply(X = 1:input_number(),
                                                 FUN = function(i) {

                                                   input[[paste0("rescom_", i)]]

                                                 }),
                               "Nsamples" = sapply(X = 1:input_number(),
                                                   FUN = function(i) {

                                                     if (input[[paste0("nsamples_", i)]] == "") {

                                                       return(NA)

                                                     } else {

                                                       input[[paste0("nsamples_", i)]]

                                                     }

                                                   }),
                               "NIsolates" = sapply(X = 1:input_number(),
                                                   FUN = function(i) {

                                                     if (input[[paste0("isolates_", i)]] == "") {

                                                       return(NA)

                                                     } else {

                                                       input[[paste0("isolates_", i)]]

                                                     }

                                                   }),
                               "Prev" = sapply(X = 1:input_number(),
                                                     FUN = function(i) {

                                                       if (input[[paste0("prevalence_", i)]] == "") {

                                                         return(NA)

                                                       } else {

                                                         input[[paste0("prevalence_", i)]]

                                                       }

                                                     }),
                               "Guidelines" = sapply(X = 1:input_number(),
                                                     FUN = function(i) {

                                                       if (input[[paste0("guidelines_", i)]] == "") {

                                                         return(NA)

                                                       } else {

                                                         input[[paste0("guidelines_", i)]]

                                                       }

                                                     }),
                               "document" = sapply(X = 1:input_number(),
                                                   FUN = function(i) {

                                                     if (input[[paste0("guidelines_", i)]] == "") {

                                                       return(NA)

                                                     } else if (input[[paste0("guidelines_", i)]] == "EUCAST") {

                                                       if (input[[paste0("document_EUCAST_", i)]] == "") {

                                                         return(NA)

                                                       } else {

                                                         input[[paste0("document_EUCAST_", i)]]

                                                       }

                                                     } else if (input[[paste0("guidelines_", i)]] == "CLSI") {

                                                       if (input[[paste0("document_CLSI_", i)]] == "") {

                                                         return(NA)

                                                       } else {

                                                         input[[paste0("document_CLSI_", i)]]

                                                       }

                                                     } else if (input[[paste0("guidelines_", i)]] == "SFM") {

                                                       if (input[[paste0("document_SFM_", i)]] == "") {

                                                         return(NA)

                                                       } else {

                                                         input[[paste0("document_SFM_", i)]]

                                                       }

                                                     }

                                                   }),
                               "Breakpoint" = sapply(X = 1:input_number(),
                                                     FUN = function(i) {

                                                       if (input[[paste0("method_", i)]] == "") {

                                                         if (input[[paste0("breakpoint_NULL_", i)]] == "") {

                                                           return(NA)

                                                         } else {

                                                           input[[paste0("breakpoint_NULL_", i)]]

                                                         }

                                                       } else if (input[[paste0("method_", i)]] == "DD" ||
                                                                  input[[paste0("method_", i)]] == "SIRscan") {

                                                         if (input[[paste0("breakpoint_Disk_", i)]] == "") {

                                                           return(NA)

                                                         } else {

                                                           gsub(pattern = " mm",
                                                                replacement = "",
                                                                x = input[[paste0("breakpoint_Disk_", i)]])

                                                         }

                                                       } else if (input[[paste0("method_", i)]] == "AD" ||
                                                                  input[[paste0("method_", i)]] == "BD" ||
                                                                  input[[paste0("method_", i)]] == "Etest" ||
                                                                  input[[paste0("method_", i)]] == "VITEK") {

                                                         if (input[[paste0("breakpoint_MIC_", i)]] == "") {

                                                           return(NA)

                                                         } else {

                                                           gsub(pattern = " mg/L",
                                                                replacement = "",
                                                                x = input[[paste0("breakpoint_MIC_", i)]])

                                                         }

                                                       }

                                                     })
    )

    if (length(deleted_rows()) > 0) {

      response_amr <- response_amr[-c(deleted_rows()), ]

    }

    return(response_amr)

  } else {

    response_amr <- NULL

  }

})



output$print_input_number <- renderText({

  print(paste0("Input number: ", input_number()))

})



output$print_delete <- renderText({

  print(paste0("Rows: ", nrow(response_amr())))

})



output$print_deleted_rows <- renderText({

  if (length(deleted_rows()) > 0) {

    print(paste0("Deleted_rows: ", deleted_rows()))

  }

})



output$print_subset_input_number <- renderText({

  if (length(deleted_rows()) > 0) {

    print(paste0("Subset_input_numbers: ", subset_input_number()))

  }

})



output$show_data <- renderTable({

  response_amr()

})
