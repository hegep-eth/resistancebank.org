
div(id = "amr_panel",

br(),

    tags$table(style = "width: 100%;
                        table-layout: fixed;",
               tags$tr(tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/species.png",
                                   width = "70%",
                                   align = "center"),
                               h6(mandatory_asterisk("Species"))
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/sample_origin.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Sample",
                                  tags$br(),
                                  mandatory_asterisk("origin"))
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/pathogen.png",
                                   width = "70%",
                                   align = "center"),
                               h6(mandatory_asterisk("Pathogen"))
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/method.png",
                                   width = "70%",
                                   align = "center"),
                               h6(mandatory_asterisk("Method"))
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/compound.png",
                                   width = "70%",
                                   align = "center"),
                               h6(mandatory_asterisk("Compound"))
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/rescom.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Resistance",
                                  tags$br(),
                                  mandatory_asterisk("prevalence"))

                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/nsamples.png",
                                   width = "70%",
                                   align = "center"),
                               h6("N°",
                                  tags$br(),
                                  "Samples")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/isolates.png",
                                   width = "70%",
                                   align = "center"),
                               h6("N°",
                                  tags$br(),
                                  "Isolates")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/prevalence.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Prevalence")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/quality.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Quality",
                                  tags$br(),
                                  "control")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/compound_use.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Antibiotic",
                                  tags$br(),
                                  "use reported")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/guidelines.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Guidelines")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/document.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Document")
                       ),
                       tags$td(style = "width: 7.14%;",
                               align = "center",
                               img(src = "form-figures/breakpoint.png",
                                   width = "70%",
                                   align = "center"),
                               h6("Breakpoint")
                       )
               )
    ),

    fluidRow(
      column(width = 12,
             tags$div(id = "amr_test_placeholder")
      )
    ),

    fluidRow(

      br(),

      column(width = 12,
             actionButton(inputId = "add_amr_test",
                          label = icon(name = "plus",
                                       lib = "font-awesome")),
             bsTooltip(id = "add_amr_test",
                       title = "Add new compound test",
                       placement = "right",
                       trigger = "hover")
      )
    ) # ,

    # br(),
    # br(),
    #
    # fluidRow(
    #   column(width = 6,
    #          tableOutput(outputId = "show_data")
    #   )
    # ),
    #
    # fluidRow(
    #   column(width = 6,
    #          textOutput(outputId = "print_input_number"))
    # ),
    #
    # fluidRow(
    #   column(width = 6,
    #          textOutput(outputId = "print_delete")
    #   )
    # ),
    #
    # fluidRow(
    #   column(width = 6,
    #          textOutput(outputId = "print_deleted_rows")
    #   )
    # ),
    #
    # fluidRow(
    #   column(width = 6,
    #          textOutput(outputId = "print_subset_input_number")
    #   )
    # ),
    #
    # br()
)
