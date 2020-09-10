
div(id = "amr_panel",

    br(),
    br(),

    fluidRow(
      column(width = 1,
             align = "center",
             style = "padding: 0px 0px 0px 29px;",
             img(src = "form-figures/species.png",
                 height = "84.2%",
                 width = "84.2%",
                 align = "center"),
             h6(mandatory_asterisk("Species"))
      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/sample_type.png",
                 height = "82%",
                 width = "82%",
                 align = "center"),
             div(style = "display: inline-block;", h6("Sample", tags$br(), mandatory_asterisk("type"))),
             actionButton(inputId = "sample_type_examples",
                          label = div(icon(name = "question-circle",
                                           lib = "font-awesome")),
                          style = "border-width: 0px; padding: 1px 1px 1px 1px; font-size:65%;"),
             bsPopover(id = "sample_type_examples",
                       title = "Examples of animal sample types",
                       content = paste("Samples taken from",
                                       "<B>living animals</B>",
                                       "may include skin, nose, cloaca and rectum, while in the",
                                       "<B>products</B>",
                                       "category there would be dairy products, egg or meat. The",
                                       "<B>fecal samples</B>",
                                       "are in general stools, droppings, egg shelves and litter, and finally samples retrieved from",
                                       "<B>slaughtered animals</B>",
                                       "may include ceacal, gut or lymph nodes.",
                                       sep = " "),
                       placement = "right",
                       trigger = "hover",
                       options = list(container = "body"))
      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/pathogen.png",
                 height = "84.2%",
                 width = "84.2%",
                 align = "center"),
             h6(mandatory_asterisk("Pathogen"))

      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/method.png",
                 height = "84.2%",
                 width = "84.2%",
                 align = "center"),
             h6(mandatory_asterisk("Method"))
      ),
      column(width = 2,
             align = "center",
             img(src = "form-figures/compound.png",
                 height = "73.5%",
                 width = "73.5%"),
             h6(mandatory_asterisk("Antibiotic"))
      ),
      column(width = 3,
             align = "center",
             splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                         div(
                           img(src = "form-figures/rescom.png",
                               height = "87.5%",
                               width = "87.5%",
                               align = "center"),
                           h6("Resistance", tags$br(), mandatory_asterisk("prevalence"))
                         ),
                         div(
                           img(src = "form-figures/nsamples.png",
                               height = "87.5%",
                               width = "87.5%",
                               align = "center"),
                           h6("N°", tags$br(), "Samples")
                         ),
                         div(
                           img(src = "form-figures/isolates.png",
                               height = "87.7%",
                               width = "87.7%",
                               align = "center"),
                           h6("N°", tags$br(), "Isolates")
                         ),
                         div(
                           img(src = "form-figures/prevalence.png",
                               height = "87.5%",
                               width = "87.5%",
                               align = "center"),
                           h6("Pathogen", tags$br(), "prevalence")
                         )
             )
      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/guidelines.png",
                 height = "84.5%",
                 width = "84.5%",
                 align = "center"),
             h6("Guidelines")
      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/document.png",
                 height = "84.5%",
                 width = "84.5%",
                 align = "center"),
             h6("Document")
      ),
      column(width = 1,
             align = "center",
             img(src = "form-figures/breakpoint.png",
                 height = "84.5%",
                 width = "84.5%",
                 align = "center"),
             h6("Breakpoint")
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
