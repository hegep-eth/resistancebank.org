
  all_mandatory_fields <- reactive({

    if (input_number() != 0) {

      all_mandatory_fields <- c("author_name",
                                "Author",
                                "author_email",
                                "country_study",
                                "coordinates",
                                c(paste0(rep(x = "species_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1))),
                                c(paste0(rep(x = "sample_type_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1))),
                                c(paste0(rep(x = "pathogen_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1))),
                                c(paste0(rep(x = "method_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1))),
                                c(paste0(rep(x = "compound_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1))),
                                # c(sapply(X = 1:length()))
                                c(paste0(rep(x = "rescom_",
                                             times = input_number()),
                                         seq(from = 1,
                                             to = input_number(),
                                             by = 1)))
                                )

    }  else {

      all_mandatory_fields <- NULL

    }

    return(all_mandatory_fields)

  })

  mandatory_fields <- reactive({

    if (length(deleted_rows()) > 0) {

      mandatory_fields <- setdiff(x = all_mandatory_fields(),
                                  y = c(sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("species_", deleted_rows()[i])

                                               }),
                                        sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("sample_type_", deleted_rows()[i])

                                               }),
                                        sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("pathogen_", deleted_rows()[i])

                                               }),
                                        sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("method_", deleted_rows()[i])

                                               }),
                                        sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("compound_", deleted_rows()[i])

                                               }),
                                        sapply(X = 1:length(deleted_rows()),
                                               FUN = function(i) {

                                                 paste0("rescom_", deleted_rows()[i])

                                               })
                                  )
      )

    } else {

      mandatory_fields <- all_mandatory_fields()

    }

    return(mandatory_fields)

  })

  ###= Check that all the mandatory fields are filled
  observe({

    mandatory_filled <- vapply(X = mandatory_fields(),
                               FUN = function(x) {

                                 !is.null(input[[x]]) && input[[x]] != "" },

                               logical(1))

    mandatory_filled <- all(mandatory_filled)

    toggleState(id = "submit",
                condition = mandatory_filled &
                            input_number() != 0 &
                            input_number() != length(deleted_rows()))

  })
