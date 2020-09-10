
unique_templatename <- reactive({

  if (nrow(templateData()) >= 1) {

    unique_templatename <- paste0(templateData()$Author[1],
                                  "_",
                                  templateData()$ISO3[1],
                                  "_TEMPLATE_",
                                  substr(digest::digest(templateData()$Author[1]),
                                         start = 1,
                                         stop = 12),
                                  sprintf(fmt = "%s%s%s.csv",
                                          as.integer(Sys.time()),
                                          substr(digest::digest(Sys.time()),
                                                 start = 1,
                                                 stop = 12),
                                          substr(digest::digest(digest::digest(Sys.Date())),
                                                 start = 1,
                                                 stop = 12)))

  } else {

    unique_templatename <- "EMPTY_RESPONSE.csv"

  }

  return(unique_templatename)

})

