
# Country report with the package rmarkdown 1.15

#####===== Libraries =====#####
library(aws.s3)
suppressMessages(library(dplyr,
                         warn.conflicts = FALSE))
suppressMessages(library(ggmap,
                         warn.conflicts = FALSE))
suppressMessages(library(ggplot2))
library(gsubfn)
library(leaflet)
library(raster)
library(rcrossref)
library(rdrop2)
suppressMessages(library(rgdal))
library(shiny)
library(shinyBS)
suppressMessages(library(shinyjs,
                         warn.conflicts = FALSE))
library(stringr)
library(svglite)
library(sp)

#####===== Credentials =====######
load("data/credentials/AWSS3_keys.rda")
Sys.setenv("AWS_ACCESS_KEY_ID" = AWSS3_keys$Access,
           "AWS_SECRET_ACCESS_KEY" = AWSS3_keys$Secret,
           "AWS_DEFAULT_REGION" = AWSS3_keys$Timezone)

load("data/credentials/API_geocoding.rda")
register_google(key = API_geocoding,
                client = "standard")

gmailr::gm_auth_configure(path = "data/credentials/API_gmail.json")
gmailr::gm_auth(email = "nico.criscuolo1618@gmail.com",
                cache = ".secrets")

#####==== Lists ====#####
load("data/lists/correct_names.rda")
load("data/lists/Antibiotics_list.rda")
load("data/lists/PPS_markers.rda")
load("data/lists/Guidelines_list.rda")

#####===== Datasets =====#####
numbers <- data.table::as.data.table(seq(from = 1,
                                         to = 5000,
                                         by = 1))

colnames(numbers)[1] <- "0"

load("data/datasets/map_initialview_data.rda")
load("data/datasets/Antibiotics_df_common_names.rda")
load("data/datasets/quality_controls.rda")

Antibiotics_df <- setNames(object = data.frame(matrix(unlist(Antibiotics_list),
                                                      nrow = length(Antibiotics_list),
                                                      byrow = TRUE),
                                               stringsAsFactors = FALSE),
                           nm = c("Name",
                                  "Code",
                                  "Combination",
                                  "ATC.Code",
                                  "Drug",
                                  "Class",
                                  "WHO_MedImp",
                                  "Important"))

s3load(object = "resistancebank.rda",
       bucket = "amr-hegep-bucket")

resistancebank$Class_WHO <- sapply(X = 1:nrow(resistancebank),
                                   FUN = function(i) {

                                     paste0(resistancebank$Class[i],
                                            " (",
                                            resistancebank$WHO_MedImp[i],
                                            ")")

                                   })

resistancebank$pathogen_compound <- sapply(X = 1:nrow(resistancebank),
                                           FUN = function(i) {

                                             paste0(resistancebank$Pathogens[i],
                                                    " - ",
                                                    Antibiotics_df$Name[Antibiotics_df$Code == resistancebank$Compound[i]])

                                           })

s3load(object = "resistancebank_surveys.rda",
       bucket = "amr-hegep-bucket")
# s3load(object = "resistancebank_surveys_subset.rda",
#        bucket = "amr-hegep-bucket")
s3load(object = "resistancebank_amr.rda",
       bucket = "amr-hegep-bucket")

resistancebank_amr$pathogen_compound <- sapply(X = 1:nrow(resistancebank_amr),
                                               FUN = function(i) {

                                                 paste0(resistancebank_amr$Pathogens[i],
                                                        " - ",
                                                        Antibiotics_df$Name[Antibiotics_df$Code == resistancebank_amr$Compound[i]])

                                               })

s3load(object = "Countries_information/Countries_PPS.rda",
       bucket = "amr-hegep-bucket")
load("data/datasets/Countries.rda")

countries_list <- as.list(Countries$Country_ISO3)
names(countries_list) <- Countries$Country

load("data/datasets/AMR_Exposure.rda")
load("data/datasets/country_exposure_images.rda")

#####===== Functions =====#####
load("data/functions/clearableTextInput.rda")
load("data/functions/NEWdownloadButton.rda")
load("data/functions/NEWfileInput.rda")
load("data/functions/P50_palette.rda") # If you see the error "function doColorRamp not found" again, load the palettes inside the leaflet object in the server.
load("data/functions/P50_bw_palette.rda")
load("data/functions/class_palette_GnYlRd.rda")
load("data/functions/marker_cluster.rda")
load("data/functions/reorder_whitin.rda")
load("data/functions/scale_x_reordered.rda")
load("data/functions/mandatory_asterisk.rda")
load("data/functions/label_fill.rda")

#####===== Helpers =====#####
source("helpers.R")
