
# Color matching with scales > 1.1.1
# Country report with rmarkdown 1.15
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

# Credentials
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

# Lists
load("data/lists/correct_names.rda")
load("data/lists/Antibiotics_list.rda")
load("data/lists/marker_PPS.rda")
load("data/lists/marker_submission.rda")
load("data/lists/marker_temporary.rda")
load("data/lists/Guidelines_list.rda")

# Datasets
s3load(object = "resistancebank_surveys.rda",
       bucket = "amr-hegep-bucket")
# s3load(object = "resistancebank_surveys_subset.rda",
#        bucket = "amr-hegep-bucket")
s3load(object = "resistancebank_amr.rda",
       bucket = "amr-hegep-bucket")
s3load(object = "Countries_information/Countries_PPS.rda",
       bucket = "amr-hegep-bucket")
load("data/datasets/Countries.rda")
load("data/datasets/AMR_Exposure.rda")
load("data/datasets/country_exposure_images.rda")
load("data/datasets/Antibiotics_df_common_names.rda")

Antibiotics_df <- setNames(object = data.frame(matrix(unlist(Antibiotics_list),
                                                      nrow = length(Antibiotics_list),
                                                      byrow = TRUE),
                                               stringsAsFactors = FALSE),
                           nm = c("Name",
                                  "Code",
                                  "Combination",
                                  "ATC.Code",
                                  "Drug"))

# Functions
load("data/functions/clearableTextInput.rda")
load("data/functions/NEWdownloadButton.rda")
load("data/functions/NEWfileInput.rda")
# load("data/functions/P50_palette.rda") # With the scales packages update (version 1.1.1) define it inside the reactive Leaflet object
load("data/functions/marker_cluster.rda")
load("data/functions/reorder_whitin.rda")
load("data/functions/scale_x_reordered.rda")
load("data/functions/mandatory_asterisk.rda")
load("data/functions/label_fill.rda")

# Helpers
source("helpers.R")
