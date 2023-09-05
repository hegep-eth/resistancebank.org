
##################################################################################################################################
############################################# TO DO BEFORE EVERY DEPLOYEMENT #####################################################
##################################################################################################################################

# - Change the variable "testing_mode" to FALSE

# - Check the correct "version.rda" file to choose the version from

# - Filtering functions based on new variables should be based on dataframes instead of tibbles, otherwise filtering becomes slow

# - If you see the error "function doColorRamp not found" again, load the colour palettes directly inside the leaflet object in
#   the server

# - When tiles are produced in R through the "tiler" package, set up the coordinates of the tiles URL as /{z}/{x}/{-y}, otherwise,
#   if the tiles are produced through QGIS, leave /{z}/{x}/{y}.





# LIBRARIES =====#####
library(aws.s3)
library(bibtex)
suppressMessages(library(ggmap,
                         warn.conflicts = FALSE))
suppressMessages(library(gsubfn))
suppressMessages(library(leaflet))
suppressMessages(library(raster)) # select masked from dplyr
library(rcrossref)
library(rdrop2)
suppressMessages(library(rgdal))
library(shiny)
library(shinyBS)
library(shinyFeedback)
suppressMessages(library(shinyjs,
                         warn.conflicts = FALSE))
library(svglite)
suppressMessages(library(sp))
suppressMessages(library(tidyverse,
                         warn.conflicts = FALSE))

# SUPPORT OBJECTS =====#####

# CREDENTIALS
load("data/credentials/AWSS3_keys.rda")
Sys.setenv("AWS_ACCESS_KEY_ID" = AWSS3_keys$Access,
           "AWS_SECRET_ACCESS_KEY" = AWSS3_keys$Secret,
           "AWS_DEFAULT_REGION" = AWSS3_keys$Timezone)

load("data/credentials/API_geocoding.rda")
register_google(key = API_geocoding,
                client = "standard")

gmailr::gm_auth_configure(path = "data/credentials/JSON_gmail_HEGEP.json")
gmailr::gm_auth(email = "healthgeographyandpolicy@gmail.com",
                cache = ".secrets")

# VECTORS
testing_mode <- FALSE
load("data/vectors/version.rda")

# LISTS
load("data/lists/correct_names.rda")
load("data/lists/Antibiotics_list.rda")
load("data/lists/PPS_markers.rda")
load("data/lists/Guidelines_list.rda")

# DATASETS
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
                                  "Important")); Antibiotics_df$name_code <- paste0(Antibiotics_df$Name,
                                                                                    " (",
                                                                                    Antibiotics_df$Code,
                                                                                    ")")

s3load(object = "resistancebank.rda",
       bucket = paste0("amr-hegep-bucket/datasets/",
                       version))

antibiotics_available <- Antibiotics_df %>%
        filter(Antibiotics_df$Code %in% unique(resistancebank$Compound)) %>%
        select(c("Code",
                 "name_code"))

antibiotics_available_list <- as.list(antibiotics_available$Code)
names(antibiotics_available_list) <- antibiotics_available$name_code

s3load(object = "resistancebank_surveys.rda",
       bucket = paste0("amr-hegep-bucket/datasets/",
                       version))
s3load(object = "resistancebank_freshwater_surveys.rda",
       bucket = paste0("amr-hegep-bucket/datasets/",
                       version))
s3load(object = "resistancebank_marine_surveys.rda",
       bucket = paste0("amr-hegep-bucket/datasets/",
                       version))
s3load(object = "resistancebank_amr.rda",
       bucket = paste0("amr-hegep-bucket/datasets/",
                       version))
s3load(object = paste0("datasets/",
                       version,
                       "/Countries_PPS.rda"),
       bucket = "amr-hegep-bucket")
load("data/datasets/Countries.rda")

countries_list <- as.list(Countries$ISO3); names(countries_list) <- Countries$Country

load("data/datasets/AMR_Exposure.rda")
load("data/datasets/country_exposure_images.rda")

# FUNCTIONS
# load("data/functions/full_screen_function.rda")
load("data/functions/email_validation_function.rda")
load("data/functions/NEWfileInput.rda")
load("data/functions/P50_red_livestock_palette.rda")
load("data/functions/P50_bw_livestock_palette.rda")
load("data/functions/P50_coloured_freshwater_palette.rda")
load("data/functions/P50_coloured_marine_palette.rda")
load("data/functions/class_palette_GnYlRd.rda")
load("data/functions/marker_cluster.rda")
load("data/functions/reorder_whitin.rda")
load("data/functions/scale_x_reordered.rda")
load("data/functions/mandatory_asterisk.rda")
load("data/functions/label_fill.rda")

# HELPERS
source("helpers.R")
