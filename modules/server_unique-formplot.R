
  ###= Max number of compounds per species
  species_compounds <- plyr::ddply(.data = study,
                                   .variables = ~Species,
                                   .fun = summarise,
                                   "UC" = length(unique(Compound)))

  ###= Animal images dataframe
  animal_images <- data.frame("image" = sapply(X = 1:length(unique(study$Species)),
                                               function (i) {

                                                 UNIQUE_SPECIES <- as.character(unique(study$Species))[i]

                                                 paste0("https://nicocriscuolo.github.io/resistancebank_plots/animal_images/",
                                                        UNIQUE_SPECIES,
                                                        ".png")

                                               }),
                              "Species" = as.factor(unique(study$Species)),
                              "X" = ifelse(test = max(species_compounds$UC) <= 3,
                                           yes = max(species_compounds$UC) + 0.5,
                                           no = ifelse(test = max(species_compounds$UC) > 3 & max(species_compounds$UC) <= 11,
                                                       yes = max(species_compounds$UC) + 0.4,
                                                       no = ifelse(test = max(species_compounds$UC) > 11,
                                                                   yes = max(species_compounds$UC) - 0.2,
                                                                   no = NA))))

  #####===== Basic ggplot =====#####
  basic_plot <- ggplot(data = study,
                       aes(x = reorder(x = Compound,
                                       X = desc(Rescom)),
                           y = Rescom)) +
    geom_errorbar(aes(ymin = 0,
                      ymax = 0,
                      width = {if (length(unique(study$Compound)) == 1 &
                                   length(unique(study$Species)) == 1) {0.2}}),
                  colour = "black") +
    geom_col(aes(fill = Species),
             width = {if (length(unique(study$Compound)) == 1 &
                          length(unique(study$Species)) == 1) {0.2}}) +
    scale_fill_manual(values = c("Cattle" = "#83C77A",
                                 "Chicken" = "#F9A600",
                                 "Pig" = "#F5C0CD",
                                 "Sheep" = "#BFBFBF",
                                 "Duck" = "#00BFC4",
                                 "Buffalo" = "#F9766D",
                                 "Horse" = "#C77CFF")[c(as.character(unique(study$Species)))]) +
    labs(x = NULL,
         y = "Resistance [%]") +
    coord_cartesian(ylim = c(0, 100),
                    clip = "off") +
    theme(
      text = element_text(family = "Arial"),
      axis.title = element_text(size = 30),
      axis.text.x = element_text(hjust = 1,
                                 angle = 90),
      axis.text.y = element_text(size = 25),
      axis.ticks.x = element_blank(),
      strip.background = element_blank(),
      panel.background = element_blank(),
      legend.position = "none")

  ###= 1 Species and 1 Pathogen #####
  if (length(unique(study$Species)) == 1 &
      length(unique(study$Pathogens)) == 1) {

    animal_images$Y <- 92
    animal_size <- c("Cattle" = 0.20,
                     "Chicken" = 0.15,
                     "Pig" = 0.20,
                     "Sheep" = 0.20,
                     "Horse" = 0.18,
                     "Buffalo" = 0.18,
                     "Duck" = 0.17)
    animal_images$Size <- animal_size[as.character(animal_images$Species)]
    animal_images$Asp_ratio <- 0.8

    study_plot <- basic_plot +
      lemon::facet_rep_grid(Species ~ Pathogens,
                            scales = "free",
                            repeat.tick.labels = "all") +
      ggimage::geom_image(data = animal_images,
                          aes(x = X,
                              y = Y),
                          image = animal_images$image,
                          size = animal_images$Size,
                          asp = animal_images$Asp_ratio) +
      theme(
        axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 23,
                                                 yes = 22,
                                                 no = 17)),
        plot.margin = unit(c(0, 2.7, 0, 0),
                           units = "cm"),
        strip.text.x = element_text(size = 32,
                                    face = "bold"),
        strip.text.y = element_blank())

    ###= 2 - 7 Species and 1 Pathogen =====#####
  } else if (length(unique(study$Species)) > 1 &
             length(unique(study$Species)) <= 7 &
             length(unique(study$Pathogens)) == 1) {

    animal_images$Y <- ifelse(test = length(unique(study$Species)) == 2,
                              yes = 81,
                              no = ifelse(test = length(unique(study$Species)) == 3,
                                          yes = 77,
                                          no = ifelse(test = length(unique(study$Species)) == 4,
                                                      yes = 74,
                                                      no = ifelse(test = length(unique(study$Species)) == 5,
                                                                  yes = 72,
                                                                  no = ifelse(test = length(unique(study$Species)) == 6,
                                                                              yes = 70,
                                                                              no = ifelse(test = length(unique(study$Species)) == 7,
                                                                                          yes = 65,
                                                                                          no = NA))))))

    if (length(unique(study$Species)) == 2) {

      animal_size <- c("Cattle" = 0.18,
                       "Chicken" = 0.13,
                       "Pig" = 0.18,
                       "Sheep" = 0.18,
                       "Horse" = 0.16,
                       "Buffalo" = 0.16,
                       "Duck" = 0.15)

    } else if (length(unique(study$Species)) == 3) {

      animal_size <- c("Cattle" = 0.16,
                       "Chicken" = 0.11,
                       "Pig" = 0.16,
                       "Sheep" = 0.16,
                       "Horse" = 0.14,
                       "Buffalo" = 0.14,
                       "Duck" = 0.13)

    } else if (length(unique(study$Species)) == 4) {

      animal_size <- c("Cattle" = 0.14,
                       "Chicken" = 0.09,
                       "Pig" = 0.14,
                       "Sheep" = 0.14,
                       "Horse" = 0.12,
                       "Buffalo" = 0.12,
                       "Duck" = 0.11)

    } else if (length(unique(study$Species)) == 5) {

      animal_size <- c("Cattle" = 0.13,
                       "Chicken" = 0.08,
                       "Pig" = 0.13,
                       "Sheep" = 0.13,
                       "Horse" = 0.11,
                       "Buffalo" = 0.11,
                       "Duck" = 0.10)

    } else if (length(unique(study$Species)) == 6) {

      animal_size <- c("Cattle" = 0.12,
                       "Chicken" = 0.07,
                       "Pig" = 0.12,
                       "Sheep" = 0.12,
                       "Horse" = 0.10,
                       "Buffalo" = 0.10,
                       "Duck" = 0.09)

    } else if (length(unique(study$Species)) == 7) {

      animal_size <- c("Cattle" = 0.11,
                       "Chicken" = 0.06,
                       "Pig" = 0.11,
                       "Sheep" = 0.11,
                       "Horse" = 0.10,
                       "Buffalo" = 0.10,
                       "Duck" = 0.08)

    }

    animal_images$Size <- animal_size[as.character(animal_images$Species)]
    animal_images$Asp_ratio <- ifelse(test = length(unique(study$Species)) == 2,
                                      yes = 1.8,
                                      no = ifelse(test = length(unique(study$Species)) == 3,
                                                  yes = 3,
                                                  no = ifelse(test = length(unique(study$Species)) == 4,
                                                              yes = 4.1,
                                                              no = ifelse(test = length(unique(study$Species)) == 5,
                                                                          yes = 4.7,
                                                                          no = ifelse(test = length(unique(study$Species)) == 6,
                                                                                      yes = 5.5,
                                                                                      no = ifelse(test = length(unique(study$Species)) == 7,
                                                                                                  yes = 7,
                                                                                                  no = NA))))))

    study_plot <- basic_plot +
      lemon::facet_rep_wrap(~Species,
                            scales = "free_x",
                            repeat.tick.labels = "all",
                            nrow = length(unique(study$Species)),
                            ncol = 1) +
      labs(title = unique(study$Pathogens)) +
      ggimage::geom_image(data = animal_images,
                          aes(x = X,
                              y = Y),
                          image = animal_images$image,
                          size = animal_images$Size,
                          asp = animal_images$Asp_ratio) +
      expand_limits(x = c(1,
                          ifelse(test = max(species_compounds$UC) <= 4,
                                 yes = max(species_compounds$UC) + 0.8,
                                 no = ifelse(test = max(species_compounds$UC) >= 5 & max(species_compounds$UC) <= 7,
                                             yes = max(species_compounds$UC) + 1.2,
                                             no = ifelse(test = max(species_compounds$UC) > 7 & max(species_compounds$UC) <= 16,
                                                         yes = max(species_compounds$UC) + 1.5,
                                                         no = ifelse(test = max(species_compounds$UC) > 16 & max(species_compounds$UC) < 29,
                                                                     yes = max(species_compounds$UC) + 2.3,
                                                                     no = max(species_compounds$UC) + 3)))))) +
      theme(
        axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 23,
                                                 yes = 22,
                                                 no = 17)),
        plot.margin = unit(c(0, 0, 0, 0),
                           units = "cm"),
        plot.title = element_text(size = 32,
                                  hjust = 0.45,
                                  vjust = -0.3,
                                  face = "bold"),
        strip.text.x = element_blank())

    # 1 Species and 2 Pathogens
  } else if (length(unique(study$Species)) == 1 &
             length(unique(study$Pathogens)) == 2) {

    animal_images$Y <- 92
    animal_size <- c("Cattle" = 0.27,
                     "Chicken" = 0.23,
                     "Pig" = 0.27,
                     "Sheep" = 0.27,
                     "Horse" = 0.25,
                     "Buffalo" = 0.25,
                     "Duck" = 0.24)
    animal_images$Size <- animal_size[as.character(animal_images$Species)]
    animal_images$Asp_ratio <- 0.4

    study_plot <- basic_plot +
      lemon::facet_rep_wrap(~Pathogens,
                            scales = "free_x",
                            repeat.tick.labels = "all",
                            nrow = 1,
                            ncol = length(unique(study$Pathogens))) +
      ggimage::geom_image(data = animal_images,
                          aes(x = X,
                              y = Y),
                          image = animal_images$image,
                          size = animal_images$Size,
                          asp = animal_images$Asp_ratio) +
      expand_limits(x = c(1,
                          ifelse(test = max(species_compounds$UC) <= 10,
                                 yes = max(species_compounds$UC) + 2,
                                 no = max(species_compounds$UC) + 3.5))) +
      theme(axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 15,
                                                     yes = 22,
                                                     no = 14)),
            strip.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 10,
                                                      yes = 32,
                                                      no = 22),
                                        face = "bold"))
    # 1 Species and 3 Pathogens
  } else if (length(unique(study$Species)) == 1 &
             length(unique(study$Pathogens)) == 3) {

    animal_images$Y <- 92
    animal_size <- c("Cattle" = 0.28,
                     "Chicken" = 0.24,
                     "Pig" = 0.28,
                     "Sheep" = 0.28,
                     "Horse" = 0.26,
                     "Buffalo" = 0.26,
                     "Duck" = 0.25)
    animal_images$Size <- animal_size[as.character(animal_images$Species)]
    animal_images$Asp_ratio <- 0.25

    study_plot <- basic_plot +
      lemon::facet_rep_wrap(~Pathogens,
                            scales = "free_x",
                            repeat.tick.labels = "all",
                            nrow = 1,
                            ncol = length(unique(study$Pathogens))) +
      ggimage::geom_image(data = animal_images,
                          aes(x = X,
                              y = Y),
                          image = animal_images$image,
                          size = animal_images$Size,
                          asp = animal_images$Asp_ratio) +
      expand_limits(x = c(1,
                          ifelse(test = max(species_compounds$UC) <= 10,
                                 yes = max(species_compounds$UC) + 2,
                                 no = max(species_compounds$UC) + 3))) +
      theme(axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 15,
                                                     yes = 18,
                                                     no = 13)),
            strip.text.x = element_text(size = 20,
                                        face = "bold"))

    # 1 Species and 4 Pathogens
  } else if (length(unique(study$Species)) == 1 &
             length(unique(study$Pathogens)) == 4) {

    animal_images$Y <- 92
    animal_size <- c("Cattle" = 0.24,
                     "Chicken" = 0.20,
                     "Pig" = 0.24,
                     "Sheep" = 0.24,
                     "Horse" = 0.22,
                     "Buffalo" = 0.22,
                     "Duck" = 0.21)
    animal_images$Size <- animal_size[as.character(animal_images$Species)]
    animal_images$Asp_ratio <- 0.19

    study_plot <- basic_plot +
      lemon::facet_rep_wrap(~Pathogens,
                            scales = "free_x",
                            repeat.tick.labels = "all",
                            nrow = 1,
                            ncol = length(unique(study$Pathogens))) +
      ggimage::geom_image(data = animal_images,
                          aes(x = X,
                              y = Y),
                          image = animal_images$image,
                          size = animal_images$Size,
                          asp = animal_images$Asp_ratio) +
      expand_limits(x = c(1,
                          ifelse(test = max(species_compounds$UC) <= 10,
                                 yes = max(species_compounds$UC) + 2,
                                 no = max(species_compounds$UC) + 3))) +
      theme(axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 15,
                                                     yes = 13,
                                                     no = 10)),
            strip.text.x = element_text(size = 13,
                                        face = "bold"))

    # > 1 Species and > 1 Pathogen
  } else {

    study_plot <- basic_plot +
      lemon::facet_rep_wrap(Pathogens~Species,
                            scales = "free_x",
                            repeat.tick.labels = "all",
                            nrow = length(unique(study$Species)),
                            ncol = length(unique(study$Pathogens))) +
      theme(
        axis.text.x = element_text(size = ifelse(test = max(species_compounds$UC) < 23,
                                                 yes = 22,
                                                 no = 17)),
        plot.margin = unit(c(0, 0, 0, 0),
                           units = "cm"),
        strip.text.x = element_text(size = 20,
                                    face = "bold"))

  }
