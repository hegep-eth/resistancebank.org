
p(tags$div(style = "text-align: justify;
                    padding-left: 5px;
                    padding-right: 5px;
                    font-size: 1.190vmin;",

                    "The data presented in resistancebank.org come with limitations. The first set of limitations concerns the quality of the event-based surveillance data and comparability across surveys.
                     In the human population, the majority of studies focus on diagnostic samples taken mainly from sick patients. In contrast, in animals, surveillance relies on different data collection
                     contexts: sampling of living versus dead animals, sampling of animal food products, outbreak investigation, sample collection required by food regulatory authorities, etc.", tags$br(),
                    "These different sampling contexts, which are inherent to event-based surveillance, represent a challenge to the harmonization and the interpretation of resistance rates reported on this
                     platform. In particular, the surveys listed on resistancebank.org may differ in terms of i) sampling strategy (random or convenient), ii) animal breeds and farming systems,
                     iii) the number of isolates tested per survey, testing, and iv) the degree of aggregation used for reporting antimicrobial rates in each survey (population versus isolate-level information).
                     For these reasons, in resistancebank.org, we allow users to specify additional surveys information such as the sampling scheme, the guidelines, and breakpoints used for AST,
                     the quality control strains used, etc., to include as much information about these factors that may affect the interpretation of the resistance rates reported.", tags$br(),
                     tags$br(),
                    "The second set of limitations concerns the attempt to summarize trends in resistance across drug-pathogen combinations using P50: the proportion of drugs tested in a survey with resistance rates
                     higher than 50%.From a practical perspective, P50 expresses the probability of providing treatments that work out of a portfolio of treatment options, when antimicrobial therapy is indicated for
                     a medical condition. Multiple summary metrics have been proposed and debated to aggregate resistance rates to multiple drug-pathogen combinations. As with every attempt to reduce this
                     complexity, P50 comes with sources of uncertainty.", tags$br(),
                    "First, the number of drugs tested in each survey can differ, and this can typically be influenced by the methods used for antimicrobial susceptibility testing in different laboratories
                     (diffusion vs dilution methods), although a good agreement has been shown between the methods.", tags$br(),
                    "Second, In some surveys, screening for resistance of second-line drugs such as imipenem may be conducted on a subset of the isolates and introduce bias in P50 estimates. In this study,
                     sub-sampling for second-line antimicrobials was limited to 34 out of 1,940 estimates of P50.", tags$br(),
                    "Third, P50 reflects the number of compounds with resistance higher than 50% rather than the number of classes of compounds with resistance higher than 50%. Therefore, resistancebank.org also
                     provides resistance rates for classes of compounds considered medically important by the WHO. The P50 is a summary metric intended to help resource allocation
                     against AMR in countries where systematic surveillance is limited. However, because of the non-systematic nature of the data P50 summarizes, comparisons of resistance rates for individual drug
                     classes should be preferred for informing public health strategies.", tags$br(),
                     tags$br(),
                    "The third set of limitations concerns the intensity of our data collection efforts between countries. Our online literature search was supplemented by field officers who collected PPS on
                     paper during visits to veterinary schools. However, these field visits could only be conducted in India, where our collaboration network is extensive. Collaboration with international
                     organizations could help leverage a larger network of field officers to supplement the information currently in resistanbank.org. We conducted the literature searches in six languages
                     (English, Mandarin Chinese, Spanish, French, Portuguese, and German).",  tags$br(),
                    "Although these languages are spoken by 46.6% of the world population, further inquiries in other languages could help supplement our database. Finally, the computational cost of re-running
                     the geospatial model is currently preventing instantaneous updates of the AMR map on a global level and should be the focus of future research efforts to move from yearly updates of our
                     maps to daily updates. For the reasons listed above, resistancebank.org is an imperfect surrogate to systematic surveillance systems. It is a platform reporting large-scale trends in AMR
                     meant to help international funders to target their efforts in the short term and facilitate the development of a global systematic surveillance system in the long term.", tags$br()

  )
  )
