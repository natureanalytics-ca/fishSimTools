

library(fishSimGTG)
#library(fishSimTools)
devtools::load_all()
library(here)
#--------------
#Kala
#--------------

#No uncertainty (except initial depletion)

runProjectionExcel(

  inputDir = here("data-raw"),
  inputFile = "DAR_example_noUncertainty.xlsx",
  outputDir = here("data-raw", "Kala_noUncertainty")

)

relSSBscatter(wd =  here("data-raw", "Kala_noUncertainty"),
              fileName = list(
                "Higher_option1",
                "Higher_option2",
                "Higher_option3",
                "Lower_option1",
                "Lower_option2",
                "Lower_option3"
              ),
              #facetName = c(as.list(rep("Higher biomass scenario", 3)), as.list(rep("Lower biomass scenario", 3))),
              facetName = c(as.list(rep("Higher biomass scenario", 6))),
              newLabel = list(
                "A",
                "B",
                "C",
                "D",
                "E",
                "F"
              ),
              chooseArea = 0,
              proYear = 50)


relSSBseries(wd =  here("data-raw", "Kala_noUncertainty"),
             fileName = list(
               "Higher_option1",
               "Higher_option2",
               "Higher_option3",
               "Lower_option1",
               "Lower_option2",
               "Lower_option3"
             ),
             #facetName = c(as.list(rep("Higher biomass scenario", 3)), as.list(rep("Lower biomass scenario", 3))),
             facetName = c(as.list(rep("Higher biomass scenario", 6))),
             # newLabel = list(
             #   "A",
             #   "B",
             #   "C",
             #   "D",
             #   "E",
             #   "F"
             # ),
             chooseArea = 0,
             percentileOuter = c(0.025, 0.975),
             percentileInner = c(0.25, 0.75),
             doHist = FALSE,
             dpi = 300)


#With uncertainty

runProjectionExcel(

  inputDir = here("data-raw"),
  inputFile = "DAR_example_withUncertainty.xlsx",
  outputDir = here("data-raw", "Kala_withUncertainty")

)

relSSBscatter(wd =  here("data-raw", "Kala_withUncertainty"),
              fileName = list(
                "Higher_option1",
                "Higher_option2",
                "Higher_option3",
                "Lower_option1",
                "Lower_option2",
                "Lower_option3"
              ),
              facetName = c(as.list(rep("Higher biomass scenario", 3)), as.list(rep("Lower biomass scenario", 3))),
              chooseArea = 0,
              proYear = 50)


relSSBseries(wd =  here("data-raw", "Kala_withUncertainty"),
             fileName = list(
               "Higher_option1",
               "Higher_option2",
               "Higher_option3",
               "Lower_option1",
               "Lower_option2",
               "Lower_option3"
             ),
             facetName = c(as.list(rep("Higher biomass scenario", 3)), as.list(rep("Lower biomass scenario", 3))),
             chooseArea = 0,
             percentile = c(0.025, 0.975),
             doHist = TRUE,
             dpi = 300)
