
#---------------------------------------
#Automate simulations using an excel input file
#---------------------------------------

#Roxygen header
#'Automate simulations using an excel input file
#'
#'FishSimGTG wrapper to automate multiple simulations using an excel input file
#'
#' @param inputDir  Where to look for the input file.
#' @param inputFile The excel input file
#' @param outputDir Where to put the output files
#' @import fishSimGTG stringr
#' @export

runProjectionExcel<-function(

  inputDir,
  inputFile,
  outputDir

  ){

  #-----------------------
  #Inputs setup
  #----------------------

  #Set the location for the batch input file
  input_file<-paste0(inputDir, "/", inputFile)

  #Catches errors in data inputs
  show_condition <- function(code) {
    tryCatch(code,
             error = function(c) {
               NULL
             }
    )
  }

  #Check the sheets available in the input file
  input_sheets <- show_condition(readxl::excel_sheets(input_file))
  if(is.null(input_sheets)) {
    print("Input file not found. Check function arguments.")
    stop()
  }

  #Read in each of the relevant sheets for analysis
  Batch_setup <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Batch_setup"))
  LifeHistory_base <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "LifeHistory_base"))
  TimeArea_base <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "TimeArea_base"))
  Fishery_base <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Fishery_base"))
  Stochastic_base <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Stochastic_base"))
  Strategy_base <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Strategy_base"))
  Recruitment_pars <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Recruitment_pars",col_names = FALSE))
  Movement_pars <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Movement_pars"))
  Effort_pars <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Effort_pars",col_names = FALSE))
  Bag_pars <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Bag_pars",col_names = FALSE))
  Sel_uncertainty_pars <- as.data.frame(readxl::read_xlsx(path=input_file, sheet = "Sel_uncertainty_pars",col_names = FALSE))

  #Check the length of the Batch Setup object to determine how many simulations will be run
  N_sims <- length(Batch_setup[,1])

  set_numeric <- function(proposed_val, alternate_val=NA){
    proposed_val <- as.numeric(proposed_val)
    alternate_val <- as.numeric(alternate_val)
    if(!is.na(proposed_val)){
      return(proposed_val)
    }else if(!is.na(alternate_val)){
      return(alternate_val)
    }else{
      return(NULL) #This should throw an error when input to an object
    }
  }

  set_numeric_vector <- function(proposed_val, alternate_val=NA){
    proposed_val <- as.numeric(proposed_val)
    alternate_val <- as.numeric(alternate_val)
    proposed_val <- proposed_val[!is.na(proposed_val)]
    alternate_val <- alternate_val[!is.na(alternate_val)]
    if(length(proposed_val)>0){
      return(proposed_val)
    }else if(length(alternate_val)>0){
      return(alternate_val)
    }else{
      return(NULL) #This should throw an error when input to an object
    }
  }

  #Provided at least one simulation is specified loop over all the simulations
  if(N_sims>0){
    for(i in 1:N_sims){
      #Check that all data specified for this loop is available (i.e. make sure target rows in each sheet actually exist)
      #Then parameterize the relevant Objects

      #1: Check the targeted life history parameter row exists and fill object
      LH_row <- as.numeric(Batch_setup[i,3])
      if(length(LifeHistory_base[,1])>=LH_row){
        #----------------------------
        #Create a LifeHistory object
        #----------------------------

        #---Populate LifeHistory object for the specific sim loop
        #---Contains the life history parameters
        LifeHistoryObj <- new("LifeHistory")
        LifeHistoryObj@title <- LifeHistory_base[LH_row,1]
        LifeHistoryObj@speciesName <- LifeHistory_base[LH_row,2]
        LifeHistoryObj@Linf <- set_numeric(LifeHistory_base[LH_row,3])
        LifeHistoryObj@K <- set_numeric(LifeHistory_base[LH_row,4])
        LifeHistoryObj@t0 <- set_numeric(LifeHistory_base[LH_row,5])
        LifeHistoryObj@L50 <- set_numeric(LifeHistory_base[LH_row,6])
        LifeHistoryObj@L95delta <- set_numeric(LifeHistory_base[LH_row,7])
        LifeHistoryObj@M <- set_numeric(LifeHistory_base[LH_row,8])
        LifeHistoryObj@L_type <- LifeHistory_base[LH_row,9]
        LifeHistoryObj@L_units <- LifeHistory_base[LH_row,10]
        LifeHistoryObj@LW_A <- set_numeric(LifeHistory_base[LH_row,11])
        LifeHistoryObj@LW_B <- set_numeric(LifeHistory_base[LH_row,12])
        LifeHistoryObj@Steep <- set_numeric(LifeHistory_base[LH_row,13])
        LifeHistoryObj@isHermaph <- LifeHistory_base[LH_row,14]
        if(LifeHistoryObj@isHermaph == "TRUE"){
          LifeHistoryObj@H50 <- set_numeric(LifeHistory_base[LH_row,15])
          LifeHistoryObj@H95delta <- set_numeric(LifeHistory_base[LH_row,16])
        }
        LifeHistoryObj@recSD <- set_numeric(LifeHistory_base[LH_row,17])
      }else{
        stop("The referenced row of the Life History tab does not exist")
      }

      #2: Check the targeted Time Area object parameter row exists and fill object
      TA_row <- as.numeric(Batch_setup[i,4])
      if(length(TimeArea_base[,1])>=TA_row){
        #----------------------------
        #Create a TimeArea object
        #----------------------------

        #---Populate a TimeArea object
        #---Contains basic inputs about time and space needed to establish simulation bounds
        #---The effort matrix is set as multipliers of initial equilibrium effort

        #These values are single inputs specified from the base Time Area input sheet
        TimeAreaObj <- new("TimeArea")
        TimeAreaObj@title <- TimeArea_base[TA_row,1]
        TimeAreaObj@gtg <- set_numeric(TimeArea_base[TA_row,2])
        TimeAreaObj@areas <- set_numeric(TimeArea_base[TA_row,3])
        TimeAreaObj@iterations <- set_numeric(TimeArea_base[TA_row,6])
        TimeAreaObj@historicalYears <- set_numeric(TimeArea_base[TA_row,7])
        TimeAreaObj@historicalBio <- set_numeric(TimeArea_base[TA_row,8])
        TimeAreaObj@historicalBioType <- TimeArea_base[TA_row,9]

        #These values are more complex objects specified from supplemental input sheets
        RA_row <- as.numeric(TimeArea_base[TA_row,4])
        MOVE_row <- as.numeric(TimeArea_base[TA_row,5])

        if(length(Recruitment_pars[,1])>=RA_row){
          temp_RA <- set_numeric_vector(Recruitment_pars[RA_row,])
          if(!is.null(temp_RA)){
            if(length(temp_RA)==TimeAreaObj@areas){
              TimeAreaObj@recArea <- temp_RA
            }else{
              temp_RA <- rep(temp_RA,TimeAreaObj@areas)[1:TimeAreaObj@areas]
              TimeAreaObj@recArea <- temp_RA
            }
          }
        }else{
          stop("The referenced row of the recruitment tab does not exist")
        }

        if(length(Movement_pars[,1])>=MOVE_row){
          temp_MA <- set_numeric_vector(Movement_pars[MOVE_row,])
          if(!is.null(temp_MA)){
            if(length(temp_MA)==(TimeAreaObj@areas^2)){
              TimeAreaObj@move <- matrix(temp_MA,nrow = TimeAreaObj@areas, ncol = TimeAreaObj@areas, byrow = FALSE)
            }else{
              temp_MA <- rep(temp_MA,(TimeAreaObj@areas^2))[1:(TimeAreaObj@areas^2)]
              TimeAreaObj@move <- matrix(temp_MA,nrow = TimeAreaObj@areas, ncol = TimeAreaObj@areas, byrow = FALSE)
            }
          }
        }else{
          stop("The referenced row of the movement tab does not exist")
        }

        if(TimeAreaObj@historicalYears>0){
          HE_row <- as.numeric(TimeArea_base[TA_row,10])
          if(length(Effort_pars[,1])>=HE_row){
            temp_HE <- set_numeric_vector(Effort_pars[HE_row,])
            if(!is.null(temp_HE)){
              if(length(temp_HE)==(TimeAreaObj@areas*TimeAreaObj@historicalYears)){
                TimeAreaObj@historicalEffort <- matrix(temp_HE, nrow = TimeAreaObj@historicalYears, ncol = TimeAreaObj@areas, byrow = FALSE)
              }else{
                temp_HE <- rep(temp_HE,(TimeAreaObj@areas*TimeAreaObj@historicalYears))[1:(TimeAreaObj@areas*TimeAreaObj@historicalYears)]
                TimeAreaObj@historicalEffort <- matrix(temp_HE, nrow = TimeAreaObj@historicalYears, ncol = TimeAreaObj@areas, byrow = FALSE)
              }
            }else{
              TimeAreaObj@historicalEffort <- matrix(1, nrow = TimeAreaObj@historicalYears, ncol = TimeAreaObj@areas, byrow = FALSE) #This default continues fishing at equilibrium effort level if no valid input provided
            }
          }else{
            stop("The referenced row of the effort tab does not exist")
          }
        }else{
          TimeAreaObj@historicalEffort<-matrix(1 ,nrow = 0, ncol = TimeAreaObj@areas, byrow = FALSE)
        }

      }else{
        stop("The referenced row of the Time Area tab does not exist")
      }

      #3: Check the targeted fishery parameter row exists and fill the historic fishery object
      HF_row <- as.numeric(Batch_setup[i,5])
      if(length(Fishery_base[,1])>=HF_row){
        #-----------------------------
        #Setup historic fishery characteristics
        #-----------------------------

        #---Populate a Fishery object
        #---Contains selectivity, retention and discard characteristics
        HistFisheryObj <- new("Fishery")
        HistFisheryObj@title <- Fishery_base[HF_row,1]
        HistFisheryObj@vulType <- Fishery_base[HF_row,2]
        vulParams_TMP<-unlist(Fishery_base[HF_row,3] %>%
          str_replace_all(fixed(" "), "") %>%
          str_split(","))
        HistFisheryObj@vulParams<-sapply(1:NROW(vulParams_TMP), function(x){
          set_numeric(vulParams_TMP[x])
        })
        HistFisheryObj@retType <- Fishery_base[HF_row,4]
        if(HistFisheryObj@retType != "full"){
          retParams_TMP<-unlist(Fishery_base[HF_row,5] %>%
                                  str_replace_all(fixed(" "), "") %>%
                                  str_split(","))
          HistFisheryObj@retParams<-sapply(1:NROW(retParams_TMP), function(x){
            set_numeric(retParams_TMP[x])
          })
        }
        HistFisheryObj@retMax <-  Fishery_base[HF_row,6]
        HistFisheryObj@Dmort <-  Fishery_base[HF_row,7]
      }else{
        stop("The referenced row of the Fishery tab does not exist")
      }

      #4: Check the targeted stochastic parameter row exists and fill object
      SO_row <- as.numeric(Batch_setup[i,6])
      if(length(Stochastic_base[,1])>=SO_row){
        #-----------------------------
        #Setup Stochastic characteristics
        #-----------------------------

        #---Populate a stochastic object
        #---Here will can setup ecological scenarios, such as low or high initial relative SSB (e.g. scenarios, states of nature)
        StochasticObj<-new("Stochastic")
        for(n in names(Stochastic_base)){
          if(n == "title") {
            StochasticObj@title <- Stochastic_base[SO_row,1]
          }

          if(n %in% c("historicalBio", "Linf", "K", "L50", "L95delta", "M", "Steep", "recSD", "recRho", "H50", "H95delta")){
            TMP<-unlist(Stochastic_base[SO_row,n] %>%
                          str_replace_all(fixed(" "), "") %>%
                          str_split(","))
            if(length(TMP) == 2) {
              slot(StochasticObj, name = n) <- sapply(1:NROW(TMP), function(x){
                set_numeric(TMP[x])
              })
            }
          }

          if(n %in% c("sameFisheryVul", "sameFisheryRet", "sameFisheryDmort")){
            slot(StochasticObj, name = n) <- Stochastic_base[SO_row,n]
          }

          if(n %in% c("histFisheryVul", "histFisheryRet", "histFisheryDmort")){
            HS_row<-as.numeric(Stochastic_base[SO_row,n])
            temp_HS <- set_numeric_vector(Sel_uncertainty_pars[HS_row,])
            if(!is.null(temp_HS)){
              slot(StochasticObj, name = n) <- matrix(temp_HS, nrow=2, byrow=FALSE)
            }
          }

          if(n %in% c("proFisheryVul_list", "proFisheryRet_list", "proFisheryDmort_list")){
            PS_TMP<-unlist(Stochastic_base[SO_row,n] %>%
                          str_replace_all(fixed(" "), "") %>%
                          str_split(","))
            PS_row<-sapply(1:NROW(PS_TMP), function(x){
              set_numeric(PS_TMP[x])
            })
            if(!is.null(PS_row[[1]])){
              slot(StochasticObj, name = n) <- lapply(1:NROW(PS_row), function(x){
                temp_PS <- set_numeric_vector(Sel_uncertainty_pars[PS_row[x],])
                matrix(temp_PS, nrow=2, byrow=FALSE)
              })
            }
          }
        }
      }else{
        stop("The referenced row of the Stochastic tab does not exist")
      }

      #5: Check the targeted fishery parameter row exists and fill the projection fishery object

      PF_row_TMP<-unlist(Batch_setup[i,7] %>%
                              str_replace_all(fixed(" "), "") %>%
                              str_split(","))
      PF_row<-sapply(1:NROW(PF_row_TMP), function(x){
        set_numeric(PF_row_TMP[x])
      })

      if(NROW(PF_row) == TimeAreaObj@areas) {

        ProFisheryObj_list<-lapply(1:NROW(PF_row), function(x){
          if(length(Fishery_base[,1])>=PF_row[x]){
            #-----------------------------
            #Setup projected fishery characteristics
            #-----------------------------

            #---Populate a Fishery object
            #---Contains selectivity, retention and discard characteristics
            ProFisheryObj <- new("Fishery")
            ProFisheryObj@title <- Fishery_base[PF_row[x],1]
            ProFisheryObj@vulType <- Fishery_base[PF_row[x],2]
            vulParams_TMP<-unlist(Fishery_base[PF_row[x],3] %>%
                                    str_replace_all(fixed(" "), "") %>%
                                    str_split(","))
            ProFisheryObj@vulParams<-sapply(1:NROW(vulParams_TMP), function(x){
              set_numeric(vulParams_TMP[x])
            })
            ProFisheryObj@retType <- Fishery_base[PF_row[x],5]
            if(ProFisheryObj@retType != "full"){
              retParams_TMP<-unlist(Fishery_base[PF_row[x],5] %>%
                                      str_replace_all(fixed(" "), "") %>%
                                      str_split(","))
              ProFisheryObj@retParams<-sapply(1:NROW(retParams_TMP), function(x){
                set_numeric(retParams_TMP[x])
              })
            }
            ProFisheryObj@retMax <-  Fishery_base[PF_row[x],6]
            ProFisheryObj@Dmort <-  Fishery_base[PF_row[x],7]
            return(ProFisheryObj)
          }else{
            stop("The referenced row of the Fishery tab does not exist")
          }
        })
      } else {
        stop("You must specify a projection fishery for each area")
      }

      #6: Check the targeted strategy parameter row exists and fill object
      SA_row <- as.numeric(Batch_setup[i,8])
      if(length(Strategy_base[,1])>=SA_row){

        #----------------------------
        #Create a Strategy object
        #----------------------------

        #These values are single inputs specified from the base Time Area input sheet
        StrategyObj <- new("Strategy")
        StrategyObj@title <- Strategy_base[SA_row,1]
        StrategyObj@projectionYears <- set_numeric(Strategy_base[SA_row,2])
        StrategyObj@projectionName <- Strategy_base[SA_row,3]

        #These values are more complex objects specified from supplemental input sheets
        BA_row <- as.numeric(Strategy_base[SA_row,4])
        if(length(Bag_pars[,1])>=BA_row){
          temp_BA <- set_numeric_vector(Bag_pars[BA_row,])
          if(!is.null(temp_BA)){
            if(length(temp_BA)==TimeAreaObj@areas){
            }else{
              temp_BA <- rep(temp_BA,TimeAreaObj@areas)[1:TimeAreaObj@areas]
            }
          }else{
            temp_BA <- rep(-99, TimeAreaObj@areas)
          }
        }else{
          stop("The referenced row of the bag tab does not exist")
        }

        if(StrategyObj@projectionYears>0){
          PE_row <- as.numeric(Strategy_base[SA_row,5])
          if(length(Effort_pars[,1])>=PE_row){
            temp_PE <- set_numeric_vector(Effort_pars[PE_row,])
            if(!is.null(temp_PE)){
              if(length(temp_PE)==(TimeAreaObj@areas*StrategyObj@projectionYears)){
                temp_PE <- matrix(temp_PE,nrow = StrategyObj@projectionYears, ncol = TimeAreaObj@areas, byrow = FALSE)
              }else{
                temp_PE <- rep(temp_PE,(TimeAreaObj@areas*StrategyObj@projectionYears))[1:(TimeAreaObj@areas*StrategyObj@projectionYears)]
                temp_PE <- matrix(temp_PE,nrow = StrategyObj@projectionYears, ncol = TimeAreaObj@areas, byrow = FALSE)
              }
            }else{
              temp_PE <- matrix(1 ,nrow = StrategyObj@projectionYears, ncol = TimeAreaObj@areas, byrow = FALSE) #This default continues fishing at recent effort level if no valid input provided
            }
          }else{
            stop("The referenced row of the effort tab does not exist")
          }
        }else{
          temp_PE<-matrix(1 ,nrow = 0, ncol = TimeAreaObj@areas, byrow = FALSE)
        }

        cpueParams_TMP<-unlist(Strategy_base[SA_row,6] %>%
                                str_replace_all(fixed(" "), "") %>%
                                str_split(","))
        temp_CPUE <- sapply(1:NROW(cpueParams_TMP), function(x){
          set_numeric(cpueParams_TMP[x])
        })


        if(StrategyObj@projectionName == "projectionStrategy"){
          StrategyObj@projectionParams <- list(bag = temp_BA, effort = temp_PE, CPUE = temp_CPUE, CPUEtype = Strategy_base[SA_row,7])
        }else{
          stop("projectionStrategy is the only projection method currently supported by this wrapper")
        }
      }else{
        stop("The referenced row of the Strategy tab does not exist")
      }

      #Now that all objects have been initialized we can run the projection simulations
      runProjection(LifeHistoryObj = LifeHistoryObj,
                    TimeAreaObj = TimeAreaObj,
                    HistFisheryObj = HistFisheryObj,
                    ProFisheryObj_list = ProFisheryObj_list,
                    StrategyObj = StrategyObj,
                    StochasticObj = StochasticObj,
                    wd = outputDir,
                    fileName = Batch_setup[i,1],
                    doPlot = TRUE,
                    titleStrategy = Batch_setup[i,2]
      )
    }
  }else{
    stop("No sims were specified in the Batch Setup tab")
  }
}
