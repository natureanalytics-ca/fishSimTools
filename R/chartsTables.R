

#---------------------------------------
#Relative biomass vs. relative catch
#---------------------------------------

#Roxygen header
#'Relative change in biomass vs. relative catch scatter plot
#'
#'Plots the ratio of year prior to projection to a specified projection year. Iterations are summarized as the median ratio. Calculation are made for relative SSB (y-axis) and relative catch in weight (a-axis). Saves a chart to the wd. Examples can be found in data-raw/DAR_projection_example.R
#'
#' @param wd A working directory where the output of runProjection is saved
#' @param fileName List. List of file names in the working directory
#' @param facetName List. Plot uses ggplot2 facets. File names can be assigned to facets, thus producing separate plots for a given facet, such a low M scenario vs. a high M scenario.
#' @param newLabel List. Replacement label for each file.
#' @param chooseArea The area to summarized in the plot. Value of 0 sums quantities across all areas
#' @param proYear Numeric. The projection year used to calculate relative change in SSB and catch in weight.
#' @param dpi Resolution in dots per inch of the resulting saved chart.
#' @param imageName Character. A name for the resulting plot(s)
#' @param outputDir Directory where the output plot is saved. If NULL, it is set to wd.
#' @param percentile Vector of length two indicating centered percent of observations to represent uncertainty in the table. For example, 95% centered observations should be entered as c(0.025, 0.975)
#' @param doPlot Logical, whether to create plot
#' @import stats ggplot2 ggrepel
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate
#' @export

relSSBscatter<-function(wd, fileName, facetName, newLabel = NULL, chooseArea = 0, proYear, dpi = 300, imageName = "Relative_SSB_catch", outputDir = NULL, percentile = c(0.025, 0.975), doPlot = TRUE){

  catchB_median<-SSB_median<-nm<-NULL

  if(is.null(outputDir)) outputDir<-wd

  totalSSB<-data.frame()
  areaSSB<-data.frame()
  for(i in 1:NROW(fileName)){
    data<-readRDS(paste0(wd, "/", fileName[[i]],".rds"))

    refYear<-1 + data$TimeAreaObj@historicalYears
    getYear<-1 + data$TimeAreaObj@historicalYears + proYear

    #Choose area
    if(chooseArea == 0) { #Sum across areas
      SSB_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        sum(data$dynamics$SB[getYear,x,]) / sum(data$dynamics$SB[refYear,x,])
      })
      cat_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        sum(data$dynamics$catchB[getYear,x,]) / sum(data$dynamics$catchB[refYear,x,])
      })
      totalSSB<-rbind(totalSSB, list(fct = facetName[[i]], nm = ifelse(is.null(newLabel[[i]]), data$titleStrategy, newLabel[[i]]), SSB_median = median(SSB_tmp), SSB_lower = quantile(SSB_tmp, probs = percentile[1]), SSB_upper = quantile(SSB_tmp, probs = percentile[2]), catchB_median = median(cat_tmp), catchB_lower = quantile(cat_tmp, probs = percentile[1]), catchB_upper = quantile(cat_tmp, probs = percentile[2])))
    } else {#Specific area selected by user
      SSB_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        data$dynamics$SB[getYear,x,chooseArea] / data$dynamics$SB[refYear,x,chooseArea]
      })
      cat_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        data$dynamics$catchB[getYear,x,chooseArea] / data$dynamics$catchB[refYear,x,chooseArea]
      })
      totalSSB<-rbind(totalSSB, list(fct = facetName[[i]], nm = ifelse(is.null(newLabel[[i]]), data$titleStrategy, newLabel[[i]]), SSB_median = median(SSB_tmp), SSB_lower = quantile(SSB_tmp, probs = percentile[1]), SSB_upper = quantile(SSB_tmp, probs = percentile[2]), catchB_median = median(cat_tmp), catchB_lower = quantile(cat_tmp, probs = percentile[1]), catchB_upper = quantile(cat_tmp, probs = percentile[2])))
    }
  }

  print(NROW(unique(totalSSB$fct)))
  #Total relative SSB
  ggplot(totalSSB, aes(catchB_median, SSB_median)) +

    geom_rect(
      aes(
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1
      ),
      color = "grey",
      fill = "pink",
      alpha = 0.1
    ) +

    geom_rect(
      aes(
      xmin = 0,
      xmax = 1,
      ymin = 1,
      ymax = max(max(SSB_median)+0.1, 2)
      ),
      color = "grey",
      fill = "yellow",
      alpha = 0.05
    ) +

    geom_rect(
      aes(
      xmin = 1,
      xmax = max(max(catchB_median)+0.1,2),
      ymin = 0,
      ymax = 1
      ),
      color = "grey",
      fill = "yellow",
      alpha = 0.05
    ) +

    geom_rect(
      aes(
      xmin = 1,
      xmax = max(max(catchB_median)+0.1,2),
      ymin = 1,
      ymax = max(max(SSB_median)+0.1,2),
      ),
      color = "grey",
      fill = "green",
      alpha = 0.1
    ) +

    #geom_vline(xintercept = 1, color="lightgrey") +
    #geom_hline(yintercept = 1, colour="lightgrey") +
    geom_point(size=1) +
    geom_label_repel(data=totalSSB,
                    aes(catchB_median, SSB_median, label = nm),
                    fontface = 'bold',
                    color="black",
                    size=2,
                    segment.size = 0.1,
                    segment.color = "black",
                    box.padding = unit(0.5, "lines"),
                    point.padding = unit(0, "lines"),
                    show.legend = FALSE,
                    max.overlaps = 25
    ) +
    ylab(paste("Relative biomass after", proYear, "years")) +
    xlab(paste("Relative catch weight after", proYear, "years")) +
    theme_classic() +
    theme(strip.text.x=element_text(colour = "black", size=8, face="bold"),
          strip.text.y=element_text(colour = "black", size=8, face="bold"),
          strip.background = element_rect(fill ="lightgrey"),
          axis.text=element_text(size=6),
          panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
          legend.position = "none") +
  facet_wrap(~fct, ncol=2)
  if(doPlot) ggsave(filename = paste0(outputDir, "/", imageName, ".png"), device = "png", dpi = dpi, width = min(6*NROW(unique(totalSSB$fct)),7), height = max((3*NROW(unique(totalSSB$fct))/2),4), units = "in")
  return(totalSSB)
}

#---------------------------------------
#Relative biomass time series
#---------------------------------------

#Roxygen header
#'Relative change in biomass and relative catch as time series with uncertainty
#'
#'Time series of changes in spawning stock biomass and catch weight (two separate plots produced) relative to the year prior to projection time period. Saves a chart to the wd. Examples can be found in data-raw/DAR_projection_example.R
#'
#' @param wd A working directly where the output of runProjection is saved
#' @param fileName List. List of file names in the working directory
#' @param facetName List. Plot uses ggplot2 facets. File names can be assigned to facets, thus producing separate plots for a given facet, such a low M scenario vs. a high M scenario.
#' @param newLabel List. Replacement label for each file.
#' @param chooseArea The area to summarized in the plot. Value of 0 sums quantities across all areas
#' @param percentile Vector of length two indicating centered percent of observations to represent uncertainty in time series plots. For example, 95% centered observations should be entered as c(0.025, 0.975)
#' @param percentileColor Color for the centered distribution of outcomes.
#' @param percentileAlpha Transparency param (between 0 and 1) for the percentile color
#' @param lineColor Color of the median trend line.
#' @param doHist Should the historical time period be included in the plot?
#' @param dpi Resolution in dots per inch of the resulting saved chart.
#' @param imageName Character. A name for the resulting plot(s)
#' @param scales From ggplot2::facet_wrap Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param outputDir Directory where the output plot is saved. If NULL, it is set to wd.
#' @import ggplot2 ggrepel
#' @importFrom stats quantile
#' @export

relSSBseries<-function(wd, fileName, facetName, newLabel = NULL, chooseArea = 0, percentile = c(0.025, 0.975), percentileColor = "#0096d6", percentileAlpha = 0.5, lineColor = "black", doHist = FALSE, dpi = 300, imageName = "Relative_timeSeries", scales = "fixed", outputDir = NULL){

  year<-med<-lower<-upper<-NULL

  if(is.null(outputDir)) outputDir<-wd

  totalSSB<-data.frame()
  totalcatchB<-data.frame()
  labelVec<-vector()

  for(i in 1:NROW(fileName)){

    data<-readRDS(paste0(wd, "/", fileName[[i]],".rds"))
    refYear <- 1 + data$TimeAreaObj@historicalYears
    startYear <- ifelse(doHist, 1, refYear)
    endYear <- 1 + data$TimeAreaObj@historicalYears + data$StrategyObj@projectionYears
    id <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
    labelVec<-append(labelVec, setNames(ifelse(is.null(newLabel[[i]]), data$titleStrategy, newLabel[[i]]), id), after=length(labelVec))

    #Choose area
    if(chooseArea == 0) { #Sum across areas
      SSB_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        rowSums(data$dynamics$SB[startYear:endYear,x,])/sum(data$dynamics$SB[refYear,x,])
      })

      cat_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        rowSums(data$dynamics$catchB[startYear:endYear,x,])/sum(data$dynamics$catchB[refYear,x,])
      })
    } else { #Specific area selected by user
      SSB_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        data$dynamics$SB[startYear:endYear,x,chooseArea]/data$dynamics$SB[refYear,x,chooseArea]
      })

      cat_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        data$dynamics$catchB[startYear:endYear,x,chooseArea]/data$dynamics$catchB[refYear,x,chooseArea]
      })
    }

    SSB_range_outer<-t(sapply(1:NROW(SSB_tmp), FUN=function(x){
      c(quantile(SSB_tmp[x,], probs =  percentile), median(SSB_tmp[x,]), x)
    }))

    totalSSB<-rbind(totalSSB, list(
      fct = rep(facetName[[i]], NROW(SSB_range_outer)),
      nm = rep(id, NROW(SSB_range_outer)),
      lowerOuter =  SSB_range_outer[,1],
      upperOuter = SSB_range_outer[,2],
      med = SSB_range_outer[,3],
      year = SSB_range_outer[,4]))

    cat_range_outer<-t(sapply(1:NROW(cat_tmp), FUN=function(x){
      c(quantile(cat_tmp[x,], probs =  percentile), median(cat_tmp[x,]), x)
    }))

    totalcatchB<-rbind(totalcatchB, list(
      fct = rep(facetName[[i]], NROW(cat_range_outer)),
      nm = rep(id, NROW(cat_range_outer)),
      lowerOuter =  cat_range_outer[,1],
      upperOuter = cat_range_outer[,2],
      med = cat_range_outer[,3],
      year = cat_range_outer[,4]))

  }

  #Total relative SSB
  ggplot(totalSSB, aes(x = year, y = med)) +
    geom_hline(yintercept = 1, colour="lightgrey") +
    geom_ribbon(aes(ymin = lowerOuter, ymax = upperOuter, fill = "fill1"), alpha = percentileAlpha) +
    geom_line(size = 0.8, aes(color='line1')) +
    ylab("Relative biomass") +
    xlab("Year") +
    theme_classic() +
    theme(strip.text.x=element_text(colour = "black", size=8, face="bold"),
          strip.text.y=element_text(colour = "black", size=8, face="bold"),
          strip.background = element_rect(fill ="lightgrey"),
          axis.text=element_text(size=6),
          panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
          legend.position = "bottom"
    ) +
    #ncol=NROW(unique(totalSSB$nm)),
    facet_wrap(~ fct + nm,  nrow = NROW(unique(totalSSB$fct)), scales = scales, labeller = labeller(nm = labelVec)) +
    scale_color_manual(name = "",
                       values = c(line1 = lineColor),
                       labels = "Median")+
    scale_fill_manual(name = "",
                      values = c(fill1 = percentileColor),
                      labels = paste0(as.character(round(100*(percentile[2]-percentile[1]),1)), "% of outcomes"))

  ggsave(filename = paste0(outputDir, "/", imageName, "_SSB.png"), device = "png", dpi = dpi, width = min(5*NROW(unique(totalSSB$fct)),8), height = 3.5*NROW(unique(totalSSB$fct)), units = "in")

  #Total relative catch weight
  ggplot(totalcatchB, aes(x = year, y = med)) +
    geom_hline(yintercept = 1, colour="lightgrey") +
    geom_ribbon(aes(ymin = lowerOuter, ymax = upperOuter, fill = "fill1"), alpha = percentileAlpha) +
    geom_line(size = 0.8, aes(color='line1')) +
    ylab("Relative catch weight") +
    xlab("Year") +
    theme_classic() +
    theme(strip.text.x=element_text(colour = "black", size=8, face="bold"),
          strip.text.y=element_text(colour = "black", size=8, face="bold"),
          strip.background = element_rect(fill ="lightgrey"),
          axis.text=element_text(size=6),
          panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
          legend.position = "bottom"
    ) +
    facet_wrap(~ fct + nm, nrow = NROW(unique(totalSSB$fct)),  scales = scales, labeller = labeller(nm = labelVec)) +
    scale_color_manual(name = "",
                       values = c(line1 = lineColor),
                       labels = "Median")+
    scale_fill_manual(name = "",
                      values = c(fill1 = percentileColor),
                      labels = paste0(as.character(round(100*(percentile[2]-percentile[1]),1)), "% of outcomes"))
  ggsave(filename = paste0(outputDir, "/", imageName, "_catchB.png"), device = "png", dpi = dpi, width = min(5*NROW(unique(totalSSB$fct)),8), height = 3.5*NROW(unique(totalcatchB$fct)), units = "in")
}


#---------------------------------------
#Retained biomass or CPUE time series
#---------------------------------------

#Roxygen header
#'Relative change in retained biomass (e.g., vulnerable biomass above size limit) with uncertainty
#'
#' @param wd A working directly where the output of runProjection is saved
#' @param fileName List. List of file names in the working directory
#' @param facetName List. Plot uses ggplot2 facets. File names can be assigned to facets, thus producing separate plots for a given facet, such a low M scenario vs. a high M scenario.
#' @param newLabel List. Replacement label for each file.
#' @param chooseArea The area to summarized in the plot. Value of 0 sums quantities across all areas
#' @param percentile Vector of length two indicating centered percent of observations to represent uncertainty in time series plots. For example, 95% centered observations should be entered as c(0.025, 0.975)
#' @param percentileColor Color for the centered distribution of outcomes.
#' @param percentileAlpha Transparency param (between 0 and 1) for the percentile color
#' @param lineColor Color of the median trend line.
#' @param doHist Should the historical time period be included in the plot?
#' @param dpi Resolution in dots per inch of the resulting saved chart.
#' @param imageName Character. A name for the resulting plot(s)
#' @param scales From ggplot2::facet_wrap Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param outputDir Directory where the output plot is saved. If NULL, it is set to wd.
#' @param doPlot Logical, whether to create plot
#' @import ggplot2 ggrepel
#' @importFrom stats quantile
#' @export

retainBioSeries<-function(wd, fileName, facetName, newLabel = NULL, chooseArea = 0, percentile = c(0.025, 0.975), percentileColor = "#0096d6", percentileAlpha = 0.5, lineColor = "black", doHist = FALSE, dpi = 300, imageName = "Relative_timeSeries", scales = "fixed", outputDir = NULL, doPlot = TRUE){

  year<-med<-lower<-upper<-NULL

  if(is.null(outputDir)) outputDir <- wd

  totalB<-data.frame()
  labelVec<-vector()

  for(i in 1:NROW(fileName)){

    data<-readRDS(paste0(wd, "/", fileName[[i]],".rds"))
    refYear <- 1 + data$TimeAreaObj@historicalYears
    startYear <- ifelse(doHist, 1, refYear)
    endYear <- 1 + data$TimeAreaObj@historicalYears + data$StrategyObj@projectionYears
    id <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
    labelVec<-append(labelVec, setNames(ifelse(is.null(newLabel[[i]]), data$titleStrategy, newLabel[[i]]), id), after=length(labelVec))

    #Choose area
    if(chooseArea == 0) { #Sum across areas
      B_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        rowSums(data$dynamics$RB[startYear:endYear,x,])/sum(data$dynamics$RB[refYear,x,])
      })
    } else { #Specific area selected by user
      B_tmp<-sapply(1:data$TimeAreaObj@iterations, FUN=function(x){
        data$dynamics$RB[startYear:endYear,x,chooseArea]/data$dynamics$RB[refYear,x,chooseArea]
      })
    }

    B_range_outer<-t(sapply(1:NROW(B_tmp), FUN=function(x){
      c(quantile(B_tmp[x,], probs =  percentile), median(B_tmp[x,]), x)
    }))

    totalB<-rbind(totalB, list(
      fct = rep(facetName[[i]], NROW(B_range_outer)),
      nm = rep(id, NROW(B_range_outer)),
      lowerOuter =  B_range_outer[,1],
      upperOuter = B_range_outer[,2],
      med = B_range_outer[,3],
      year = B_range_outer[,4]))
  }

  #Total relative SSB
  ggplot(totalB, aes(x = year, y = med)) +
    geom_hline(yintercept = 1, colour="lightgrey") +
    geom_ribbon(aes(ymin = lowerOuter, ymax = upperOuter, fill = "fill1"), alpha = percentileAlpha) +
    geom_line(size = 0.8, aes(color='line1')) +
    ylab("Relative biomass") +
    xlab("Year") +
    theme_classic() +
    theme(strip.text.x=element_text(colour = "black", size=8, face="bold"),
          strip.text.y=element_text(colour = "black", size=8, face="bold"),
          strip.background = element_rect(fill ="lightgrey"),
          axis.text=element_text(size=6),
          panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
          legend.position = "bottom"
    ) +
    #ncol=NROW(unique(totalSSB$nm)),
    facet_wrap(~ fct + nm,  nrow = NROW(unique(totalB$fct)), scales = scales, labeller = labeller(nm = labelVec)) +
    scale_color_manual(name = "",
                       values = c(line1 = lineColor),
                       labels = "Median")+
    scale_fill_manual(name = "",
                      values = c(fill1 = percentileColor),
                      labels = paste0(as.character(round(100*(percentile[2]-percentile[1]),1)), "% of outcomes"))
  if(doPlot)  ggsave(filename = paste0(outputDir, "/", imageName, "_retainBio.png"), device = "png", dpi = dpi, width = min(5*NROW(unique(totalB$fct)),8), height = 3.5*NROW(unique(totalB$fct)), units = "in")

  #Replace labels
  return(totalB %>%
           mutate(newName = str_replace_all(as.character(nm), labelVec)))
}

