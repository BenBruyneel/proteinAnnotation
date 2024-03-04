# ---- Basics ----

#' @title theme_minimal_adapted
#'
#' @description to be able to use the theme theme_minimal with some adjustments
#'
#' @param base_size size of the lettering  of axis, title etc
#' @param base_family letter type of axis, title etc
#' @param base_line_size width of gridlines, set  to 0 for none
#' @param axis_line_size width of axis lines, set to 0 for none
#' @param xAxis if TRUE then display xAxis title
#' @param yAxis if TRUE then display yAxis title
#' @param showLegend if TRUE then show legend
#' @param legend.position defines where to place the legend
#' @param gridLines if TRUE then display gridlines
#' @param gridLinesX if TRUE then display gridlines 'along' the x-axis
#' @param gridLinesY if TRUE then display gridlines 'along' the y-axis
#' @param titleSize if NA, use default title size, else use titleSize value
#'
#' To be used as ggplot-object + theme_minimal_adapted()
#'
#' @returns theme definition
#'
#' @note also part of the personal package 'BBPersonalR'
#'
#' @export
theme_minimal_adapted <- function(base_size = 11, base_family = "",
                                  base_line_size = base_size/22,
                                  axis_line_size = base_line_size/2,
                                  xAxis = TRUE, yAxis = TRUE,
                                  showLegend = TRUE, legend.position = "bottom",
                                  gridLines = TRUE,
                                  gridLinesX = TRUE,
                                  gridLinesY = TRUE,
                                  titleSize = NA){
  theTheme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                                base_line_size = base_line_size) %>%
    ggplot2::theme_replace(axis.ticks = ggplot2::element_blank(), legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(), #panel.border = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "black", linewidth = axis_line_size, linetype = "solid", fill = NA))
  if (!xAxis) {
    theTheme <- theTheme %>%
      ggplot2::theme_replace(axis.title.x = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  }
  if (!yAxis) {
    theTheme <- theTheme %>%
      ggplot2::theme_replace(axis.title.y = ggplot2::element_blank(),
                             axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }
  if (showLegend){
    theTheme <- theTheme %>%
      ggplot2::theme_replace(legend.position = legend.position)
  } else {
    theTheme <- theTheme %>%
      ggplot2::theme_replace(legend.position = "none")
  }
  if (!gridLines){
    theTheme <- theTheme %>%
      ggplot2::theme_replace(panel.grid = ggplot2::element_blank())
  } else {
    if (!gridLinesX){
      theTheme <- theTheme %>%
        ggplot2::theme_replace(panel.grid.major.x = ggplot2::element_blank(),
                               panel.grid.minor.x = ggplot2::element_blank())
    } else {
      theTheme <- theTheme %>%
        ggplot2::theme_replace(panel.grid.major.x = ggplot2::element_line(color = "black", linewidth = axis_line_size, linetype = "dotted"),
                               panel.grid.minor.x = ggplot2::element_blank())
      
    }
    if (!gridLinesY){
      theTheme <- theTheme %>%
        ggplot2::theme_replace(panel.grid.major.y = ggplot2::element_blank(),
                               panel.grid.minor.y = ggplot2::element_blank())
    } else {
      theTheme <- theTheme %>%
        ggplot2::theme_replace(panel.grid.major.y = ggplot2::element_line(color = "black", linewidth = axis_line_size, linetype = "dotted"),
                               panel.grid.minor.y = ggplot2::element_blank())
      
    }
  }
  if (!identical(titleSize,NA)){
    theTheme <- theTheme %>%
      ggplot2::theme_replace(plot.title = ggplot2::element_text(size = titleSize))
  }
  return(theTheme)
}

#' @title graphsAdjust
#'
#' @description Make adjustments to a graph eg zoom, titles etc etc
#'
#' @param graphs list of ggplot-objects to which the adjustments have to be made
#'  Note: MUST be a list
#' @param vertical if TRUE, flips x- and y-axis
#' @param xDiscrete specifies whether the x-axis should be discrete
#' @param yDiscrete specifies whether the y-axis should be discrete
#' @param xReverse specifies whether to reverse the x-axis
#' @param yReverse specifies whether to reverse the y-axis
#' @param xDefault if TRUE, then xExpand, xLimits and xOob are ignored
#'  (essentially autoscaling the x-axis)
#' @param yDefault same as xDefault, but for y-axis
#' @param xLimits range of the x-axis, normally xLimits = c(minimum, maximum)
#' if minimum and/or maximum is NA, then they are autoscaled, if xLimits = NA
#'  then the range is 0, putting all datapoints in one line (x-axis-wise)
#' @param yLimits range of the y-axis (see xLimits)
#' @param xExpand allows for padding around data (x-axis),
#'  see ?ggplot2::expansion for proper explanation
#' @param yExpand allows for padding around data (y-axis),
#'  see ?ggplot2::expansion for proper explanation
#' @param xLabelFormat defines the numeric format to be used for the x-axis
#'  labels (see fromatDigits() & formatScientificDigits() for examples)
#' @param yLabelFormat defines the numeric format to be used for the y-axis
#'  labels (see fromatDigits() & formatScientificDigits() for examples)
#' @param xOob defines what to do with data that's out of range of the data,
#'  see ?scales::oob for proper explanation. Note: only deals with x-axis
#' @param yOob defines what to do with data that's out of range of the data,
#'  see ?scales::oob for proper explanation. Note: only deals with y-axis
#' @param xLog if TRUE then automatic transformation of the x-axis to logarihmic
#'  scale
#' @param yLog if TRUE then automatic transformation of the y-axis to logarihmic
#'  scale
#' @param xIsDate if TRUE then all settings regarding the x-axis are ignored,
#'  except xLimits which should be dates (defaults are not dates). Alternatively
#'  xDefault can be set to TRUE for autoscaling
#' @param yIsDate if TRUE then all settings regarding the y-axis are ignored
#'  except yLimits which should be dates (defaults are not dates). Alternatively
#'  xDefault can be set to TRUE for autoscaling
#' @param titles sets title of graph
#' @param xLabel sets x-axis title
#' @param yLabel set y-axos title
#' @param setTheme if NA, then no theme is applied, otherwise uses the defined
#'  theme (can also be ggplot2 included themes, such as theme_bw())
#' @param plot.margins.default if TRUE, ignore plot.margins and other parameters
#' @param plot.margins defines margins (from the border) of the plot
#'  c(top, right, bottom, left)
#' @param plot.margins.units default = "points", other possibilities:
#'  ?grid::unit , examples "cm", "points", "inches", "npc" (viewport) etc etc
#'
#' @returns a list of ggplot objects
#'
#' @note also part of the personal package 'BBPersonalR'
#'
#' @export
graphsAdjust <- function(graphs, vertical = FALSE,
                         xDiscrete = FALSE, yDiscrete = FALSE,
                         xReverse = FALSE, yReverse = FALSE,
                         xDefault = FALSE, yDefault = FALSE,
                         xLimits = c(0,NA), yLimits = c(0,NA),
                         xExpand = ggplot2::expansion(mult = 0, add = 0),
                         yExpand = ggplot2::expansion(mult = 0, add = 0),
                         xLabelFormat = ggplot2::waiver(), yLabelFormat = ggplot2::waiver(),
                         xOob = scales::oob_squish_infinite,
                         yOob = scales::oob_squish_infinite,
                         xLog = FALSE, yLog = FALSE,
                         xIsDate = FALSE, yIsDate = FALSE,
                         titles = NA, xLabel = NA, yLabel = NA,
                         setTheme = theme_minimal_adapted(),
                         plot.margins.default = TRUE,
                         plot.margins = c(5,5,5,5),
                         plot.margins.units = "points"){
  if (xLog){
    if (xReverse){
      xReverse <- ggforce::trans_reverser('log10')
      xLimits <- xLimits[2:1]
    }  else {
      xReverse <- scales::log10_trans()
    }
  }
  if (yLog){
    if (yReverse){
      yReverse <- ggforce::trans_reverser('log10')
      yLimits <- xLimits[2:1]
    }  else {
      yReverse <- scales::log10_trans()
    }
  }
  for (counter in 1:(length(graphs))){
    # axis transformations
    if (!xIsDate){
      if (xLog) {
        if (!xDefault){
          graphs[[counter]] <- graphs[[counter]] +
            ggplot2::scale_x_continuous(expand = xExpand,
                                        limits = xLimits,
                                        oob = xOob,
                                        trans = xReverse,
                                        labels = xLabelFormat)
        } else {
          graphs[[counter]] <- graphs[[counter]] +
            ggplot2::scale_x_continuous(labels = xLabelFormat, trans = xReverse)
        }
      } else {
        if (!xDefault){
          if (!xReverse){
            graphs[[counter]] <- graphs[[counter]] +
              ggplot2::scale_x_continuous(expand = xExpand, limits = xLimits, oob = xOob,
                                          labels = xLabelFormat)
          } else {
            graphs[[counter]] <- graphs[[counter]] +
              ggplot2::scale_x_reverse(expand = xExpand, limits = xLimits[2:1], oob = xOob,
                                       labels = xLabelFormat)
          }
        } else {
          if (!xDiscrete){
            if (!xReverse){
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_x_continuous(labels = xLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_x_reverse(labels = xLabelFormat)
            }
          } else {
            if (!xReverse){
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_x_discrete(labels = xLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_x_discrete(limits = rev, labels = xLabelFormat)
            }
          }
        }
      }
    } else {
      if (!xDefault){
        graphs[[counter]] <- graphs[[counter]] +
          ggplot2::scale_x_datetime(limits = xLimits,
                                    labels = xLabelFormat)#, trans = xReverse)  -- do not know if works
      }
    }
    if (!yIsDate){
      if (yLog) {
        if (!yDefault){
          graphs[[counter]] <- graphs[[counter]] +
            ggplot2::scale_y_continuous(expand = yExpand,
                                        limits = yLimits,
                                        oob = yOob,
                                        trans = yReverse,
                                        labels = yLabelFormat)
        } else {
          graphs[[counter]] <- graphs[[counter]] +
            ggplot2::scale_y_continuous(labels = yLabelFormat, trans = yReverse)
        }
      } else {
        if (!yDefault){
          if (!yReverse){
            graphs[[counter]] <- graphs[[counter]] +
              ggplot2::scale_y_continuous(expand = yExpand, limits = yLimits, oob = yOob,
                                          labels = yLabelFormat)
          } else {
            graphs[[counter]] <- graphs[[counter]] +
              ggplot2::scale_y_reverse(expand = yExpand, limits = yLimits[2:1], oob = yOob,
                                       labels = yLabelFormat)
          }
        } else {
          if (!yDiscrete){
            if (!yReverse){
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_y_continuous(labels = yLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_y_reverse(labels = yLabelFormat)
            }
          } else {
            if (!yReverse){
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_y_discrete(labels = yLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] +
                ggplot2::scale_y_discrete(limits = rev, labels = yLabelFormat)
            }
          }
        }
      }
    } else {
      if (!yDefault){
        graphs[[counter]] <- graphs[[counter]] +
          ggplot2::scale_y_datetime(limits = yLimits,
                                    labels = yLabelFormat)#, trans = xReverse)  -- do not know if works
      }
    }
    # swap x & y
    if (vertical){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::coord_flip()
    }
    # labels
    if (!identical(titles,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::ggtitle(titles[counter])
    }
    if (!identical(xLabel,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::xlab(xLabel)
    }
    if (!identical(yLabel,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::ylab(yLabel)
    }
    if (!identical(setTheme,NA)){
      graphs[[counter]] <- graphs[[counter]] + setTheme
    }
    if (!plot.margins.default){
      graphs[[counter]] <- graphs[[counter]] +
        ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
    }
  }
  return(graphs)
}

# ---- proteinData specific ----

#' @title displayProtein
#'
#' @description displays the protein data in a data frame via ggplot2's geom_tile
#'
#' @param proteinData the protein data data.frame
#' @param columns numeric vector that specifies the width (in characters) of the protein data display
#' @param emptySequence if the protein data is extended, this character vector is used to put in the sequence column in the new rows. The position column is simply
#'  filled with increasing integers.
#' @param replaceNA default is NA, if defined as something else, then ALL NA values in the protein data will be replace with this argument.
#'  Ignored if columns is not NA.
#' @param backGroundColor background color to use for the 'tiles' if no fill parameters are set. Default is 'white'
#' @param frontColor color to use to border the 'tiles' in the fill color legend. If no text color parameters are set Default = 'black'
#' @param borderColor color to use for the 'tiles'. Default is 'backGroundColor'
#' @param defaultTextColor color to use for the text in the 'tiles' if no text color parameters are set Default = 'black'
#' @param textColorColumn character vector: name of the column in the proteinData data.frame that contains info for the color of the text
#'  in the 'tiles'. Note: this should not be continuous data
#' @param replaceTextNA vector that specifies with what to replace NA if there are NA values in the textColorColumn in the protein data. Default is "0" (for coverage purposes)
#' @param textColorLevels if the data in textColor column is non-continous, eg character data, then this argument allows definition/ordering of the levels during
#'  the 'transformation' into factors (done) during visualization of the text colors
#' @param textColorLabels if the data in textColor Column is non-continous, eg character data, then this argument allows definition/ordering of the names of the levels
#'  during the 'transformation' into factors (done) during visualization of the text colors. See also \code{\link[base]{factor}}
#' @param textColors vector with the text colors to use. Note: the number required is defined by the number of 'discrete' values in the textColor
#'  column of the protein data
#' @param textColorFunction allows custom definition of the function scale_color_ - function to be used during display, see example on 'fillFunction' argument
#' @param showTextLegend logical vector, if TRUE show the legend for the text color(s)
#' @param textLegendTitle title to use for the legend of the text colors
#' @param textLegendDirection direction text color legend. Default is 'vertical'. Alternative is 'horizontal'
#' @param fillColorColumn character vector: name of the column in the proteinData data.frame that contains info for the fillcolor
#'  of the 'tiles'. Note: this should be continuous data. If discrete data, then fillColorLevels can be defined for more exact coloring
#' @param replaceFillNA vector that specifies with what to replace NA if there are NA values in the fillColorColumn in the protein data
#' @param fillColorLevels if the data in fillColor column is non-continous, eg character data, then this argument allows definition/ordering of the names of the levels
#'  during the 'transformation' into factors (done) during visualization of the text colors. See also \code{\link[base]{factor}}
#' @param fillColorLabels vector with the text colors to use. Note: the number required is defined by the number of 'discrete' values in the fillColor
#'  column of the protein data
#' @param fillColors the lower (minimum) and upper (maximum) bound colors to use for fillcolor of the tiles. Usually this is just two different colors:
#'  the 'tiles' will be filled with a mixture of both depending on the values in the textColorColumn
#' @param fillTransformation this allows the scale of the fill colors to be non-linear. Default is NA. One frequently used is 'log10' to generate
#'  a logarithmic scale. Other options can be found in \code{\link[ggplot2]{scale_fill_gradientn}}
#' @param fillColorsMinMax the minimum and maximum value to use for forming gradient colors for the tiles. If some values in the textColorColumn are
#'  outside the range c(minimum, maximum), the some unexpected things may happen. Usually this is used to 'stretch' the color scale. Only works when
#'  working with a continuous scake
#' @param fillFunction allows for precise setting of the fill scale, see examples
#' @param showFillLegend logical vector, if TRUE show the legend for the fill color(s)
#' @param fillLegendTitle title to use for the legend of the text colors
#' @param fillLegendDirection direction fill color legend. Default is 'horizontal'. Alternative is 'vertical'
#' @param legendPosition character vector: where to put the legends ("none", "left", "right", "bottom", "top"). Default is 'bottom'
#' @param legendBox arrangement of multiple legends ("horizontal" or "vertical")
#' @param naFillColor still used ????????????? <------------
#' @param textSize default is 4. Sets the size of the text in the 'tiles'
#' @param textFontFace allows manipulation of the text font used. Default is 'bold'. Alternatives are "plain", "italic", & "bold.italic"
#' @param linewidth sets the width of the lines between the 'tiles', default is 0.35
#' @param alpha sets the alpha for the fill color of the 'tiles'. Default is 1
#' @param tileWidth sets the width for the 'tiles', default is 1
#' @param tileHeight sets the height for the 'tiles', default is 1
#' @param title specifies the title
#' @param subtitle specifies the subtitle
#' @param caption specifies the caption
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' newTable <- proteinCoverage(sequence = standardProtein("OVA"), start = 2,
#'  nterm = 1, peptideTable = OVATable("peptide"), Accession = "P01012",
#'   positionColumn = "PositionsinProteins")
#' newTable <- mapPeptidesToProtein(proteinDF = newTable,
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", variable = "Abundances_1",
#'  dataName = "Abundances_1")
#' newTable <- mapPeptidesToProtein(proteinDF = newTable,
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins",
#'  variable = "Abundances_2", dataName = "Abundances_2")
#' newTable <- mapPeptidesToProtein(proteinDF = newTable,
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins",
#'  variable = "XCorrbySearchEngine_1", dataName = "Score")
#' newTable[35:40,]
#' # display protein sequence info
#' displayProtein(newTable, columns = 50)
#' displayProtein(newTable, columns = 50, textColorColumn = "sequence",
#'  textColors = rainbow(21))
#' displayProtein(newTable, columns = 50, textColorColumn = "sequence",
#'  textColors = terrain.colors(21))
#' # simply display sequencing coverage
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"))
#' displayProtein(boxData(newTable, ncols = 50), textColorColumn = "coverage",
#'  textColors = c("black","red"))
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), backGroundColor = "lightblue")
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), textLegendTitle = "Coverage",
#'  showTextLegend = TRUE, textColorLabels = c("No Coverage", "Coverage"), title = "Ovalbumin")
#' # display score & coverage
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), textLegendTitle = "Coverage", showTextLegend = TRUE,
#'  textColorLabels = c("No Coverage", "Coverage"), title = "Ovalbumin",
#'  fillColorColumn = "Score", fillColors = c("white","brown"), alpha = 0.5)
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), textLegendTitle = "Coverage", showTextLegend = TRUE,
#'  textColorLabels = c("No Coverage", "Coverage"), title = "Ovalbumin", fillColorColumn = "Score",
#'  fillColors = c("white","brown"), alpha = 0.5, fillColorsMinMax = c(0,50))
#' # display abundance & coverage
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_1",
#'  fillColors = c("white","brown"), alpha = 0.5)
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_1",
#'  fillColors = c("white","brown"), alpha = 0.5, fillColorsMinMax = c(1E3, 1.5E10))
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_2",
#'  fillColors = c("white","brown"), alpha = 0.5, fillColorsMinMax = c(1E3, 1.5E10))
#' # display abundance logarithmic color scale & coverage
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_1",
#'  fillColors = c("white","#8b0000"), alpha = 0.5, fillColorsMinMax = c(1E6, 1.5E10),
#'  fillTransformation = "log10")
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_2",
#'  fillColors = c("white","#8b0000"), alpha = 0.5, fillColorsMinMax = c(1E6, 1E11),
#'  fillTransformation = "log10")
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), title = "Ovalbumin", fillColorColumn = "Abundances_1",
#'  fillColors = c("white","#8b0000"), alpha = 0.5, fillColorsMinMax = c(1E6, 1.5E10),
#'  fillTransformation = "log10", fillLegendTitle = "Abundance", fillLegendDirection = "vertical",
#'  legendPosition = "right")
#' # use discrete data for fill can be done via 'fillFunction' argument
#' trypticSites <- getPeptideStart(proteinSequence = standardProtein("OVA"),
#'  peptideSequence = "(?<!P)R|(?<!P)K")
#' trypticSites <- data.frame(position = trypticSites, data = "tryptic")
#' trypticSites
#' newTable <- addDataToProtein(proteinDF = newTable, dataframe = trypticSites,
#' dataColumn = "data", dataName = "trypticSite", NAValue = "")
#' newTable[75:90,]
#' # w/o using the fillFunction argument
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#'  textColors = c("black","red"), textLegendTitle = "Coverage", showTextLegend = TRUE,
#'  textColorLabels = c("No Coverage", "Coverage"), title = "Ovalbumin - tryptic sites",
#'  fillColorColumn = "trypticSite", fillColors = c("white","yellow"), replaceFillNA = "",
#'  fillColorLevels = c("","tryptic"), fillColorLabels = c("Non-tryptic site", "Tryptic site"),
#'  showFillLegend = FALSE, borderColor = "black")
#' # using the fillFunction argument
#' displayProtein(newTable, columns = 50, textColorColumn = "coverage",
#' textColors = c("black","red"), textLegendTitle = "Coverage", showTextLegend = TRUE,
#' textColorLabels = c("No Coverage", "Coverage"), title = "Ovalbumin - tryptic sites",
#' fillColorColumn = "trypticSite", fillFunction = ggplot2::scale_fill_manual(values = c("white",
#' "yellow"), na.value = "white"), showFillLegend = FALSE, borderColor = "black")
displayProtein <- function(proteinData, columns = NA, emptySequence = "", replaceNA = NA,
                           backGroundColor = "white", frontColor = "black",
                           borderColor = backGroundColor,
                           defaultTextColor = "black",
                           textColorColumn = NA, replaceTextNA = "0",
                           textColorLevels = NA, textColorLabels = NA,
                           textColors = c("black"),
                           textColorFunction = NA,
                           showTextLegend = F,
                           textLegendTitle = textColorColumn, textLegendDirection = "vertical",
                           fillColorColumn = NA, replaceFillNA = NA,
                           fillColorLevels = NA, fillColorLabels = NA,
                           fillColors = backGroundColor,
                           fillTransformation = NA, fillColorsMinMax = NA,
                           fillFunction = NA,
                           showFillLegend = T,
                           fillLegendTitle = fillColorColumn, fillLegendDirection = "horizontal",
                           legendPosition = "bottom", legendBox = "vertical",
                           naFillColor = backGroundColor,
                           textSize = 4, textFontFace = "bold",
                           linewidth = 0.35, alpha = 1,
                           tileWidth = 1, tileHeight = 1,
                           title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver()){
  if (!is.na(columns)){
    proteinData <- boxData(proteinData, ncols = columns, emptySequence = emptySequence, replaceNA = replaceNA)
  }
  if (!is.na(textColorColumn)){
    if (!identical(replaceTextNA, NA)){
      whichNA <- which(is.na(proteinData[, textColorColumn]))
      proteinData[whichNA, textColorColumn] <- replaceTextNA
    }
    if (identical(textColorLevels, NA)){
      textColorLevels <- sort(unique(proteinData[, textColorColumn]))
      if (identical(textColorLabels, NA)){
        textColorLabels <- textColorLevels
      }
    }
    if (length(textColorLevels) != length(textColorLabels)){
      stop("The length textColumnLevels must be identical to the length of the textColorLabels")
    }
    proteinData[, textColorColumn] <- factor(proteinData[, textColorColumn], levels = textColorLevels, labels = textColorLabels)
  }
  if (!is.na(fillColorColumn)){
    if (!identical(replaceFillNA, NA)){
      whichNA <- which(is.na(proteinData[, fillColorColumn]))
      proteinData[whichNA, fillColorColumn] <- replaceFillNA
    }
    if (!identical(fillColorLevels, NA)){    # this means scale_fill_manual needs to be used, otherwise scale_fill_gradientn
      if (identical(fillColorLabels, NA)){
        fillColorLabels <- fillColorLevels
      }
      if (length(fillColorLevels) != length(fillColorLabels)){
        stop("The length textColumnLevels must be identical to the length of the textColorLabels")
      }
      proteinData[, fillColorColumn] <- factor(proteinData[, fillColorColumn], levels = fillColorLevels, labels = fillColorLabels)
    }
  }
  g <- ggplot2::ggplot(data = proteinData, ggplot2::aes(y = col, x = row))
  g <- g + ifelseProper(!is.na(fillColorColumn),
                        ggplot2::geom_tile(show.legend = showFillLegend, linewidth = linewidth, alpha = alpha, width = tileWidth, height = tileHeight, ggplot2::aes(fill = !!ggplot2::sym(fillColorColumn)), color = borderColor),
                        ggplot2::geom_tile(show.legend = showFillLegend, linewidth = linewidth, alpha = alpha, width = tileWidth, height = tileHeight, fill = backGroundColor, color = borderColor))
  g <- g + ifelseProper(is.na(textColorColumn),
                        ggplot2::geom_text(ggplot2::aes(label = sequence), color = defaultTextColor, size = textSize, fontface = textFontFace, show.legend = showTextLegend),
                        ggplot2::geom_text(ggplot2::aes(label = sequence, color = !!ggplot2::sym(textColorColumn)), size = textSize, fontface = textFontFace, show.legend = showTextLegend))
  if (identical(textColorFunction, NA)){
    if (!identical(textColors, NA)){
      if (!identical(textColorLabels, NA)){
        if (length(textColors) != length(textColorLabels)){
          stop(paste0("Incorrect number of textColors defined. Required number is: ", length(textColorLabels)))
        }
        g <- g + ggplot2::scale_color_manual(values = textColors, labels = textColorLabels)
      } else {
        g <- g + ggplot2::scale_color_manual(values = textColors)
      }
    }
  } else {
    g <- g + textColorFunction
  }
  if (!identical(fillColorColumn, NA)){
    if (!identical(fillFunction, NA)){
      g <- g + fillFunction
    } else {
      if (identical(fillColorLevels, NA)){
        g <- g + ifelseProper(!identical(fillTransformation, NA),
                              ifelseProper(!identical(fillColorsMinMax, NA),
                                           ggplot2::scale_fill_gradientn(colors = fillColors, limits = fillColorsMinMax, na.value = naFillColor, trans = fillTransformation),
                                           ggplot2::scale_fill_gradientn(colors = fillColors, na.value = naFillColor, trans = fillTransformation)),
                              ifelseProper(!identical(fillColorsMinMax, NA),
                                           ggplot2::scale_fill_gradientn(colors = fillColors, limits = fillColorsMinMax, na.value = naFillColor),
                                           ggplot2::scale_fill_gradientn(colors = fillColors, na.value = naFillColor)))
      } else {
        g <- g + ggplot2::scale_fill_manual(values = fillColors, labels = fillColorLabels)
      }
    }
  }
  g <- g + ggplot2::coord_fixed() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),  # remove x axis labels
                   axis.ticks.x = ggplot2::element_blank(), # remove x axis ticks
                   axis.text.y = ggplot2::element_blank(),  # remove y axis labels
                   axis.ticks.y = ggplot2::element_blank(), # remove y axis ticks
                   axis.title.x = ggplot2::element_blank(), # remove x axis ticks
                   axis.title.y = ggplot2::element_blank(), # remove y axis title
                   panel.grid = ggplot2::element_blank(),   # remove grid
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_discrete(expand=c(0,0)) + # to remove margins
    ggplot2::scale_y_discrete(expand=c(0,0))
  if (showFillLegend | showTextLegend) {
    g <- g +  ggplot2::guides(fill = ifelseProper(showFillLegend,
                                                  ggplot2::guide_legend(title = fillLegendTitle, direction = fillLegendDirection, override.aes = list(label = "", color = frontColor)),
                                                  "none"),
                              color = ifelseProper(showTextLegend,
                                                   ggplot2::guide_legend(title = textLegendTitle, direction = textLegendDirection, override.aes = list(fill = backGroundColor, label = "X")),
                                                   "none")) + ggplot2::theme(legend.position = legendPosition, legend.box = legendBox)
  }
  g <- g + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
  return(g)
}

#' @title displayProteinText
#'
#' @description displays the protein data in a data frame as text (for console use) or HTML (for use with gt table package). Obviously more
#'  limited than the 'displayProtein' function
#'
#' @param proteinData the protein data data.frame
#' @param columns numeric vector that specifies the width (in characters) of the protein data display
#' @param emptySequence character vector that specifies which value is used for empty pieces of sequence at the beginning or the end of the protein data.frame
#'  please note that these rows, with this in the sequence column, at the beginning of the data.frame are removed for display. The first non-'emptySequence' row
#'  will be the first part displayed (see also examples). If this argument is not the same as the 'emptySequence' argument used in eg \code{\link{proteinCoverage}},
#'  an error will be generated
#' @param textColorColumn character vector: name of the column in the proteinData data.frame that contains info for the color of the text
#'  in the 'tiles'. Note: this should not be continuous data
#' @param textColors vector withthe text colors to use. Note: the number required is defined by the number of 'discrete' values in the textColor
#'  column of the protein data
#' @param textBackgroundColorColumn character vector: name of the column in the proteinData data.frame that contains info for the backcolor of the text
#'  in the 'tiles'. Note: this should not be continuous data
#' @param textBackgroundColors vector with the text background colors to use. Note: the number required is defined by the number of 'discrete' values in the
#'  textBackground color column of the protein data
#' @param HTMLoutput logical value. Default is FALSE, if TRUE then the output will be a character vector formatted for display in an HTML environment
#' @param linebreakChar character vector that is used to split the resulting character vector. For console text this is '\\n', for HTML output it's '< br >'.
#'  Can be adjusted to eg '< hr >' for HTML output
#' @param returnText default is FALSE. If no textColor and no textBackgroundColor columns are defined, then setting this to TRUE can be used to retrieve
#'  the result w/o printing it to the screen (it will contain line breaks)
#'
#' @note colors for display in the console are limited. Only the ones defined by the package 'crayon' should be used. For HTML use there's (obviously) more freedom
#'
#' @return character vector
#' @export
#'
#' @examples
#' newTable <- proteinCoverage(sequence = standardProtein("OVA"), start = 2, nterm = 1,
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", emptySequence = " ")
#' trypticSites <- getPeptideStart(proteinSequence = standardProtein("OVA"),
#'  peptideSequence = "(?<!P)R|(?<!P)K")
#' trypticSites <- data.frame(position = trypticSites, data = 1)
#' trypticSites
#' newTable <- addDataToProtein(proteinDF = newTable, dataframe = trypticSites, dataColumn = "data",
#'  dataName = "trypticSite", NAValue = 0)
#' newTable[46:55,]
#' # print to console
#' # displayProteinText(proteinData = newTable, columns = 75,
#' #  textColorColumn = "coverage", textColors = c("white","red"), emptySequence = " ")
#' # displayProteinText(proteinData = newTable, columns = 75, textColorColumn = "coverage",
#' #  textColors = c("white","red"), textBackgroundColorColumn = "trypticSite",
#' #  textBackgroundColors = c("black","yellow"), emptySequence = " ")
#' # HTML output via gt package
#' withTryptic <- displayProteinText(proteinData = newTable, columns = 75,
#'  textColorColumn = "coverage", textColors = c("black","red"),
#'  textBackgroundColorColumn = "trypticSite", textBackgroundColors = c("white","yellow"),
#'  emptySequence = " ", HTMLoutput = TRUE)
#' withoutTryptic <- displayProteinText(proteinData = newTable, columns = 75,
#'  textColorColumn = "coverage", textColors = c("black","red"), emptySequence = " ",
#'  HTMLoutput = TRUE)
#' different <- displayProteinText(proteinData = newTable, columns = 75,
#'  textColorColumn = "coverage", textColors = c("blue","green"), emptySequence = " ",
#'  HTMLoutput = TRUE, linebreakChar = "<hr>")
#' library(gt)
#' data.frame(Description = c("Coverage without tryptic sites", "Coverage with tryptic sites",
#'  "A different display..."), Sequence = c(withoutTryptic, withTryptic, different)) |>
#'   gt::gt() |>
#'   gt::fmt_markdown(columns = Sequence) |>
#'   gt::tab_style(
#'     style = list(
#'       gt::cell_text(font = "courier")
#'     ),
#'     locations = gt::cells_body(
#'       columns = "Sequence"
#'     )
#'   )
displayProteinText <- function(proteinData, columns = NA, emptySequence = "",
                               textColorColumn = NA, textColors = NA,
                               textBackgroundColorColumn = NA, textBackgroundColors = NA,
                               HTMLoutput = FALSE,  linebreakChar = ifelse(HTMLoutput, "<br>" ,"\n"),
                               returnText = FALSE){
  # remove leading empty sequence
  while (proteinData$sequence[1] == emptySequence){
    proteinData <- proteinData[-1,]
  }
  if (!is.na(columns)){
    proteinData <- boxData(proteinData, ncols = columns, emptySequence = emptySequence)
  }
  currentCol <- proteinData$col[1]
  currentAA <- proteinData$sequence[1]
  if (!identical(textColorColumn, NA)){
    proteinData$sequence <- purrr::map_chr(1:nrow(proteinData), ~ifelse(is.na(proteinData[.x, textColorColumn]) | (proteinData$sequence[.x] == emptySequence),
                                                                        proteinData$sequence[.x],
                                                                        ifelse(!HTMLoutput,
                                                                               paste(c("{", textColors[as.integer(proteinData[.x, textColorColumn])+1]," ",
                                                                                       proteinData$sequence[.x], "}"), collapse =""),
                                                                               paste(c("<span style = 'color:", textColors[as.integer(proteinData[.x, textColorColumn])+1],";'>",
                                                                                       proteinData$sequence[.x], "</span>"), collapse =""))))
  } else {
    proteinData$sequence <- ifelseProper(identical(textColors, NA),
                                         proteinData$sequence,
                                         purrr::map_chr(proteinData$sequence, ~ifelse(.x == emptySequence,
                                                                                      .x,
                                                                                      ifelse(!HTMLoutput,
                                                                                             paste(c("{", textColors," ", .x ,"}"), collapse =""),
                                                                                             paste(c("<span style = 'color:", textColors, ";'>", .x , "</span>"), collapse ="")))))
  }
  if (!identical(textBackgroundColorColumn, NA)){
    proteinData$sequence <- purrr::map_chr(1:nrow(proteinData), ~ifelse(is.na(proteinData[.x, textBackgroundColorColumn]) | (proteinData$sequence[.x] == emptySequence),
                                                                        proteinData$sequence[.x],
                                                                        ifelse(!HTMLoutput,
                                                                               paste(c("{bg", stringr::str_to_title(textBackgroundColors[as.integer(proteinData[.x, textBackgroundColorColumn])+1])," ",
                                                                                       proteinData$sequence[.x], "}"), collapse =""),
                                                                               paste(c("<span style = 'background-color:", textBackgroundColors[as.integer(proteinData[.x, textBackgroundColorColumn])+1],";'>",
                                                                                       proteinData$sequence[.x], "</span>"), collapse =""))))
  } else {
    proteinData$sequence <- ifelseProper(identical(textBackgroundColors, NA),
                                         proteinData$sequence,
                                         purrr::map_chr(proteinData$sequence, ~ifelse(.x == emptySequence,
                                                                                      .x,
                                                                                      ifelse(!HTMLoutput,
                                                                                             paste(c("{bg", str_to_title(textBackgroundColors)," ", .x ,"}"), collapse =""),
                                                                                             paste(c("<span style = 'background-color:", textBackgroundColors,";'>", .x ,"</span>"), collapse ="")))))
  }
  currentCol <- proteinData$col[1]
  currentAA <- proteinData$sequence[1]
  proteinString <- ifelse(currentAA == emptySequence,
                          "", currentAA)

  for (counter in 2:nrow(proteinData)){
    currentAA <- proteinData$sequence[counter]
    if (proteinData$col[counter] != currentCol){
      proteinString <- paste(c(proteinString, ifelse(currentAA == emptySequence,
                                                     " ", currentAA)),
                             collapse = linebreakChar)
      currentCol <- proteinData$col[counter]
    } else {
      proteinString <-paste(c(proteinString, ifelse(currentAA == emptySequence,
                                                    " ", currentAA)),
                            collapse = "")
    }
  }
  if (identical(textColors, NA) & identical(textBackgroundColors, NA)){
    if (returnText){
      return(proteinString)
    } else {
      cat(proteinString)
    }
  } else {
    if (!HTMLoutput){
      return(glue::glue_col(proteinString))
    } else {
      return(proteinString)
    }
  }
}

#' @title plotProteinVariable
#'
#' @description creates a line plot of protein data with the amino acid position on the x-axis
#'
#' @param proteinDF the protein data data.frame. Obligatory columns are 'position' and 'sequence' (in case this needs to be displayed)
#' @param column character vector: name(s) of the data column(s) that need to be displayed
#' @param positionColumn name of the position column in the protein data  which has the position info (default is 'position')
#' @param addSequence logical vector (default FALSE). If TRUE, then the info in the argument 'sequenceColumn' is displayed in the plot
#' @param sequenceColumn name of the column containing the sequence info, usually the amino acid sequence, tough it can be something else
#' @param sequenceLevel defines where to display the 'sequenceColumn' info. Default is NA (ignored): sequence is displayed at the height (y-axis) of the line plot itself.
#'  Possible other values: 'min' (display at minimum y-value), 'max' (display at maximum y-value) or a numeric value (display at exactly this y-value)
#' @param textColor color of the sequence information displayed
#' @param textSize size of the sequence information displayed
#' @param textAlpha alpha of the sequence information displayed
#' @param textNudgeX offset (x-axis direction) of the sequence information displayed
#' @param textNudgeY offset (y-axis direction) of the sequence information displayed
#' @param textAngle number of degrees to rotate the sequence information displayed, default is 0
#' @param textFontFace allows manipulation of the text font used. Default is 'plain'. Alternatives are "bold", "italic", & "bold.italic"
#' @param lineColor specifies color of the line
#' @param lineWidth specifies width of the line
#' @param lineType specifies linetype of the line
#' @param lineAlpha specifies alpha of the line
#' @param drawYzero logical vector. If TRUE then a horizontal line a y = 0 is drawn/  (default is FALSE)
#' @param zeroColor specifies color of the y=0 line
#' @param zeroWidth specifies width of the y=0 line
#' @param zeroType specifies linetype of the y=0 line
#' @param zeroAlpha specifies alpha of the y=0 line
#' @param yLimits range of the y-axis, normally yLimits = c(minimum, maximum) or 'auto' for automatic (default)
#' @param yLog if TRUE then automatic transformation of the y-axis to logarihmic scale (default is FALSE)
#' @param ylogSetValue in case yLog is numeric, then this value allows any y-value equal to zero or below zero to be set to this value.
#'  Default is 1. Can be set to FALSE: no change of any value
#' @param incrY increases the upper limit of the  y-axis by 100*incrY percent. Only works if yLimits is set to 'auto'
#' @param decrY decreases the lower limit of the  y-axis by 100*decrY percent. Only works if yLimits is set to 'auto'
#' @param xLimits range of the x-axis, normally xLimits = c(minimum, maximum) or 'auto' for automatic (default)
#' @param xLabel sets x-axis title
#' @param yLabel set y-axos title
#' @param xLabelFormat defines the numeric format to be used for the x-axis
#'  labels (see BBPersonalR::fromatDigits() & BBPersonalR::formatScientificDigits() for examples)
#' @param yLabelFormat defines the numeric format to be used for the y-axis
#'  labels (see BBPersonalR::fromatDigits() & BBPersonalR::formatScientificDigits() for examples)
#' @param title specifies the title
#' @param subtitle specifies the subtitle
#' @param caption specifies the caption
#' @param setTheme specifies the theme see also \code{\link{ggplot2}{theme}}. Default is \code{\link{theme_minimal_adapted}}.
#'  If set to NA then no theme us used.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' newTable <- proteinCoverage(sequence = standardProtein("OVA"), start = 2, nterm = 1,
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", emptySequence = " ")
#' trypticSites <- getPeptideStart(proteinSequence = standardProtein("OVA"),
#'  peptideSequence = "(?<!P)R|(?<!P)K")
#' trypticSites <- data.frame(position = trypticSites, data = 1)
#' trypticSites |> head()
#' newTable <- addDataToProtein(proteinDF = newTable, dataframe = trypticSites,
#'  dataColumn = "data", dataName = "trypticSite", NAValue = 0)
#' newTable <- mapPeptidesToProtein(proteinDF = newTable, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins",
#'  variable = "XCorrbySearchEngine_1", combineFunction = max)
#' newTable <- mapPeptidesToProtein(proteinDF = newTable, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_1")
#' newTable <- mapPeptidesToProtein(proteinDF = newTable, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_2")
#' newTable[41:50,]
#' plotProteinVariable(proteinDF = newTable, column = "XCorrbySearchEngine_1",
#'  yLabel = "Score")
#' plotProteinVariable(proteinDF = newTable, column = "XCorrbySearchEngine_1",
#'  yLabel = "Score", addSequence = TRUE)
#' plotProteinVariable(proteinDF = newTable, column = "XCorrbySearchEngine_1",
#'  yLabel = "Score", addSequence = TRUE, textColor = "blue", xLimits = c(51,100),
#'  yLimits = c(-0.1, 11.1))
#' plotProteinVariable(proteinDF = newTable, column = "XCorrbySearchEngine_1", yLabel = "Score",
#'  addSequence = TRUE,  textColor = "blue", sequenceLevel = "min", xLimits = c(51,100),
#'   yLimits = c(-0.1, 11.1))
#' plotProteinVariable(proteinDF = newTable, column = "XCorrbySearchEngine_1",
#'  yLabel = "Score", addSequence = TRUE,  textColor = "blue", sequenceLevel = 5,
#'  xLimits = c(51,100), yLimits = c(-0.1, 11.1))
#' plotProteinVariable(proteinDF = newTable, column = "Abundances_1", yLabel = "Abundance")
#' plotProteinVariable(proteinDF = newTable, column = "Abundances_1", yLabel = "Abundance",
#'  yLog = TRUE, ylogSetValue = FALSE, yLimits = c(1E3, 2E10), drawYzero = TRUE,
#'  zeroType = "dotted", zeroAlpha = 0.5)
#' plotProteinVariable(proteinDF = newTable, column = "Abundances_1",
#'  yLabel = "Abundance", yLog = TRUE, ylogSetValue = 1E4, yLimits = c(1E3, 2E10),
#'  drawYzero = TRUE, zeroType = "dotted", zeroAlpha = 0.5)
#' plotProteinVariable(proteinDF = newTable, column = c("Abundances_1", "Abundances_2"),
#'  lineColor = c("blue", "red"), yLabel = "Abundance", yLog = TRUE, ylogSetValue = 1E4,
#'  yLimits = c(1E3, 2E10), drawYzero = TRUE, zeroType = "dotted", zeroAlpha = 0.5)
plotProteinVariable <- function(proteinDF, column, positionColumn = "position",
                                addSequence = FALSE, sequenceColumn = "sequence", sequenceLevel = NA,
                                textColor = "black", textSize = 2, textAlpha = 1,
                                textNudgeX = 0, textNudgeY = 0, textAngle = 0,
                                textFontFace = "plain",
                                lineColor = "red", lineWidth = 0.25, lineType = "solid", lineAlpha = 1,
                                drawYzero = FALSE,
                                zeroColor = "blue", zeroWidth = 0.5, zeroType = "solid", zeroAlpha = 0.5,
                                yLimits = "auto", yLog = FALSE, ylogSetValue = 1, incrY = ifelse(yLog, 0.5, 0.025), decrY = 0,
                                xLimits = "auto",
                                xLabel = positionColumn, yLabel = column,
                                xLabelFormat = ggplot2::waiver(), yLabelFormat = ggplot2::waiver(),
                                title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver(),
                                setTheme = theme_minimal_adapted()){
  if (length(lineColor) != length(column)){
    lineColor <- rep(lineColor, length(column))
  }
  if (length(lineWidth) != length(column)){
    lineWidth <- rep(lineWidth, length(column))
  }
  if (length(lineType) != length(column)){
    lineType <- rep(lineType, length(column))
  }
  if (length(lineAlpha) != length(column)){
    lineAlpha <- rep(lineAlpha, length(column))
  }
  if (yLog){
    if (!identical(ylogSetValue, FALSE)){
      if (length(ylogSetValue) != length(column)){
        ylogSetValue <- rep(ylogSetValue, length(column))
      }
      for (counter in 1:length(column)){
        proteinDF[,column[counter]] <- purrr::map_dbl(proteinDF[,column[counter]], ~ifelse(.x <= 0, ylogSetValue[counter], .x))
      }
    }
  }
  yMin <- ifelse(yLog, ylogSetValue, min(proteinDF[,column], na.rm = T))
  yMax <- max(proteinDF[,column], na.rm = T)
  if (identical(yLimits, "auto")){
    yLimits <- c(yMin + ifelse(yLog, -decrY, -decrY*(yMax-yMin)) , yMax * (1+incrY))
  }
  xMin <- min(proteinDF[, positionColumn], na.rm = T)
  xMax <- max(proteinDF[, positionColumn], na.rm = T)
  if (identical(xLimits, "auto")){
    xLimits <- c(xMin, xMax)
  }
  result <- ggplot2::ggplot()
  for (counter in 1:length(column)){
    result <- result +
      ggplot2::geom_step(data = proteinDF, ggplot2::aes(x = !!ggplot2::sym(positionColumn), y = !!ggplot2::sym(column[counter])),
                         color = lineColor[counter],
                         linewidth = lineWidth[counter],
                         linetype = lineType[counter],
                         alpha = lineAlpha[counter])
  }
  if (drawYzero){
    result <- result + ggplot2::geom_hline(yintercept = ifelseProper(yLog, ylogSetValue, 0),
                                           color = zeroColor, linewidth = zeroWidth, linetype = zeroType, alpha = zeroAlpha)
  }
  if (addSequence){
    if (length(column) > 1){
      column <- column[1]
    }
    if (is.na(sequenceLevel)){
      result <- result +
        ggplot2::geom_text(data = proteinDF, ggplot2::aes(label = !!ggplot2::sym(sequenceColumn), x = !!ggplot2::sym(positionColumn), y = !!ggplot2::sym(column)),
                           color = textColor, size = textSize, angle = textAngle,
                           nudge_x = textNudgeX, nudge_y = textNudgeY,
                           alpha = textAlpha, fontface = textFontFace,)
    } else {
      result <- result +
        ggplot2::geom_text(data = proteinDF, ggplot2::aes(label = !!ggplot2::sym(sequenceColumn), x = !!ggplot2::sym(positionColumn), y = ifelse(sequenceLevel == "max",
                                                                                                                                           yMax,
                                                                                                                                           ifelse(sequenceLevel == "min",
                                                                                                                                                  yMin,
                                                                                                                                                  sequenceLevel))),
                           color = textColor, size = textSize, angle = textAngle,
                           nudge_x = textNudgeX, nudge_y = textNudgeY,
                           alpha = textAlpha, fontface = textFontFace)
    }
  }
  result <- list(result) %>% graphsAdjust(yLimits = yLimits, xLimits = xLimits, yLog = yLog,
                                          xLabel = xLabel, yLabel = yLabel,
                                          xLabelFormat = xLabelFormat, yLabelFormat = yLabelFormat,
                                          setTheme = setTheme)
  return(result[[1]] + ggplot2::labs(title = title, subtitle = subtitle, caption = caption))
}
