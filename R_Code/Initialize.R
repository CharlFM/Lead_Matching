# Set Seed
set.seed(1)

# Disable Scientific Notation
options(scipen = 999)

#########################################################################################################
# Load Libraries and Custom Functions #
#######################################

library(XLConnect)
library(readxl)
library(ChainLadder)
library(ggplot2)
library(reshape)
library(gbm)
library(randomForest)
library(AUC)
library(xgboost)
library(RMySQL)
library(Ckmeans.1d.dp)
library(data.table)
library(dplyr)
library(Matrix)
library(pROC) 
library(glmnet) 
library(caret)
library(gtools)

########################################################################################################################################
########################################################################################################################################

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

########################################################################################################################################
########################################################################################################################################

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

########################################################################################################################################
########################################################################################################################################

DateConv <- function(Cont){
  
  #############################################################################################
  
  Cont <- as.character(Cont)
  Cont <- gsub(" ", "", Cont)
  Cont <- gsub("/", "-", Cont)
  Cont <- gsub("\\", "-", Cont, fixed = TRUE)
  Cont <- gsub(".", "-", Cont, fixed = TRUE)
  
  #############################################################################################
  
  countr <- length(Cont)
  
  ContDf <- data.frame(Content = character(countr), 
                       Six_Char = character(countr), 
                       Leftyear = character(countr), 
                       RightYear = character(countr), 
                       YearFromNum = character(countr),
                       Out = as.Date(countr, origin = "1899-12-30"),
                       stringsAsFactors=FALSE)
  ContDf$Content <- Cont 
  ContDf$Content[ContDf$Content == ""] <- NA
  ContDf$Out[is.na(ContDf$Content)] <- NA
  
  #############################################################################################
  
  ContDf$Six_Char[nchar(ContDf$Content) == 6] <- ContDf$Content[nchar(ContDf$Content) == 6]
  
  year <- strtrim(ContDf$Six_Char,2)
  cent <- ifelse(as.numeric(year) < as.numeric(substr(format(Sys.Date(), "%Y"),3,4)),as.numeric(substr(format(Sys.Date(), "%Y"),1,2)),as.numeric(substr(format(Sys.Date(), "%Y"),1,2))-1)
  cent[is.na(cent)] <- ""
  month <- substr(ContDf$Six_Char,3,4)
  day <- substr(ContDf$Six_Char,5,6)
  Result6 <- paste(cent, year, "-", month, "-", day, sep = "")
  Result6[Result6 == "--"] <- NA
  ContDf$Six_Char <- Result6
  ContDf$Content[!is.na(Result6)] <- NA
  ContDf$Out[!is.na(ContDf$Six_Char)] <- ContDf$Six_Char[!is.na(ContDf$Six_Char)]
  
  #############################################################################################
  
  ContDf$YearFromNum[!is.na(as.numeric(ContDf$Content))] <- ContDf$Content[!is.na(as.numeric(ContDf$Content))]
  ContDf$YearFromNum <- as.Date(as.numeric(ContDf$YearFromNum), origin = "1899-12-30")
  ContDf$Content[!is.na(ContDf$YearFromNum)] <- NA
  ContDf$Out[!is.na(ContDf$YearFromNum)] <- ContDf$YearFromNum[!is.na(ContDf$YearFromNum)]
  
  #############################################################################################
  
  ContDf$Leftyear[!is.na(as.numeric(substr(ContDf$Content,1,4)))] <- ContDf$Content[!is.na(as.numeric(substr(ContDf$Content,1,4)))]
  LYDat <- ContDf$Leftyear
  
  leftprt <- as.numeric(substr(LYDat, 6, 7))
  rightprt <- as.numeric(substr(LYDat, 9, 10))
  
  leftprt_ch <- ifelse(leftprt > 12, 1, 0)
  leftprt_ch[is.na(leftprt_ch)] <- FALSE
  
  LYDat[leftprt_ch == 1] <- paste(substr(LYDat, 1, 4), "-", 
                                  substr(LYDat, 9, 10), "-", 
                                  substr(LYDat, 6, 7), sep = "")[leftprt_ch == 1]
  
  LYDat[LYDat == ""] <- NA
  
  ContDf$Leftyear <- LYDat
  
  ContDf$Content[!is.na(ContDf$Leftyear)] <- NA
  ContDf$Out[!is.na(ContDf$Leftyear)] <- ContDf$Leftyear[!is.na(ContDf$Leftyear)]
  
  
  #############################################################################################
  
  ContDf$RightYear[!is.na(ContDf$Content)] <- ContDf$Content[!is.na(ContDf$Content)]
  RYDat <- ContDf$RightYear
  
  leftprt <- as.numeric(substr(RYDat, 1, 2))
  rightprt <- as.numeric(substr(RYDat, 4, 5))
  
  leftprt_ch <- ifelse(leftprt > 12, 2, 1)
  leftprt_ch[is.na(leftprt_ch)] <- 0
  
  RYDat[leftprt_ch == 2] <- paste(substr(RYDat, 7, 10), "-", 
                                  substr(RYDat, 4, 5), "-", 
                                  substr(RYDat, 1, 2), sep = "")[leftprt_ch == 2]
  
  RYDat[leftprt_ch == 1] <- paste(substr(RYDat, 7, 10), "-", 
                                  substr(RYDat, 1, 2), "-", 
                                  substr(RYDat, 4, 5), sep = "")[leftprt_ch == 1]
  RYDat[RYDat == ""] <- NA
  ContDf$RightYear <- RYDat
  
  ContDf$Content[!is.na(ContDf$RightYear)] <- NA
  ContDf$Out[!is.na(ContDf$RightYear)] <- ContDf$RightYear[!is.na(ContDf$RightYear)]
  
  return(ContDf$Out)
  
}

########################################################################################################################################
########################################################################################################################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

########################################################################################################################################
########################################################################################################################################

eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon == 13) # if month was December add a year
  mon[mon == 13] <- 1
  iso = ISOdate(1900 + year, mon, 1, hour = 0)
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}


########################################################################################################################################
########################################################################################################################################

firstDayMonth <- function(x)
{           
  
  x <- as.Date(as.character(x))
  
  monthYr <- format(x, format = "%Y-%m")
  day     <- 1
  
  first <- as.Date(paste(monthYr, day, sep = "-"))
  
  as.Date(first)
  
}

########################################################################################################################################
########################################################################################################################################

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

########################################################################################################################################
########################################################################################################################################

excelToCsv <- function(file_path, keep_sheets = NULL, ...) {
  
  if (dir.exists(paste(tempdir(), "\\RRRRtemp",sep=""))){
    unlink(paste(tempdir(), "\\RRRRtemp",sep=""), recursive = T)
  }
  
  dir.create(paste(tempdir(), "\\RRRRtemp",sep=""), showWarnings = T)
  
  temp_already <- list.files(paste(tempdir(), "\\RRRRtemp",sep=""))
  
  file_root <- gsub("([[:print:]]+(/|\\\\))[[:print:]]+", "\\1", file_path)
  
  file_name <- gsub("[[:print:]]+(/|\\\\)", "", file_path)
  file_ext <- gsub("[[:print:]]+(.xls.?)", "\\1", file_path)
  newName <- gsub("[[:print:]]+(/|\\\\)", "", gsub(file_ext, ".csv", file_path))
  
  converter_file <- file(paste0(paste(tempdir(), "\\RRRRtemp",sep=""),"\\", "converter.vbs"))
  
  writeLines(
    c('rem  XLS_To_CSV.vbs',
      'rem =============================================================',
      'rem  convert all NON-empty worksheets in an Excel file to csv',
      'rem  CSV file names will default to Sheet names',
      'rem  output folder defaults to the folder where the script resides or',
      'rem  if path is specified with the input file, that path is used',
      'rem  ',
      'rem  input parameter 1:  Excel path\\file in argument 1 ',
      'rem                     (if path is not specified, the current path is defaulted)',
      'rem  ',
      'rem ============================================================',
      '',
      'Dim strExcelFileName',
      'Dim strCSVFileName',
      '',
      'strExcelFileName = WScript.Arguments.Item(0)',
      '',
      'rem get path where script is running',
      'Set fso = CreateObject ("Scripting.FileSystemObject")',
      'strScript = Wscript.ScriptFullName',
      'strScriptPath = fso.GetAbsolutePathName(strScript & "\\..")',
      '',
      'rem If the Input file is NOT qualified with a path, default the current path',
      'LPosition = InStrRev(strExcelFileName, "\\") ',
      'if LPosition = 0 Then ',
      '    strExcelFileName = strScriptPath & "\\" & strExcelFileName',
      'strScriptPath = strScriptPath & "\\" ',
      'else ',
      'strScriptPath = Mid(strExcelFileName, 1, LPosition) ',
      'End If',
      'rem msgbox LPosition & " - " & strExcelFileName & " - " & strScriptPath',
      '',
      'Set objXL = CreateObject("Excel.Application")',
      'Set objWorkBook = objXL.Workbooks.Open(strExcelFileName)',
      'objXL.DisplayAlerts = False',
      '',
      'rem loop over worksheets',
      '  For Each sheet In objWorkBook.Sheets  ',
      '   sheet.UsedRange.Columns.NumberFormat = "0.0000000"  ',
      'if objXL.Application.WorksheetFunction.CountA(sheet.Cells) <> 0 Then ',
      'rem             sheet.Rows(1).delete',
      'sheet.SaveAs strScriptPath & sheet.Name & ".csv", 6',
      '   End If',
      '  Next',
      '',
      'rem clean up  ',
      'objWorkBook.Close ',
      'objXL.quit',
      'Set objXL = Nothing ',
      'Set objWorkBook = Nothing',
      'Set fso = Nothing',
      '',
      'rem end script'),
    con = converter_file)
  
  close(converter_file)
  
  file.copy(file_path, paste(tempdir(), "\\RRRRtemp",sep=""))
  
  orig_wd <- getwd()
  setwd(paste(tempdir(), "\\RRRRtemp",sep=""))
  
  file.rename(file_name, paste0("filetoconvert", file_ext))
  
  shell(paste("converter.vbs", 
              paste0("filetoconvert", file_ext)), intern = TRUE)
  
  setwd(orig_wd)
  
  if(is.null(keep_sheets)) {
    keep_sheets <- gsub("\\.csv", "", list.files(paste(tempdir(), "\\RRRRtemp",sep=""), pattern = "\\.csv"))
  }
  
  file_flags <- paste0(keep_sheets, ".csv")
  
  for(i in 1:length(file_flags)) {
    file.copy(
      paste0(paste(tempdir(), "\\RRRRtemp",sep=""), "/", file_flags[i]), 
      paste(file_root, "CSV\\", newName, sep = ""), 
      overwrite = TRUE)
  }
  
  suppressWarnings(file.remove(
    paste0(paste(tempdir(), "\\RRRRtemp",sep=""),
           "/",
           list.files(paste(tempdir(), "\\RRRRtemp",sep=""))[!(list.files(paste(tempdir(), "\\RRRRtemp",sep="")) %in% temp_already)])))
  
}

########################################################################################################################################
########################################################################################################################################

excelToCsv <- function(file_path, keep_sheets = NULL, ...) {
  
  if (dir.exists(paste(tempdir(), "\\RRRRtemp",sep=""))){
    unlink(paste(tempdir(), "\\RRRRtemp",sep=""), recursive = T)
  }
  
  dir.create(paste(tempdir(), "\\RRRRtemp",sep=""), showWarnings = T)
  
  temp_already <- list.files(paste(tempdir(), "\\RRRRtemp",sep=""))
  
  file_root <- gsub("([[:print:]]+(/|\\\\))[[:print:]]+", "\\1", file_path)
  
  file_name <- gsub("[[:print:]]+(/|\\\\)", "", file_path)
  file_ext <- gsub("[[:print:]]+(.xls.?)", "\\1", file_path)
  newName <- gsub("[[:print:]]+(/|\\\\)", "", gsub(file_ext, ".csv", file_path))
  
  converter_file <- file(paste0(paste(tempdir(), "\\RRRRtemp",sep=""),"\\", "converter.vbs"))
  
  writeLines(
    c('rem  XLS_To_CSV.vbs',
      'rem =============================================================',
      'rem  convert all NON-empty worksheets in an Excel file to csv',
      'rem  CSV file names will default to Sheet names',
      'rem  output folder defaults to the folder where the script resides or',
      'rem  if path is specified with the input file, that path is used',
      'rem  ',
      'rem  input parameter 1:  Excel path\\file in argument 1 ',
      'rem                     (if path is not specified, the current path is defaulted)',
      'rem  ',
      'rem ============================================================',
      '',
      'Dim strExcelFileName',
      'Dim strCSVFileName',
      '',
      'strExcelFileName = WScript.Arguments.Item(0)',
      '',
      'rem get path where script is running',
      'Set fso = CreateObject ("Scripting.FileSystemObject")',
      'strScript = Wscript.ScriptFullName',
      'strScriptPath = fso.GetAbsolutePathName(strScript & "\\..")',
      '',
      'rem If the Input file is NOT qualified with a path, default the current path',
      'LPosition = InStrRev(strExcelFileName, "\\") ',
      'if LPosition = 0 Then ',
      '    strExcelFileName = strScriptPath & "\\" & strExcelFileName',
      'strScriptPath = strScriptPath & "\\" ',
      'else ',
      'strScriptPath = Mid(strExcelFileName, 1, LPosition) ',
      'End If',
      'rem msgbox LPosition & " - " & strExcelFileName & " - " & strScriptPath',
      '',
      'Set objXL = CreateObject("Excel.Application")',
      'Set objWorkBook = objXL.Workbooks.Open(strExcelFileName)',
      'objXL.DisplayAlerts = False',
      '',
      'rem loop over worksheets',
      '  For Each sheet In objWorkBook.Sheets  ',
      '   sheet.UsedRange.Columns.NumberFormat = "0"  ',
      'if objXL.Application.WorksheetFunction.CountA(sheet.Cells) <> 0 Then ',
      'rem             sheet.Rows(1).delete',
      'sheet.SaveAs strScriptPath & sheet.Name & ".csv", 6',
      '   End If',
      '  Next',
      '',
      'rem clean up  ',
      'objWorkBook.Close ',
      'objXL.quit',
      'Set objXL = Nothing ',
      'Set objWorkBook = Nothing',
      'Set fso = Nothing',
      '',
      'rem end script'),
    con = converter_file)
  
  close(converter_file)
  
  file.copy(file_path, paste(tempdir(), "\\RRRRtemp",sep=""))
  
  orig_wd <- getwd()
  setwd(paste(tempdir(), "\\RRRRtemp",sep=""))
  
  file.rename(file_name, paste0("filetoconvert", file_ext))
  
  shell(paste("converter.vbs", 
              paste0("filetoconvert", file_ext)), intern = TRUE)
  
  setwd(orig_wd)
  
  if(is.null(keep_sheets)) {
    keep_sheets <- gsub("\\.csv", "", list.files(paste(tempdir(), "\\RRRRtemp",sep=""), pattern = "\\.csv"))
  }
  
  file_flags <- paste0(keep_sheets, ".csv")
  
  for(i in 1:length(file_flags)) {
    file.copy(
      paste0(paste(tempdir(), "\\RRRRtemp",sep=""), "/", file_flags[i]), 
      paste(file_root, "CSV\\", newName, sep = ""), 
      overwrite = TRUE)
  }
  
  suppressWarnings(file.remove(
    paste0(paste(tempdir(), "\\RRRRtemp",sep=""),
           "/",
           list.files(paste(tempdir(), "\\RRRRtemp",sep=""))[!(list.files(paste(tempdir(), "\\RRRRtemp",sep="")) %in% temp_already)])))
  
}

######################################################################################################

countLetter <- function(charvec, letter){
  sapply(charvec, function(x, letter){
    sum(unlist(strsplit(x, split = "")) == letter)
  }, letter = letter)
}

























