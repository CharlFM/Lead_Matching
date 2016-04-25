# Set Seed
set.seed(1)

# Disable Scientific Notation
options(scipen = 999)

#########################################################################################################
# Load Libraries and Custom Functions #
#######################################

library(XLConnect)
library(readxl)
library(RMySQL)
library(jsonlite)

library(data.table)
library(dplyr)
library(tidyr)
library(Matrix)
library(stringr)
library(gtools)
library(reshape)

library(lubridate)

library(rpart)
library(rpart.plot)
library(party)
library(rattle)

library(ggplot2)

library(gbm)
library(dismo)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(glmnet) 
library(caret)

library(AUC)
library(pROC) 
library(ROCR)

library(mixtools)

########################################################################################################################################
########################################################################################################################################

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

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
  
  Cont <- as.character(toupper(Cont))
  
  numCont <- floor(as.numeric(Cont))
  numCont[is.na(numCont)] <- Cont[is.na(numCont)]
  
  Cont <- numCont
  
  Cont <- gsub(" ", "",   Cont)
  Cont <- gsub("/", "-",  Cont)
  Cont <- gsub("\\", "-", Cont, fixed = TRUE)
  Cont <- gsub(".", "-",  Cont, fixed = TRUE)
  
  #############################################################################################
  
  ContDf <- strsplit(Cont, "-")
  
  n <- max(sapply(ContDf, length))
  l <- lapply(ContDf, function(X) c(X, rep(NA, n - length(X))))
  
  ContDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
  colnames(ContDf) <- paste("Date", seq(1:ncol(ContDf)), sep = "")
  
  ContDf$Date1[is.na(ContDf$Date1)] <- as.Date(0, origin = "1899-12-30")
  
  Months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  for (i in 1:ncol(ContDf)) {
    OnNames <- which(ContDf[, i] %in% Months)
    ContDf[, i][OnNames] <- which(Months %in% ContDf[, i][OnNames])
  }
  
  ContDf$OUT <- rep(as.Date(0, origin = "1899-12-30"), length(Cont))
  
  if (ncol(ContDf) == 2) {
    
    #############################################################################################
    # Date is of the numeric format -> 42095 
    # NB - five digits, 
    #       anything more than won't be a valid date for the next 150 years ( the year 2173)
    #       anything less than 5 digits is before 1928
    #       therefore a five digit value is a safe number to indicate a numeric year format
    
    onepartonly <- rep(1, nrow(ContDf))

    if (sum(onepartonly) > 0) {
      onepartonlyfinal                   <- NA
      onepartonlyfinal[onepartonly == 1] <- ContDf$Date1[onepartonly == 1]
      
      onepartonlyfinal[nchar(onepartonlyfinal) != 5] <- ""
      
      onepartonlyfinal <-  as.Date(as.numeric(onepartonlyfinal), origin = "1899-12-30")
      
      ContDf$OUT[!is.na(onepartonlyfinal)] <- as.Date(onepartonlyfinal[!is.na(onepartonlyfinal)]) 
    }
    
  } else {
    #############################################################################################
    # Year is left -> yyyy-dd-mm or yyyy-mm-dd
    
    yyPrt                    <- as.numeric(ContDf$Date1)
    yyPrt[nchar(yyPrt) != 4] <- ""
    
    if (sum(yyPrt != "") > 0){
      mmPrt              <- as.numeric(ContDf$Date2)
      mmPrt[yyPrt == ""] <- NA
      ddPrt              <- as.numeric(substr(ContDf$Date3, 1, 2))
      ddPrt[yyPrt == ""] <- NA
      
      yyPrt[ddPrt == 0 | mmPrt == 0] <- 1899
      mmPrt[ddPrt == 0 | mmPrt == 0] <- 12
      ddPrt[ddPrt == 0 | mmPrt == 0] <- 30
      
      YrDat                <- paste(yyPrt, mmPrt, ddPrt, sep = "-")
      YrDat                <- gsub("NA", "", YrDat)
      YrDat[YrDat == "--"] <- NA
      
      mmPrt_ch                   <- ifelse(mmPrt > 12, 1, 0)
      mmPrt_ch[is.na(mmPrt_ch)]  <- FALSE
      
      YrDat[mmPrt_ch == 1] <- paste(yyPrt, ddPrt, mmPrt, sep = "-")[mmPrt_ch == 1]
      
      ContDf$OUT[!(is.na(YrDat))] <- as.Date(YrDat[!(is.na(YrDat))])
    }
    
    #############################################################################################
    # Year is right -> dd-mm-yyyy or mm-dd-yyyy
    
    yyPrt                    <- as.numeric(ContDf$Date3)
    yyPrt[nchar(yyPrt) != 4] <- ""
    
    if (sum(yyPrt != "") > 0){
      ddPrt              <- as.numeric(ContDf$Date1)
      ddPrt[yyPrt == ""] <- NA
      mmPrt              <- as.numeric(ContDf$Date2)
      mmPrt[yyPrt == ""] <- NA
      
      yyPrt[ddPrt == 0 | mmPrt == 0] <- 1899
      mmPrt[ddPrt == 0 | mmPrt == 0] <- 12
      ddPrt[ddPrt == 0 | mmPrt == 0] <- 30
      
      YrDat                <- paste(yyPrt, mmPrt, ddPrt, sep = "-")
      YrDat                <- gsub("NA", "", YrDat)
      YrDat[YrDat == "--"] <- NA
      
      mmPrt_ch                   <- ifelse(mmPrt > 12, 1, 0)
      mmPrt_ch[is.na(mmPrt_ch)]  <- FALSE
      
      YrDat[mmPrt_ch == 1] <- paste(yyPrt, ddPrt, mmPrt, sep = "-")[mmPrt_ch == 1]
      
      ContDf$OUT[!is.na(YrDat)] <- as.Date(YrDat[!is.na(YrDat)])
    }
    
    #############################################################################################
    # Date is of the numeric format -> 42095 
    # NB - five digits, 
    #       anything more than won't be a valid date for the next 150 years ( the year 2173)
    #       anything less than 5 digits is before 1928
    #       therefore a five digit value is a safe number to indicate a numeric year format
    
    onepartonly <- rowSums(is.na(ContDf))
    onepartonly[onepartonly != 0] <- onepartonly[onepartonly != 0] - 1 # Remove 1 for the added "OUT" column, now the entries with a 1 is numonly
    
    if (sum(onepartonly) > 0) {
      onepartonlyfinal                   <- NA
      onepartonlyfinal[onepartonly == 1] <- ContDf$Date1[onepartonly == 1]
      
      onepartonlyfinal[nchar(onepartonlyfinal) != 5] <- ""
      
      onepartonlyfinal <-  as.Date(as.numeric(onepartonlyfinal), origin = "1899-12-30")
      
      ContDf$OUT[!is.na(onepartonlyfinal)] <- as.Date(onepartonlyfinal[!is.na(onepartonlyfinal)]) 
    }
    
  }
  
  #############################################################################################
  # Date format could also be 20050109 or 01092005, but this could also include things like:
  # 20050109 -> 050109 or 50109
  # 01092005 -> 010905 or 10905
  # these will barely makse sense even if you try and interpret them manually

  ContDf$OUT[ContDf$OUT <= as.Date("1900/01/01")] <- NA

  return(ContDf$OUT)
  
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

######################################################################################################

CountAllNums <- function(charvec){
  countr <- nchar(charvec)
  onlychars <- gsub("[^0-9]", "", charvec)
  countr
}

######################################################################################################

sumSplitValues <- function(dat) {
  tmp <- strsplit(dat, "")
  unlist(lapply(lapply(tmp, as.numeric), sum))
}

######################################################################################################

UniDash <- function(Cont){

  Cont <- as.character(toupper(Cont))

  Cont <- gsub(" ", "",   Cont)
  Cont <- gsub("/", "-",  Cont)
  Cont <- gsub("\\", "-", Cont, fixed = TRUE)
  Cont <- gsub(".", "-",  Cont, fixed = TRUE)
  
  return(Cont)
  
}


######################################################################################################

# define some helper and reporting functions
# calulcate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 and x increasing
areaCalc <- function(x,y) {
  # append extra points to get rid of degenerate cases
  x <- c(0,x,1)
  y <- c(0,y,1)
  n <- length(x)
  sum(0.5*(y[-1]+y[-n])*(x[-1]-x[-n]))
}

gainCurve = function(truthcol, predcol, title) {
  truthcol <- as.numeric(truthcol)
  # data frame of pred and truth, sorted in order of the predictions
  d = data.frame(predcol=predcol,truthcol=truthcol)
  predord = order(d[['predcol']], decreasing=TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']], decreasing=TRUE)
  npop = dim(d)[1]
  
  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results = data.frame(pctpop= (1:npop)/npop,
                       model = cumsum(d[predord,'truthcol'])/sum(d[['truthcol']]),
                       wizard = cumsum(d[wizard, 'truthcol'])/sum(d[['truthcol']]))
  
  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(results$pctpop,results$wizard) - 0.5
  modelArea = areaCalc(results$pctpop,results$model) - 0.5
  giniScore = modelArea/idealArea # actually, normalized gini score
  
  # melt the frame into the tall form, for plotting
  results = melt(results, id.vars="pctpop", measure.vars=c("model", "wizard"),
                 variable.name="sort_criterion", value.name="pct_outcome")
  
  gplot = ggplot(data=results, aes(x=pctpop, y=pct_outcome, color=sort_criterion)) + 
    geom_point() + geom_line() + 
    geom_abline(color="gray") +
    ggtitle(paste("Gain curve,", title, '\n', 
                  'relative Gini score', format(giniScore,digits=2))) +
    xlab("% items in score order") + ylab("% total category") +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    scale_y_continuous(breaks=seq(0,1,0.1)) +
    scale_color_manual(values=c('model'='darkblue', 'wizard'='darkgreen'))
  gplot
}


plotROC <- function(title,outcol,predcol) {
  pred <- prediction(predcol,outcol)
  perf <- performance(pred,'tpr','fpr')
  auc <- as.numeric(performance(pred,'auc')@y.values)
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  plot=ggplot() +
    geom_ribbon(data=pf,aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                fill='blue',alpha=0.3) +
    geom_point(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(aes(x=c(0,1),y=c(0,1))) + coord_fixed() +
    ggtitle(paste(title,'\nAUC:',format(auc,digits=2)))
  list(pf=pf,plot=plot)
}


deviance <- function(truth,pred,epsilon=0) {
  pred = pmax(pred, epsilon)
  pred = pmin(pred, 1-epsilon)
  S = 0.0 # assumed log-likelihood of saturated model
  -2*(sum(ifelse(truth,log(pred),log(1-pred)))-S)
}


reportStats <- function(d,test,modelName,title,epsilon) {
  dSub <- d[d$isTest==test,,drop=FALSE]
  tab <- table(truth=dSub[,yColumn],pred=dSub[,modelName]>0.5)
  accuracy <- (tab[1,1] + tab[2,2])/sum(tab)
  note = ifelse(test,'test','train')
  print(paste('\t',note,'accuracy',modelName,format(accuracy,digits=2)))
  residual.deviance <- deviance(dSub[,yColumn],dSub[,modelName],epsilon)
  #print(paste('\tresidual.deviance',residual.deviance))
  null.deviance <- deviance(dSub[,yColumn],mean(dSub[,yColumn]),epsilon)
  #print(paste('\tnull.deviance',null.deviance))
  print(paste("\tmodel explained a",
              format((1-residual.deviance/null.deviance),digits=2),
              "fraction of the variation on",note))  
}

report <- function(d,modelName,title,epsilon=1.0e-2) {
  print("***********")
  print(paste("model",modelName,title))
  reportStats(d,FALSE,modelName,title,epsilon)
  reportStats(d,TRUE,modelName,title,epsilon)
  print(ggplot(data=d[d$isTest==TRUE,,drop=FALSE],
               aes_string(x=modelName,color=yColumn)) + 
          geom_density() + 
          ggtitle(paste(title,'test')))
  print(plotROC(paste(title,'train'),
                d[d$isTest==FALSE,yColumn],
                d[d$isTest==FALSE,modelName])$plot)
  print(plotROC(paste(title,'test'),
                d[d$isTest==TRUE,yColumn],
                d[d$isTest==TRUE,modelName])$plot)
  print(gainCurve(d[d$isTest==FALSE,yColumn],
                  d[d$isTest==FALSE,modelName],
                  paste(title,'train')))
  print(gainCurve(d[d$isTest==TRUE,yColumn],
                  d[d$isTest==TRUE,modelName],
                  paste(title,'test')))
  print("***********")
}

######################################################################################################

bimodalDistFunc <- function (n, cpct, mu1, mu2, sig1, sig2) { 
  
  y0 <- rnorm(n, mean = mu1, sd = sig1)
  y1 <- rnorm(n, mean = mu2, sd = sig2)
  
  flag <- rbinom(n, size = 1, prob = cpct) 
  
  y <- y0*(1 - flag) + y1*flag 
  
}

######################################################################################################









