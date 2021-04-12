## ----knitr_init, echo = FALSE, results = "asis", cache = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(rmdformats)
library(kableExtra)

res        <- read.csv(file = "2_output/1_data/res.csv")
allModsRes <- read.csv(file = "2_output/1_data/allMods.csv")

## Global options
options(max.print = "75")
opts_chunk$set(echo    = TRUE,
               cache   = FALSE,
               prompt  = FALSE,
               tidy    = FALSE,
               comment = NA,
               message = FALSE,
               warning = FALSE)
opts_knit$set(width = 75)


## ----fig.show = "hold", out.width = "100%", fig.align = "center", echo = FALSE--------------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("3_misc/team.PNG")


## ----fig.show = "hold", out.width = "50%", fig.align = "center", echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("3_misc/groupLogo.PNG")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(scipen = 999)
library(lubridate)
library(dplyr)
library(readr)
library(plotly)
library(cluster)
library(factoextra)
library(FactoMineR)
library(missMDA)
library(caret)
library(openxlsx)
library(GGally)
library(naniar)
library(MLmetrics)
library(gbm)
library(rsample)
library(recipes)
library(h2o)
library(gridExtra)

day.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

crmDat   <- readr::read_csv("0_data/crm_model.csv",     col_types = cols(.default = "c"))
finDat   <- readr::read_csv("0_data/finance_model.csv", col_types = cols(.default = "c"))
salesDat <- readr::read_csv("0_data/sales_model.csv",   col_types = cols(.default = "c"))
twiter   <- readr::read_csv("0_data/twitter_model.csv", col_types = cols(.default = "c"))

# Upload the winning model and prediction threshold
h2o::h2o.init(nthreads = 8, enable_assertions = FALSE)
finMod    <- h2o::h2o.upload_model(path = list.files(path = "2_output/3_model/bin/", full.names = TRUE))
finThresh <- finMod@model$default_threshold


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
propFunc <- function(datIn, vars, totToReturn, asDF = TRUE) {
    outpList <- list()
    for (vr in vars) {
        varSelected <- datIn[[vr]]
        varSelected[grepl(x = varSelected, pattern = "NA", 
            ignore.case = TRUE)] <- NA
        outp <- as.data.frame(prop.table(table(varSelected, useNA = "always")))
        NAValue <- outp$Freq[is.na(outp$varSelected)]
        outp <- outp[!is.na(outp$varSelected), ]
        outp <- outp[order(-outp$Freq), ]
        FinalOutp <- data.frame(varSelected = "NA", Freq = NAValue)
        ender <- min((totToReturn - 1), nrow(outp))
        toDrop <- nrow(outp) - ender
        toDropData <- data.frame(varSelected = c("Rest", 
            "RestCount"), Freq = c(ifelse(nrow(outp) == 
            0, 0, toDrop/nrow(outp)), toDrop))
        if (nrow(outp) > 0) {
            FinalOutp <- rbind(FinalOutp, outp[1:ender, ], toDropData)
        }
        else {
            FinalOutp <- rbind(FinalOutp, toDropData)
        }
        FinalOutp$Freq <- round(FinalOutp$Freq, 4)
        tempList <- list(FinalOutp$Freq)
        names(tempList) <- vr
        names(tempList[[1]]) <- FinalOutp$varSelected
        outpList <- append(outpList, tempList)
    }
    if (asDF) {
        for (ii in 1:length(outpList)) {
            rowVals <- outpList[ii]
            rowValsNames <- names(rowVals[[1]])
            tempDf <- as.data.frame(rowVals)
            rownames(tempDf) <- rowValsNames
            tempDf <- t(tempDf)
            RestRestCount <- t(as.data.frame(tempDf[, c(c("Rest", 
                "RestCount"))]))
            rownames(RestRestCount) <- "1"
            otherDat <- tempDf[, !(colnames(tempDf) %in% c("Rest", 
                "RestCount"))]
            finDat <- t(data.frame(paste0(names(otherDat), " : ", 
                otherDat)))
            rownames(finDat) <- "1"
            colnames(finDat) <- paste0("Var_", seq(1:dim(finDat)[2]))
            finDat <- cbind(finDat, RestRestCount)
            finDat <- as.data.frame(finDat)
            if (ii == 1) {
                combinedFinDat <- finDat
            }
            else {
                combinedFinDat <- dplyr::bind_rows(combinedFinDat, 
                  finDat)
            }
        }
        rownames(combinedFinDat) <- names(outpList)
        outpList <- combinedFinDat
        outpList <- dplyr::select(outpList, -c(Rest, RestCount), 
            everything())
    }
    return(outpList)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
propFunc(datIn = mtcars, vars = names(mtcars), totToReturn = 3, asDF = TRUE) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotTarget <- function(datIn, var_inspect, var_expo = NULL, target, numvar = TRUE, numVarBreaks = NULL, autoBin = TRUE, varStrength = 2, expoThreshold = 0.02, predVar = NULL, countVar = NULL) {
  
  if (is.null(var_expo)) {
    datIn$exposureVar <- 1
    var_expo <- "exposureVar"
  }
  
  datIn[[target]] <- as.numeric(as.character(datIn[[target]]))
  
  if (numvar) {
    if (!is.null(numVarBreaks)) {
      datIn[[var_inspect]] <- cut(datIn[[var_inspect]], breaks = numVarBreaks) %>% as.character()
    } else {
      datIn[[var_inspect]] <- cut(datIn[[var_inspect]], breaks = 10) %>% as.character()
    }
  } 
  
  if (is.null(countVar)) {
    
    if (is.null(predVar)) {
      stat_summary <- datIn %>% 
      group_by(!!sym(var_inspect)) %>%
      summarise(expo       = sum(!!sym(var_expo), na.rm = TRUE),
                totTarget  = sum(!!sym(target), na.rm = T),
                prop       = sum(!!sym(target), na.rm = T) / expo)
    } else {
      stat_summary <- datIn %>% 
      group_by(!!sym(var_inspect)) %>%
      summarise(expo       = sum(!!sym(var_expo), na.rm = TRUE),
                totTarget  = sum(!!sym(target),   na.rm = T),
                predClaims = sum(!!sym(predVar),  na.rm = T),
                prop       = sum(!!sym(target),   na.rm = T) / expo,
                predprop   = sum(!!sym(predVar),  na.rm = T) / expo)
    }
    
  } else {
    
    if (is.null(predVar)) {
      stat_summary <- datIn %>% 
      group_by(!!sym(var_inspect)) %>%
      summarise(expo       = sum(!!sym(var_expo),       na.rm = TRUE),
                totTarget  = sum(!!sym(target), na.rm = T),
                prop       = sum(!!sym(target), na.rm = T) / sum(!!sym(countVar), na.rm = T))
    } else {
      stat_summary <- datIn %>% 
      group_by(!!sym(var_inspect)) %>%
      summarise(expo       = sum(!!sym(var_expo),       na.rm = TRUE),
                totTarget  = sum(!!sym(target), na.rm = T),
                predClaims = sum(!!sym(predVar), na.rm = T),
                prop       = sum(!!sym(target), na.rm = T) / sum(!!sym(countVar), na.rm = T),
                predprop   = sum(!!sym(predVar), na.rm = T) / sum(!!sym(countVar), na.rm = T))
    }
    
  }
  
  n <- sum(datIn[[var_expo]], na.rm = TRUE)
  
  meanprop <- sum(datIn[[target]], na.rm = TRUE) / n
  varprop  <- sum((datIn[[target]] - meanprop)^2, na.rm = TRUE) / (n - 1)

  stat_summary[[var_inspect]] <- as.character(stat_summary[[var_inspect]])
  
  if (numvar) {
    ordr <- data.frame(uniqueVars = stat_summary[[var_inspect]]) %>% 
      mutate(numversion = gsub(x = uniqueVars, pattern = "\\((.*),.*]", replacement = "\\1") %>% as.numeric()) %>% 
      arrange(numversion)
    
    stat_summary[[var_inspect]][is.na(stat_summary[[var_inspect]])] <- -abs(min(ordr$uniqueVars) %>% as.character() %>% as.numeric()*1000)
    stat_summary[[var_inspect]] <- factor(stat_summary[[var_inspect]], levels = ordr$uniqueVars)
  } else {
    stat_summary[[var_inspect]][is.na(stat_summary[[var_inspect]])] <- "NA"
    stat_summary[[var_inspect]] <- factor(stat_summary[[var_inspect]], levels = unique(stat_summary[[var_inspect]]) %>% sort())
  }
  
  if (autoBin) {
    stat_summary[[var_inspect]] <- as.character(stat_summary[[var_inspect]])
    stat_summary <- stat_summary %>% 
      mutate(expoProp = expo/sum(expo, na.rm = TRUE),
             other    = dplyr::if_else((prop >= meanprop + (varprop/varStrength) | prop <= meanprop - (varprop/varStrength)) &
                                       (expoProp >= expoThreshold) , !!sym(var_inspect), "other")) %>% 
      select(-expoProp)
    stat_summary[[var_inspect]] <- stat_summary$other
    stat_summary$other <- NULL
    stat_summary <- stat_summary %>% 
      group_by(!!sym(var_inspect)) %>%
      summarise(expo      = sum(expo,      na.rm = TRUE),
                totTarget = sum(totTarget, na.rm = T),
                prop      = sum(totTarget, na.rm = T) / expo)
  }
  
  if (is.null(predVar)) {
    p <- stat_summary %>%
      plotly::plot_ly() %>% 
      plotly::add_markers(x = ~get(var_inspect), y = ~prop, name = paste0("Observed ", target), marker = list(color = 'maroon')) %>% 
      plotly::add_lines(x = stat_summary[[var_inspect]], y = rep(meanprop, nrow(stat_summary)), name = paste0("Average ", target), line = list(color = 'maroon')) %>%
      plotly::add_bars(x = ~get(var_inspect), y = ~expo, alpha = 0.7, yaxis = "y2", name = "Expo", marker = list(color = 'wheat'), opacity = 0.7) %>% 
      plotly::layout(title = paste0("Observed ", target, " by ", var_inspect),
                     xaxis  = list(title    = var_inspect,
                                   zeroline = FALSE),
                     yaxis  = list(title    = target,
                                   zeroline = FALSE, 
                                   showgrid = F),
                     yaxis2 = list(zeroline   = FALSE,
                                   showgrid   = F,
                                   overlaying = "y",
                                   side       = "right",
                                   title      = "Exposure"))
  } else {
    p <- stat_summary %>%
      plotly::plot_ly() %>% 
      plotly::add_markers(x = ~get(var_inspect), y = ~prop, name = paste0("Observed ", target), marker = list(color = 'maroon')) %>% 
      plotly::add_markers(x = ~get(var_inspect), y = ~predprop, name = paste0("Predicted ", target), marker = list(color = 'blue')) %>% 
      plotly::add_lines(x = stat_summary[[var_inspect]], y = rep(meanprop, nrow(stat_summary)), name = paste0("Average ", target), line = list(color = 'maroon')) %>%
      plotly::add_bars(x = ~get(var_inspect), y = ~expo, alpha = 0.7, yaxis = "y2", name = "Expo", marker = list(color = 'wheat'), opacity = 0.7) %>% 
      plotly::layout(title = paste0("Observed and Predicted ", target, " by ", var_inspect),
                     xaxis  = list(title    = var_inspect,
                                   zeroline = FALSE),
                     yaxis  = list(title    = target,
                                   zeroline = FALSE, 
                                   showgrid = F),
                     yaxis2 = list(zeroline   = FALSE,
                                   showgrid   = F,
                                   overlaying = "y",
                                   side       = "right",
                                   title      = "Exposure"))
  }
  names(stat_summary)[names(stat_summary) == "totTarget"] <- paste0("tot_",  target)
  names(stat_summary)[names(stat_summary) == "prop"]      <- paste0("ave_", target)
  return(list(summary = stat_summary, plt = p, nonGroupedVars = stat_summary[[var_inspect]][stat_summary[[var_inspect]] != "other"]))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = mtcars, var_inspect = "cyl", target = "mpg", autoBin = FALSE, numvar = FALSE)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
smartGrouper <- function(datIn, varIn, replaceVar = TRUE, newNames = FALSE, groupThresh = 0.05) {
  
  levsMax <- datIn[[varIn]] %>% unique() %>% length()

  # Need to add imputation step here!
  
  set.seed(2020)
  res <- FactoMineR::MCA(X = datIn %>% select(!!sym(varIn)) %>% mutate_all(.funs = as.character))
  
  mcaDims <- res$ind$coord %>% dplyr::as_tibble() 
  
  silhouette_score <- function(datIn, k) {
    finRes <- c()
    for (ii in k) {
      set.seed(2021)
      km <- kmeans(mcaDims, centers = ii, nstart = 25)
      ss <- silhouette(km$cluster, dist(datIn))
      res <- mean(ss[, 3])
      finRes <- c(finRes, res)
    }
    return(finRes)
  }
  
  siScores <- silhouette_score(datIn = mcaDims, k = 2:levsMax)
  cc       <- which(siScores >= quantile(siScores, 0.95))[1]
  set.seed(2022)
  km       <- kmeans(mcaDims, centers = cc, nstart = 25)
  
  if (newNames) {
    finClusts <- openxlsx::int2col(km$cluster)
  } else {
    finClusts <- data.frame(oldName = datIn[[varIn]], cluster = km$cluster, id = 1:nrow(datIn))
    replacements <- finClusts %>% select(-id) %>% distinct() %>% group_by(cluster) %>% mutate(oldName = oldName %>% unique() %>% na.omit() %>% paste(collapse = "|")) %>% distinct(cluster, .keep_all = TRUE)
    names(replacements) <- c(varIn, "cluster")
    finClusts <- finClusts %>% select(-oldName) %>% left_join(replacements) %>% select(-cluster) %>% arrange(id) %>% select(-id)
    finClusts <- finClusts[[1]]
  }
  
  if (replaceVar) {
    datIn[[varIn]] <- finClusts
  } else {
    varIn <- paste0(varIn, "_", "bis")
    datIn[[varIn]] <- finClusts
  }
  
  levs  <- datIn[[varIn]] %>% table() 
  other <- names(levs)[levs < round(datIn %>% nrow() * groupThresh, 0)]
  datIn[[varIn]][datIn[[varIn]] %in% other] <- "other"
  
  return(datIn)
}


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
result <- smartGrouper(datIn = mtcars %>% mutate(disp = as.character(disp)), varIn = "disp")
result %>% kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%") 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(result)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getSeason <- function(dateIn) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(dateIn, format = "2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getSeason(c(as.Date("2020-12-30", format = "%Y-%m-%d"), as.Date("2021-04-12", format = "%Y-%m-%d")))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
range01 <- function(x) {
  res <- ((x-min(x))/(max(x)-min(x))) %>% round(4)
  return(res)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
range01(x = c(1, 5, 20, 16, 18, 5, 9))


## import pandas as pd

## df = pd.read_csv("0_data/twitter_model.csv", encoding = 'utf8')


## def strip_accents(text):

##     return ''.join(char for char in

##                    unicodedata.normalize('NFKD', text)

##                    if unicodedata.category(char) != 'Mn')

## 
## df["text"] = df["text"].apply(strip_accents)

## 
## import re

## import string

## 
## def clean_text(text):

##     text = re.sub('\n',             '',  text) # Remove new line

##     text = re.sub('@\w+ ',          ' ', text) # Removing @mentions

##     text = re.sub('#',              '',  text) # Removing '#' hash tag

##     text = re.sub('https?:\/\/\S+', '',  text) # Removing hyperlink

##     text = re.sub(' +',             ' ', text)

##     text = text.strip()

##     return text

## 
## df['clean_text'] = df['text'].apply(clean_text)

## df


## from nltk.sentiment import SentimentIntensityAnalyzer

## sia = SentimentIntensityAnalyzer()

## 
## def sentiment_calc(text):

##     sent = sia.polarity_scores(text)

##     return sent.get("compound")

## 
## df['sentiment'] = df['clean_text'].apply(sentiment_calc)

## 
## from textblob import TextBlob

## pol = lambda x: TextBlob(x).sentiment.polarity

## sub = lambda x: TextBlob(x).sentiment.subjectivity

## df['polarity']     = df['clean_text'].apply(pol)

## df['subjectivity'] = df['clean_text'].apply(sub)

## 
## df


## df.to_csv('2_output/1_data/sentTrain.csv', index = False)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(salesDat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
salesDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% count(ID_SALES) %>% pull(n) %>% unique()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(crmDat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
crmDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% 
  dplyr::left_join((crmDat %>% mutate(fromCRM = 1) %>% select(ID_CRM, fromCRM)), by = c("ID_SALES" = "ID_CRM")) %>% 
  dplyr::filter(fromCRM == 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat <- salesDat %>% 
  mutate(id_progCode = stringr::str_sub(ID_SALES, start = 1,  end = nchar(Program_Code)),
         id_travType = stringr::str_sub(ID_SALES, start = -nchar(Travel_Type)),
         id_actual   = stringr::str_replace(string = ID_SALES,  pattern = id_progCode, replacement = ""),
         id_actual   = stringr::str_replace(string = id_actual, pattern = paste0(id_travType, "$"), replacement = ""))

set.seed(2021)
salesDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% filter(id_progCode != Program_Code)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% filter(id_travType != Travel_Type) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% count(id_actual) %>% count(n)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
crmDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat <- crmDat %>% 
  mutate(id_povCode = stringr::str_sub(string = ID_CRM, start = 1, end = nchar(Poverty_Code)),
         id_incLev  = stringr::str_sub(ID_CRM, start = -nchar(Income_Level)),
         id_actual  = stringr::str_replace(string = ID_CRM,    pattern = id_povCode, replacement = ""),
         id_actual  = stringr::str_replace(string = id_actual, pattern = paste0(id_incLev, "$"), replacement = ""))

set.seed(2021)
crmDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% filter(Poverty_Code != id_povCode)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% filter(Income_Level != id_incLev)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% count(id_actual) %>% count(n)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- salesDat %>% 
  select(-c(id_progCode, id_travType)) %>% 
  left_join((crmDat %>% select(-c(id_povCode, id_incLev)) %>% mutate(fromCRM = 1)), by = "id_actual") 

sales_crm %>% dplyr::filter(is.na(fromCRM)) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- sales_crm %>% select(-c(ID_CRM, fromCRM))
rm(salesDat, crmDat, tst)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(finDat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
finDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% count(ID_FINANCE) %>% count(n)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Special_Pay = dplyr::if_else(is.na(Special_Pay), "NA", Special_Pay),
         id_specPay  = dplyr::if_else(Special_Pay == "0", "0", stringr::str_sub(ID_FINANCE, start = 1,  end = nchar(Special_Pay))),
         id_actual   = dplyr::if_else(Special_Pay == "0", 
                                      ID_FINANCE, 
                                      stringr::str_replace(string = ID_FINANCE, pattern = id_specPay, replacement = "")))
set.seed(2021)
finDat %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% filter(Special_Pay != id_specPay)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% count(id_actual) %>% count(n)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm <- sales_crm %>% 
  left_join((finDat %>% select(-id_specPay) %>% mutate(fromFIN = 1)), by = "id_actual") 

fin_sales_crm %>% dplyr::filter(is.na(fromFIN)) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm <- fin_sales_crm %>% select(-c(ID_FINANCE, fromFIN, id_actual))
rm(finDat, sales_crm)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(twiter)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
twiter %>% sample_n(., 10) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm %>% 
  left_join((twiter %>% distinct(ID_SALES) %>% mutate(fromTWT = 1)), by = "ID_SALES") %>% 
  filter(is.na(fromTWT)) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twiter %>% 
  filter(ID_SALES %in% fin_sales_crm$ID_SALES) %>% 
  nrow()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twiter %>% count(ID_SALES) %>% summary() 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twiter <- readr::read_csv("2_output/1_data/sentTrain.csv", locale = readr::locale(encoding = "latin1")) %>%
  mutate(tweetLength = nchar(text)) %>% 
  group_by(ID_SALES) %>% 
  summarise(pol_med   = polarity     %>% median(na.rm = TRUE),
            sent_med  = sentiment    %>% median(na.rm = TRUE),
            sub_med   = subjectivity %>% median(na.rm = TRUE),
            twl_med   = tweetLength  %>% median(na.rm = TRUE),
            numTweets = n())

finDat <- fin_sales_crm %>% 
  left_join(twiter)

rm(twiter, fin_sales_crm)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% glimpse()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
propFunc(datIn = finDat, vars = names(finDat), totToReturn = 10, asDF = TRUE) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Program_Code", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = smartGrouper(datIn = finDat, varIn = "Program_Code"), var_inspect = "Program_Code", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- smartGrouper(datIn = finDat, varIn = "Program_Code")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "From_Grade", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(From_Grade = as.numeric(From_Grade))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "To_Grade", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(To_Grade = as.numeric(To_Grade))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Group_State", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
stateGrouping <- tibble(
  Group_State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  Group_main_region = c("south", "west", "west", "south", "west", "west", "northeast", "south", "south", "south", "west", "west", "midwest", "midwest", "midwest", "midwest", "south", "south", "northeast", "south", "northeast", "midwest", "midwest", "south", "midwest", "west", "midwest", "west", "northeast", "northeast", "west", "northeast", "south", "midwest", "midwest", "south", "west", "northeast", "northeast", "south", "midwest", "south", "south", "west", "northeast", "south", "west", "south", "midwest", "west"),
  Group_north_south = c("south", "north", "south", "south", "south", "north", "north", "south", "south", "south", "south", "north", "north", "north", "north", "north", "south", "south", "north", "north", "north", "north", "north", "south", "north", "north", "north", "north", "north", "north", "south", "north", "south", "north", "north", "south", "north", "north", "north", "south", "north", "south", "south", "north", "north", "south", "north", "north", "north", "north"),
  Group_east_west = c("east", "west", "west", "west", "west", "west", "east", "east", "east", "east", "west", "west", "east", "east", "west", "west", "east", "west", "east", "east", "east", "east", "west", "east", "west", "west", "west", "west", "east", "east", "west", "east", "east", "west", "east", "west", "west", "east", "east", "east", "west", "east", "west", "west", "east", "east", "west", "east", "east", "west")
)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% left_join(stateGrouping) 
rm(stateGrouping)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Group_main_region", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Group_north_south", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Group_east_west", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% filter(is.na(Group_main_region)) %>% pull(Group_State) %>% unique()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Group_main_region = ifelse(is.na(Group_main_region), "other", Group_main_region),
         Group_north_south = ifelse(is.na(Group_north_south), "other", Group_north_south),
         Group_east_west   = ifelse(is.na(Group_east_west),   "other", Group_east_west))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Days", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Days = as.numeric(Days))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
((finDat$Return_Date %>% lubridate::mdy() - finDat$Departure_Date %>% lubridate::mdy()) %>% as.character() %>% as.numeric() - finDat$Days) %>% sum()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Days", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(min(finDat$Days), max(finDat$Days), 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Days", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Travel_Type", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Travel_Type = if_else(Travel_Type == "A", "air", "ground"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Departure_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Departure_Date = lubridate::month(lubridate::mdy(Departure_Date))), var_inspect = "Departure_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Departure_Date = getSeason(lubridate::mdy(Departure_Date))), var_inspect = "Departure_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Departure_Date = lubridate::wday(lubridate::mdy(Departure_Date))), var_inspect = "Departure_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(departureMonth  = month.abb[lubridate::month(lubridate::mdy(Departure_Date))],
         departureSeason = getSeason(lubridate::mdy(Departure_Date)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Return_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Return_Date = lubridate::month(lubridate::mdy(Return_Date))), var_inspect = "Return_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Return_Date = getSeason(lubridate::mdy(Return_Date))), var_inspect = "Return_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Return_Date = lubridate::wday(lubridate::mdy(Return_Date))), var_inspect = "Return_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(returnMonth  = month.abb[lubridate::month(lubridate::mdy(Return_Date))],
         returnSeason = getSeason(lubridate::mdy(Return_Date))) %>%
  select(-Return_Date)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Early_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Early_RPL = lubridate::month(lubridate::mdy(Early_RPL))), var_inspect = "Early_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Early_RPL = getSeason(lubridate::mdy(Early_RPL))), var_inspect = "Early_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Early_RPL = lubridate::wday(lubridate::mdy(Early_RPL))), var_inspect = "Early_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(earlyRPLFlag = dplyr::if_else(is.na(Early_RPL), "no_notification", "notified")) %>%
  select(-Early_RPL)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Latest_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Latest_RPL = lubridate::month(lubridate::mdy(Latest_RPL))), var_inspect = "Latest_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Latest_RPL = getSeason(lubridate::mdy(Latest_RPL))), var_inspect = "Latest_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Latest_RPL = lubridate::wday(lubridate::mdy(Latest_RPL))), var_inspect = "Latest_RPL", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(latestRPLFlag = dplyr::if_else(is.na(Latest_RPL), "no_notification", "notified")) %>%
  select(-Latest_RPL)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Cancelled_Pax", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Cancelled_Pax = as.numeric(Cancelled_Pax))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Cancelled_Pax", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(min(finDat$Cancelled_Pax), max(finDat$Cancelled_Pax), 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Cancelled_Pax", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Discount_Pax", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_Discount_Pax = as.numeric(Total_Discount_Pax))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Discount_Pax", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(min(finDat$Total_Discount_Pax), max(finDat$Total_Discount_Pax), 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Discount_Pax", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Initial_System_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Initial_System_Date = lubridate::month(lubridate::mdy(Initial_System_Date))), var_inspect = "Initial_System_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Initial_System_Date = getSeason(lubridate::mdy(Initial_System_Date))), var_inspect = "Initial_System_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Initial_System_Date = lubridate::wday(lubridate::mdy(Initial_System_Date))), var_inspect = "Initial_System_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(initDayOfWeek = day.abb[lubridate::month(lubridate::mdy(Initial_System_Date))],
         initSeason    = getSeason(lubridate::mdy(Initial_System_Date))) %>%
  select(-Initial_System_Date)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SPR_Product_Type", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(SPR_Product_Type = if_else(SPR_Product_Type %in% c("International", "IL History", "Costa Rica"), "other", SPR_Product_Type)), var_inspect = "SPR_Product_Type", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SPR_Product_Type = if_else(SPR_Product_Type %in% c("CA History", "East Coast", "Science"), SPR_Product_Type, "other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP = as.numeric(FPP))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(min(finDat$FPP), max(finDat$FPP), 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Pax", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_Pax = as.numeric(Total_Pax))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Pax", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(min(finDat$Total_Pax), max(finDat$Total_Pax), 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_Pax", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
((finDat$FPP + finDat$Total_Discount_Pax - finDat$Total_Pax)/finDat$Total_Pax) %>% summary()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(totPax_Diff = FPP + Total_Discount_Pax - Total_Pax), var_inspect = "totPax_Diff", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-1000, 1000, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(totPaxDiff = FPP + Total_Discount_Pax - Total_Pax)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DepartureMonth", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-DepartureMonth)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "GroupGradeTypeLow", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(GroupGradeTypeLow = ifelse(GroupGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeLow))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "GroupGradeTypeHigh", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(GroupGradeTypeHigh = ifelse(GroupGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeHigh))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "GroupGradeType", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(GroupGradeType = ifelse(GroupGradeType == "Undefined->Undefined", NA, GroupGradeType))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "MajorProgramCode", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat$Retained %>% table(useNA = "always") %>% prop.table()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
naiveF1 <- MLmetrics::Precision(y_pred = (finDat %>% mutate(pred = "yes") %>% pull(pred)),
                                y_true = (finDat %>% mutate(obs = if_else(Retained == "1", "yes", "no")) %>% pull(obs)),
                                positive = "yes")
(2*naiveF1*1)/(naiveF1+1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat$Retained %>% class()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Retained       = as.numeric(Retained),
                            Retained_class = if_else(Retained == 0, "no", "yes"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Poverty_Code", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Poverty_Code = ifelse(Poverty_Code %in% c("A", "B", "C", "D"), Poverty_Code, "other")), var_inspect = "Poverty_Code", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Poverty_Code = ifelse(Poverty_Code %in% c("A", "B", "C", "D"), Poverty_Code, "other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Region", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Region = ifelse(Region %in% c("Southern California", "Northern California", "Pacific Northwest", "Houston", "Dallas"), Region, "other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "CRM_Segment", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(CRM_Segment = CRM_Segment %>% as.numeric()), var_inspect = "CRM_Segment", target = "Retained",
                  numvar = T, numVarBreaks = seq(0, 20, 1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = smartGrouper(datIn = finDat, varIn = "CRM_Segment"), var_inspect = "CRM_Segment", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(CRM_Segment = ifelse(CRM_Segment %in% c("1", "2", "4", "5", "6", "7", "8", "9", "10", "11"), CRM_Segment, "other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "School_Type", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Parent_Meeting_Flag", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Parent_Meeting_Flag = if_else(is.na(Parent_Meeting_Flag), "1", Parent_Meeting_Flag))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "MDR_Low_Grade", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_Low_Grade = ifelse(MDR_Low_Grade == "K",  "0", MDR_Low_Grade),
         MDR_Low_Grade = ifelse(MDR_Low_Grade == "PK", "-1", MDR_Low_Grade) %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "MDR_High_Grade", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_High_Grade = ifelse(MDR_High_Grade == "K",  "0", MDR_High_Grade),
         MDR_High_Grade = ifelse(MDR_High_Grade == "PK", "-1", MDR_High_Grade) %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_School_Enrollment", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_School_Enrollment = as.numeric(Total_School_Enrollment))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_School_Enrollment", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 3000, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Total_School_Enrollment", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_School_enrollment", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_School_enrollment = gsub(FPP_to_School_enrollment, pattern = ",", replacement = ".") %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_School_enrollment", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 0.23, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_School_enrollment", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
((finDat$FPP/finDat$Total_School_Enrollment-finDat$FPP_to_School_enrollment)/finDat$FPP_to_School_enrollment) %>% summary()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(FPP_to_School_enrollment_Diff = FPP/Total_School_Enrollment - FPP_to_School_enrollment), var_inspect = "FPP_to_School_enrollment_Diff", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-5, 5, 0.0025)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_School_enrollment = if_else(is.na(FPP_to_School_enrollment) & Total_School_Enrollment > 0 & !is.na(Total_School_Enrollment), FPP/Total_School_Enrollment, FPP_to_School_enrollment))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Income_Level", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Income_Level = ifelse(Income_Level %in% c("P", "P1", "P3", "P4", "P5"), "P", Income_Level),
                            Income_Level = ifelse(Income_Level %in% c("0", "NA", NA, "Z"), "other", Income_Level))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Income_Level", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SPR_New_Existing", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SPR_New_Existing = if_else(is.na(SPR_New_Existing), "1", SPR_New_Existing))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "NumberOfMeetingswithParents", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(NumberOfMeetingswithParents = as.numeric(NumberOfMeetingswithParents))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "NumberOfMeetingswithParents", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-1, 5, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FirstMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(FirstMeeting = lubridate::month(lubridate::mdy(FirstMeeting))), var_inspect = "FirstMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(FirstMeeting = getSeason(lubridate::mdy(FirstMeeting))), var_inspect = "FirstMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(FirstMeeting = lubridate::wday(lubridate::mdy(FirstMeeting))), var_inspect = "FirstMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-FirstMeeting)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "LastMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(LastMeeting = lubridate::month(lubridate::mdy(LastMeeting))), var_inspect = "LastMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(LastMeeting = getSeason(lubridate::mdy(LastMeeting))), var_inspect = "LastMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(LastMeeting = lubridate::wday(lubridate::mdy(LastMeeting))), var_inspect = "LastMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(LastMeetingSeason  = getSeason(lubridate::mdy(LastMeeting)),
         LastMeetingWeekDay = lubridate::wday(lubridate::mdy(LastMeeting)) %>% as.character(),
         LastMeetingWeekDay = ifelse(is.na(LastMeetingWeekDay), "other", LastMeetingWeekDay)) %>%
  select(-LastMeeting)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoFirstMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(DifferenceTraveltoFirstMeeting = as.numeric(DifferenceTraveltoFirstMeeting))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoFirstMeeting", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-1000, 1000, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoFirstMeeting", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoLastMeeting", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(DifferenceTraveltoLastMeeting = as.numeric(DifferenceTraveltoLastMeeting))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoLastMeeting", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-1000, 1000, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "DifferenceTraveltoLastMeeting", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SchoolGradeTypeLow", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SchoolGradeTypeLow = ifelse(SchoolGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeLow))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SchoolGradeTypeHigh", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SchoolGradeTypeHigh = ifelse(SchoolGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeHigh))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SchoolGradeType", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SchoolGradeType = ifelse(SchoolGradeType == "Undefined->Undefined", NA, SchoolGradeType))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SchoolSizeIndicator", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Deposit_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Deposit_Date = lubridate::month(lubridate::mdy(Deposit_Date))), var_inspect = "Deposit_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Deposit_Date = getSeason(lubridate::mdy(Deposit_Date))), var_inspect = "Deposit_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Deposit_Date = lubridate::wday(lubridate::mdy(Deposit_Date))), var_inspect = "Deposit_Date", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(dD_to_travD = (lubridate::mdy(Departure_Date)-lubridate::mdy(Deposit_Date)) %>% as.character() %>% as.numeric()), var_inspect = "dD_to_travD", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(depositToDeparture = (lubridate::mdy(Departure_Date) - lubridate::mdy(Deposit_Date)) %>% as.character() %>% as.numeric()) %>%
  select(-c(Deposit_Date, Departure_Date))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Special_Pay", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(Special_Pay = ifelse(Special_Pay %in% c("CP", "FR", "SA"), Special_Pay, "other")), var_inspect = "Special_Pay", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Special_Pay = ifelse(Special_Pay %in% c("CP", "FR", "SA"), Special_Pay, "other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Tuition", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Tuition = as.numeric(Tuition))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Tuition", target = "Retained",
                  numvar = T, numVarBreaks = seq(-100, 10000, 1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Tuition", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(avePrice = as.numeric(SPR_Group_Revenue)/Total_Pax), var_inspect = "avePrice", target = "Retained",
                  numvar = T, 
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(avePrice = as.numeric(SPR_Group_Revenue)/Total_Pax)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Active", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Active = as.numeric(FRP_Active))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Active", target = "Retained",
                  numvar = T, numVarBreaks = seq(-100, 10000, 1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Active", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Cancelled", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Cancelled = as.numeric(FRP_Cancelled))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Cancelled", target = "Retained",
                  numvar = T, numVarBreaks = seq(-100, 10000, 1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Cancelled", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Take_up_percent_", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Take_up_percent_ = gsub(FRP_Take_up_percent_, pattern = ",", replacement = ".") %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Take_up_percent_", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 1.2, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FRP_Take_up_percent_", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "EZ_Pay_Take_Up_Rate", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(EZ_Pay_Take_Up_Rate = gsub(EZ_Pay_Take_Up_Rate, pattern = ",", replacement = ".") %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "EZ_Pay_Take_Up_Rate", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 1.2, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "EZ_Pay_Take_Up_Rate", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "School_Sponsor", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(School_Sponsor = if_else(is.na(School_Sponsor), "0", School_Sponsor))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SPR_Group_Revenue", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SPR_Group_Revenue = as.numeric(SPR_Group_Revenue))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SPR_Group_Revenue", target = "Retained",
                  numvar = T, numVarBreaks = seq(-100, 10000, 1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "SPR_Group_Revenue", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_PAX", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_PAX = gsub(FPP_to_PAX, pattern = ",", replacement = ".") %>% as.numeric())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_PAX", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 1, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "FPP_to_PAX", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
((finDat$FPP/finDat$Total_Pax - finDat$FPP_to_PAX)/finDat$FPP_to_PAX) %>% summary()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat %>% mutate(FPP_to_PAX = FPP/Total_Pax - FPP_to_PAX), var_inspect = "FPP_to_PAX", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-5, 5, 0.0025)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_PAX = if_else(is.na(FPP_to_PAX) & Total_Pax > 0 & !is.na(Total_Pax), FPP/Total_Pax, FPP_to_PAX))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Num_of_Non_FPP_PAX", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Num_of_Non_FPP_PAX = as.numeric(Num_of_Non_FPP_PAX))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Num_of_Non_FPP_PAX", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(0, 100, 1)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "Num_of_Non_FPP_PAX", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "pol_med", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(pol_med = as.numeric(pol_med),
                            pol_med = ifelse(is.na(pol_med), 0, pol_med))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "pol_med", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-100, 100, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "pol_med", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sent_med", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sent_med = as.numeric(sent_med),
                            sent_med = ifelse(is.na(sent_med), 0, sent_med))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sent_med", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-100, 100, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sent_med", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sub_med", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sub_med = as.numeric(sub_med),
                            sub_med = ifelse(is.na(sub_med), 0, sub_med))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sub_med", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-100, 100, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "sub_med", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "twl_med", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(twl_med = as.numeric(twl_med),
                            twl_med = ifelse(is.na(twl_med), 0, twl_med))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "twl_med", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-100, 100, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "twl_med", target = "Retained",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "numTweets", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(numTweets = as.numeric(numTweets),
                            numTweets = ifelse(is.na(numTweets), 0, numTweets))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat, var_inspect = "numTweets", target = "Retained",
                  numvar = T, numVarBreaks = c(seq(-100, 100, 0.01)),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PerformanceAnalytics::chart.Correlation(finDat %>% select(ends_with("_med")), histogram = TRUE)
PerformanceAnalytics::chart.Correlation(rbind(trainDF, testDF) %>% select(ends_with("_med")), histogram = TRUE)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PerformanceAnalytics::chart.Correlation(finDat %>% select(contains("PAX")), histogram = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% select(Total_Discount_Pax, Num_of_Non_FPP_PAX, Total_Pax) %>% summary()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(Total_Discount_Pax, Num_of_Non_FPP_PAX))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
highlyCorDescr <- cor(finDat %>% select_if(is.numeric), use = "complete.obs") %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  tidyr::gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1, abs(value) < 1, abs(value) > 0.8)

PerformanceAnalytics::chart.Correlation(finDat %>% select(c(highlyCorDescr$var1, highlyCorDescr$var2) %>% unique()), histogram = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(FPP, Tuition, To_Grade, Cancelled_Pax, FRP_Active))
rm(highlyCorDescr)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nzv <- caret::nearZeroVar(finDat, saveMetrics = TRUE) %>% filter(zeroVar == TRUE | nzv == TRUE) 
nzv


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(rownames(nzv)))
rm(nzv)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
naniar::vis_miss(finDat, sort_miss = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
naniar::vis_miss(finDat %>% select(names(finDat)[colSums(is.na(finDat))>0]), sort_miss = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate_if(.predicate = is.character, 
            .funs = function(x) { x[is.na(x)] <- "other"; return(x)})


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2020)
splitIndex <- createDataPartition(finDat$Retained_class, p = .75, list  = FALSE, times = 1)
trainDF <- finDat[splitIndex,  ]
testDF  <- finDat[-splitIndex, ]
rm(splitIndex, finDat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDF_targ <- trainDF %>% select(Retained)
trainDF      <- trainDF %>% select(-Retained)
trainDF_cat  <- trainDF %>% select_if(is.character)
trainDF_num  <- trainDF %>% select_if(is.numeric)

nb_pca      <- missMDA::estim_ncpPCA(trainDF_num, ncp.max = 5)
resMI_pca   <- missMDA::MIPCA(trainDF_num,        ncp = nb_pca$ncp)
trainDF_num <- resMI_pca$res.imputePCA %>% as_tibble()

trainDF <- bind_cols(trainDF_targ, trainDF_cat, trainDF_num)
trainDF <- trainDF %>% mutate(MDR_High_Grade_Class = if_else(MDR_High_Grade <= 8, "lower", "higher"))

rm(nb_pca, resMI_pca, trainDF_targ, trainDF_cat, trainDF_num)

testDF_targ <- testDF %>% select(Retained)
testDF      <- testDF %>% select(-Retained)
testDF_cat  <- testDF %>% select_if(is.character)
testDF_num  <- testDF %>% select_if(is.numeric)

nb_pca     <- missMDA::estim_ncpPCA(testDF_num, ncp.max = 5)
resMI_pca  <- missMDA::MIPCA(testDF_num,        ncp = nb_pca$ncp)
testDF_num <- resMI_pca$res.imputePCA %>% as_tibble()

testDF <- bind_cols(testDF_targ, testDF_cat, testDF_num)
testDF <- testDF %>% mutate(MDR_High_Grade_Class = if_else(MDR_High_Grade <= 8, "lower", "higher"))

rm(nb_pca, resMI_pca, testDF_targ, testDF_cat, testDF_num)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c(names(trainDF)[colSums(is.na(trainDF)) > 0], names(testDF)[colSums(is.na(testDF)) > 0])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make sure we have consistent categorical levels
formFeats  <- names(trainDF)[!(names(trainDF) %in% c("ID_SALES", "Retained", "Retained_class"))]
modFormula <- formula(paste0("Retained_class ~ ", paste0(formFeats, collapse = " + ")))

blueprint <- recipes::recipe(modFormula, data = trainDF) %>% recipes::step_scale(recipes::all_numeric())

trainDF_baked <- recipes::prep(blueprint, training = trainDF, retain = TRUE) %>% recipes::juice()
testDF_baked  <- recipes::prep(blueprint, training = trainDF)                %>% recipes::bake(new_data = testDF)

h2o::h2o.init(nthreads = 8, enable_assertions = FALSE)

# Create training & test sets for h2o
train_h2o <- trainDF_baked %>% h2o::as.h2o(destination_frame = "train_h2o")
test_h2o  <- testDF_baked  %>% h2o::as.h2o(destination_frame = "test_h2o")

# Get response and feature names
Y <- "Retained_class"
X <- formFeats


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get the features that exist in both datasets - this is for the SVM model
X_both <- c()
for (var in X) {
  if (is.character(trainDF[[var]])) {
    trLevs <- unique(trainDF[[var]])
    tsLevs <- unique(testDF[[var]])
    if (length(setdiff(trLevs, tsLevs)) == 0){
      X_both <- c(X_both, var)
    }
  } else {
    trRange <- trainDF[[var]] %>% range()
    tsRange <- testDF[[var]]  %>% range()
    
    if (trRange[1] == tsRange[1] & trRange[2] == tsRange[2]) {
      X_both <- c(X_both, var) 
    }
  }
}

formFeats_svm  <- names(trainDF[, c(Y, X_both)])[!(names(trainDF[, c(Y, X_both)]) %in% c("ID_SALES", "Retained", "Retained_class"))]
modFormula_svm <- formula(paste0("Retained_class ~ ", paste0(formFeats_svm, collapse = " + ")))

blueprint_svm <- recipes::recipe(modFormula_svm, data = trainDF[, c(Y, X_both)]) %>% recipes::step_scale(recipes::all_numeric())

train_h2o_svm <- recipes::prep(blueprint_svm, training = trainDF[, c(Y, X_both)], retain = TRUE) %>%
  recipes::juice() %>%
  h2o::as.h2o(destination_frame = "train_h2o_svm")
test_h2o_svm <- recipes::prep(blueprint_svm, training = trainDF) %>%
  recipes::bake(new_data = testDF[, c(Y, X_both)]) %>%
  h2o::as.h2o(destination_frame = "test_h2o_svm")

res <- tibble(model    = c("glm",   "glm",  "nb",    "nb",   "svm",   "svm",  "gbm",   "gbm",  "nn",    "nn",   "stacked", "stacked", "auto",    "auto"),
              set      = c("train", "test", "train", "test", "train", "test", "train", "test", "train", "test", "train",   "test",    "train",   "test"),
              auc      = c(NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,        NA,        NA,        NA),
              accuracy = c(NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,        NA,        NA,        NA),
              f1_score = c(NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,      NA,     NA,        NA,        NA,        NA))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# GLM hyperparameters
set.seed(2020)
gridSize <- 100
glmParams <- data.frame(
  alpha = runif(gridSize, 0, 1) %>% round(4)
) %>% distinct() %>% as.list()

# Train and validate a cartesian grid of GLMs
glmGrid <- h2o::h2o.grid(
    algorithm                         = "glm"
  , x                                 = X
  , y                                 = Y
  , grid_id                           = "glm_grid"
  , family                            = "binomial"   
  , lambda_search                     = TRUE
  , training_frame                    = train_h2o
  , nfolds                            = 2
  , seed                              = 1
  , hyper_params                      = glmParams
  , keep_cross_validation_predictions = TRUE
)

# Get the grid results, sorted by validation F1 Score
glmGridPerf <- h2o::h2o.getGrid(
    grid_id    = "glm_grid"
  , sort_by    = "f1"
  , decreasing = TRUE
)

# Grab the top GLM model, chosen by validation F1 Score
best_glm <- h2o::h2o.getModel(glmGridPerf@model_ids[[1]])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
best_glm_perf_test <- h2o::h2o.performance(
    model   = best_glm
  , newdata = test_h2o
)

best_glm_perf_train <- h2o::h2o.performance(
    model   = best_glm
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_glm <- best_glm@model$default_threshold
res$auc     [res$model == "glm" & res$set == "train"] <- h2o::h2o.auc(object = best_glm_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "glm" & res$set == "train"] <- h2o::h2o.F1(object = best_glm_perf_train, thresholds = f1_thresh_glm)[[1]] %>% round(4)
res$accuracy[res$model == "glm" & res$set == "train"] <- h2o::h2o.accuracy(object = best_glm_perf_train, thresholds = f1_thresh_glm)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "glm" & res$set == "test"] <- h2o::h2o.auc(object = best_glm_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "glm" & res$set == "test"] <- h2o::h2o.F1(object = best_glm_perf_test, thresholds = f1_thresh_glm)[[1]] %>% round(4)
res$accuracy[res$model == "glm" & res$set == "test"] <- h2o::h2o.accuracy(object = best_glm_perf_test, thresholds = f1_thresh_glm)[[1]] %>% round(4)
res %>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# NB hyperparameters
nbParams <- list(
    laplace = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 50, 100, 1000)
)

search_criteria_nb <- list(
    strategy         = "RandomDiscrete"
  , max_runtime_secs = 3600
  , max_models       = 100
  , seed             = 1
)

# Train and validate a cartesian grid of GLMs
nbGrid <- h2o::h2o.grid(
    algorithm                         = "naivebayes"
  , x                                 = X
  , y                                 = Y
  , grid_id                           = "nb_grid"
  , training_frame                    = train_h2o
  , nfolds                            = 2
  , seed                              = 1
  , hyper_params                      = nbParams
  , search_criteria                   = search_criteria_nb
  , keep_cross_validation_predictions = TRUE
)

# Get the grid results, sorted by validation F1 Score
nbGridPerf <- h2o::h2o.getGrid(
    grid_id    = "nb_grid"
  , sort_by    = "f1"
  , decreasing = TRUE
)

# Grab the top GLM model, chosen by validation F1 Score
best_nb <- h2o::h2o.getModel(nbGridPerf@model_ids[[1]])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
best_nb_perf_test <- h2o::h2o.performance(
    model   = best_nb
  , newdata = test_h2o
)

best_nb_perf_train <- h2o::h2o.performance(
    model   = best_nb
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_nb <- best_nb@model$default_threshold
res$auc     [res$model == "nb" & res$set == "train"] <- h2o::h2o.auc(object = best_nb_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "nb" & res$set == "train"] <- h2o::h2o.F1(object = best_nb_perf_train, thresholds = f1_thresh_nb)[[1]] %>% round(4)
res$accuracy[res$model == "nb" & res$set == "train"] <- h2o::h2o.accuracy(object = best_nb_perf_train, thresholds = f1_thresh_nb)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "nb" & res$set == "test"] <- h2o::h2o.auc(object = best_nb_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "nb" & res$set == "test"] <- h2o::h2o.F1(object = best_nb_perf_test, thresholds = f1_thresh_nb)[[1]] %>% round(4)
res$accuracy[res$model == "nb" & res$set == "test"] <- h2o::h2o.accuracy(object = best_nb_perf_test, thresholds = f1_thresh_nb)[[1]] %>% round(4)
res%>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm", "nb"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SVM hyperparameters
svmParams <- list(
    gamma       = c(0.001, 0.005, 0.01, 0.05)
  , hyper_param = c(0.001, 0.01, 0.1, 1, 10, 100)
) 

search_criteria_svm <- list(
    strategy         = "RandomDiscrete"
  , max_runtime_secs = 3600
  , max_models       = 100
  , seed             = 1
)

# Train and validate a cartesian grid of SVMs
svmGrid <- h2o::h2o.grid(
    algorithm                         = "psvm"
  , x                                 = X_both
  , y                                 = Y
  , grid_id                           = "svm_grid"
  , training_frame                    = train_h2o_svm
  , nfolds                            = 3
  , seed                              = 1
  , hyper_params                      = svmParams
  , search_criteria                   = search_criteria_svm
  , keep_cross_validation_predictions = TRUE
)

# Get the grid results, sorted by validation F1 Score
svmGridPerf <- h2o::h2o.getGrid(grid_id = "svm_grid")
svmF1Res <- c()
for (ii in 1:(svmGridPerf@model_ids %>% length())) {
  svmMod <- h2o::h2o.getModel(svmGridPerf@model_ids[[ii]])
  temp_perf <- h2o::h2o.performance(
      model   = svmMod
    , newdata = train_h2o_svm)
  f1_thresh_svm_temp <- h2o::h2o.F1(temp_perf) %>% filter(f1 == max(f1)) %>% pull(threshold)
  if (length(f1_thresh_svm_temp) == 0) {
    svmResF1 <- 0
  } else {
    svmResF1 <- h2o::h2o.F1(object = temp_perf, thresholds = f1_thresh_svm_temp)[[1]]
  }
  svmF1Res <- c(svmF1Res, svmResF1)
}

best_svm <- h2o::h2o.getModel(svmGridPerf@model_ids[[which(svmF1Res==max(svmF1Res))]])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
best_svm_perf_test <- h2o::h2o.performance(
    model   = best_svm
  , newdata = test_h2o_svm
)

best_svm_perf_train <- h2o::h2o.performance(
    model   = best_svm
  , newdata = train_h2o_svm
)

## Get the results for Train
f1_thresh_svm <- best_svm@model$default_threshold
res$auc     [res$model == "svm" & res$set == "train"] <- NA
res$f1_score[res$model == "svm" & res$set == "train"] <- h2o::h2o.F1(object = best_svm_perf_train, thresholds = f1_thresh_svm)[[1]] %>% round(4)
res$accuracy[res$model == "svm" & res$set == "train"] <- h2o::h2o.accuracy(object = best_svm_perf_train, thresholds = f1_thresh_svm)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "svm" & res$set == "test"] <- NA
res$f1_score[res$model == "svm" & res$set == "test"] <- h2o::h2o.F1(object = best_svm_perf_test, thresholds = f1_thresh_svm)[[1]] %>% round(4)
res$accuracy[res$model == "svm" & res$set == "test"] <- h2o::h2o.accuracy(object = best_svm_perf_test, thresholds = f1_thresh_svm)[[1]] %>% round(4)
res %>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm", "nb", "svm"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gbmParams <- list( 
    max_depth = seq(1, 10, 1) ## restrict the search to the range of max_depth established above
  , sample_rate = seq(0.2, 1, 0.01) ## Space of row sampling rates per tree
  , col_sample_rate = seq(0.2, 1, 0.01) ## Column sampling rates per split
  , col_sample_rate_per_tree = seq(0.2, 1, 0.01) ## column sampling rates per tree
  , col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01) ## How column sampling per split should change as a function of depth of the split
  , min_rows = c(50, 70, 100) ## search a large space of the number of min rows in a terminal node
  , nbins = 2^seq(4, 10, 1) ## search a large space of the number of bins for split-finding for continuous and integer columns
  , nbins_cats = 2^seq(4, 12, 1) ## search a large space of the number of bins for split-finding for categorical columns
  , min_split_improvement = c(0, 0.0001, 0.000001, 0.00000001)## search a few minimum required relative error improvement thresholds for a split to happen
  , histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin") ## Try types (QuantilesGlobal, RoundRobin good for numeric columns with outliers)
)

search_criteria_gbm <- list(
    strategy = "RandomDiscrete" ## Random grid search
  , max_runtime_secs = 3600 ## limit the runtime to 60 minutes
  , max_models = 100 ## build no more than 100 models
  , seed = 1 ## random number generator seed to make sampling of parameter combinations reproducible
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  , stopping_rounds = 5
  , stopping_tolerance = 0.01
)

gbmGrid <- h2o::h2o.grid(
    ## standard model parameters
    x = X
  , y = Y
  , training_frame = train_h2o

  , hyper_params = gbmParams  ## hyper parameters
  , search_criteria = search_criteria_gbm ## hyper-parameter search configuration (see above)
  , algorithm = "gbm" ## which algorithm to run
  , grid_id = "gbm_grid" ## identifier for the grid, to later retrieve it
  , nfolds = 2 ## number of CV folds
  , ntrees = 10000 ## more trees is better if the learning rate is small enough use "more than enough" trees - we have early stopping
  , learn_rate = 0.1 ## smaller learning rate is better since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  , learn_rate_annealing = 0.99 ## learning rate annealing: learning_rate shrinks by 1% after every tree
  , max_runtime_secs = 3600 ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  , stopping_rounds = 5
  , stopping_tolerance = 0.01

  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  , score_tree_interval = 10
  , seed = 1
  
  , keep_cross_validation_predictions = TRUE
)

# Get the grid results, sorted by validation F1 Score
gbmGridPerf <- h2o::h2o.getGrid(
    grid_id    = "gbm_grid"
  , sort_by    = "f1"
  , decreasing = TRUE
)

# Grab the top GBM model, chosen by validation F1 Score
best_gbm <- h2o::h2o.getModel(gbmGridPerf@model_ids[[1]])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
best_gbm_perf_test <- h2o::h2o.performance(
    model   = best_gbm
  , newdata = test_h2o
)

best_gbm_perf_train <- h2o::h2o.performance(
    model   = best_gbm
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_gbm <- best_gbm@model$default_threshold
res$auc     [res$model == "gbm" & res$set == "train"] <- h2o::h2o.auc(object = best_gbm_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "gbm" & res$set == "train"] <- h2o::h2o.F1(object = best_gbm_perf_train, thresholds = f1_thresh_gbm)[[1]] %>% round(4)
res$accuracy[res$model == "gbm" & res$set == "train"] <- h2o::h2o.accuracy(object = best_gbm_perf_train, thresholds = f1_thresh_gbm)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "gbm" & res$set == "test"] <- h2o::h2o.auc(object = best_gbm_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "gbm" & res$set == "test"] <- h2o::h2o.F1(object = best_gbm_perf_test, thresholds = f1_thresh_gbm)[[1]] %>% round(4)
res$accuracy[res$model == "gbm" & res$set == "test"] <- h2o::h2o.accuracy(object = best_gbm_perf_test, thresholds = f1_thresh_gbm)[[1]] %>% round(4)
res %>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm", "nb", "svm", "gbm"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# NN hyper parameters
nnParams <- list(
    activation          = c("Rectifier", "Tanh", "Maxout", "RectifierWithDropout", "TanhWithDropout", "MaxoutWithDropout")
  , l1                  = c(0, 0.00001, 0.0001, 0.001, 0.01)
  , l2                  = c(0, 0.00001, 0.0001, 0.001, 0.01)
  , hidden              = list(c(20, 20), c(32, 32, 32), c(64, 64, 64), c(128, 128, 128))
  , input_dropout_ratio = c(0, 0.05)
)

search_criteria_nn <-  list(
    strategy           = "RandomDiscrete"
  , max_runtime_secs   = 3600
  , max_models         = 100
  , seed               = 1
  , stopping_rounds    = 5
  , stopping_tolerance = 0.01
)

# Train and validate grid of NN
nnGrid <- h2o::h2o.grid(
    algorithm                         = "deeplearning"
  , x                                 = X
  , y                                 = Y
  
  , epochs                            = 100000
  
  , rate                              = 0.05
  , rate_annealing                    = 0.99
  , max_runtime_secs                  = 3600

  , stopping_tolerance                = 0.01
  , stopping_rounds                   = 5
  , max_w2                            = 10
  , grid_id                           = "nn_grid"
  , training_frame                    = train_h2o
  , nfolds                            = 2
  , seed                              = 1
  , hyper_params                      = nnParams
  , search_criteria                   = search_criteria_nn
  , keep_cross_validation_predictions = TRUE
)

# Get the grid results, sorted by validation F1 Score
nnGridPerf <- h2o::h2o.getGrid(
    grid_id    = "nn_grid"
  , sort_by    = "f1"
  , decreasing = TRUE
)

# Grab the top GLM model, chosen by validation F1 Score
best_nn <- h2o::h2o.getModel(nnGridPerf@model_ids[[1]])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
best_nn_perf_test <- h2o::h2o.performance(
    model   = best_nn
  , newdata = test_h2o
)

best_nn_perf_train <- h2o::h2o.performance(
    model   = best_nn
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_nn <- best_nn@model$default_threshold
res$auc     [res$model == "nn" & res$set == "train"] <- h2o::h2o.auc(object = best_nn_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "nn" & res$set == "train"] <- h2o::h2o.F1(object = best_nn_perf_train, thresholds = f1_thresh_nn)[[1]] %>% round(4)
res$accuracy[res$model == "nn" & res$set == "train"] <- h2o::h2o.accuracy(object = best_nn_perf_train, thresholds = f1_thresh_nn)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "nn" & res$set == "test"] <- h2o::h2o.auc(object = best_nn_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "nn" & res$set == "test"] <- h2o::h2o.F1(object = best_nn_perf_test, thresholds = f1_thresh_nn)[[1]] %>% round(4)
res$accuracy[res$model == "nn" & res$set == "test"] <- h2o::h2o.accuracy(object = best_nn_perf_test, thresholds = f1_thresh_nn)[[1]] %>% round(4)
res %>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm", "nb", "svm", "gbm", "nn"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Train a stacked tree ensemble
ensemble <- h2o::h2o.stackedEnsemble(
    x                     = X
  , y                     = Y
  , training_frame        = train_h2o
  , model_id              = "ensemble"
  , base_models           = append(append(glmGrid@model_ids[1:3], gbmGrid@model_ids[1:3]), nnGrid@model_ids[1:3])
  , metalearner_algorithm = "deeplearning" # drf glm gbm deeplearning
)

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
stack_perf_test <- h2o::h2o.performance(
    model   = ensemble
  , newdata = test_h2o
)

stack_perf_train <- h2o::h2o.performance(
    model   = ensemble
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_stack <- ensemble@model$default_threshold
res$auc     [res$model == "stacked" & res$set == "train"] <- h2o::h2o.auc(object = stack_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "stacked" & res$set == "train"] <- h2o::h2o.F1(object = stack_perf_train, thresholds = f1_thresh_stack)[[1]] %>% round(4)
res$accuracy[res$model == "stacked" & res$set == "train"] <- h2o::h2o.accuracy(object = stack_perf_train, thresholds = f1_thresh_stack)[[1]] %>% round(4)

## Get the results for Train
res$auc     [res$model == "stacked" & res$set == "test"] <- h2o::h2o.auc(object = stack_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "stacked" & res$set == "test"] <- h2o::h2o.F1(object = stack_perf_test, thresholds = f1_thresh_stack)[[1]] %>% round(4)
res$accuracy[res$model == "stacked" & res$set == "test"] <- h2o::h2o.accuracy(object = stack_perf_test, thresholds = f1_thresh_stack)[[1]] %>% round(4)
res %>% filter(!is.na(f1_score))


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% filter(model %in% c("glm", "nb", "svm", "gbm", "nn", "stacked"))


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aml <- h2o::h2o.automl(
    x                                     = X
  , y                                     = Y
  , training_frame                        = train_h2o
  , max_runtime_secs                      = 3600
  , seed                                  = 1
  , nfolds                                = 2
  , keep_cross_validation_predictions     = TRUE
  , keep_cross_validation_fold_assignment = TRUE
)

scores <- c()
for (md in aml@leaderboard %>% as_tibble() %>% pull(model_id)) {
  autoScore <- h2o::h2o.getModel(md)
  scores <- c(scores, autoScore@model$training_metrics@metrics$AUC)
}

mods <- aml@leaderboard %>% as_tibble() %>% pull(model_id)

best_auto <- h2o::h2o.getModel(mods[which(scores == max(scores[scores<0.999]))])

# Now let's evaluate the model performance on a test set so we get an honest estimate of top model performance
auto_perf_test <- h2o::h2o.performance(
    model   = best_auto
  , newdata = test_h2o
)

auto_perf_train <- h2o::h2o.performance(
    model   = best_auto
  , newdata = train_h2o
)

## Get the results for Train
f1_thresh_auto <- best_auto@model$default_threshold
res$auc     [res$model == "auto" & res$set == "train"] <- h2o::h2o.auc(object = auto_perf_train)[[1]] %>% round(4)
res$f1_score[res$model == "auto" & res$set == "train"] <- h2o::h2o.F1(object = auto_perf_train, thresholds = f1_thresh_auto)[[1]] %>% round(4)
res$accuracy[res$model == "auto" & res$set == "train"] <- h2o::h2o.accuracy(object = auto_perf_train, thresholds = f1_thresh_auto)[[1]] %>% round(4)

## Get the results for Train
res$auc[res$model == "auto" & res$set == "test"] <- h2o::h2o.auc(object = auto_perf_test)[[1]] %>% round(4)
res$f1_score[res$model == "auto" & res$set == "test"] <- h2o::h2o.F1(object = auto_perf_test, thresholds = f1_thresh_auto)[[1]] %>% round(4)
res$accuracy[res$model == "auto" & res$set == "test"] <- h2o::h2o.accuracy(object = auto_perf_test, thresholds = f1_thresh_auto)[[1]] %>% round(4)
# Save the final results
write.csv(res, file = "2_output/1_data/res.csv", row.names = FALSE)
# Display
res %>% filter(!is.na(f1_score)) %>% tail(6)


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res %>% tail(6)


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allMods <- h2o::h2o.list_models()

allModsRes <- tibble(modID = allMods) %>% mutate(trainF1 = 0, testF1 = 0)
for (ii in 1:length(allMods)) {
  mod    <- allMods[ii]
  modObj <- h2o::h2o.getModel(mod)
  
  all_perf_test  <- h2o::h2o.performance(model = modObj, newdata = test_h2o)
  all_perf_train <- h2o::h2o.performance(model = modObj, newdata = train_h2o)
  
  allModsRes$modID[ii]   <- mod
  f1_thresh_all          <- modObj@model$default_threshold
  allModsRes$trainF1[ii] <- h2o::h2o.F1(object = all_perf_train, thresholds = f1_thresh_all)[[1]] %>% round(4)
  allModsRes$testF1[ii]  <- h2o::h2o.F1(object = all_perf_test, thresholds = f1_thresh_all)[[1]] %>% round(4)

}

allModsRes <- allModsRes %>% mutate(diff = abs(trainF1 - testF1)) %>% arrange(-testF1) %>% filter(trainF1 < 1, diff < 0.05)
# Store the result
write.csv(allModsRes, file = "2_output/1_data/allMods.csv", row.names = FALSE)
allModsRes %>% filter(!grepl(x = modID, pattern = "_cv_"))
allModsRes %>% filter(!grepl(x = modID, pattern = "_cv_"), diff<0.025) %>% arrange(-testF1)

## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allModsRes %>% filter(!grepl(x = modID, pattern = "_cv_"))

allModsRes %>% filter(modID=="gbm_grid_model_51") %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")

## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finMod    <- h2o::h2o.getModel("gbm_grid_model_51")
finThresh <- finMod@model$default_threshold


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_path <- h2o::h2o.saveModel(object = finMod, path = "2_output/3_model/", force = TRUE)

# load the model
saved_model <- h2o::h2o.loadModel(model_path)

# download the model built above to your local machine
my_local_model <- h2o::h2o.download_model(model = finMod, path = "2_output/3_model/bin", export_cross_validation_predictions = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finMod@allparameters


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finMod@model$cross_validation_metrics_summary


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finMod@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finMod@model$training_metrics@metrics$max_criteria_and_metric_scores


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exm_train <- h2o::h2o.explain(finMod, train_h2o)
exm_test  <- h2o::h2o.explain(finMod, test_h2o)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exm_train$confusion_matrix


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exm_test$confusion_matrix


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(exm_train$varimp$plots[[1]] %>% ggplotly(), exm_test$varimp$plots[[1]] %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(exm_train$shap_summary$plots[[1]] %>% ggplotly(), exm_test$shap_summary$plots[[1]] %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(exm_train$pdp$plots$pol_med %>% ggplotly(), exm_test$pdp$plots$pol_med %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(exm_train$pdp$plots$avePrice %>% ggplotly(), exm_test$pdp$plots$avePrice %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(exm_train$pdp$plots$Group_State %>% ggplotly(), exm_test$pdp$plots$Group_State %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDF <- trainDF %>%
  mutate(pred       = h2o::h2o.predict(object = finMod, newdata = train_h2o) %>% as_tibble() %>% pull(yes),
         pred_class = ifelse(pred >= finThresh, "yes", "no"))

liftDatTrain <- data.frame(obs = as.numeric(trainDF$Retained), pred = trainDF$pred) %>%
  arrange(-pred) %>%
  mutate(perc    = obs/sum(obs),
         obsCum  = cumsum(obs),
         predCum = cumsum(pred),
         n       = 100 * (1:nrow(.))/nrow(.))

f1_Train  <- MLmetrics::F1_Score(y_pred = trainDF %>% pull(pred_class), 
                                 y_true = trainDF %>% pull(Retained_class), 
                                 positive = "yes")
acc_Train <- MLmetrics::Accuracy(y_pred = trainDF %>% pull(pred_class), 
                                 y_true = trainDF %>% pull(Retained_class))
auc_Train <- MLmetrics::AUC(y_pred = trainDF %>% pull(pred), 
                            y_true = trainDF %>% pull(Retained))

liftDatTrain %>% 
  plotly::plot_ly(x   = ~n, y = ~obsCum,  type = 'scatter', mode = 'lines', name = "Observed") %>%
  plotly::add_lines(x = ~n, y = ~predCum, type = 'scatter', mode = 'lines', name = "Predicted") %>% 
  plotly::layout(title = "Train Data - Lift Curve",
                 xaxis = list(title = "Percentiles"),
                 yaxis = list(title = "Observed/Predicted")) %>%
  plotly::add_annotations(x         = 20,
                          y         = 1500,
                          text      = paste0("F1 Score = ", f1_Train %>% round(digits = 4)),
                          showarrow = FALSE) %>%
  plotly::add_annotations(x         = 20,
                          y         = 1400,
                          text      = paste0("Accuracy = ", acc_Train %>% round(digits = 4)),
                          showarrow = FALSE) %>%
  plotly::add_annotations(x         = 20,
                          y         = 1300,
                          text      = paste0("AUC = ", auc_Train %>% round(digits = 4)),
                          showarrow = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pROC::roc(trainDF %>% pull(Retained), trainDF %>% pull(pred))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(pROC::roc(trainDF %>% pull(Retained), trainDF %>% pull(pred)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testDF <- testDF %>%
  mutate(pred       = h2o::h2o.predict(object = finMod, newdata = test_h2o) %>% as_tibble() %>% pull(yes),
         pred_class = ifelse(pred >= finThresh, "yes", "no"))

liftDatTest <- data.frame(obs = as.numeric(testDF$Retained), pred = testDF$pred) %>%
  arrange(-pred) %>%
  mutate(perc    = obs/sum(obs),
         obsCum  = cumsum(obs),
         predCum = cumsum(pred),
         n       = 100 * (1:nrow(.))/nrow(.))

f1_Test  <- MLmetrics::F1_Score(y_pred = testDF %>% pull(pred_class),
                                y_true = testDF %>% pull(Retained_class), 
                                 positive = "yes")
acc_Test <- MLmetrics::Accuracy(y_pred = testDF %>% pull(pred_class),
                                y_true = testDF %>% pull(Retained_class))
auc_Test <- MLmetrics::AUC(y_pred = testDF %>% pull(pred),
                           y_true = testDF %>% pull(Retained))

liftDatTest %>% 
  plotly::plot_ly(x   = ~n, y = ~obsCum,  type = 'scatter', mode = 'lines', name = "Observed") %>%
  plotly::add_lines(x = ~n, y = ~predCum, type = 'scatter', mode = 'lines', name = "Predicted") %>% 
  plotly::layout(title = "Test Data - Lift Curve",
                 xaxis = list(title = "Percentiles"),
                 yaxis = list(title = "Observed/Predicted")) %>%
  plotly::add_annotations(x         = 20,
                          y         = 650,
                          text      = paste0("F1 Score = ", f1_Test %>% round(digits = 4)),
                          showarrow = FALSE) %>%
  plotly::add_annotations(x         = 20,
                          y         = 600,
                          text      = paste0("Accuracy = ", acc_Test %>% round(digits = 4)),
                          showarrow = FALSE) %>%
  plotly::add_annotations(x         = 20,
                          y         = 550,
                          text      = paste0("AUC = ", auc_Test %>% round(digits = 4)),
                          showarrow = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pROC::roc(testDF %>% pull(Retained), testDF %>% pull(pred))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(pROC::roc(testDF %>% pull(Retained), testDF %>% pull(pred)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = testDF, var_inspect = "pol_med", target = "Retained", predVar = "pred",
                  numvar = TRUE, numVarBreaks = seq(-2,2,0.1),
                  autoBin = F)
plt$plt

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = trainDF, var_inspect = "pol_med", target = "Retained", predVar = "pred",
                  numvar = TRUE, numVarBreaks = seq(-2,2,0.1),
                  autoBin = F)
plt$plt

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = testDF, var_inspect = "Program_Code", target = "Retained", predVar = "pred",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = trainDF, var_inspect = "Program_Code", target = "Retained", predVar = "pred",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = testDF, var_inspect = "avePrice", target = "Retained", predVar = "pred",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = trainDF, var_inspect = "avePrice", target = "Retained", predVar = "pred",
                  numvar = T,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = testDF, var_inspect = "SchoolGradeTypeHigh", target = "Retained", predVar = "pred",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = trainDF, var_inspect = "SchoolGradeTypeHigh", target = "Retained", predVar = "pred",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lr <- h2o::h2o.explain_row(finMod, test_h2o, row_index = which(testDF$pred==min(testDF$pred)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testDF %>% filter(pred == min(pred)) %>% select(pred, Retained, pred_class)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lr$shap_explain_row$plots[[1]] %>% ggplotly()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(lr$ice$plots$pol_med %>% ggplotly(), lr$ice$plots$avePrice %>% ggplotly(), lr$ice$plots$Income_Level %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mr <- h2o::h2o.explain_row(finMod, test_h2o, row_index = which(testDF$pred > finThresh*0.99 & testDF$pred < finThresh*1.01)[1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testDF[which(testDF$pred > finThresh*0.99 & testDF$pred < finThresh*1.01)[1], ] %>% select(pred, Retained, pred_class)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finThresh


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mr$shap_explain_row$plots[[1]] %>% ggplotly()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(mr$ice$plots$pol_med %>% ggplotly(), mr$ice$plots$avePrice %>% ggplotly(), mr$ice$plots$Income_Level %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hr <- h2o::h2o.explain_row(finMod, test_h2o, row_index = which(testDF$pred==max(testDF$pred)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testDF %>% filter(pred == max(pred)) %>% select(pred, Retained, pred_class)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hr$shap_explain_row$plots[[1]] %>% ggplotly()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotly::subplot(hr$ice$plots$pol_med %>% ggplotly(), hr$ice$plots$Income_Level %>% ggplotly(), hr$ice$plots$avePrice %>% ggplotly(), nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF <- trainDF %>% bind_rows(testDF)
plt <- plotTarget(datIn = finDF, var_inspect = "pol_med", target = "Retained", predVar = "pred",
                  numvar = T, numVarBreaks = seq(-1, 1, 0.1),
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = rbind(trainDF, testDF) %>% filter(Income_Level%in%c("A","B","C","D","E","F","G","H")), var_inspect = "Program_Code", target = "SPR_Group_Revenue",
                  numvar = F, 
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF <- trainDF %>% bind_rows(testDF)

print(paste0("Predicted : ", finDF$pred %>% mean() %>% round(3), " | Observed : ", finDF$Retained %>% mean() %>% round(3)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF_baked <- recipes::prep(blueprint, training = trainDF) %>% recipes::bake(new_data = finDF %>% 
                                                                                mutate(
                                                                                    Program_Code      = if_else(Income_Level %in% c("A", "B", "C", "D", "E", "F", "G", "H"), "HS", Income_Level)
                                                                                  , SPR_Group_Revenue = if_else(Income_Level %in% c("A", "B", "C", "D", "E", "F", "G", "H"), finDF$SPR_Group_Revenue[finDF$Program_Code=="HS"] %>% median(), SPR_Group_Revenue)
                                                                                  , avePrice          = as.numeric(SPR_Group_Revenue)/Total_Pax
                                                                                  )
                                                                              )
fin_h2o <- finDF_baked %>% h2o::as.h2o()

pred <- h2o::h2o.predict(object = finMod, newdata = fin_h2o) %>% as_tibble() %>% pull(yes)

pred %>% mean()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(paste0("Proportion : ", sum(finDF$SPR_Group_Revenue*pred)/sum(finDF$SPR_Group_Revenue*finDF$pred) %>% round(3), " | Absolute : ", sum(finDF$SPR_Group_Revenue*pred)-sum(finDF$SPR_Group_Revenue*finDF$pred) %>% round(3)))


## ----fig.show = "hold", out.width = "75%", fig.align = "center", echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("3_misc/pricevsdemand-png.png")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF <- trainDF %>%
  bind_rows(testDF) %>% 
  mutate(profitMargin = SPR_Group_Revenue*0.05)

finDF_loop <- trainDF %>% bind_rows(testDF) 

optimDF <- tibble(
    SPR_Group_Revenue_1 = rep(NA, nrow(finDF))
  , pred_1              = rep(NA, nrow(finDF))
  , SPR_Group_Revenue_2 = rep(NA, nrow(finDF))
  , pred_2              = rep(NA, nrow(finDF))
  , SPR_Group_Revenue_3 = rep(NA, nrow(finDF))
  , pred_3              = rep(NA, nrow(finDF))
  , SPR_Group_Revenue_4 = rep(NA, nrow(finDF))
  , pred_4              = rep(NA, nrow(finDF))
  , SPR_Group_Revenue_5 = rep(NA, nrow(finDF))
  , pred_5              = rep(NA, nrow(finDF))
)

for (ii in 1:5) {
  discount <- 0.01*ii
  
  finDF_baked <- recipes::prep(blueprint, training = trainDF) %>% recipes::bake(new_data = finDF_loop %>% mutate(SPR_Group_Revenue = SPR_Group_Revenue*(1-discount),
                                                                                                                 avePrice          = as.numeric(SPR_Group_Revenue)/Total_Pax))
  fin_h2o <- finDF_baked %>% h2o::as.h2o()
  
  optimDF[[which(names(optimDF) == paste0("SPR_Group_Revenue_", ii))]] <- finDF_loop %>% mutate(SPR_Group_Revenue = SPR_Group_Revenue*(1-discount)) %>% pull(SPR_Group_Revenue)
  optimDF[[which(names(optimDF) == paste0("pred_", ii))]] <- h2o::h2o.predict(object = finMod, newdata = fin_h2o) %>% as_tibble() %>% pull(yes)
 
}

finDF <- finDF %>% bind_cols(optimDF)
finDF <- finDF %>% mutate(
    profit_1   = SPR_Group_Revenue_1*0.05
  , profit_2   = SPR_Group_Revenue_2*0.05
  , profit_3   = SPR_Group_Revenue_3*0.05
  , profit_4   = SPR_Group_Revenue_4*0.05
  , profit_5   = SPR_Group_Revenue_5*0.05
  , e_profit   = profitMargin*pred
  , e_profit_1 = profit_1*pred_1
  , e_profit_2 = profit_2*pred_2
  , e_profit_3 = profit_3*pred_3
  , e_profit_4 = profit_4*pred_4
  , e_profit_5 = profit_5*pred_5
  , maxProfit  = pmax(e_profit, e_profit_1, e_profit_2, e_profit_3, e_profit_4, e_profit_5)
  )

(finDF$maxProfit %>% sum())/(finDF$e_profit %>% sum())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF$maxProfit %>% sum()-finDF$e_profit %>% sum()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDF %>%
  mutate(diff = maxProfit - e_profit) %>% 
  filter(diff == max(diff)) %>% 
  select(pred, pred_1, profitMargin, profit_1, e_profit, e_profit_1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDF, var_inspect = "Income_Level", target = "Retained",
                  numvar = F,
                  autoBin = F)
plt$plt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt_a <- plotTarget(datIn = finDF, var_inspect = "Income_Level", target = "SPR_Group_Revenue",
                  numvar = F,
                  autoBin = F)
plt_b <- plotTarget(datIn = finDF, var_inspect = "Income_Level", target = "SPR_Group_Revenue", var_expo = "SPR_Group_Revenue",
                  numvar = F,
                  autoBin = F)
plotly::subplot(plt_a$plt, plt_b$plt, nrows = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDF, var_inspect = "Total_Pax", target = "Retained",
                  numvar = T, numVarBreaks = seq(-10,500,1),
                  autoBin = F)
plt$plt


## ----fig.show = "hold", out.width = "50%", fig.align = "center", echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("3_misc/groupLogo.PNG")

