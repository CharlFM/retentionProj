## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

crmDat_test   <- readr::read_csv("0_data/test_data/crm_test.csv",     col_types = cols(.default = "c"))
finDat_test   <- readr::read_csv("0_data/test_data/finance_test.csv", col_types = cols(.default = "c"))
salesDat_test <- readr::read_csv("0_data/test_data/sales_test.csv",   col_types = cols(.default = "c"))
twiter_test   <- readr::read_csv("0_data/test_data/twitter_test.csv", col_types = cols(.default = "c"))

# Upload the winning model and prediction threshold
h2o::h2o.init(nthreads = 8, enable_assertions = FALSE)
finMod    <- h2o::h2o.upload_model(path = list.files(path = "2_output/3_model/bin/", full.names = TRUE))
finThresh <- finMod@model$default_threshold

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
range01 <- function(x) {
  res <- ((x-min(x))/(max(x)-min(x))) %>% round(4)
  return(res)
}

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat <- salesDat %>% 
  mutate(id_progCode = stringr::str_sub(ID_SALES, start = 1,  end = nchar(Program_Code)),
         id_travType = stringr::str_sub(ID_SALES, start = -nchar(Travel_Type)),
         id_actual   = stringr::str_replace(string = ID_SALES,  pattern = id_progCode, replacement = ""),
         id_actual   = stringr::str_replace(string = id_actual, pattern = paste0(id_travType, "$"), replacement = ""))

salesDat_test <- salesDat_test %>% 
  mutate(id_progCode = stringr::str_sub(ID_SALES, start = 1,  end = nchar(Program_Code)),
         id_travType = stringr::str_sub(ID_SALES, start = -nchar(Travel_Type)),
         id_actual   = stringr::str_replace(string = ID_SALES,  pattern = id_progCode, replacement = ""),
         id_actual   = stringr::str_replace(string = id_actual, pattern = paste0(id_travType, "$"), replacement = ""))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% filter(id_progCode != Program_Code)
salesDat_test %>% filter(id_progCode != Program_Code)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% filter(id_travType != Travel_Type)
salesDat_test %>% filter(id_travType != Travel_Type)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salesDat %>% count(id_actual) %>% count(n)
salesDat_test %>% count(id_actual) %>% count(n)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat <- crmDat %>% 
  mutate(id_povCode = stringr::str_sub(string = ID_CRM, start = 1, end = nchar(Poverty_Code)),
         id_incLev  = stringr::str_sub(ID_CRM, start = -nchar(Income_Level)),
         id_actual  = stringr::str_replace(string = ID_CRM,    pattern = id_povCode, replacement = ""),
         id_actual  = stringr::str_replace(string = id_actual, pattern = paste0(id_incLev, "$"), replacement = ""))
crmDat_test <- crmDat_test %>% 
  mutate(id_povCode = stringr::str_sub(string = ID_CRM, start = 1, end = nchar(Poverty_Code)),
         id_incLev  = stringr::str_sub(ID_CRM, start = -nchar(Income_Level)),
         id_actual  = stringr::str_replace(string = ID_CRM,    pattern = id_povCode, replacement = ""),
         id_actual  = stringr::str_replace(string = id_actual, pattern = paste0(id_incLev, "$"), replacement = ""))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% filter(Poverty_Code != id_povCode)
crmDat_test %>% filter(Poverty_Code != id_povCode)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% filter(Income_Level != id_incLev)
crmDat_test %>% filter(Income_Level != id_incLev)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crmDat %>% count(id_actual) %>% count(n)
crmDat_test %>% count(id_actual) %>% count(n)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- salesDat %>% 
  select(-c(id_progCode, id_travType)) %>% 
  left_join((crmDat %>% select(-c(id_povCode, id_incLev)) %>% mutate(fromCRM = 1)), by = "id_actual") 
sales_crm_test <- salesDat_test %>% 
  select(-c(id_progCode, id_travType)) %>% 
  left_join((crmDat_test %>% select(-c(id_povCode, id_incLev)) %>% mutate(fromCRM = 1)), by = "id_actual") 

sales_crm %>% dplyr::filter(is.na(fromCRM)) 
sales_crm_test %>% dplyr::filter(is.na(fromCRM)) 

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- sales_crm %>% select(-c(ID_CRM, fromCRM))
sales_crm_test <- sales_crm_test %>% select(-c(ID_CRM, fromCRM))
rm(salesDat, crmDat)
rm(salesDat_test, crmDat_test)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% count(ID_FINANCE) %>% count(n)
finDat_test %>% count(ID_FINANCE) %>% count(n)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Special_Pay = dplyr::if_else(is.na(Special_Pay), "NA", Special_Pay),
         id_specPay  = dplyr::if_else(Special_Pay == "0", "0", stringr::str_sub(ID_FINANCE, start = 1,  end = nchar(Special_Pay))),
         id_actual   = dplyr::if_else(Special_Pay == "0", 
                                      ID_FINANCE, 
                                      stringr::str_replace(string = ID_FINANCE, pattern = id_specPay, replacement = "")))

finDat_test <- finDat_test %>% 
  mutate(Special_Pay = dplyr::if_else(is.na(Special_Pay), "NA", Special_Pay),
         id_specPay  = dplyr::if_else(Special_Pay == "0", "0", stringr::str_sub(ID_FINANCE, start = 1,  end = nchar(Special_Pay))),
         id_actual   = dplyr::if_else(Special_Pay == "0", 
                                      ID_FINANCE, 
                                      stringr::str_replace(string = ID_FINANCE, pattern = id_specPay, replacement = "")))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% filter(Special_Pay != id_specPay)
finDat_test %>% filter(Special_Pay != id_specPay)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% count(id_actual) %>% count(n)
finDat_test %>% count(id_actual) %>% count(n)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm <- sales_crm %>% 
  left_join((finDat %>% select(-id_specPay) %>% mutate(fromFIN = 1)), by = "id_actual") 
fin_sales_crm_test <- sales_crm_test %>% 
  left_join((finDat_test %>% select(-id_specPay) %>% mutate(fromFIN = 1)), by = "id_actual") 

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm      <- fin_sales_crm      %>% select(-c(ID_FINANCE, fromFIN, id_actual))
fin_sales_crm_test <- fin_sales_crm_test %>% select(-c(ID_FINANCE, fromFIN, id_actual))
rm(finDat, sales_crm)
rm(finDat_test, sales_crm_test)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm %>% 
  left_join((twiter %>% distinct(ID_SALES) %>% mutate(fromTWT = 1)), by = "ID_SALES") %>% 
  filter(is.na(fromTWT))

fin_sales_crm_test %>% 
  left_join((twiter_test %>% distinct(ID_SALES) %>% mutate(fromTWT = 1)), by = "ID_SALES") %>% 
  filter(is.na(fromTWT))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twiter %>% count(ID_SALES) %>% summary() 
twiter_test %>% count(ID_SALES) %>% summary() 

twiter <- readr::read_csv("2_output/1_data/sentTrain.csv", locale = readr::locale(encoding = "latin1")) %>%
  mutate(tweetLength = nchar(text)) %>% 
  group_by(ID_SALES) %>% 
  summarise(pol_med   = polarity     %>% median(na.rm = TRUE),
            sent_med  = sentiment    %>% median(na.rm = TRUE),
            sub_med   = subjectivity %>% median(na.rm = TRUE),
            twl_med   = tweetLength  %>% median(na.rm = TRUE),
            numTweets = n())
twiter_test <- readr::read_csv("2_output/1_data/sentTest.csv", locale = readr::locale(encoding = "latin1")) %>%
  mutate(tweetLength = nchar(text)) %>% 
  group_by(ID_SALES) %>% 
  summarise(pol_med   = polarity     %>% median(na.rm = TRUE),
            sent_med  = sentiment    %>% median(na.rm = TRUE),
            sub_med   = subjectivity %>% median(na.rm = TRUE),
            twl_med   = tweetLength  %>% median(na.rm = TRUE),
            numTweets = n())

finDat <- fin_sales_crm %>% 
  left_join(twiter)
finDat_test <- fin_sales_crm_test %>% 
  left_join(twiter_test)

rm(twiter, fin_sales_crm)
rm(fin_sales_crm_test)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- smartGrouper(datIn = finDat, varIn = "Program_Code")
finDat_test <- finDat_test %>% mutate(
  Program_Code = if_else(Program_Code %in% c("CD", "HN"), "CD|HN", Program_Code)
  , Program_Code = if_else(Program_Code %in% c("CN", "HD"), "CN|HD", Program_Code)
  , Program_Code = if_else(Program_Code %in% c("CD|HN", "CN|HD", "HC", "HS"), Program_Code, "other")
)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(From_Grade = as.numeric(From_Grade))
finDat_test <- finDat_test %>% mutate(From_Grade = as.numeric(From_Grade))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(To_Grade = as.numeric(To_Grade))
finDat_test <- finDat_test %>% mutate(To_Grade = as.numeric(To_Grade))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
stateGrouping <- tibble(
  Group_State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  Group_main_region = c("south", "west", "west", "south", "west", "west", "northeast", "south", "south", "south", "west", "west", "midwest", "midwest", "midwest", "midwest", "south", "south", "northeast", "south", "northeast", "midwest", "midwest", "south", "midwest", "west", "midwest", "west", "northeast", "northeast", "west", "northeast", "south", "midwest", "midwest", "south", "west", "northeast", "northeast", "south", "midwest", "south", "south", "west", "northeast", "south", "west", "south", "midwest", "west"),
  Group_north_south = c("south", "north", "south", "south", "south", "north", "north", "south", "south", "south", "south", "north", "north", "north", "north", "north", "south", "south", "north", "north", "north", "north", "north", "south", "north", "north", "north", "north", "north", "north", "south", "north", "south", "north", "north", "south", "north", "north", "north", "south", "north", "south", "south", "north", "north", "south", "north", "north", "north", "north"),
  Group_east_west = c("east", "west", "west", "west", "west", "west", "east", "east", "east", "east", "west", "west", "east", "east", "west", "west", "east", "west", "east", "east", "east", "east", "west", "east", "west", "west", "west", "west", "east", "east", "west", "east", "east", "west", "east", "west", "west", "east", "east", "east", "west", "east", "west", "west", "east", "east", "west", "east", "east", "west")
)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% left_join(stateGrouping) 
finDat_test <- finDat_test %>% left_join(stateGrouping) 
rm(stateGrouping)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Group_main_region = ifelse(is.na(Group_main_region), "other", Group_main_region),
         Group_north_south = ifelse(is.na(Group_north_south), "other", Group_north_south),
         Group_east_west   = ifelse(is.na(Group_east_west),   "other", Group_east_west))

finDat_test <- finDat_test %>% 
  mutate(Group_main_region = ifelse(is.na(Group_main_region), "other", Group_main_region),
         Group_north_south = ifelse(is.na(Group_north_south), "other", Group_north_south),
         Group_east_west   = ifelse(is.na(Group_east_west),   "other", Group_east_west))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Days = as.numeric(Days))
finDat_test <- finDat_test %>% mutate(Days = as.numeric(Days))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Travel_Type = if_else(Travel_Type == "A", "air", "ground"))
finDat_test <- finDat_test %>% mutate(Travel_Type = if_else(Travel_Type == "A", "air", "ground"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(departureMonth  = month.abb[lubridate::month(lubridate::mdy(Departure_Date))],
         departureSeason = getSeason(lubridate::mdy(Departure_Date)))

finDat_test <- finDat_test %>%
  mutate(departureMonth  = month.abb[lubridate::month(lubridate::mdy(Departure_Date))],
         departureSeason = getSeason(lubridate::mdy(Departure_Date)))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(returnMonth  = month.abb[lubridate::month(lubridate::mdy(Return_Date))],
         returnSeason = getSeason(lubridate::mdy(Return_Date))) %>%
  select(-Return_Date)

finDat_test <- finDat_test %>%
  mutate(returnMonth  = month.abb[lubridate::month(lubridate::mdy(Return_Date))],
         returnSeason = getSeason(lubridate::mdy(Return_Date))) %>%
  select(-Return_Date)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(earlyRPLFlag = dplyr::if_else(is.na(Early_RPL), "no_notification", "notified")) %>%
  select(-Early_RPL)
finDat_test <- finDat_test %>%
  mutate(earlyRPLFlag = dplyr::if_else(is.na(Early_RPL), "no_notification", "notified")) %>%
  select(-Early_RPL)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(latestRPLFlag = dplyr::if_else(is.na(Latest_RPL), "no_notification", "notified")) %>%
  select(-Latest_RPL)
finDat_test <- finDat_test %>%
  mutate(latestRPLFlag = dplyr::if_else(is.na(Latest_RPL), "no_notification", "notified")) %>%
  select(-Latest_RPL)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Cancelled_Pax = as.numeric(Cancelled_Pax))
finDat_test <- finDat_test %>% mutate(Cancelled_Pax = as.numeric(Cancelled_Pax))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Total_Discount_Pax = as.numeric(Total_Discount_Pax))
finDat_test <- finDat_test %>% mutate(Total_Discount_Pax = as.numeric(Total_Discount_Pax))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(initDayOfWeek = day.abb[lubridate::month(lubridate::mdy(Initial_System_Date))],
         initSeason    = getSeason(lubridate::mdy(Initial_System_Date))) %>%
  select(-Initial_System_Date)

finDat_test <- finDat_test %>%
  mutate(initDayOfWeek = day.abb[lubridate::month(lubridate::mdy(Initial_System_Date))],
         initSeason    = getSeason(lubridate::mdy(Initial_System_Date))) %>%
  select(-Initial_System_Date)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SPR_Product_Type = if_else(SPR_Product_Type %in% c("CA History", "East Coast", "Science"), SPR_Product_Type, "other"))
finDat_test <- finDat_test %>% mutate(SPR_Product_Type = if_else(SPR_Product_Type %in% c("CA History", "East Coast", "Science"), SPR_Product_Type, "other"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FPP = as.numeric(FPP))
finDat_test <- finDat_test %>% mutate(FPP = as.numeric(FPP))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Total_Pax = as.numeric(Total_Pax))
finDat_test <- finDat_test %>% mutate(Total_Pax = as.numeric(Total_Pax))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(totPaxDiff = FPP + Total_Discount_Pax - Total_Pax)
finDat_test <- finDat_test %>% mutate(totPaxDiff = FPP + Total_Discount_Pax - Total_Pax)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% select(-DepartureMonth)
finDat_test <- finDat_test %>% select(-DepartureMonth)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(GroupGradeTypeLow = ifelse(GroupGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeLow))
finDat_test <- finDat_test %>% mutate(GroupGradeTypeLow = ifelse(GroupGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeLow))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(GroupGradeTypeHigh = ifelse(GroupGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeHigh))
finDat_test <- finDat_test %>% mutate(GroupGradeTypeHigh = ifelse(GroupGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeHigh))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(GroupGradeType = ifelse(GroupGradeType == "Undefined->Undefined", NA, GroupGradeType))
finDat_test <- finDat_test %>% mutate(GroupGradeType = ifelse(GroupGradeType == "Undefined->Undefined", NA, GroupGradeType))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Retained       = as.numeric(Retained),
                            Retained_class = if_else(Retained == 0, "no", "yes"))

finDat_test <- finDat_test %>% mutate(Retained       = as.numeric(Retained),
                                      Retained_class = if_else(Retained == 0, "no", "yes"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Poverty_Code = ifelse(Poverty_Code %in% c("A", "B", "C", "D"), Poverty_Code, "other"))
finDat_test <- finDat_test %>% mutate(Poverty_Code = ifelse(Poverty_Code %in% c("A", "B", "C", "D"), Poverty_Code, "other"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Region = ifelse(Region %in% c("Southern California", "Northern California", "Pacific Northwest", "Houston", "Dallas"), Region, "other"))
finDat_test <- finDat_test %>% mutate(Region = ifelse(Region %in% c("Southern California", "Northern California", "Pacific Northwest", "Houston", "Dallas"), Region, "other"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(CRM_Segment = ifelse(CRM_Segment %in% c("1", "2", "4", "5", "6", "7", "8", "9", "10", "11"), CRM_Segment, "other"))
finDat_test <- finDat_test %>% mutate(CRM_Segment = ifelse(CRM_Segment %in% c("1", "2", "4", "5", "6", "7", "8", "9", "10", "11"), CRM_Segment, "other"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Parent_Meeting_Flag = if_else(is.na(Parent_Meeting_Flag), "1", Parent_Meeting_Flag))
finDat_test <- finDat_test %>% mutate(Parent_Meeting_Flag = if_else(is.na(Parent_Meeting_Flag), "1", Parent_Meeting_Flag))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_Low_Grade = ifelse(MDR_Low_Grade == "K",  "0", MDR_Low_Grade),
         MDR_Low_Grade = ifelse(MDR_Low_Grade == "PK", "-1", MDR_Low_Grade) %>% as.numeric())
finDat_test <- finDat_test %>% 
  mutate(MDR_Low_Grade = ifelse(MDR_Low_Grade == "K",  "0", MDR_Low_Grade),
         MDR_Low_Grade = ifelse(MDR_Low_Grade == "PK", "-1", MDR_Low_Grade) %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_High_Grade = ifelse(MDR_High_Grade == "K",  "0", MDR_High_Grade),
         MDR_High_Grade = ifelse(MDR_High_Grade == "PK", "-1", MDR_High_Grade) %>% as.numeric())
finDat_test <- finDat_test %>% 
  mutate(MDR_High_Grade = ifelse(MDR_High_Grade == "K",  "0", MDR_High_Grade),
         MDR_High_Grade = ifelse(MDR_High_Grade == "PK", "-1", MDR_High_Grade) %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Total_School_Enrollment = as.numeric(Total_School_Enrollment))
finDat_test <- finDat_test %>% mutate(Total_School_Enrollment = as.numeric(Total_School_Enrollment))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FPP_to_School_enrollment = gsub(FPP_to_School_enrollment, pattern = ",", replacement = ".") %>% as.numeric())
finDat_test <- finDat_test %>% mutate(FPP_to_School_enrollment = gsub(FPP_to_School_enrollment, pattern = ",", replacement = ".") %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FPP_to_School_enrollment = if_else(is.na(FPP_to_School_enrollment) & Total_School_Enrollment > 0 & !is.na(Total_School_Enrollment), FPP/Total_School_Enrollment, FPP_to_School_enrollment))
finDat_test <- finDat_test %>% mutate(FPP_to_School_enrollment = if_else(is.na(FPP_to_School_enrollment) & Total_School_Enrollment > 0 & !is.na(Total_School_Enrollment), FPP/Total_School_Enrollment, FPP_to_School_enrollment))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Income_Level = ifelse(Income_Level %in% c("P", "P1", "P3", "P4", "P5"), "P", Income_Level),
                            Income_Level = ifelse(Income_Level %in% c("0", "NA", NA, "Z"), "other", Income_Level))
finDat_test <- finDat_test %>% mutate(Income_Level = ifelse(Income_Level %in% c("P", "P1", "P3", "P4", "P5"), "P", Income_Level),
                                      Income_Level = ifelse(Income_Level %in% c("0", "NA", NA, "Z"), "other", Income_Level))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SPR_New_Existing = if_else(is.na(SPR_New_Existing), "1", SPR_New_Existing))
finDat_test <- finDat_test %>% mutate(SPR_New_Existing = if_else(is.na(SPR_New_Existing), "1", SPR_New_Existing))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(NumberOfMeetingswithParents = as.numeric(NumberOfMeetingswithParents))
finDat_test <- finDat_test %>% mutate(NumberOfMeetingswithParents = as.numeric(NumberOfMeetingswithParents))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat %>% select(-FirstMeeting)
finDat_test <- finDat_test %>% select(-FirstMeeting)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(LastMeetingSeason  = getSeason(lubridate::mdy(LastMeeting)),
         LastMeetingWeekDay = lubridate::wday(lubridate::mdy(LastMeeting)) %>% as.character(),
         LastMeetingWeekDay = ifelse(is.na(LastMeetingWeekDay), "other", LastMeetingWeekDay)) %>%
  select(-LastMeeting)

finDat_test <- finDat_test %>%
  mutate(LastMeetingSeason  = getSeason(lubridate::mdy(LastMeeting)),
         LastMeetingWeekDay = lubridate::wday(lubridate::mdy(LastMeeting)) %>% as.character(),
         LastMeetingWeekDay = ifelse(is.na(LastMeetingWeekDay), "other", LastMeetingWeekDay)) %>%
  select(-LastMeeting)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(DifferenceTraveltoFirstMeeting = as.numeric(DifferenceTraveltoFirstMeeting))
finDat_test <- finDat_test %>% mutate(DifferenceTraveltoFirstMeeting = as.numeric(DifferenceTraveltoFirstMeeting))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(DifferenceTraveltoLastMeeting = as.numeric(DifferenceTraveltoLastMeeting))
finDat_test <- finDat_test %>% mutate(DifferenceTraveltoLastMeeting = as.numeric(DifferenceTraveltoLastMeeting))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SchoolGradeTypeLow = ifelse(SchoolGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeLow))
finDat_test <- finDat_test %>% mutate(SchoolGradeTypeLow = ifelse(SchoolGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeLow))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SchoolGradeTypeHigh = ifelse(SchoolGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeHigh))
finDat_test <- finDat_test %>% mutate(SchoolGradeTypeHigh = ifelse(SchoolGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeHigh))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SchoolGradeType = ifelse(SchoolGradeType == "Undefined->Undefined", NA, SchoolGradeType))
finDat_test <- finDat_test %>% mutate(SchoolGradeType = ifelse(SchoolGradeType == "Undefined->Undefined", NA, SchoolGradeType))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(depositToDeparture = (lubridate::mdy(Departure_Date) - lubridate::mdy(Deposit_Date)) %>% as.character() %>% as.numeric()) %>%
  select(-c(Deposit_Date, Departure_Date))

finDat_test <- finDat_test %>%
  mutate(depositToDeparture = (lubridate::mdy(Departure_Date) - lubridate::mdy(Deposit_Date)) %>% as.character() %>% as.numeric()) %>%
  select(-c(Deposit_Date, Departure_Date))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Special_Pay = ifelse(Special_Pay %in% c("CP", "FR", "SA"), Special_Pay, "other"))
finDat_test <- finDat_test %>% mutate(Special_Pay = ifelse(Special_Pay %in% c("CP", "FR", "SA"), Special_Pay, "other"))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Tuition = as.numeric(Tuition))
finDat_test <- finDat_test %>% mutate(Tuition = as.numeric(Tuition))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(avePrice = as.numeric(SPR_Group_Revenue)/Total_Pax)
finDat_test <- finDat_test %>% mutate(avePrice = as.numeric(SPR_Group_Revenue)/Total_Pax)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FRP_Active = as.numeric(FRP_Active))
finDat_test <- finDat_test %>% mutate(FRP_Active = as.numeric(FRP_Active))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FRP_Cancelled = as.numeric(FRP_Cancelled))
finDat_test <- finDat_test %>% mutate(FRP_Cancelled = as.numeric(FRP_Cancelled))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FRP_Take_up_percent_ = gsub(FRP_Take_up_percent_, pattern = ",", replacement = ".") %>% as.numeric())
finDat_test <- finDat_test %>% mutate(FRP_Take_up_percent_ = gsub(FRP_Take_up_percent_, pattern = ",", replacement = ".") %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(EZ_Pay_Take_Up_Rate = gsub(EZ_Pay_Take_Up_Rate, pattern = ",", replacement = ".") %>% as.numeric())
finDat_test <- finDat_test %>% mutate(EZ_Pay_Take_Up_Rate = gsub(EZ_Pay_Take_Up_Rate, pattern = ",", replacement = ".") %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(School_Sponsor = if_else(is.na(School_Sponsor), "0", School_Sponsor))
finDat_test <- finDat_test %>% mutate(School_Sponsor = if_else(is.na(School_Sponsor), "0", School_Sponsor))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(SPR_Group_Revenue = as.numeric(SPR_Group_Revenue))
finDat_test <- finDat_test %>% mutate(SPR_Group_Revenue = as.numeric(SPR_Group_Revenue))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FPP_to_PAX = gsub(FPP_to_PAX, pattern = ",", replacement = ".") %>% as.numeric())
finDat_test <- finDat_test %>% mutate(FPP_to_PAX = gsub(FPP_to_PAX, pattern = ",", replacement = ".") %>% as.numeric())

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(FPP_to_PAX = if_else(is.na(FPP_to_PAX) & Total_Pax > 0 & !is.na(Total_Pax), FPP/Total_Pax, FPP_to_PAX))
finDat_test <- finDat_test %>% mutate(FPP_to_PAX = if_else(is.na(FPP_to_PAX) & Total_Pax > 0 & !is.na(Total_Pax), FPP/Total_Pax, FPP_to_PAX))

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(Num_of_Non_FPP_PAX = as.numeric(Num_of_Non_FPP_PAX))
finDat_test <- finDat_test %>% mutate(Num_of_Non_FPP_PAX = as.numeric(Num_of_Non_FPP_PAX))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat %>% mutate(pol_med = as.numeric(pol_med),
                                 pol_med = ifelse(is.na(pol_med), 0, pol_med))
finDat_test <- finDat_test %>% mutate(pol_med = as.numeric(pol_med),
                                      pol_med = ifelse(is.na(pol_med), 0, pol_med))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(sent_med = as.numeric(sent_med),
                                      sent_med = ifelse(is.na(sent_med), 0, sent_med))
finDat_test <- finDat_test %>% mutate(sent_med = as.numeric(sent_med),
                                      sent_med = ifelse(is.na(sent_med), 0, sent_med))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(sub_med = as.numeric(sub_med),
                                      sub_med = ifelse(is.na(sub_med), 0, sub_med))
finDat_test <- finDat_test %>% mutate(sub_med = as.numeric(sub_med),
                                      sub_med = ifelse(is.na(sub_med), 0, sub_med))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(twl_med = as.numeric(twl_med),
                                      twl_med = ifelse(is.na(twl_med), 0, twl_med))
finDat_test <- finDat_test %>% mutate(twl_med = as.numeric(twl_med),
                                      twl_med = ifelse(is.na(twl_med), 0, twl_med))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% mutate(numTweets = as.numeric(numTweets),
                                      numTweets = ifelse(is.na(numTweets), 0, numTweets))
finDat_test <- finDat_test %>% mutate(numTweets = as.numeric(numTweets),
                                      numTweets = ifelse(is.na(numTweets), 0, numTweets))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% select(-c(Total_Discount_Pax, Num_of_Non_FPP_PAX))
finDat_test <- finDat_test %>% select(-c(Total_Discount_Pax, Num_of_Non_FPP_PAX))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat      <- finDat      %>% select(-c(FPP, Tuition, To_Grade, Cancelled_Pax, FRP_Active))
finDat_test <- finDat_test %>% select(-c(FPP, Tuition, To_Grade, Cancelled_Pax, FRP_Active))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nzv         <- caret::nearZeroVar(finDat, saveMetrics = TRUE) %>% filter(zeroVar == TRUE | nzv == TRUE) 
finDat      <- finDat      %>% select(-c(rownames(nzv)))
finDat_test <- finDat_test %>% select(-c(rownames(nzv)))
rm(nzv)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate_if(.predicate = is.character, 
            .funs = function(x) { x[is.na(x)] <- "other"; return(x)})

finDat_test <- finDat_test %>% 
  mutate_if(.predicate = is.character, 
            .funs = function(x) { x[is.na(x)] <- "other"; return(x)})

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2020)
splitIndex <- createDataPartition(finDat$Retained_class, p = .75, list  = FALSE, times = 1)
trainDF <- finDat[splitIndex,  ]
testDF  <- finDat[-splitIndex, ]
rm(splitIndex, finDat)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDF_targ <- trainDF %>% select(Retained)
trainDF      <- trainDF %>% select(-Retained)
trainDF_cat  <- trainDF %>% select_if(is.character)
trainDF_num  <- trainDF %>% select_if(is.numeric)

nb_pca      <- missMDA::estim_ncpPCA(trainDF_num, ncp.max = 5)
resMI_pca   <- missMDA::MIPCA(trainDF_num,        ncp = ifelse(nb_pca$ncp==0, names(which(nb_pca$criterion == min(nb_pca$criterion))) %>% as.numeric(), nb_pca$ncp))
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

finDat_test_targ <- finDat_test %>% select(Retained)
finDat_test      <- finDat_test %>% select(-Retained)
finDat_test_cat  <- finDat_test %>% select_if(is.character)
finDat_test_num  <- finDat_test %>% select_if(is.numeric)

nb_pca     <- missMDA::estim_ncpPCA(finDat_test_num, ncp.max = 5)
resMI_pca  <- missMDA::MIPCA(finDat_test_num,        ncp = ifelse(nb_pca$ncp==0, names(which(nb_pca$criterion == min(nb_pca$criterion))) %>% as.numeric(), nb_pca$ncp))
finDat_test_num <- resMI_pca$res.imputePCA %>% as_tibble()

finDat_test <- bind_cols(finDat_test_targ, finDat_test_cat, finDat_test_num)
finDat_test <- finDat_test %>% mutate(MDR_High_Grade_Class = if_else(MDR_High_Grade <= 8, "lower", "higher"))

rm(nb_pca, resMI_pca, finDat_test_targ, finDat_test_cat, finDat_test_num)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c(names(trainDF)[colSums(is.na(trainDF)) > 0], names(testDF)[colSums(is.na(testDF)) > 0], names(finDat_test)[colSums(is.na(finDat_test)) > 0])


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make sure we have consistent categorical levels
formFeats  <- names(trainDF)[!(names(trainDF) %in% c("ID_SALES", "Retained", "Retained_class"))]
modFormula <- formula(paste0("Retained_class ~ ", paste0(formFeats, collapse = " + ")))

blueprint <- recipes::recipe(modFormula, data = trainDF) %>% recipes::step_scale(recipes::all_numeric())

# Create training & test sets for h2o
train_h2o <- recipes::prep(blueprint, training = trainDF, retain = TRUE) %>%
  recipes::juice() %>%
  h2o::as.h2o(destination_frame = "train_h2o")
test_h2o <- recipes::prep(blueprint, training = trainDF) %>%
  recipes::bake(new_data = testDF) %>%
  h2o::as.h2o(destination_frame = "test_h2o")
finTest <- recipes::prep(blueprint, training = trainDF) %>%
  recipes::bake(new_data = finDat_test) %>%
  h2o::as.h2o(destination_frame = "fin_test_h2o")

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDF <- trainDF %>%
  mutate(pred       = h2o::h2o.predict(object = finMod, newdata = train_h2o) %>% as_tibble() %>% pull(yes),
         pred_class = ifelse(pred >= finThresh, "yes", "no"))
testDF <- testDF %>%
  mutate(pred          = h2o::h2o.predict(object = finMod, newdata = test_h2o) %>% as_tibble() %>% pull(yes),
         pred_class    = ifelse(pred >= finThresh, "yes", "no"))
finDat_test <- finDat_test %>%
  mutate(pred          = h2o::h2o.predict(object = finMod, newdata = finTest) %>% as_tibble() %>% pull(yes),
         pred_class    = ifelse(pred >= finThresh, "yes", "no"))

write.csv(x = finDat_test %>% select(ID_SALES, pred_class), file = "2_output/1_data/TestPredictions.csv", row.names = FALSE)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exm_train    <- h2o::h2o.explain(finMod, train_h2o)
exm_test     <- h2o::h2o.explain(finMod, test_h2o)
exm_finTest  <- h2o::h2o.explain(finMod, finTest)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h2o::h2o.ice_plot(model = finMod, newdata = train_h2o, column = "MajorProgramCode") %>% plotly::ggplotly()
h2o::h2o.ice_plot(model = finMod, newdata = finTest,   column = "MajorProgramCode") %>% plotly::ggplotly()
h2o::h2o.ice_plot(model = finMod, newdata = train_h2o, column = "pol_med") %>% plotly::ggplotly()

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- plotTarget(datIn = finDat_test, var_inspect = "pol_med", target = "pred", predVar = "pred",
                  numvar = TRUE, numVarBreaks = seq(-2,2,0.1),
                  autoBin = F)

plotTarget(datIn = finDat_test, var_inspect = "MajorProgramCode", target = "pred", predVar = "pred",
           numvar = FALSE,
           autoBin = F)
plotTarget(datIn = trainDF, var_inspect = "MajorProgramCode", target = "Retained", predVar = "Retained",
           numvar = FALSE,
           autoBin = F)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pullingID <- tst %>% arrange(-diff) %>% pull(ID_SALES)
which(finDat_test$ID_SALES==pullingID[2])

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
twiter_test <- readr::read_csv("0_data/test_data/twitter_test.csv", col_types = cols(.default = "c"))

ii <- 56 # 146

finDat_test$ID_SALES[ii]

finDat_test$pred[ii]

h2o::h2o.shap_explain_row_plot(finMod, finTest, row_index = ii)

finDat_test$pol_med[ii]
finDat_test$sent_med[ii]
twiter_test$text[twiter_test$ID_SALES == finDat_test$ID_SALES[[ii]]]

rbind(testDF, trainDF) %>% filter(pol_med == 0) %>% group_by(MajorProgramCode) %>% summarise(mean(Retained))
rbind(testDF, trainDF) %>% filter(pol_med == 0) %>% group_by(Group_State) %>% summarise(mean(Retained))
rbind(testDF, trainDF) %>% filter(pol_med == 0) %>% group_by(Program_Code) %>% summarise(n=n(), mean(Retained))
rbind(testDF, trainDF) %>% filter(pol_med == 0) %>% filter(twl_med==13) %>% summarise(mean(Retained))

rbind(testDF, trainDF %>% select(-c(pred, pred_class))) %>% filter(pol_med == 0, MajorProgramCode == "C") %>% 
  summarise(n     = n(), 
            score = mean(Retained))

rbind(testDF, trainDF %>% select(-c(pred, pred_class))) %>% filter(pol_med == 0) %>% group_by(Program_Code) %>% summarise(mean(Retained))
rbind(testDF, trainDF %>% select(-c(pred, pred_class))) %>% filter(pol_med == 0) %>% group_by(Group_State) %>% summarise(mean(Retained)) %>% head(20)
rbind(testDF, trainDF %>% select(-c(pred, pred_class))) %>% filter(pol_med == 0) %>% group_by(Income_Level) %>% summarise(mean(Retained))
rbind(testDF, trainDF %>% select(-c(pred, pred_class, pred_NT, pred_class_NT))) %>% filter(pol_med == 0) %>% group_by(MajorProgramCode) %>% summarise(mean(Retained))


questionablePreds <- c(20, 25, 43, 46, 63)
wrongPreds        <- c(33, 36, 56, 108, 134, 138, 139)

finDat_test$ID_SALES[wrongPreds]

twiter_test$text[twiter_test$ID_SALES %in% finDat_test$ID_SALES[wrongPreds]]





prevRes <- read.csv("2_output/1_data/finalTestPred.csv")


ConfusionMatrix(prevRes$pred_class, finDat_test$pred_class)
F1_Score(prevRes$pred_class, finDat_test$pred_class, positive = "yes")



AllRes <- finDat_test %>% left_join((prevRes %>% select(ID_SALES, pred_class) %>% rename(pred_class_old = pred_class)))

AllRes %>% filter(pred_class_old != pred_class, pred_class == "yes") %>% select(ID_SALES)




ii <- 2
ID <- (AllRes %>% filter(pred_class_old != pred_class, pred_class == "no") %>% pull(ID_SALES))[ii]

twiter_test$text[twiter_test$ID_SALES %in% ID]

AllRes %>% filter(ID_SALES == ID) %>% select(ID_SALES, pred, pred_class, pol_med, sent_med)
















