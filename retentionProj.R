## ----knitr_init, echo = FALSE, results = "asis", cache = FALSE------------------------------------------------------------------------------------
library(knitr)
library(rmdformats)
library(kableExtra)

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


## -------------------------------------------------------------------------------------------------------------------------------------------------
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

day.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

crmDat   <- readr::read_csv("0_data/crm_model.csv",     col_types = cols(.default = "c"))
finDat   <- readr::read_csv("0_data/finance_model.csv", col_types = cols(.default = "c"))
salesDat <- readr::read_csv("0_data/sales_model.csv",   col_types = cols(.default = "c"))
twiter   <- readr::read_csv("0_data/twitter_model.csv", col_types = cols(.default = "c"))


## -------------------------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------------------------
propFunc(datIn = mtcars, vars = names(mtcars), totToReturn = 3, asDF = TRUE) %>% 
  kableExtra::kable(format = "html") %>% kableExtra::kable_styling("striped") %>% kableExtra::scroll_box(width = "100%")


## -------------------------------------------------------------------------------------------------------------------------------------------------
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

## -------------------------------------------------------------------------------------------------------------------------------------------------
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

## -------------------------------------------------------------------------------------------------------------------------------------------------
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

## -------------------------------------------------------------------------------------------------------------------------------------------------
salesDat <- salesDat %>% 
  mutate(id_progCode = stringr::str_sub(ID_SALES, start = 1,  end = nchar(Program_Code)),
         id_travType = stringr::str_sub(ID_SALES, start = -nchar(Travel_Type)),
         id_actual   = stringr::str_replace(string = ID_SALES,  pattern = id_progCode, replacement = ""),
         id_actual   = stringr::str_replace(string = id_actual, pattern = paste0(id_travType, "$"), replacement = ""))

## -------------------------------------------------------------------------------------------------------------------------------------------------
crmDat <- crmDat %>% 
  mutate(id_povCode = stringr::str_sub(string = ID_CRM, start = 1, end = nchar(Poverty_Code)),
         id_incLev  = stringr::str_sub(ID_CRM, start = -nchar(Income_Level)),
         id_actual  = stringr::str_replace(string = ID_CRM,    pattern = id_povCode, replacement = ""),
         id_actual  = stringr::str_replace(string = id_actual, pattern = paste0(id_incLev, "$"), replacement = ""))

## -------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- salesDat %>% 
  select(-c(id_progCode, id_travType)) %>% 
  left_join((crmDat %>% select(-c(id_povCode, id_incLev)) %>% mutate(fromCRM = 1)), by = "id_actual") 

## -------------------------------------------------------------------------------------------------------------------------------------------------
sales_crm <- sales_crm %>% select(-c(ID_CRM, fromCRM))
rm(salesDat, crmDat, tst)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Special_Pay = dplyr::if_else(is.na(Special_Pay), "NA", Special_Pay),
         id_specPay  = dplyr::if_else(Special_Pay == "0", "0", stringr::str_sub(ID_FINANCE, start = 1,  end = nchar(Special_Pay))),
         id_actual   = dplyr::if_else(Special_Pay == "0", 
                                      ID_FINANCE, 
                                      stringr::str_replace(string = ID_FINANCE, pattern = id_specPay, replacement = "")))

## -------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm <- sales_crm %>% 
  left_join((finDat %>% select(-id_specPay) %>% mutate(fromFIN = 1)), by = "id_actual") 

## -------------------------------------------------------------------------------------------------------------------------------------------------
fin_sales_crm <- fin_sales_crm %>% select(-c(ID_FINANCE, fromFIN, id_actual))
rm(finDat, sales_crm)

## -------------------------------------------------------------------------------------------------------------------------------------------------
twiter <- readr::read_csv("2_output/1_data/twitter_results.csv") %>%
  mutate(tweetLength = nchar(text)) %>% 
  group_by(ID_SALES) %>% 
  summarise(pol_min   = polarity %>% min(na.rm    = TRUE),
            pol_mean  = polarity %>% mean(na.rm   = TRUE),
            pol_med   = polarity %>% median(na.rm = TRUE),
            pol_max   = polarity %>% max(na.rm    = TRUE),
            sub_min   = subjectivity %>% min(na.rm    = TRUE),
            sub_mean  = subjectivity %>% mean(na.rm   = TRUE),
            sub_med   = subjectivity %>% median(na.rm = TRUE),
            sub_max   = subjectivity %>% max(na.rm    = TRUE),
            twl_min   = tweetLength %>% min(na.rm    = TRUE),
            twl_mean  = tweetLength %>% mean(na.rm   = TRUE),
            twl_med   = tweetLength %>% median(na.rm = TRUE),
            twl_max   = tweetLength %>% max(na.rm    = TRUE),
            numTweets = n(),
            top_0     = `TOPIC 0` %>% sum(na.rm = TRUE),
            top_1     = `TOPIC 1` %>% sum(na.rm = TRUE),
            top_2     = `TOPIC 2` %>% sum(na.rm = TRUE),
            top_3     = `TOPIC 3` %>% sum(na.rm = TRUE),
            top_4     = `TOPIC 4` %>% sum(na.rm = TRUE),
            top_5     = `TOPIC 5` %>% sum(na.rm = TRUE),
            top_6     = `TOPIC 6` %>% sum(na.rm = TRUE),
            top_7     = `TOPIC 7` %>% sum(na.rm = TRUE),
            top_8     = `TOPIC 8` %>% sum(na.rm = TRUE),
            top_9     = `TOPIC 9` %>% sum(na.rm = TRUE))

finDat <- fin_sales_crm %>% 
  left_join(twiter)

rm(twiter, fin_sales_crm)


## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat %>% glimpse()


## -------------------------------------------------------------------------------------------------------------------------------------------------
propFunc(datIn = finDat, vars = names(finDat), totToReturn = 10, asDF = TRUE)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- smartGrouper(datIn = finDat, varIn = "Program_Code")

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(From_Grade = as.numeric(From_Grade))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(To_Grade = as.numeric(To_Grade))

## -------------------------------------------------------------------------------------------------------------------------------------------------
stateGrouping <- tibble(Group_State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                        Group_main_region = c("south", "west", "west", "south", "west", "west", "northeast", "south", "south", "south", "west", "west", "midwest", "midwest", "midwest", "midwest", "south", "south", "northeast", "south", "northeast", "midwest", "midwest", "midwest", "south", "midwest", "west", "midwest", "west", "northeast", "northeast", "west", "northeast", "northeast", "south", "midwest", "midwest", "south", "west", "northeast", "northeast", "south", "midwest", "south", "south", "west", "northeast", "south", "west", "south", "midwest", "west"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% left_join(stateGrouping) 
rm(stateGrouping)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(Group_main_region = ifelse(is.na(Group_main_region), "other", Group_main_region)) %>% 
  select(-Group_State)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Days = as.numeric(Days))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Travel_Type = if_else(Travel_Type == "A", "air", "ground"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(departureMonth  = month.abb[lubridate::month(lubridate::mdy(Departure_Date))],
         departureSeason = getSeason(lubridate::mdy(Departure_Date)))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(returnMonth  = month.abb[lubridate::month(lubridate::mdy(Return_Date))],
         returnSeason = getSeason(lubridate::mdy(Return_Date))) %>%
  select(-Return_Date)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(earlyRPLFlag = dplyr::if_else(is.na(Early_RPL), "no_notification", "notified")) %>%
  select(-Early_RPL)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(latestRPLFlag = dplyr::if_else(is.na(Latest_RPL), "no_notification", "notified")) %>%
  select(-Latest_RPL)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Cancelled_Pax = as.numeric(Cancelled_Pax))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_Discount_Pax = as.numeric(Total_Discount_Pax))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(initDayOfWeek = day.abb[lubridate::month(lubridate::mdy(Initial_System_Date))],
         initSeason    = getSeason(lubridate::mdy(Initial_System_Date))) %>%
  select(-Initial_System_Date)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SPR_Product_Type = if_else(SPR_Product_Type %in% c("CA History", "East Coast", "Science"), SPR_Product_Type, "other"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP = as.numeric(FPP))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_Pax = as.numeric(Total_Pax))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(totPaxDiff = FPP + Total_Discount_Pax - Total_Pax)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-DepartureMonth)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(GroupGradeTypeLow = ifelse(GroupGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeLow))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(GroupGradeTypeHigh = ifelse(GroupGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, GroupGradeTypeHigh))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Retained_class = if_else(Retained == 0, "no", "yes"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Poverty_Code = ifelse(Poverty_Code %in% c("A", "B", "C", "D"), Poverty_Code, "other"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Region = ifelse(Region %in% c("Southern California", "Northern California", "Pacific Northwest", "Houston", "Dallas"), Region, "other"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(CRM_Segment = ifelse(CRM_Segment %in% c("1", "2", "4", "5", "6", "7", "8", "9", "10", "11"), CRM_Segment, "other"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_Low_Grade = ifelse(MDR_Low_Grade == "K",  "-1", MDR_Low_Grade),
         MDR_Low_Grade = ifelse(MDR_Low_Grade == "PK", "-2", MDR_Low_Grade) %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% 
  mutate(MDR_High_Grade = ifelse(MDR_High_Grade == "K",  "-1", MDR_High_Grade),
         MDR_High_Grade = ifelse(MDR_High_Grade == "PK", "-2", MDR_High_Grade) %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Total_School_Enrollment = as.numeric(Total_School_Enrollment))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_School_enrollment = gsub(FPP_to_School_enrollment, pattern = ",", replacement = ".") %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_School_enrollment = if_else(is.na(FPP_to_School_enrollment) & Total_School_Enrollment > 0 & !is.na(Total_School_Enrollment), FPP/Total_School_Enrollment, FPP_to_School_enrollment))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Income_Level = ifelse(Income_Level %in% c("P", "P1", "P3", "P4", "P5"), "P", Income_Level),
                            Income_Level = ifelse(Income_Level %in% c("0", "NA", NA, "Z"), "other", Income_Level))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(NumberOfMeetingswithParents = as.numeric(NumberOfMeetingswithParents))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-FirstMeeting)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(LastMeetingSeason  = getSeason(lubridate::mdy(LastMeeting)),
         LastMeetingWeekDay = lubridate::wday(lubridate::mdy(LastMeeting)) %>% as.character(),
         LastMeetingWeekDay = ifelse(is.na(LastMeetingWeekDay), "other", LastMeetingWeekDay)) %>%
  select(-LastMeeting)

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(DifferenceTraveltoFirstMeeting = as.numeric(DifferenceTraveltoFirstMeeting))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(DifferenceTraveltoLastMeeting = as.numeric(DifferenceTraveltoLastMeeting))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SchoolGradeTypeLow = ifelse(SchoolGradeTypeLow %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeLow))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SchoolGradeTypeHigh = ifelse(SchoolGradeTypeHigh %in% c(NA, "NA", "Undefined"), NA, SchoolGradeTypeHigh))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>%
  mutate(depositToDeparture = (lubridate::mdy(Departure_Date)-lubridate::mdy(Deposit_Date)) %>% as.character() %>% as.numeric()) %>%
  select(-c(Deposit_Date, Departure_Date))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Special_Pay = ifelse(Special_Pay %in% c("CP", "FR", "SA"), Special_Pay, "other"))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Tuition = as.numeric(Tuition))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Active = as.numeric(FRP_Active))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Cancelled = as.numeric(FRP_Cancelled))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FRP_Take_up_percent_ = gsub(FRP_Take_up_percent_, pattern = ",", replacement = ".") %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(EZ_Pay_Take_Up_Rate = gsub(EZ_Pay_Take_Up_Rate, pattern = ",", replacement = ".") %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(SPR_Group_Revenue = as.numeric(SPR_Group_Revenue))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_PAX = gsub(FPP_to_PAX, pattern = ",", replacement = ".") %>% as.numeric())

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(FPP_to_PAX = if_else(is.na(FPP_to_PAX) & Total_Pax > 0 & !is.na(Total_Pax), FPP/Total_Pax, FPP_to_PAX))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(Num_of_Non_FPP_PAX = as.numeric(Num_of_Non_FPP_PAX))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(pol_min = as.numeric(pol_min),
                            pol_min = ifelse(is.na(pol_min), 0, pol_min))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(pol_mean = as.numeric(pol_mean),
                            pol_mean = ifelse(is.na(pol_mean), 0, pol_mean))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(pol_med = as.numeric(pol_med),
                            pol_med = ifelse(is.na(pol_med), 0, pol_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(pol_max = as.numeric(pol_max),
                            pol_max = ifelse(is.na(pol_max), 0, pol_max))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sub_min = as.numeric(sub_min),
                            sub_min = ifelse(is.na(sub_min), 0, sub_min))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sub_mean = as.numeric(sub_mean),
                            sub_mean = ifelse(is.na(sub_mean), 0, sub_mean))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sub_med = as.numeric(sub_med),
                            sub_med = ifelse(is.na(sub_med), 0, sub_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(sub_max = as.numeric(sub_max),
                            sub_max = ifelse(is.na(sub_max), 0, sub_max))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(twl_min = as.numeric(twl_min),
                            twl_min = ifelse(is.na(twl_min), 0, twl_min))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(twl_mean = as.numeric(twl_mean),
                            twl_mean = ifelse(is.na(twl_mean), 0, twl_mean))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(twl_med = as.numeric(twl_med),
                            twl_med = ifelse(is.na(twl_med), 0, twl_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(twl_max = as.numeric(twl_max),
                            twl_max = ifelse(is.na(twl_max), 0, twl_max))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(numTweets = as.numeric(numTweets),
                            numTweets = ifelse(is.na(numTweets), 0, numTweets))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_0 = as.numeric(top_0),
                            top_0 = ifelse(is.na(top_0), 0, top_0))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_1 = as.numeric(top_1),
                            top_1 = ifelse(is.na(top_1), 0, top_1))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_2 = as.numeric(top_2),
                            top_2 = ifelse(is.na(top_2), 0, top_2))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_3 = as.numeric(top_3),
                            top_3 = ifelse(is.na(top_3), 0, top_3))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_4 = as.numeric(top_4),
                            top_4 = ifelse(is.na(top_4), 0, top_4))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_5 = as.numeric(top_5),
                            top_5 = ifelse(is.na(top_5), 0, top_5))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_6 = as.numeric(top_6),
                            top_6 = ifelse(is.na(top_6), 0, top_6))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_7 = as.numeric(top_7),
                            top_7 = ifelse(is.na(top_7), 0, top_7))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_8 = as.numeric(top_8),
                            top_8 = ifelse(is.na(top_8), 0, top_8))


## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% mutate(top_9 = as.numeric(top_9),
                            top_9 = ifelse(is.na(top_9), 0, top_9))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(pol_min, pol_max, pol_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(sub_min, sub_max, sub_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(twl_min, twl_max, twl_med))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(Total_Discount_Pax, Num_of_Non_FPP_PAX))

## -------------------------------------------------------------------------------------------------------------------------------------------------
finDat <- finDat %>% select(-c(FPP, Tuition, To_Grade, Cancelled_Pax, FRP_Active))

## -------------------------------------------------------------------------------------------------------------------------------------------------
nzv <- caret::nearZeroVar(finDat, saveMetrics = TRUE) %>% filter(zeroVar == TRUE | nzv == TRUE) 
finDat <- finDat %>% select(-c(rownames(nzv)))
rm(nzv)

