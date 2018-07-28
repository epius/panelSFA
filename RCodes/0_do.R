rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(pdftools)
library(stringr)
library(bbmle)
setwd('/Users/BWChen/Dropbox (Work)/FarmHeterogeneity')

#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#

{# Clean data
dat1 <- read_xlsx('./DATA/Enterprise/EnterpriseDatabank-State-CROP-2008-2017-BChen-YKim.xlsx', sheet = 'DrylandWheat-2008-2017')
# dat2 <- read_xlsx('./DATA//Enterprise/EnterpriseDatabank-State-CROP-2008-2017-BChen-YKim.xlsx', sheet = 'IrrigWheat-2008-2017')

# Geocoding
text <- pdf_text("./DATA/Enterprise/CountyData.pdf")
text <- str_split(text, "\n", simplify = TRUE)[c(1:3), ]

text1 <- data.frame(raw = as.character(text)) %>%
  mutate(ID1 = as.numeric(substr(raw, 1, 1))) %>%
  dplyr::filter(is.na(ID1)) %>%
  mutate(ID = as.character(substr(raw, 1, 6)),
         Name = substr(raw, 7, 30)) %>%
  dplyr::select(-ID1) %>%
  mutate(temp = as.numeric(ID),
         ID = str_trim(ID, 'both'),
         Name = tolower(substr(Name, 1, nchar(Name) - 1)),
         Name = str_trim(Name, 'both')) %>%
  drop_na() %>%
  dplyr::select(-temp, -raw)

text2NC <- data.frame(raw = str_split(paste0(text[1, 2 : 20], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))
text2SC <- data.frame(raw = str_split(paste0(text[1, 21 : 32], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))
text2SW1 <- data.frame(raw = str_split(paste0(text[1, 33 : 44], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))
text2SW2 <- data.frame(raw = str_split(paste0(text[2, 2 : 16], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))

text2NE <- data.frame(raw = str_split(paste0(text[2, 17 : 33], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))

text2NW1 <- data.frame(raw = str_split(paste0(text[2, 34 : 44], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))
text2NW2 <- data.frame(raw = str_split(paste0(text[3, 2 : 11], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))

text2SE <- data.frame(raw = str_split(paste0(text[3, 12 : 44], collapse = ''), '\r')[[1]]) %>%
  mutate(ID = tolower(str_trim(substr(raw, 8, 20), 'both')))

text_final <- text1 %>%
  mutate(RegionID  = case_when(Name %in% text2NC$ID ~ 1,
                               Name %in%text2SC$ID ~ 2,
                               Name %in%text2SW1$ID | Name %in% text2SW2$ID ~ 3,
                               Name %in%text2NE$ID ~ 4,
                               Name %in%text2NW1$ID | Name %in% text2NW2$ID ~ 5,
                               Name %in%text2SE$ID ~ 6,
                               TRUE ~ 999),
         ID = paste0(RegionID, ID),
         Name = ifelse(Name == 'commanche', 'comanche', Name)) %>%
  dplyr::select(-RegionID)


library(maps)
data(county.fips)
fipsDat <- county.fips %>%
  dplyr::filter(str_detect(polyname,'\\bkansas\\b')) %>%
  mutate(polyname = as.character(polyname),
         county = substr(polyname, 8, nchar(polyname)),
         county = str_trim(county, 'both')) %>%
  dplyr::select(-2) %>%
  right_join(., text_final, by = c('county' = 'Name'))

# Match with the KFMA data
dat <- dat1 %>%
  mutate(FarmNbr = ifelse(FarmNbr == 38350000, 58350000, FarmNbr),
         ID  = substr(FarmNbr, 1, 3)) %>%
  left_join(., fipsDat, by = 'ID') %>%
  dplyr::select(-ID)

write.csv(dat, './DATA/Enterprise/KFMAEnterpriseWheat.csv', row.names = FALSE)
}

#----------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------   Get the weather data   ------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
rm(list=ls())

CropArea <- readstata13::read.dta13('./Data/WeatherData/cropArea.dta') %>%
  mutate(longitude = -125 + ((gridNumber - 1) - 1405*floor((gridNumber - 1)/1405))/24,
         latitude = 49.9375+1/48 - ceiling(gridNumber/1405)/24) %>%
  inner_join(., readstata13::read.dta13('./Data/WeatherData/linkGridnumberFIPS.dta'),
             by = 'gridNumber')


list_file <- list.dirs(path = 'D:/07_Research/0_Research_Record/Dissertation_0516/5_Essay_Production/2_Data/RawWeatherData')
source('./Code/source_func.r')
# State-level aggregation for raw data

# Winter
for (i in seq(2007, 2017, 1)){
  if(sum(grepl(i, list_file)) == 0) {
    cat('Data in this year ', i, 'is not found.\n');
    next
  }
  suppressWarnings(weather_func(year_sel = i, state_sel = 20, growingSeason = c(12, 2), seasonBreak = TRUE, threshold = c(0, 5, 10)))
  gc() # Clear cache.
  mem <- memory.size()
  cat('Current memory occupancy: ', mem, '\n')
  stopifnot(mem <= 3000) # Stop if the occupied memory nearly reaches the upper bound.
}

# Fall
for (i in seq(2007, 2017, 1)){
  if(sum(grepl(i, list_file)) == 0) {
    cat('Data in this year ', i, 'is not found.\n');
    next
  }
  suppressWarnings(weather_func(year_sel = i, state_sel = 20, growingSeason = c(9, 11), seasonBreak = FALSE, threshold = c(0, 10, 17)))
  gc() # Clear cache.
  mem <- memory.size()
  cat('Current memory occupancy: ', mem, '\n')
  stopifnot(mem <= 3000) # Stop if the occupied memory nearly reaches the upper bound.
}

# Spring
for (i in seq(2007, 2017, 1)){
  if(sum(grepl(i, list_file)) == 0) {
    cat('Data in this year ', i, 'is not found.\n');
    next
  }
  suppressWarnings(weather_func(year_sel = i, state_sel = 20, growingSeason = c(3, 5), seasonBreak = FALSE, threshold = c(0, 18, 34)))
  gc() # Clear cache.
  mem <- memory.size()
  cat('Current memory occupancy: ', mem, '\n')
  stopifnot(mem <= 3000) # Stop if the occupied memory nearly reaches the upper bound.
}
## KS ID: 20
## Year: 2008~2017
## Following Tack et al. (2015), the threshold temperature are:
##   Fall: 0-10-17
##   Winter: 0-5-10
##   Spring: 0-18-34
##   Temperature below zero are measured as heating degree days.
## Growing season, September to May (Fall: September to November; Winter: December to February; Spring: March-May)


#----------------------------------------------------------------------------------------------------------------------------------#
list_file <- list.files(path = './DATA/WeatherData', pattern = '.csv')


readFunc <- function(k, YearChange){
WeaDat <- fread(paste0('./DATA/WeatherData/', k)) %>%
  'colnames<-'(c(paste0(colnames(.), unique(.$Month)))) %>%
  dplyr::select(-3, -4, -5, -10, -12, -13) %>%
  'colnames<-'(c('Year', 'fips', 'prec', 'low', 'medium', 'high', 'freezingDays')) %>%
  mutate(low = low - medium,
         medium = medium - high)
if(isTRUE(YearChange)){
WeaDat$Year <- WeaDat$Year + 1
}
return(WeaDat)
}

# Fall season
list_file_fall <- list_file[grepl('Month9-11', list_file)]

FallWeaDat <- lapply(list_file_fall, readFunc, YearChange = TRUE) %>%
  bind_rows() %>%
  filter(Year <= 2016) %>%
  rename(FallPrec = prec,
         FallLow = low,
         FallMedium = medium,
         FallHigh = high,
         FallFreeze = freezingDays)

# Winter season
list_file_winter <- list_file[grepl('Month12-2', list_file)]

WinterWeaDat <- lapply(list_file_winter, readFunc, YearChange = TRUE) %>%
  bind_rows() %>%
  rename(WinterPrec = prec,
         WinterLow = low,
         WinterMedium = medium,
         WinterHigh = high,
         WinterFreeze = freezingDays)

# Winter season
list_file_spring <- list_file[grepl('Month3-5', list_file)]

SpringWeaDat <- lapply(list_file_spring, readFunc, YearChange = FALSE) %>%
  bind_rows() %>%
  filter(Year >= 2008) %>%
  rename(SpringPrec = prec,
         SpringLow = low,
         SpringMedium = medium,
         SpringHigh = high,
         SpringFreeze = freezingDays)


WeaDatAll <- FallWeaDat %>%
  full_join(., WinterWeaDat, by = c('Year', 'fips')) %>%
  full_join(., SpringWeaDat, by = c('Year', 'fips'))

write.csv(WeaDatAll, file = './DATA/Enterprise/WeatherDatCounty.csv', row.names = F)

#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
# Descriptive analysis
rm(list = ls())
WheatDat <- fread('./DATA/Enterprise/KFMAEnterpriseWheat.csv', stringsAsFactors = FALSE)
WeatherDat <- fread('./DATA/Enterprise/WeatherDatCounty.csv', stringsAsFactors = FALSE)

RegDat <- WheatDat %>%
  dplyr::filter(Year <= 2016) %>% # 2017 weather data are not available yet.
  left_join(., WeatherDat, by = c('Year', 'fips')) %>%
  dplyr::select(1, Year, TotalUnits, AcreYield, TotalEnterpriseAcres, LaborHired, Repairs, Fuel, AutoExpense, Utilities, SeedOtherExpense, Fertilizer, MachineHire,
                HerbicideInsecticide, fips, county, contains('Winter'), contains('Fall'), contains('Spring'), UnpaidLabor) %>%
  mutate(Others = Repairs + Utilities + SeedOtherExpense + HerbicideInsecticide,
         Labor = LaborHired + UnpaidLabor,
         Machinery = MachineHire + Fuel + AutoExpense)

FarmSelect1 <- RegDat %>%
  group_by(FarmNbr) %>%
  summarise(num = n()) %>%
  dplyr::filter(num >= 4)

FarmSelect2 <- RegDat %>%
  select(1, 2) %>%
  group_by(FarmNbr, Year) %>%
  summarise(num = n())%>%
  dplyr::filter(num > 1)

RegDat <- filter(RegDat, FarmNbr %in% FarmSelect1$FarmNbr, !(FarmNbr %in% FarmSelect2$FarmNbr))

length(unique(RegDat$FarmNbr))
length(unique(substr(RegDat$FarmNbr, 2, 3)))

AveYear <- RegDat %>%
  group_by(FarmNbr) %>%
  summarise(Num = n())
summary(AveYear)
summary(RegDat)

SummaryDat <- RegDat %>%
  dplyr::select(3, 5, 12, Machinery, 32, Labor, Others, Repairs, Utilities, SeedOtherExpense, HerbicideInsecticide)

summary(SummaryDat)
apply(SummaryDat, 2, sd)

AcreYear <- RegDat %>%
  group_by(Year) %>%
  summarise(Acre = sum(TotalUnits))

### Input shares
InputShareDat <- SummaryDat %>%
  mutate(Inputs = Labor + Fertilizer + Machinery + Others,
         LaborShare = Labor/Inputs,
         FertilizerShare = Fertilizer/Inputs,
         MachineryShare = Machinery/Inputs,
         OthersShare = Others/Inputs)

apply(InputShareDat, 2, mean)

#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
# Regression
rm(list = ls())
WheatDat <- fread('./DATA/Enterprise/KFMAEnterpriseWheat.csv', stringsAsFactors = FALSE)
WeatherDat <- fread('./DATA/Enterprise/WeatherDatCounty.csv', stringsAsFactors = FALSE)

RegDat <- WheatDat %>%
  dplyr::filter(Year <= 2016) %>% # 2017 weather data are not available yet.
  left_join(., WeatherDat, by = c('Year', 'fips')) %>%
  dplyr::select(1, Year, TotalUnits, AcreYield, TotalEnterpriseAcres, LaborHired, Repairs, Fuel, AutoExpense, Utilities, SeedOtherExpense, Fertilizer, MachineHire,
                         HerbicideInsecticide, fips, county, contains('Winter'), contains('Fall'), contains('Spring')) %>%
  mutate(Others = Repairs + Fuel + AutoExpense + Utilities + SeedOtherExpense + HerbicideInsecticide) %>%
  dplyr::filter(LaborHired >= 0)

# # Inputs: labor, fuel, capital.
# Reg1 <- lm(TotalUnits ~ TotalEnterpriseAcres + LaborHired + Others + SeedOtherExpense + Fertilizer + MachineHire + HerbicideInsecticide +
#                         `dday0C9-11`+`dday0C12-2`+`dday0C3-5`, data = RegDat)
# summary(Reg1)
#
# # Convert to elasticities.
# MeanDat1 <- apply(Reg1$model, 2, mean)
# coefficients(Reg1)[2: 8]*MeanDat[2:8]/MeanDat[1]
#
# # Fixed effect model.
# Reg2 <- lm(TotalUnits ~ TotalEnterpriseAcres + LaborHired + Others + SeedOtherExpense + Fertilizer + MachineHire + HerbicideInsecticide + factor(county) + factor(Year),
#            data = test)
# summary(Reg2)
# MeanDat2 <- apply(Reg2$model[, c(1:8)], 2, mean)
# coefficients(Reg2)[2: 8]*MeanDat2[2:8]/MeanDat2[1]
#
# # Standard fixed-effect model.
# FEest <- coefficients(Reg2)[9:94]
# Muhat <- max(FEest) - FEest
# Efficiency <- Muhat/max(Muhat)
# hist(Efficiency)
# summary(Efficiency)



#-----------------------------------------------------------------------------------------------------------------# P
# Panel SFA model
#-----------------------------------------------------------------------------------------------------------------# P
# Take the first-order differences.
RegDatLog <- RegDat %>%
  mutate(TotalUnits = ifelse(TotalUnits == 0, log(TotalUnits + 1), log(TotalUnits)),
         TotalEnterpriseAcres = log(TotalEnterpriseAcres),
         LaborHired = ifelse(LaborHired == 0, log(LaborHired + 1), log(LaborHired)),
         Others = ifelse(Others == 0, log(Others + 1), log(Others)),
         # SeedOtherExpense = ifelse(SeedOtherExpense == 0, log(SeedOtherExpense + 1), log(SeedOtherExpense)),
         Fertilizer = ifelse(Fertilizer == 0, log(Fertilizer + 1), log(Fertilizer)),
         MachineHire = ifelse(MachineHire == 0, log(MachineHire + 1), log(MachineHire))
         # MachineHire = ifelse(MachineHire == 0, log(MachineHire + 1), log(MachineHire)),
         # HerbicideInsecticide = ifelse(HerbicideInsecticide == 0, log(HerbicideInsecticide + 1), log(HerbicideInsecticide))
         ) %>%
  mutate(SpringHigh = ifelse(SpringHigh == 0, 1, SpringHigh)) %>%
  mutate_at(vars(WinterPrec:SpringFreeze), log) %>%
  rename(z1 = WinterPrec, z2 = WinterLow, z3 = WinterMedium, z4 = WinterHigh,
         z5 = FallPrec, z6 = FallLow, z7 = FallMedium, z8 = FallHigh,
         z9 = SpringPrec, z10 = SpringLow, z11 = SpringMedium, z12 = SpringHigh,
         z13 = WinterFreeze, z14 = FallFreeze, z15 = SpringFreeze)

summary(RegDatLog)
# Select data with more than 3 observations.
FarmSelect1 <- RegDatLog %>%
  group_by(FarmNbr) %>%
  summarise(num = n()) %>%
  dplyr::filter(num >= 4)

FarmSelect2 <- RegDatLog %>%
  select(1, 2) %>%
  group_by(FarmNbr, Year) %>%
  summarise(num = n())%>%
  dplyr::filter(num > 1)

RegDatLog <- filter(RegDatLog, FarmNbr %in% FarmSelect1$FarmNbr, !(FarmNbr %in% FarmSelect2$FarmNbr))

# Linear regression for yield.
Reg1 <- lm(log(AcreYield) ~ TotalEnterpriseAcres + LaborHired + Others + Fertilizer + factor(FarmNbr) + factor(Year),
           data = dplyr::filter(RegDatLog, AcreYield > 0))

Reg2 <- lm(log(AcreYield) ~ TotalEnterpriseAcres + LaborHired + Others + SeedOtherExpense + Fertilizer + MachineHire + HerbicideInsecticide +
             z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10 + z11 + z12 + z13 + z14 + z15 + factor(FarmNbr) + factor(Year),
           data = dplyr::filter(RegDatLog, AcreYield > 0))
Reg3 <- lm(log(AcreYield) ~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10 + z11 + z12 + z13 + z14 + z15 + factor(FarmNbr) + factor(Year),
           data = dplyr::filter(RegDatLog, AcreYield > 0))

summary(Reg1)
summary(Reg3)
coefficients(Reg1)[1:10]
coefficients(Reg2)[1:15]
coefficients(Reg3)[1:15]

# Linear regression for production.
Reg1 <- lm(TotalUnits ~ TotalEnterpriseAcres + LaborHired + Others + MachineHire + Fertilizer + factor(FarmNbr) + factor(Year),
           data = RegDatLog)

Reg2 <- lm(TotalUnits ~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10 + z11 + z12 + z13 + z14 + z15 + factor(FarmNbr) + factor(Year),
           data = RegDatLog)

summary(Reg1)
summary(Reg2)
coefficients(Reg1)[1:10]
coefficients(Reg2)[1:15]


### Now, get ready for PSFA analysis.
# Convert the variables into consistent names.
DifDat <- RegDatLog %>%
  as.data.frame() %>%
  group_by(FarmNbr, Year) %>%
  arrange(FarmNbr) %>%
  group_by(FarmNbr) %>%
  mutate(y = TotalUnits - dplyr::lag(TotalUnits),
         x1 = TotalEnterpriseAcres - dplyr::lag(TotalEnterpriseAcres),
         x2 = LaborHired - dplyr::lag(LaborHired),
         x3 = Others - dplyr::lag(Others),
         x4 = Fertilizer - dplyr::lag(Fertilizer),
         x5 = MachineHire - dplyr::lag(MachineHire)) %>%
  mutate_at(vars(z1:z15), funs(. - dplyr::lag(.))) %>%
  drop_na()

summary(DifDat)

Year <- length(unique(DifDat$Year))
Num <- length(unique(DifDat$FarmNbr))
ID <- unique(DifDat$FarmNbr)

## Log likelihood function for ALL cross-sectional unit.
maxlikelihood <- function(beta1, beta2, beta3, beta4, beta5,
                          delta1, delta2, delta3, delta4, delta5, delta6, delta7, delta8, delta9, delta10, delta11, delta12, delta13, delta14, delta15,
                          mu1, cv, cu){
  mu <- exp(mu1)
  sigma_v <- exp(cv)^0.5
  sigma_u <- exp(cu)^0.5

  DifDatFarmYear <- DifDat %>% select(1, 2)

  Temp_hvalue <- RegDatLog %>%   # Need to get the differenced h value from the original dataset. # This has to be the original data!!!
    mutate(hvalue  = exp(delta1*z1 + delta2*z2 + delta3*z3 + delta4*z4 + delta5*z5 +
                         delta6*z6 + delta7*z7 + delta8*z8 + delta9*z9 + delta10*z10 +
                         delta11*z11 + delta12*z12 + delta13*z13 + delta14*z14 + delta15*z15)) %>%
    as.data.frame() %>%
    group_by(FarmNbr, Year) %>%
    arrange(FarmNbr) %>%
    group_by(FarmNbr) %>%
    mutate(HvalueDif = hvalue - dplyr::lag(hvalue)) %>%
    ungroup() %>%
    right_join(., DifDatFarmYear, by = c('FarmNbr', 'Year'))

  Temp <- DifDat %>%
    ungroup() %>%
    mutate(epislon_value = y - beta1*x1 - beta2*x2 - beta3*x3- beta4*x4- beta5*x5,
           hvalue = Temp_hvalue$HvalueDif) # The H-value function must be positive

  JointLikelihood <- -sum(sapply(ID, function(i) LogLikelihoodFunc(i, Temp, mu, sigma_v, sigma_u)))
  #cat(beta1, delta1, mu, cv, cu, JointLikelihood, '||')
  cat(JointLikelihood, '-')
  return(JointLikelihood)
}

## Log likelihood function for SINGLE cross-sectional unit.
LogLikelihoodFunc <- function(i, Temp, mu, sigma_v, sigma_u){
  # As noted in p290 of Wang and Ho (2010), we modity T to Ti to run with unbalanced panel.
  SingleUnitDat <- dplyr::filter(Temp, FarmNbr == i) # Observation for one unit over time.
  Year_i <- nrow(SingleUnitDat) + 1 # Number of years observed for the unit.
  # Note that because the NA value is deleted, this Year_i actually equals to Year - 1, so I add 1 to it.

  epislon_m <- matrix(SingleUnitDat$epislon_value, 1, Year_i - 1) #
  hvalue_m <- matrix(SingleUnitDat$hvalue, 1, Year_i - 1) #

  # Construct the variance-covariance matrix (equation 12.)

  VarCovM <- matrix(0, nrow = Year_i - 1, ncol = Year_i - 1)
  diag(VarCovM) <- 2*sigma_v^2
  if(Year_i >= 4){
  diag(VarCovM[-(Year_i - 1), -1]) <- -sigma_v^2
  diag(VarCovM[-1, -(Year_i - 1)]) <- -sigma_v^2
  }else{
    VarCovM[2, 1] <- VarCovM[1, 2] <- -sigma_v^2
  }

  # Below is equation 14
  MuStar <- (mu/sigma_u^2 - epislon_m %*% tcrossprod(solve(VarCovM), hvalue_m))/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2)
  # Below is equation 15.
  SigmaStar <- (1/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2))^0.5
  # Below is equation 13.
  loglikelihood <- -0.5*(Year_i-1)*log(2*pi) - 0.5*log(Year_i) - 0.5*(Year_i-1)*log(sigma_v^2) - 0.5*epislon_m %*% tcrossprod(solve(VarCovM), epislon_m) +
    0.5*(MuStar^2/SigmaStar^2 - mu^2/sigma_u^2) + log(SigmaStar*pnorm(MuStar/SigmaStar)) - log(sigma_u*pnorm(mu/sigma_u))
  loglikelihood
}

# Maximum likelihood estimation.
fit <- mle2(maxlikelihood, start = list(beta1 = 0.9, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0,
                                        delta1 = 0.5, delta2 = 0.5, delta3 = 0.5, delta4 = 0.5, delta5 = 0.5,
                                        delta6 = 0, delta7 = 0, delta8 = 0, delta9 = 0, delta10 = 0,
                                        delta11 = 1, delta12 = 0, delta13 = 0, delta14 = 0, delta15 = 0,
                                        mu1 = -2, cv = -2, cu = -2),
            method = "BFGS", trace = FALSE)
# No data is specified in the function, and the functions call directly from the global environments.


# mu1, cv and cu are restricted to be positive.
test <- summary(fit)@coef
#The Monte Carlo analysis shows that the program successfully reproduce the results.
exp(coef(fit))

save(fit, file = './Temp/RegWeather.rda')


library(stargazer)

SummaryObject <- summary(fit)
EstOut <- data.frame(SummaryObject@coef) %>%
  mutate(Variable = row.names(.)) %>%
  mutate(VariableFullName = c('Land', 'Labor', 'Others', 'Fertilizer', 'Machinery',
                              'WinterPrec', 'WinterLow', 'WinterMedium', 'WinterHigh',
                              'FallPrec', 'FallLow', 'FallMedium', 'FallHigh',
                              'SpringPrec', 'SpringLow', 'SpringMedium', 'SpringHigh',
                              'WinterFreeze', 'FallFreeze', 'SpringFreeze', 'mu', 'cv', 'cu'))

library(ggplot2)

font.size <- 12

g1 <-
  ggplot(data = EstOut) +
  geom_point(aes(VariableFullName, Estimate)) +
  geom_errorbar(aes(VariableFullName, ymin = Estimate - 1.68*`Std..Error`, ymax = Estimate + 1.68*`Std..Error`), width = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = c('Land', 'Labor', 'Others', 'Fertilizer', 'Machinery')) +
  scale_y_continuous(limits = c(-0.5, 1.5)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = '', y = 'Estimate')

g1


g2 <-
  ggplot(data = filter(EstOut, grepl('delta', Variable))) +
  geom_point(aes(VariableFullName, Estimate)) +
  geom_errorbar(aes(VariableFullName, ymin = Estimate - 1.68*`Std..Error`, ymax = Estimate + 1.68*`Std..Error`), width = 0.2) +
  geom_hline(yintercept = 0) +
  # scale_x_discrete(limits = c('Land', 'Labor', 'Others', 'Fertilizer', 'Machinery')) +
  scale_y_continuous(limits = c(-5, 5)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = '', y = 'Estimate')

g2

# End of regression.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                         Analysis on regression results.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
rm(list=ls())
load('./Temp/RegWeatherReduced_NMopt.rda')
test <- summary(fit)@coef


length(unique(RegDatLog$FarmNbr))




CoefEst <- coef(fit)
# Calculate for the inefficiency values.
efficiency <- DifDat %>%
  mutate(hit = CoefEst['delta1']*x7)


Temp_hvalue <- dat %>%   # Need to get the differenced h value from the original dataset. # This has to be the original data.
  dplyr::filter(FarmNbr %in% dat0$FarmNbr) %>%
  mutate(hvalue  = exp(CoefEst['delta1']*z1)) %>%
  as.data.frame() %>%
  group_by(FarmNbr, Year) %>%
  arrange(FarmNbr) %>%
  group_by(FarmNbr) %>%
  mutate(HvalueDif = hvalue - dplyr::lag(hvalue)) %>%
  ungroup() %>%
  drop_na()

Temp <- DifDat %>%
  ungroup() %>%
  mutate(epislon_value = y - CoefEst['beta1']*x1 - CoefEst['beta2']*x2 - CoefEst['beta3']*x3-
                             CoefEst['beta4']*x4- CoefEst['beta5']*x5- CoefEst['beta6']*x6 - CoefEst['beta7']*x7,
         hvalue = Temp_hvalue$HvalueDif) # The H-value function must be positive

mu <- exp(CoefEst['mu1'])
sigma_v <- exp(CoefEst['cv'])^0.5
sigma_u <- exp(CoefEst['cu'])^0.5

getEfficiency <- function(i){
  # As noted in p290 of Wang and Ho (2010), we modity T to Ti to run with unbalanced panel.
  SingleUnitDat <- dplyr::filter(Temp, FarmNbr == i) # Observation for one unit over time.
  Year_i <- nrow(SingleUnitDat) + 1 # Number of years observed for the unit.
  # Note that because the NA value is deleted, this Year_i actually equals to Year - 1, so I add 1 to it.

  epislon_m <- matrix(SingleUnitDat$epislon_value, 1, Year_i - 1) #
  hvalue_m <- matrix(SingleUnitDat$hvalue, 1, Year_i - 1) #

  # Construct the variance-covariance matrix (equation 12.)

  VarCovM <- matrix(0, nrow = Year_i - 1, ncol = Year_i - 1)
  diag(VarCovM) <- 2*sigma_v^2
  if(Year_i >= 4){
    diag(VarCovM[-(Year_i - 1), -1]) <- -sigma_v^2
    diag(VarCovM[-1, -(Year_i - 1)]) <- -sigma_v^2
  }else{
    VarCovM[2, 1] <- VarCovM[1, 2] <- -sigma_v^2
  }

  # Below is equation 14
  MuStar <- (mu/sigma_u^2 - epislon_m %*% tcrossprod(solve(VarCovM), hvalue_m))/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2)
  # Below is equation 15.
  SigmaStar <- (1/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2))^0.5
  # Below is equation 13.
  hit = CoefEst['delta1']*SingleUnitDat$FixedCosts
  efficiency <- hit*(MuStar[1,1] + dnorm(MuStar[1,1]/SigmaStar[1,1])*SigmaStar[1,1]/pnorm(MuStar[1,1]/SigmaStar[1,1]))
  efficiency
}

EffEst <- lapply(ID, getEfficiency)
max(unlist(EffEst))
EffEstNorm <- sapply(EffEst, mean)/max(unlist(EffEst))

hist(unlist(EffEst)/max(unlist(EffEst)))

#-------------------------------------------------------------------------------------------------------------------#
# Test codes below.
#-------------------------------------------------------------------------------------------------------------------#
maxlikelihood(beta1 = 0.9, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0, beta7 = 0,
              delta1 = 1, delta2 = 0, delta3 = 0, delta4 = 0, delta5 = 0, delta6 = 0, delta7 = 0, delta8 = 0, delta9 = 0,
              mu1 = -2, cv = -2, cu = -2)

beta1 = 0.9; beta2 = 0; beta3 = 0; beta4 = 0; beta5 = 0; beta6 = 0; beta7 = 0;
delta1 = 1; delta2 = 0; delta3 = 0; delta4 = 0; delta5 = 0; delta6 = 0; delta7 = 0;delta8 = 0; delta9 = 0
mu1 = -2; cv = -2; cu = -2


i <- ID[7]
for(i in ID){
  # As noted in p290 of Wang and Ho (2010), we modity T to Ti to run with unbalanced panel.
  SingleUnitDat <- dplyr::filter(Temp, FarmNbr == i) # Observation for one unit over time.
  Year_i <- nrow(SingleUnitDat) + 1 # Number of years observed for the unit.
  # Note that because the NA value is deleted, this Year_i actually equals to Year - 1, so I add 1 to it.

  epislon_m <- matrix(SingleUnitDat$epislon_value, 1, Year_i - 1) #
  hvalue_m <- matrix(SingleUnitDat$hvalue, 1, Year_i - 1) #

  # Construct the variance-covariance matrix (equation 12.)
  VarCovM <- matrix(0, nrow = Year_i - 1, ncol = Year_i - 1)
  diag(VarCovM) <- 2*sigma_v^2
  if(Year_i >= 4){
    diag(VarCovM[-(Year_i - 1), -1]) <- -sigma_v^2
    diag(VarCovM[-1, -(Year_i - 1)]) <- -sigma_v^2
  }else{
    VarCovM[2, 1] <- VarCovM[1, 2] <- -sigma_v^2
  }

  # Below is equation 14
  MuStar <- (mu/sigma_u^2 - epislon_m %*% tcrossprod(solve(VarCovM), hvalue_m))/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2)
  # Below is equation 15.
  SigmaStar <- (1/(hvalue_m %*% tcrossprod(solve(VarCovM), hvalue_m) + 1/sigma_u^2))^0.5
  # Below is equation 13.
  loglikelihood <- -0.5*(Year_i-1)*log(2*pi) - 0.5*log(Year_i) - 0.5*(Year_i-1)*log(sigma_v^2) - 0.5*epislon_m %*% tcrossprod(solve(VarCovM), epislon_m) +
    0.5*(MuStar^2/SigmaStar^2 - mu^2/sigma_u^2) + log(SigmaStar*pnorm(MuStar/SigmaStar)) - log(sigma_u*pnorm(mu/sigma_u))
  print(i)
}

