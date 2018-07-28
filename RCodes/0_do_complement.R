# Reduce the number of parameters.
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
  mutate(HarvestLow = WinterLow + SpringLow,
         HarvestMedium = WinterMedium + SpringMedium,
         HarvestHigh = WinterHigh + SpringHigh,
         HarvestFreeze = WinterFreeze + SpringFreeze,
         HarvestPrec = WinterPrec + SpringPrec) %>%
  mutate_at(vars(WinterPrec:HarvestPrec), log) %>%
  rename(z1 = FallPrec, z2 = FallLow, z3 = FallMedium, z4 = FallHigh,
         z5 = HarvestPrec, z6 = HarvestLow, z7 = HarvestMedium, z8 = HarvestHigh,
         z9 = FallFreeze, z10 = HarvestFreeze)


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

Reg <- lm(TotalUnits ~ TotalEnterpriseAcres + LaborHired + Fertilizer + MachineHire + Others +
           z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10 + factor(FarmNbr) + factor(Year),
           data = dplyr::filter(RegDatLog, AcreYield > 0))

test <- summary(Reg)$coefficients
test

coefficients(Reg3)[1:15]


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
  mutate_at(vars(contains('z')), funs(. - dplyr::lag(.))) %>%
  drop_na()

summary(DifDat)

Year <- length(unique(DifDat$Year))
Num <- length(unique(DifDat$FarmNbr))
ID <- unique(DifDat$FarmNbr)

## Log likelihood function for ALL cross-sectional unit.
maxlikelihood <- function(beta1, beta2, beta3, beta4, beta5,
                          delta1, delta2, delta3, delta4, delta5, delta6, delta7, delta8, delta9, delta10,
                          mu1, cv, cu){
  mu <- exp(mu1)
  sigma_v <- exp(cv)^0.5
  sigma_u <- exp(cu)^0.5

  DifDatFarmYear <- DifDat %>% select(1, 2)

  Temp_hvalue <- RegDatLog %>%   # Need to get the differenced h value from the original dataset. # This has to be the original data!!!
    mutate(#hvalue  = exp(delta1*z1 + delta2*z2 + delta3*z3 + delta4*z4 + delta5*z5 +
           #                 delta6*z6 + delta7*z7 + delta8*z8 + delta9*z9 + delta10*z10)
           hvalue  = delta1*z1 + delta2*z2 + delta3*z3 + delta4*z4 + delta5*z5 +
                     delta6*z6 + delta7*z7 + delta8*z8 + delta9*z9 + delta10*z10
           ) %>%  # Specification of the efficiency function!!!!!
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
                                        mu1 = -2, cv = -2, cu = -2),
            method = "CG", trace = FALSE)

# hessian.opts = list(method = 'complex')
# No data is specified in the function, and the functions call directly from the global environments.
save(fit, file = './Temp/RegWeatherReduced_NMopt_Linear.rda')

# mu1, cv and cu are restricted to be positive.
summary(fit)
#The Monte Carlo analysis shows that the program successfully reproduce the results.
exp(coef(fit))


