# super_dea <- function(dat){  #This is a function to get data including the super dea values
#   #1. rename column names
#   dat <- rename(dat,output=v274,feed=v075,labor=v289,utilities=v305,veterinary=v298,capital=v296,miscellaneous=v297,
#                 age=v013,beet_cows=v344,leverage=v980,income_ratio=iratio,acres_owned=aratio)
#   #2. get super dea value
#   sdea_dat <- select(dat,feed,labor,utilities,veterinary,capital,miscellaneous)
#   vrs <- sdea(sdea_dat, dat$output, RTS='vrs', ORIENTATION = "in")
#   crs <- sdea(sdea_dat, dat$output, RTS='crs', ORIENTATION = "in")
#   #insert super dea values into dataset
#   dat$vrs <- vrs$eff
#   dat$crs <- crs$eff
#   #convert infinite value to be 1
#   dat[which(is.infinite(dat$vrs)),]$vrs <- 1
#
#   #3. group efficient farms and inefficient farms
#   dat$group_vrs <- NA
#   dat$group_crs <- NA
#
#   for (i in 1:length(dat$vrs)){
#     ifelse(dat$vrs[i] >= 1,dat$group_vrs[i]<-1,dat$group_vrs[i]<-0)
#   }
#   for (i in 1:length(dat$crs)){
#     ifelse(dat$crs[i] >= 1,dat$group_crs[i]<-1,dat$group_crs[i]<-0)
#   }
#   return(dat)
# }


# Growing season data.
weather_func <- function(year_sel, state_sel, growingSeason = NULL, seasonBreak = FALSE, threshold){
  cat('Start for State', state_sel, 'in', year_sel, '\n')
  start_time <- Sys.time()
  if(isTRUE(seasonBreak)){
    climate_dat_first <- readstata13::read.dta13(paste0('D:/07_Research/0_Research_Record/Dissertation_0516/5_Essay_Production/2_Data/RawWeatherData/year', year_sel, '/state', state_sel, '.dta'))
    climate_dat_second <- readstata13::read.dta13(paste0('D:/07_Research/0_Research_Record/Dissertation_0516/5_Essay_Production/2_Data/RawWeatherData/year', year_sel + 1, '/state', state_sel, '.dta'))

    climate_dat_first <- climate_dat_first %>%
      mutate(Year = lubridate::year(dateNum),
             Mon = lubridate::month(dateNum)) %>%
      dplyr::filter(Mon >= growingSeason[1])

    climate_dat_second <- climate_dat_second %>%
      mutate(Year = lubridate::year(dateNum),
             Mon = lubridate::month(dateNum),
             Year = Year - 1) %>%
      dplyr::filter(Mon <= growingSeason[2])

   climate_dat2 <- bind_rows(climate_dat_first, climate_dat_second) %>%
      inner_join(., CropArea, by = 'gridNumber') %>% # Keep matching rows
      dplyr::filter(cropArea > 0) %>% # keep positive weights
      mutate(tAvg = (tMin+tMax)/2)
    }else{

  climate_dat <- readstata13::read.dta13(paste0('D:/07_Research/0_Research_Record/Dissertation_0516/5_Essay_Production/2_Data/RawWeatherData/year', year_sel, '/state', state_sel, '.dta'))

  climate_dat2 <- climate_dat  %>%
      mutate(Year = lubridate::year(dateNum),
             Mon = lubridate::month(dateNum)) %>%
      dplyr::filter(Mon >= growingSeason[1] & Mon <= growingSeason[2]) %>%
      inner_join(., CropArea, by = 'gridNumber') %>% # Keep matching rows
      dplyr::filter(cropArea > 0) %>% # keep positive weights
      mutate(tAvg = (tMin+tMax)/2)
  }

  for (i in threshold){  #If change this, line 32 and line 34 for "bin_out" function shall change.
    climate_dat2 <- climate_dat2 %>%
      mutate(tempSave = suppressWarnings(acos((2*i-tMax-tMin)/(tMax-tMin))), # this could be NA, once the value is greater than 1 or less than 01.
             !!paste0('dday', i, 'C') := case_when(i <= tMin ~ tAvg - i,  # if bound <= tMin
                                                   i >= tMax ~ 0,  # if bound >= tMax
                                                   TRUE ~ ((tAvg-i)*tempSave + (tMax-tMin)*sin(tempSave)/2)/base::pi)) # if at the between.
  } # These are constructed based on D'Agostino and Schlenker (2016), p162.
  climate_dat2 <- climate_dat2 %>%
    mutate(tempSave2 = suppressWarnings(acos((2*0-tMax-tMin)/(tMax-tMin))),
           FreezingDays = case_when(0 <= tMin ~ 0,  # if bound <= tMin
                                    0 >= tMax ~ 0 - tAvg,  # if bound >= tMax
                                    TRUE ~ (0-tAvg)*(1-tempSave2/base::pi) + (tMax-tMin)*sin(tempSave2)/(2*base::pi)))  # FreezingDays below 9 Celsius degree.

  # Collapse by aggregation level;
  temp_out <- climate_dat2 %>%
    group_by(dateNum, Year, fips) %>%
    summarise_at(vars(tMin,tMax, tAvg), funs(weighted.mean(., cropArea))) %>%
    group_by(Year, fips) %>%
    summarise_at(vars(tMin, tMax, tAvg), mean)

  bin_out <- climate_dat2 %>%
    group_by(dateNum, Year, fips) %>%
    summarise_at(vars(prec, dday0C:FreezingDays), funs(weighted.mean(., cropArea))) %>%
    group_by(Year, fips) %>%
    summarise_at(vars(prec, dday0C:FreezingDays), sum)

  dat_out <- temp_out %>%
    full_join(., bin_out, by = c('Year', 'fips')) %>%
    mutate(state_id = state_sel,
           Month = paste0(growingSeason[1], '-', growingSeason[2]))

  write.csv(dat_out, file = paste0('./Data/WeatherData/agg', year_sel, state_sel, 'Month', growingSeason[1], '-',growingSeason[2],'.csv'), row.names = FALSE)
  cat('Finished for State', state_sel, 'in', year_sel, '\n')
  end_time <- Sys.time()
  cat('Time Cost: ', difftime(end_time, start_time, units = 'mins'), '\n')
}
