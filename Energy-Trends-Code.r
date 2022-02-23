# Dependencies
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(assertr)
library(mgcv)

#install.packages('assertr')
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(assertr)
library(mgcv)

owid_energy_data = read_excel("C:/Users/mdyer/Downloads/owid-energy-data.xlsx")
head(owid_energy_data)
str(owid_energy_data)
summary(owid_energy_data)

myvars = c("iso_code", "country", "year", "biofuel_electricity", "coal_electricity",
           "gas_electricity","hydro_electricity", "nuclear_electricity", "oil_electricity",
           "other_renewable_electricity", "solar_electricity", "wind_electricity", "population",
           "gdp")

energy = owid_energy_data[myvars]
head(energy)
str(energy)
summary(energy)

energy = energy %>% dplyr::rename(
                      biofuel = biofuel_electricity,
                      coal = coal_electricity,
                      gas = gas_electricity,
                      hydro = hydro_electricity,
                      nuclear = nuclear_electricity,
                      oil = oil_electricity,
                      other_renewable = other_renewable_electricity,
                      solar = solar_electricity,
                      wind = wind_electricity,
                      pop = population
                  ) 
energy = energy %>% melt(id.vars = c("iso_code", "country", "year", "pop", "gdp"))
energy = energy %>% dplyr::rename(type = variable, output = value)
head(energy)

usa = energy %>% filter(country == 'United States')
#summary(usa)
#str(usa)
#head(usa)

#No energy data for first 85 years
usa = usa %>% filter(year > 1984)
summary(usa)

#Still 15 years of missing biofuel data

area_plt = ggplot(data = usa, mapping = aes(x=year,y=output,fill=type)) +
            geom_area() +
            ggtitle("USA Energy Production") +
            xlab("Year") +
            ylab("Output (Terawatt-Hours)")
area_plt

usa_2020 = usa %>% filter(year == 2020)
bar_graph = ggplot(usa_2020, mapping = aes(type,output)) +
            geom_col() +
            ggtitle("USA Energy Production in 2020") +
            xlab("Energy Source") +
            ylab("Output(Terrawatt-Hours)") +
            theme_bw()
bar_graph

#Can GDP be used to predict energy output?

energy = energy %>% mutate(gdp_per_cap = gdp/pop)
#head(energy)

par(mfrow = c(2,2))
energy_2010 = energy %>% filter(year == 2016)
energy_2010 %>% filter(type == 'biofuel' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Biofuel",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita",
                                                    ))
energy_2010 %>% filter(type == 'biofuel' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'coal' &  gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Coal",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'coal' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'gas' &  gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Gas",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'gas' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'hydro' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Hydro",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'hydro' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'nuclear' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Nuclear",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'nuclear' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'oil' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Oil",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'oil' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'solar' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Solar",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'solar' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'wind' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Wind",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'wind' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'other_renewable' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Other Renewables",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
energy_2010 %>% filter(type == 'other_renewable' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))

#Correlations
cat("Correlation with electrical output:")
cat("\n Biofuel:", energy_2010 %>% filter(type == 'biofuel' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Coal:", energy_2010 %>% filter(type == 'coal' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Gas:", energy_2010 %>% filter(type == 'gas' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Hydro:", energy_2010 %>% filter(type == 'hydro' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Nuclear:", energy_2010 %>% filter(type == 'nuclear' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Oil:", energy_2010 %>% filter(type == 'oil' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Solar:", energy_2010 %>% filter(type == 'solar' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Wind:", energy_2010 %>% filter(type == 'wind' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))
cat("\n Other Renewables:", energy_2010 %>% filter(type == 'other_renewable' & gdp_per_cap>0 & output>0) %>% with(cor(gdp_per_cap,output/pop)))

past_decade_US = usa %>% filter(year >= 2010 & year <= 2020)

ggplot(data = past_decade_US, mapping = aes(x = year, y = output, group = type, col = type)) +
    ggtitle("Trends in US Energy Output in Last 10 Years") +
    xlab("Year") +
    ylab("Output (Terrawatt-Hours)") +
    geom_line() +
    scale_x_continuous(breaks=seq(2010,2020,1))

#First World Countries
list_first_wrld = energy %>% filter(year == 2016 & gdp_per_cap >= 25000)
list_first_wrld = unique(list_first_wrld)$country
first_wrld = energy %>% filter(year >= 2010 & year <2020 & country %in% list_first_wrld)
first_wrld = first_wrld %>% mutate(yrs_after = year - 2010)
wrld_ave = first_wrld %>% group_by(year,type) %>% summarise(mean_output = mean(output,na.rm = TRUE))

ggplot(data = wrld_ave, mapping = aes(x = year, y = mean_output, group = type, col = type)) +
    ggtitle("Trends in First World Energy Output in Last 10 Years") +
    xlab("Year") +
    ylab("Output (Terrawatt-Hours)") +
    geom_line() +
    scale_x_continuous(breaks=seq(2000,2020,5))

past_decade_US = past_decade_US %>% mutate(yrs_after = year - 2010)
past_wrld = past_wrld %>% mutate(yrs_after = year - 2010)

lm_biofuel_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'biofuel'))
lm_coal_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'coal'))
lm_gas_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'gas'))
lm_hydro_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'hydro'))
lm_nuclear_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'nuclear'))
lm_oil_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'oil'))
lm_other_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'other_renewable'))
lm_solar_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'solar'))
lm_wind_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'wind'))

lm_biofuel_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'biofuel'))
lm_coal_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'coal'))
lm_gas_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'gas'))
lm_hydro_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'hydro'))
lm_nuclear_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'nuclear'))
lm_oil_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'oil'))
lm_other_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'other_renewable'))
lm_solar_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'solar'))
lm_wind_wrld =  lm(mean_output ~ yrs_after, data = past_wrld %>% filter(type == 'wind'))

cat("Significant P-values for US Model:")
cat("\n Biofuel:", summary(lm_biofuel_us)$coefficients[2,4] < 0.05)
cat("\n Coal:", summary(lm_coal_us)$coefficients[2,4] < 0.05)
cat("\n Gas:", summary(lm_gas_us)$coefficients[2,4] < 0.05)
cat("\n Hydro:", summary(lm_hydro_us)$coefficients[2,4] < 0.05)
cat("\n Nuclear:", summary(lm_nuclear_us)$coefficients[2,4] < 0.05)
cat("\n Oil:", summary(lm_oil_us)$coefficients[2,4] < 0.05)
cat("\n Other Renewable:", summary(lm_other_us)$coefficients[2,4] < 0.05)
cat("\n Solar:", summary(lm_solar_us)$coefficients[2,4] < 0.05)
cat("\n Wind:", summary(lm_wind_us)$coefficients[2,4] < 0.05)

cat("\n\n Significant P-values for First World Model:")
cat("\n Biofuel:", summary(lm_biofuel_wrld)$coefficients[2,4] < 0.05)
cat("\n Coal:", summary(lm_coal_wrld)$coefficients[2,4] < 0.05)
cat("\n Gas:", summary(lm_gas_wrld)$coefficients[2,4] < 0.05)
cat("\n Hydro:", summary(lm_hydro_wrld)$coefficients[2,4] < 0.05)
cat("\n Nuclear:", summary(lm_nuclear_wrld)$coefficients[2,4] < 0.05)
cat("\n Oil:", summary(lm_oil_wrld)$coefficients[2,4] < 0.05)
cat("\n Other Renewable:", summary(lm_other_wrld)$coefficients[2,4] < 0.05)
cat("\n Solar:", summary(lm_solar_wrld)$coefficients[2,4] < 0.05)
cat("\n Wind:", summary(lm_wind_wrld)$coefficients[2,4] < 0.05)

unpooled_US = lm(output ~ type + yrs_after:type, data = past_decade_US)
summary(unpooled_US)
par(mfrow = c(2,2))
plot(unpooled_US)
hist(resid(unpooled_US), main = "Residuals Histogram", xlab = "Residuals" )

past_decade_US[c(14,16,25),]

unpooled_US = lm(output ~ type + yrs_after:type, data = past_decade_US)
summary(unpooled_US)
pooled = lm(output/pop ~ type + country + yrs_after:type, data = first_wrld)
par(mfrow = c(2,2))
plot(pooled)
hist(resid(pooled), main = "Residuals Histogram", xlab = "Residuals" )

newdata = data.frame(country = rep('United States',30), yrs_after = 1:30, type = 'solar')
pool_preds = predict(pooled,newdata)
unpooled_preds = predict(unpooled_US,newdata)

# GAM Model
# Make years start from 0.
usa = usa %>% mutate(yrs_after = year - 1985)

#GAM models
#Biofuel not enough data points
gam_coal =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'coal'))
gam_gas =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'gas'))
gam_hydro =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'hydro'))
gam_nuclear =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'nuclear'))
gam_oil =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'oil'))
gam_other =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'other_renewable'))
gam_solar =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'solar'))
gam_wind =  gam(output ~ s(yrs_after), data = usa_gam %>% filter(type == 'wind'))

par(mfrow = c(2,2))
plot(gam_coal, main = "Coal", xlab = "Years")
plot(gam_gas,  main = "Gas", xlab = "Years")
plot(gam_hydro, main = "Hydro", xlab = "Years")
plot(gam_nuclear, main = "Nuclear", xlab = "Years")
plot(gam_oil, main = "Oil", xlab = "Years")
plot(gam_other, main = "Other", xlab = "Years")
plot(gam_solar, main = "Solar", xlab = "Years")
plot(gam_wind, main = "Wind", xlab = "Years")


d_predict = data.frame(yrs_after = c(35:65))

gam_coal_preds = predict(gam_coal,d_predict)
coal_adj = usa_2020 %>% filter(type == 'coal') %>% select(output) - gam_coal_preds[1]

gam_coal_preds = gam_coal_preds + coal_adj

gam_gas_preds = predict(gam_gas,d_predict)
gas_adj = usa_2020 %>% filter(type == 'gas') %>% select(output)
gam_gas_preds = gam_gas_preds + gas_adj

gam_solar_preds = predict(gam_solar,d_predict)
solar_adj = usa_2020 %>% filter(type == 'solar') %>% select(output)
gam_solar_preds = gam_solar_preds + solar_adj

gam_wind_preds = predict(gam_wind,d_predict)
wind_adj = usa_2020 %>% filter(type == 'wind') %>% select(output)
gam_wind_preds = gam_wind_preds + wind_adj

gam_preds = data.frame(d_predict, gam_coal_preds, gam_gas_preds, gam_solar_preds, gam_wind_preds)
gam_preds

gam_area_plt = area_plt
                       

lm_solar_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'solar'))
lm_wind_us =  lm(output ~ yrs_after, data = past_decade_US %>% filter(type == 'wind'))
lm_solar_exp =  lm(output ~ exp(yrs_after), data = past_decade_US %>% filter(type == 'solar'))
lm_wind_exp =  lm(output ~ exp(yrs_after), data = past_decade_US %>% filter(type == 'wind'))
summary(lm_solar_us)
summary(lm_solar_exp)
summary(lm_wind_us)
summary(lm_wind_exp)

newpred = data.frame(yrs_after = c(11:25))
solar_preds = predict(lm_solar_us, newdata = newpred, interval = 'predict')
solar_preds
wind_preds = predict(lm_wind_us, newdata = newpred, interval = 'predict')
wind_preds



total_elec = owid_energy_data %>% filter(country == "United States")
total_elec = total_elec[c("country","year","electricity_generation")]
total_elec = total_elec %>% filter(year >=1985)

total_plt = ggplot(data = total_elec, mapping = aes(x = year, y = electricity_generation)) +
            geom_line()
total_plt
total_elec_modern = total_elec %>% filter(year >=2005)
total_plt = ggplot(data = total_elec_modern, mapping = aes(x = year, y = electricity_generation)) +
            geom_line() +
            geom_smooth()
total_plt

#WLS
wts = 1 / lm(abs(unpooled_US$residuals) ~ unpooled_US$fitted.values)$fitted.values^2
unpooled_US = lm(output ~ type + yrs_after:type, data = past_decade_US)

lm_wls =  lm(output ~ type + yrs_after:type, weights = wts, data = past_decade_US)

par(mfrow = c(2,2))
summary(lm_wls)
plot(lm_wls)

past_decade_US = past_decade_US[-79,]
unpooled_US = lm(output ~ type + yrs_after:type, data = past_decade_US)
wts = 1 / lm(abs(unpooled_US$residuals) ~ unpooled_US$fitted.values)$fitted.values^2
lm_wls =  lm(output ~ type + yrs_after:type, weights = wts, data = past_decade_US)
summary(lm_wls)

#Predictions
coal_2035 = 0
gas_2035 = 0
oil_2035 = 0
hydro_2035 = usa %>% filter(year == 2020 & type == 'hydro') %>% select(output)
biofuel_2035 = usa %>% filter(year == 2020 & type == 'biofuel') %>% select(output)
nuclear_2035 = usa %>% filter(year == 2020 & type == 'nuclear') %>% select(output)
other_2035 = usa %>% filter(year == 2020 & type == 'other_renewable') %>% select(output)
solar_2035 = predict(lm_wls, newdata = data.frame(type = "solar", yrs_after = 35),interval = 'predict')
wind_2035 =  predict(lm_wls, newdata = data.frame(type = "wind", yrs_after = 35), interval = 'predict')


cat("2035 Predictions:",
   "\n Coal:", coal_2035,
    "\n Gas:", gas_2035,
    "\n Oil:", oil_2035,
    "\n Hydro:", hydro_2035$output,
    "\n Biofuel:", biofuel_2035$output,
    "\n Nuclear:", nuclear_2035$output,
    "\n Other:", other_2035$output,
    "\n Solar:", solar_2035,
    "\n Wind:", wind_2035,
    "\n Sum:", (coal_2035 + 
                   gas_2035 + 
                   oil_2035 + 
                   hydro_2035$output +  
                   nuclear_2035$output + 
                   biofuel_2035$output +
                   other_2035$output + 
                   solar_2035 + 
                   wind_2035))


par(mfrow = c(2,2))
energy_2010 %>% filter(type == 'biofuel' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Biofuel",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita",
                                                    ))
biofuel_gdp = energy_2010 %>% filter(type == 'biofuel' & gdp_per_cap>0 & output>0)
biofuel_lm = lm(output/pop ~ gdp_per_cap, data = biofuel_gdp)
abline(biofuel_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'biofuel' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'hydro' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Hydro",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
hydro_gdp = energy_2010 %>% filter(type == 'hydro' & gdp_per_cap>0 & output>0)
hydro_lm = lm(output/pop ~ gdp_per_cap, data = hydro_gdp)
abline(hydro_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'hydro' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'nuclear' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Nuclear",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
nuclear_gdp = energy_2010 %>% filter(type == 'nuclear' & gdp_per_cap>0 & output>0)
nuclear_lm = lm(output/pop ~ gdp_per_cap, data = nuclear_gdp)
abline(nuclear_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'nuclear' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'solar' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Solar",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
solar_gdp = energy_2010 %>% filter(type == 'solar' & gdp_per_cap>0 & output>0)
solar_lm = lm(output/pop ~ gdp_per_cap, data = solar_gdp)
abline(solar_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'solar' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'wind' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Wind",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
wind_gdp = energy_2010 %>% filter(type == 'wind' & gdp_per_cap>0 & output>0)
wind_lm = lm(output/pop ~ gdp_per_cap, data = wind_gdp)
abline(wind_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'wind' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))
energy_2010 %>% filter(type == 'other_renewable' & gdp_per_cap>0 & output>0) %>% with(plot(gdp_per_cap,output/pop,
                                                    main = "Other Renewables",
                                                    xlab = "GDP Per Capita",
                                                    ylab = "Output (Terrawatt-Hours) Per Capita"))
other_gdp = energy_2010 %>% filter(type == 'other_renewable' & gdp_per_cap>0 & output>0)
other_lm = lm(output/pop ~ gdp_per_cap, data = other_gdp)
abline(other_lm, type = 'dashed', col = 'blue')
energy_2010 %>% filter(type == 'other_renewable' & country == 'United States') %>% with(points(gdp_per_cap,output/pop, pch = 19, col='red'))


