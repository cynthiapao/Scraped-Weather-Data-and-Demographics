# Weather and Demographics
Kangxin Bao - kb2900  
December 11, 2017  

### 1.Data Acquistion and Preparation


```r
# data from us census
library(tidyr)
library(dplyr)
uscensus <- read.csv("DEC_00_SF1_DP1/DEC_00_SF1_DP1_with_ann.csv", header = TRUE)[, 3:53]
uscensus <-  uscensus %>% 
  separate(Geography, into = c("Cities", "States"), sep = ", ")

library(stringr)
uscensus$Cities <- gsub(" city", "", uscensus$Cities, fixed=TRUE)
uscensus$Cities <- gsub(" (balance)", "", uscensus$Cities, fixed=TRUE)
uscensus$Cities <- gsub("-Davidson", "", uscensus$Cities, fixed=TRUE)
colnames(uscensus) <- str_replace(colnames(uscensus), ".Total.population...SEX.AND.AGE...","")
```


```r
# data from 50states website
library(httr)
library(rvest)
library(xml2)
url <- "https://www.50states.com/abbreviations.htm"

state <- read_html(url)
state_text <- html_nodes(state, "td") %>%
  html_text()

state_df <- as.data.frame(matrix(state_text, nrow = 65, ncol = 2, byrow = T))
colnames(state_df) <- c("States", "States_Abb")
uscensus <- left_join(uscensus, state_df, by = "States")
```



```r
# data from Weather Underground API
citylist <- str_replace(uscensus$Cities, " ", "_")
statelist <- as.character(uscensus$States_Abb)

GetWeatherInfo <- function(city, state){
  url <- paste0("http://api.wunderground.com/api/781b41f91352ee19/conditions/q", "/",
state, "/", city, ".json")
  
  r <- GET(url)
  http_status(r)
  df <- content(r, as = "parsed")
  
  data <- c(city, state, df$current_observation$observation_time, df$current_observation$weather,
            df$current_observation$temperature_string, df$current_observation$wind_string,
            df$current_observation$wind_dir, df$current_observation$precip_today_string)
  return(data)
}

weather <- data.frame(matrix(NA, nrow = 50, ncol = 8))
for (i in 1:50){
  city <- citylist[i]
  state <- statelist[i]
  weather[i, ] <- GetWeatherInfo(city, state)
  Sys.sleep(10)
}
colnames(weather) <- c("city", "state", "obstime", "weather", "temperature", "wind", "wind_dir", "precipitation")
weather$temperature_F <- weather$temperature %>% str_extract("\\d+\\.*\\d*") %>% as.numeric
weather$city <- str_replace(weather$city, "_", " ")
```

### 2.Data Cleaning


```r
# for all cities
library(tidyverse)
citydata <- uscensus %>%
  left_join(weather, by = c("Cities" = "city")) %>%
  select(Cities, States, States_Abb, Number..Total.population, Number.Median.age..years., 
         Number.Male, Percent.Male, Number.Female, Percent.Female, 
         Number.Under.5.years, Percent.Under.5.years,
         Number.65.years.and.over, Percent.65.years.and.over,
         weather, temperature_F, wind) %>%
  arrange(desc(Number..Total.population))
save(citydata, file="city_demoinfo.Rda")
```


```r
# function for select cities or states
select_city_state <- function(city=NULL, state.abb=NULL){
  if ((!is.null(city)) && (!is.null(state.abb))){
    data <- citydata %>% filter(Cities == city & States_Abb == state.abb)
  } else if ((is.null(city)) && (!is.null(state.abb))){
    data <-  citydata %>% filter(States_Abb == state.abb)
  } else if ((!is.null(city)) && (is.null(state.abb))){
    data <-  citydata %>% filter(Cities == city)
  } else
    print("Error: at least one argument.")
  return(data)
}
select_city_state(state.abb = "CA")
```

```
##          Cities     States States_Abb Number..Total.population
## 1   Los Angeles California         CA                  3694820
## 2     San Diego California         CA                  1223400
## 3      San Jose California         CA                   894943
## 4 San Francisco California         CA                   776733
## 5    Long Beach California         CA                   461522
## 6        Fresno California         CA                   427652
## 7    Sacramento California         CA                   407018
## 8       Oakland California         CA                   399484
##   Number.Median.age..years. Number.Male Percent.Male Number.Female
## 1                      31.6     1841805         49.8       1853015
## 2                      32.5      616884         50.4        606516
## 3                      32.6      454798         50.8        440145
## 4                      36.5      394828         50.8        381905
## 5                      30.8      226718         49.1        234804
## 6                      28.5      210107         49.1        217545
## 7                      32.8      197784         48.6        209234
## 8                      33.3      192757         48.3        206727
##   Percent.Female Number.Under.5.years Percent.Under.5.years
## 1           50.2               285976                   7.7
## 2           49.6                82523                   6.7
## 3           49.2                68243                   7.6
## 4           49.2                31633                   4.1
## 5           50.9                38587                   8.4
## 6           50.9                38996                   9.1
## 7           51.4                29066                   7.1
## 8           51.7                28292                   7.1
##   Number.65.years.and.over Percent.65.years.and.over       weather
## 1                   357129                       9.7 Mostly Cloudy
## 2                   128008                      10.5      Overcast
## 3                    73860                       8.3 Mostly Cloudy
## 4                   106111                      13.7 Partly Cloudy
## 5                    41902                       9.1      Overcast
## 6                    39574                       9.3         Clear
## 7                    46443                      11.4         Clear
## 8                    41788                      10.5 Mostly Cloudy
##   temperature_F                                        wind
## 1          62.6  From the WNW at 4.0 MPH Gusting to 9.0 MPH
## 2          64.4                                        Calm
## 3          59.8  From the SSW at 4.0 MPH Gusting to 4.0 MPH
## 4          54.5 From the SSW at 9.0 MPH Gusting to 17.0 MPH
## 5          64.2 From the East at 1.0 MPH Gusting to 5.0 MPH
## 6          79.0 From the West at 5.4 MPH Gusting to 7.6 MPH
## 7          66.1   From the SW at 5.2 MPH Gusting to 7.5 MPH
## 8          57.9   From the SW at 2.0 MPH Gusting to 4.0 MPH
```

```r
# function for running a model              
fit_reg <- function(dv, iv, df, intercept=TRUE) {
  if((is.numeric(dv)) && (intercept=TRUE)){
    lm <- lm(dv ~ iv, data = df) 
  }else if((is.numeric(dv)) && (intercept=FALSE)){
    lm <- lm(dv ~ iv - 1, data = df) 
  }else{
    print("Error: wrong data type.")
  }
  return(summary(lm))
}

fit_reg(citydata$Number.Under.5.years, citydata$temperature_F, citydata)
```

```
## 
## Call:
## lm(formula = dv ~ iv, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -63314 -38304 -22188  -4550 463912 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   136161      76273   1.785   0.0805 .
## iv             -1021       1066  -0.957   0.3433  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 84890 on 48 degrees of freedom
## Multiple R-squared:  0.01873,	Adjusted R-squared:  -0.001717 
## F-statistic: 0.916 on 1 and 48 DF,  p-value: 0.3433
```


```r
# function for calculate sex ratio             
sex_age_ratio <- function(sex=TRUE, age=TRUE) {
  stopifnot(is.logical(sex), is.logical(age))
  
  if((isTRUE(sex)) && (isTRUE(age))){
    ratio <- citydata %>%
      mutate(Sex_ratio = Number.Male/Number.Female*100,
             Age_ratio = Percent.65.years.and.over) %>%
      select(Cities, States, States_Abb, Sex_ratio, Age_ratio)
    
  }else if((isTRUE(sex)) && (!isTRUE(age))){
    ratio <- citydata %>%
      mutate(Sex_ratio = Number.Male/Number.Female*100) %>%
      select(Cities, States, States_Abb, Sex_ratio)
    
  }else if((!isTRUE(sex)) && (isTRUE(age))){
    ratio <- citydata %>%
      mutate(Age_ratio = Percent.65.years.and.over) %>%
      select(Cities, States, States_Abb, Age_ratio)
  }
  return(ratio)
}

sex_age_ratio(sex = T, age = F)
```

```
##              Cities               States States_Abb Sex_ratio
## 1          New York             New York         NY  90.03648
## 2       Los Angeles           California         CA  99.39504
## 3           Chicago             Illinois         IL  94.24499
## 4           Houston                Texas         TX  99.74143
## 5      Philadelphia         Pennsylvania         PA  86.78849
## 6           Phoenix              Arizona         AZ 103.46150
## 7         San Diego           California         CA 101.70944
## 8            Dallas                Texas         TX 101.59467
## 9       San Antonio                Texas         TX  93.54820
## 10          Detroit             Michigan         MI  89.13771
## 11         San Jose           California         CA 103.32913
## 12     Indianapolis              Indiana         IN  93.74319
## 13    San Francisco           California         CA 103.38383
## 14     Jacksonville              Florida         FL  93.92381
## 15         Columbus                 Ohio         OH  94.60765
## 16           Austin                Texas         TX 105.82333
## 17        Baltimore             Maryland         MD  87.40024
## 18          Memphis            Tennessee         TN  89.83405
## 19        Milwaukee            Wisconsin         WI  91.57668
## 20           Boston        Massachusetts         MA  92.81139
## 21       Washington District of Columbia         DC  88.98983
## 22          El Paso                Texas         TX  90.41927
## 23          Seattle           Washington         WA  99.49434
## 24           Denver             Colorado         CO 102.10546
## 25        Nashville            Tennessee         TN  93.84072
## 26        Charlotte       North Carolina         NC  96.05873
## 27       Fort Worth                Texas         TX  97.32299
## 28         Portland               Oregon         OR  97.76084
## 29    Oklahoma City             Oklahoma         OK  95.55442
## 30           Tucson              Arizona         AZ  96.01959
## 31        Las Vegas               Nevada         NV 103.28012
## 32        Cleveland                 Ohio         OH  89.95327
## 33       Long Beach           California         CA  96.55628
## 34      Albuquerque           New Mexico         NM  94.43785
## 35      Kansas City             Missouri         MO  93.31754
## 36           Fresno           California         CA  96.58094
## 37   Virginia Beach             Virginia         VA  98.03989
## 38          Atlanta              Georgia         GA  98.55828
## 39       Sacramento           California         CA  94.52766
## 40          Oakland           California         CA  93.24230
## 41             Mesa              Arizona         AZ  98.19047
## 42            Tulsa             Oklahoma         OK  93.51343
## 43            Omaha             Nebraska         NE  95.02788
## 44      Minneapolis            Minnesota         MN 100.96961
## 45            Miami              Florida         FL  98.85778
## 46 Colorado Springs             Colorado         CO  97.83358
## 47          Wichita               Kansas         KS  97.09411
## 48        Arlington                Texas         TX  99.97658
## 49          Raleigh       North Carolina         NC  97.99419
## 50       Louisville             Kentucky         KY  89.69114
```
