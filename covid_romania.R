library(writexl)
library(RCurl)

time_series_covid19_confirmed_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
time_series_covid19_confirmed_global <- read.csv(text = time_series_covid19_confirmed_global)

time_series_covid19_recovered_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
time_series_covid19_recovered_global <- read.csv(text = time_series_covid19_recovered_global)

time_series_covid19_deaths_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
time_series_covid19_deaths_global <-read.csv(text = time_series_covid19_deaths_global)

rowRomaniaConfirmed<-which(time_series_covid19_confirmed_global[,2]=='Romania')
rowRomaniaConfirmed
confirmedRomania <- as.numeric(time_series_covid19_confirmed_global[rowRomaniaConfirmed,5:ncol(time_series_covid19_confirmed_global)])
rownames(confirmedRomania)<-NULL
names(confirmedRomania)<-NULL


rowRomaniaRecovered<-which(time_series_covid19_recovered_global[,2]=='Romania')
rowRomaniaRecovered
recoveredRomania <- as.numeric(time_series_covid19_recovered_global[rowRomaniaRecovered,5:ncol(time_series_covid19_recovered_global)])
rownames(recoveredRomania)<-NULL
names(recoveredRomania)<-NULL


rowRomaniaDeaths<-which(time_series_covid19_deaths_global[,2]=='Romania')
rowRomaniaDeaths
deathsRomania <- as.numeric(time_series_covid19_deaths_global[rowRomaniaDeaths,5:ncol(time_series_covid19_deaths_global)])
rownames(deathsRomania)<-NULL
names(deathsRomania)<-NULL


covid_romania<-data.frame(confirmed=confirmedRomania, recovered=recoveredRomania,deaths=deathsRomania)
dt1 <- as.Date("01/22/2020", format = "%m/%d/%Y")
interval<-seq(from = dt1, length=length(confirmedRomania), by="day")
covid_romania<-cbind(interval,covid_romania)
write_xlsx(covid_romania,"cov.xlsx")
