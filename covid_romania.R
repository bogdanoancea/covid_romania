library(writexl)
library(RCurl)

time_series_covid19_confirmed_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
time_series_covid19_confirmed_global <- read.csv(text = time_series_covid19_confirmed_global)

time_series_covid19_recovered_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
time_series_covid19_recovered_global <- read.csv(text = time_series_covid19_recovered_global)

time_series_covid19_deaths_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
time_series_covid19_deaths_global <-read.csv(text = time_series_covid19_deaths_global)


countries<- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark',
              'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
              'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania',
              'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'UK', 'Iceland', 'Liechtenstein', 'Norway',
              'Switzerland', 'US', 'Canada')

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
par(mfrow=c(1,3))
plot(covid_romania$interval, covid_romania$confirmed, xlab="Date", ylab="Confirmed")
plot(covid_romania$interval, covid_romania$recovered, xlab="Date", ylab = "Recovered")
plot(covid_romania$interval, covid_romania$deaths, xlab= "Date", ylab = "Deaths")



################################################
library(writexl)
library(RCurl)

time_series_covid19_confirmed_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
time_series_covid19_confirmed_global <- read.csv(text = time_series_covid19_confirmed_global, stringsAsFactors = FALSE)

time_series_covid19_recovered_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
time_series_covid19_recovered_global <- read.csv(text = time_series_covid19_recovered_global)

time_series_covid19_deaths_global <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
time_series_covid19_deaths_global <-read.csv(text = time_series_covid19_deaths_global)

countries<- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark',
              'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
              'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania',
              'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom', 'Iceland', 'Liechtenstein', 'Norway',
              'Switzerland', 'US')

rowsConfirmed<-which(time_series_covid19_confirmed_global[,2] %in% countries & time_series_covid19_confirmed_global[,1]=='')
rowsConfirmed
confirmed <- (time_series_covid19_confirmed_global[rowsConfirmed,5:ncol(time_series_covid19_confirmed_global)])
rownames(confirmed)<-time_series_covid19_confirmed_global[rowsConfirmed,2]
colnames(confirmed)<-NULL


covid_global<-data.frame(confirmed=confirmed)
dt1 <- as.Date("01/22/2020", format = "%m/%d/%Y")
interval<-seq(from = dt1, length=length(confirmed), by="day")
covid_global<-as.data.frame(t(as.matrix(covid_global)))
covid_global<-cbind(interval,covid_global)
write_xlsx(covid_global,"cov_global.xlsx")


##### grafic numarul cumulativ de cazuri
for(j in 1:6) {
  filename<-paste0("plots-", j,".pdf" )
  pdf(filename)
  par(mfrow=c(3,2))
  for(i in (6*(j-1)+1):(6*(j-1)+6) ) {
    if(i>33)
      break
    plot(covid_global$interval, covid_global[,i+1], xlab="Date", ylab=colnames(covid_global)[i+1])
  }
  dev.off()
}



###### determinam perioada de crestre exponentiala########

#### buid the data set with new cases ########
covid_global_new<-data.frame(interval=covid_global$interval[2:length(covid_global$interval)])
for(i in 1:length(countries)) {
  dif<-vector()
  for(j in 1: (nrow(covid_global)-1)) {
    dif[j] <- covid_global[j+1,countries[i]]-covid_global[j,countries[i]]
  }
  covid_global_new<-cbind(covid_global_new, dif)
}
covid_global_new<-as.data.frame(covid_global_new)
for(i in 1:length(countries)) {
  names(covid_global_new)[i+1]<-countries[i]
}

cov_global_new2 <- read_excel("cov_global2.xlsx", sheet = "new")
cov_global_new2 <- cov_global_new2[-1,]
cov_global_new2 <- as.data.frame(cov_global_new2)

countries<- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark',
              'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
              'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania',
              'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom', 'Iceland', 'Liechtenstein', 'Norway',
              'Switzerland', 'US')

##### grafic numarul noi de cazuri
for(j in 1:6) {
  filename<-paste0("plots-new-", j,".pdf" )
  pdf(filename)
  par(mfrow=c(3,2))
  for(i in (6*(j-1)+1):(6*(j-1)+6) ) {
    if(i>33)
      break

    print(i)
    plot(cov_global_new2$interval[1:length(cov_global_new2$interval)], cov_global_new2[,i+1], xlab="Date", ylab=colnames(cov_global_new2)[i+1])
  }
  dev.off()
}

#############estimari#############
i<-26
countries<-sort(countries)[i]

countries<-sort(countries)
country<-(countries[i])

estimate_growth_rate<-function(data_new_cases, start, len, country, end_point, list_param) {

  t_start<-start
  t_end<-t_start+len
  k<-0
  adj_r<-vector()
  p_value<-vector()
  #aic2<-vector()
  domain<-vector()
  for(t_start in start:(end_point-len)) {
    for(t_end in (t_start+len):(end_point) ) {
    #print(paste0(t_start, "-", t_end))
    #k<-k+1
    #print(k)
      x<-t_start:t_end
      print(x)
      y<-data_new_cases[x,country]
      m2 <- nls(y ~ a * exp(b* x), control = nls.control(maxiter=50000), start = list_param)
      adj_r<-c(adj_r, r2(m2,y))
      p_value<-c(p_value, summary(m2)$parameters[2,4])
      #aic2 <- c(aic2, aic(summary(m2)$df[2], stats:::logLik.nls(m2)))
      ll<-list(x)
      domain<-c(domain, ll )
    }
  }
  index<-which.max(adj_r)
  x<-domain[[index]]
  y<-data_new_cases[x,country]
  m2 <- nls(y ~ a * exp(b* x), control = nls.control(maxiter=50000), start = list_param)
  m2
  summary(m2)
  adj_r[index]
  cov_global_new2$interval[x[1]]
  cov_global_new2$interval[x[length(x)]]
  x[1]
  x[length(x)]
  result <-list(date_start=cov_global_new2$interval[x[1]], date_end=cov_global_new2$interval[x[length(x)]], r2=adj_r[index],
                index_start=x[1], index_end=x[length(x)],
                a=
                sd_a=
                b=
                sd_b=
                  )
  return (result)
}

############grafice######################
###CI as bands +/-1.96*sigma(error)

xnew<-seq(min(x), max(x), 0.01)
preds<-predict(m2, newdata = data.frame(x=xnew))
plot(x,y)
lines(xnew, preds, col='blue', lwd=2)
lines(xnew, preds+summary(m2)$sigma, lwd=2, lty=3, col='red')
lines(xnew, preds-summary(m2)$sigma, lwd=2, lty=3, col='red')


plot(x,y, pch=20)
xnew<-seq(par()$usr[1],par()$usr[2], 0.01 )
preds<-predict(m2, newdata = data.frame(x=xnew))
lines(xnew, preds, col='blue', lwd=2)
lines(xnew, preds+summary(m2)$sigma*1.96, lwd=2, lty=3, col='red')
lines(xnew, preds-summary(m2)$sigma*1.96, lwd=2, lty=3, col='red')
############################################



plotare_ci <- function(m2,x,y, country, tip) {
  setEPS()
  filename <- paste0("plots-new-", country,"-",tip,".eps" )
  postscript(filename)

  #graficul initial
  #plot(cov_global_new2$interval[1:length(cov_global_new2$interval)], cov_global_new2[,i+1], xlab="Date", ylab=colnames(cov_global_new2)[i+1])
  par(mar = c(4,4,2,1))
  par(oma = c(1,1,0,0) )
  plot(cov_global_new2[1:length(cov_global_new2$interval),1], cov_global_new2[,country], xaxt = 'n', xlab = "Date", ylab = "New cases", main = colnames(cov_global_new2)[i + 1], las = 2, type = 'p', pch = 16 )
  xlabels <- format(as.Date(cov_global_new2[seq(from = 1, to = length(cov_global_new2$interval), by = 4),1]),"%b-%d")
  xpointsLab <- cov_global_new2[seq(from = 1, to = length(cov_global_new2$interval), by = 4),1]
  axis(1, at = xpointsLab,label = xlabels, las = 2, cex.axis = 0.7)
  grid()

  se <- summary(m2)$sigma
  df<-data.frame(x=x,y=y, pred=predict(m2))

  #ci = outer(df$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')

  ci<-matrix(nrow = length(x), ncol = 2)

  a<-summary(m2)$parameters[1,1]
  b<-summary(m2)$parameters[2,1]-1.96*summary(m2)$parameters[2,2]
  ci[,1]<-a*exp(x*b)
  b <- summary(m2)$parameters[2,1]+1.96*summary(m2)$parameters[2,2]
  ci[,2] <- a*exp(x*b)

  ii = order(df$x)
  with(df[ii,], points(cov_global_new2[x,1], pred, ylim=range(ci), type='l', col='blue'))
  matlines(cov_global_new2[df[ii,'x'],1], ci[ii,], lty=2, col=1)


  #aria gri din zona CI
  if(tip==2) {
    low=ci[ii,1]
    high=ci[ii,2]
    base=cov_global_new2[df[ii,'x'],1]
    polygon(c(base, rev(base)), c(low, rev(high)), col='grey')
    with(df[ii,], lines(cov_global_new2[x,1], pred, col='blue'))
    with(df, points(cov_global_new2[x,1], y,type='p', pch=16) )
  }
  #legenda
  #varianta 1 - fara arie gri in CI
  if(tip==1)
    legend("topleft", inset = 0.025, legend=c("Observed", "Fitted", "95% CI"), col=c("black", "blue", "black"), lty=c(NA,1,2), pch=c(16,NA,NA), cex=0.8)

  #varianta 2 - cu arie gri in CI
  if(tip==2)
    legend("topleft", inset = 0.025, legend=c("Observed", "Fitted", "95% CI"), col=c("black", "blue", "gray"), lty=c(NA,1,NA), pch=c(16,NA,15),
           pt.cex=c(0.8,0.8,2.5),  xjust=1, cex=0.8)

  #end
  dev.off()
}





aic <- function(k, L) {
  return (2*k-2*L)
}


r2 <- function(model, y) {
  RSS.p <- sum(residuals(model)^2)
  TSS <- sum((y - mean(y))^2)
  return (1 - (RSS.p/TSS))
}




####### cu pachetul R0 ##################
austria2020<-cov_global_new2[, 'Austria']
names(austria2020)<-as.character(cov_global_new2[,1])
mGT<-generation.time("gamma", c(3, 1.5))
sen=sensitivity.analysis(sa.type="time", incid=austria2020, GT=mGT, begin=30:40, end=45:70, est.method="EG")
plot(sen)

romania2020<-cov_global_new2[, 'Romania']
names(romania2020)<-as.character(cov_global_new2[,1])
sen=sensitivity.analysis(sa.type="time", incid=romania2020, GT=mGT, begin=33:40, end=44:85, est.method="EG")
plot(sen)


i<-32
countries<-sort(countries)
country<-(countries[i])
datele<-cov_global_new2[, country]
names(datele)<-as.character(cov_global_new2[,1])
mGT<-generation.time("gamma", c(3, 1.5))
sen=sensitivity.analysis(sa.type="time", incid=datele, GT=mGT, begin=33:45, end=45:57, est.method="EG")
plot(sen)
