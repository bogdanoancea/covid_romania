countries<- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark',
              'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
              'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania',
              'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom', 'Iceland', 'Liechtenstein', 'Norway',
              'Switzerland', 'US')
countries<-sort(countries)

###### read data #####
library(readxl)
covid_global_new <- read_excel("r-projects/covid_romania/covid_global.xlsx", sheet = "new")
covid_global_new <- as.data.frame(covid_global_new)
######################



##### calcul AIC ########
aic <- function(k, L) {
    return (2*k-2*L)
}

#### calcul R^2_adj #######
r2 <- function(model, y) {
    RSS.p <- sum(residuals(model)^2)
    TSS <- sum((y - mean(y))^2)
    return (1 - (RSS.p/TSS))
}

####### estimare growth rate ######
estimate_growth_rate<-function(data_new_cases, start, len, country, end_point, list_param) {
    t_start<-start
    t_end<-t_start+len
    k<-0
    adj_r<-vector()
    #p_value<-vector()
    domain<-vector()
    for(t_start in start:(end_point-len)) {
        for(t_end in (t_start+len):(end_point) ) {
            x<-t_start:t_end
            print(x)
            y<-data_new_cases[x,country]
            m2 <- nls(y ~ a * exp(b* x), control = nls.control(maxiter=50000), start = list_param)
            adj_r<-c(adj_r, r2(m2,y))
            #p_value<-c(p_value, summary(m2)$parameters[2,4])
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
    data_new_cases$interval[x[1]]
    data_new_cases$interval[x[length(x)]]
    x[1]
    x[length(x)]
    result <- list(date_start=data_new_cases$interval[x[1]],
                  date_end=data_new_cases$interval[x[length(x)]],
                  r2=adj_r[index],
                  index_start=x[1],
                  index_end=x[length(x)],
                  a=summary(m2)$parameters[1,1],
                  sd_a=summary(m2)$parameters[1,2],
                  p_value_a=summary(m2)$parameters[1,4],
                  b=summary(m2)$parameters[2,1],
                  sd_b=summary(m2)$parameters[2,2],
                  p_value_b=summary(m2)$parameters[2,4],
                  model=m2
     )
    return (result)
}
######################################################

###### plot number of new cases
plotare_ci <- function(data_cases_new, m2,x_s,x_e, country, tip) {
    setEPS()
    filename <- paste0("plots-new-", country,"-",tip,".eps" )
    postscript(filename)

    par(mar = c(4,4,2,1))
    par(oma = c(1,1,0,0) )
    plot(data_cases_new[1:length(data_cases_new$interval),1], data_cases_new[,country], xaxt = 'n', xlab = "Date", ylab = "New cases", main = country, las = 2, type = 'p', pch = 16 )
    xlabels <- format(as.Date(data_cases_new[seq(from = 1, to = length(data_cases_new$interval), by = 4),1]),"%b-%d")
    xpointsLab <- data_cases_new[seq(from = 1, to = length(data_cases_new$interval), by = 4),1]
    axis(1, at = xpointsLab,label = xlabels, las = 2, cex.axis = 0.7)
    grid()

    x<-x_s:x_e
    y<-data_cases_new[x_s:x_e, country]
    se <- summary(m2)$sigma
    df<-data.frame(x=x,y=y, pred=predict(m2))

    #ci = outer(df$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')

    ci<-matrix(nrow = length(x), ncol = 2)


    a<-summary(m2)$parameters[1,1]
    b<-summary(m2)$parameters[2,1]-1.96*summary(m2)$parameters[2,2]
    ci[,1]<-a*exp(x*b)
    b <- summary(m2)$parameters[2,1]+1.96*summary(m2)$parameters[2,2]
    ci[,2] <- a*exp(x*b)

    if(tip==1 & tip==2) {
        ii = order(df$x)
        with(df[ii,], points(data_cases_new[x,1], pred, ylim=range(ci), type='l', col='blue'))
        matlines(data_cases_new[df[ii,'x'],1], ci[ii,], lty=2, col=1)
    }

    #aria gri din zona CI
    if(tip==2) {
        low=ci[ii,1]
        high=ci[ii,2]
        base=data_cases_new[df[ii,'x'],1]
        polygon(c(base, rev(base)), c(low, rev(high)), col='grey')
        with(df[ii,], lines(data_cases_new[x,1], pred, col='blue'))
        with(df, points(data_cases_new[x,1], y,type='p', pch=16) )
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
