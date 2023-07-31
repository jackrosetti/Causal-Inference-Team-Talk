#library(haven)
#df1 <-as.data.frame(matrix(nrow=15,ncol=3))
#df1$time <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
#time <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
#df1$NJ<- c(5, 5.2,	5.6,	5.5,	5.1,	5.1,	5.2,	5,	4.8,	4.9,	5.1,	5.1,	5,	4.8,	4.7)
#df1$Penn <- c(3,	3.2,	3.6,	3.5,	3.1,	3.3,	4.398919324,	5.026981169,	4.728113625,	5.296923809,	5.779081888,	5.411152243,	4.698507501,	5.335775358,	4.924001604)

#ggplot(df1, aes(x=time)) + 
#  geom_line(aes(y = NJ), color = "blue") + 
#  geom_line(aes(y = Penn), color="red") 


library(dplyr)
set.seed(12345)
T = 100 # no of periods
N = 40 # no of subjects

dat = expand.grid( t = 1:T,i = 1:N) 

# Simulate a common AR(1) time trend
time.trend = as.numeric(arima.sim(n=T,list(ar = c(0.4,0.5), ma=c(0.6,0.5))))*3+0.7*(1:T)

dat = mutate(dat,
             group = ifelse(i > N/2,"treat","control"),
             treat = 1L*(group == "treat"), 
             exp = 1L*(t > T/2),
             treat_exp = exp*treat,
             mu.t = time.trend[t],
             eps = rnorm(n()),
             y = mu.t + treat*40 + treat_exp*50 + eps
)
sample_n(dat, 5)

show.plot = function(dat,label="", show.means=TRUE) {
  library(ggplot2)
  gdat = dat %>%
    group_by(group, t,exp,treat) %>%
    summarize(y = mean(y))
  
  gg = ggplot(gdat, aes(y=y,x=t, color= group)) +
    geom_line() + 
    geom_vline(xintercept=T/2) +
    theme_bw() +
    annotate("text",x=T/4, y = 0.9*max(gdat$y), label=label)
  
  if (show.means) {
    y.pre.tr <<- mean(filter(gdat,treat==1, exp==0)$y) %>% round(1)
    y.exp.tr <<- mean(filter(gdat,treat==1, exp==1)$y) %>% round(1)
    y.pre.co <<- mean(filter(gdat,treat==0, exp==0)$y) %>% round(1)
    y.exp.co <<- mean(filter(gdat,treat==0, exp==1)$y) %>% round(1)
    gg = gg + 
      annotate("label", x=T/4, y=y.pre.tr+15,label=y.pre.tr) +
      annotate("label", x=T/4, y=y.pre.co-15,label=y.pre.co) +
      annotate("label", x=T*0.75, y=y.exp.tr+15,label=y.exp.tr) +
      annotate("label", x=T*0.75, y=y.exp.co-15,label=y.exp.co)
  }
  gg
}  
show.plot(dat)
dat = dat %>% mutate(
  x = ifelse(treat,-t, t)+runif(n())*2,
  y = mu.t + treat*40 + treat_exp*50 + 0.8*x + eps
)
show.plot(dat, show.means=FALSE, label="Pre-trends not parallel")