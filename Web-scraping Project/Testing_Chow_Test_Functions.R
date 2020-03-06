# Test various Chow Test functions found online
# Web scraping project
# STA 6233 - Advanced R Programming
# by Ben Graf
# Created 2020-02-24, Last edited 2020-02-24

pacman::p_load(gap)

dat1 <- matrix(c(
  1.2, 1.9, 0.9,
  1.6, 2.7, 1.3,
  3.5, 3.7, 2.0,
  4.0, 3.1, 1.8,
  5.6, 3.5, 2.2,
  5.7, 7.5, 3.5,
  6.7, 1.2, 1.9,
  7.5, 3.7, 2.7,
  8.5, 0.6, 2.1,
  9.7, 5.1, 3.6), byrow=TRUE, ncol=3)

dat2 <- matrix(c(
  1.4, 1.3, 0.5,
  1.5, 2.3, 1.3,
  3.1, 3.2, 2.5,
  4.4, 3.6, 1.1,
  5.1, 3.1, 2.8,
  5.2, 7.3, 3.3,
  6.5, 1.5, 1.3,
  7.8, 3.2, 2.2,
  8.1, 0.1, 2.8,
  9.5, 5.6, 3.9), byrow=TRUE, ncol=3)

y1<-dat1[,3]
y2<-dat2[,3]
x1<-dat1[,1:2]
x2<-dat2[,1:2]
chow.test.r<-chow.test(y1,x1,y2,x2)

sp <- ggscatter(namesubset, x = "babyyear", y = "total",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "timeframe", palette = "jco", # Color by groups "timeframe"
                shape = "timeframe",                  # Change point shape by groups "timeframe"
                title = paste0("10 Years Before & After ",allcharnames1977$moviename[nameindex]," for the name ",allcharnames1977$indivname[nameindex])
)+
  stat_cor(aes(color = timeframe))#, label.x = 3)       # Add correlation coefficient
sp



require(graphics)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)



pacman::p_load(svars)
# Testing for structural break in USA data
#' # data contains quartlery observations from 1965Q1 to 2008Q2
# assumed structural break in 1979Q3
# x = output gap
# pi = inflation
# i = interest rates
set.seed(23211)
v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
z1 <- chow.test(v1, SB = 59)
summary(z1)

#Using stability() to find potential break point and sample split
x1 <- stability(v1, type = "mv-chow-test")
plot(x1)
z1.1 <- chow.test(x1)
summary(z1.1)
#Or using sample split as benchmark
x1$break_point <- FALSE
z1.1 <- chow.test(x1)
summary(z1.1)

#Structural brake via Dates
#given that time series vector with dates is available
dateVector <- seq(as.Date("1965/1/1"), as.Date("2008/7/1"), "quarter")
z2 <- chow.test(v1, SB = "1979-07-01", format = "%Y-%m-%d", dateVector = dateVector)
summary(z2)

# alternatively pass sequence arguments directly
z3 <- chow.test(v1, SB = "1979-07-01", format = "%Y-%m-%d",
                start = "1965-01-01", end = "2008-07-01",
                frequency = "quarter")
summary(z3)

# or provide ts date format (For quarterly, monthly, weekly and daily frequencies only)
z4 <- chow.test(v1, SB = c(1979,3))
summary(z4)



pacman::p_load(strucchange)
x=seq(1,5,length=100)
y=numeric(100)
y[1:50]=2*x[1:50]
y[51:100]=rep(2*x[51],50)
z=rnorm(100,0,.15)
y=y+z
plot(x,y)
sctest(y ~ x, type = "Chow", point = 3)


x=seq(1,5,length=100)
y=numeric(100)
y[1:50]=2*x[1:50]
y[51:100]=2*x[1:50]+15
z=rnorm(100,0,.15)
y=y+z
plot(x,y)
sctest(y ~ x, type = "Chow", point = 51)


x=seq(1,5,length=100)
y=numeric(100)
y=2*x
z=rnorm(100,0,.15)
y=y+z
plot(x,y)
sctest(y ~ x, type = "Chow", point = 51)


x=seq(1,5,length=100)
y=numeric(100)
y=seq(2,2,length=100)
z=rnorm(100,0,.15)
y=y
plot(x,y)
sctest(y ~ x, type = "Chow", point = 51)
