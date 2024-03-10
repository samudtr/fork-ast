library(fpp)
library(readxl)
a <- as.vector(read_excel("C:\\Users\\samud\\Documents\\NovaIMS\\Forecasting Methods\\TS_Tourists.xlsx"))

tourist <- ts(data = rev(a$Tourists),start = c(2017,1), end = c(2023,8), frequency = 12)
plot(tourist, main = "", ylab="Overnight Stays", xlab = "Year")

t1 <-tourist

Decomp1<-decompose(tourist)
Decomp_plot<- plot(Decomp1)

lambda<- BoxCox.lambda(tourist)
lambda #The lambda for this data set is 0.94 which is close to 1, and indicates that there is no strong increasing variance across time.

Detrend<- diff(t1,1,1)
Detrendplot<- plot.ts(Detrend)
Disp<- tsdisplay(Detrend, main = NA)

Seasonadj<- diff(Detrend,12,1) 
Seasonplot<- plot.ts(Seasonadj)
tsdisplay(Seasonadj)
Disp1<- tsdisplay(Seasonadj, main=NA, lag.max = 48)


auto.arima(tourist)

fit_2<- Arima(tourist,order=c(0,1,1),seasonal = c(2,1,0))
plot(tourist)
lines(fitted(fit_2), col='blue')

Res_2<- residuals(fit_2)
plot(Res_2, ylab=NA, xlab=NA, main=NA)
abline(h=0, col='blue')
Acf_2<-acf(Res_2, main='ACF of Residuals')
checkresiduals(fit_2)
tsdiag(fit_2, main='Residuals for 2nd fit') #Ljung-Box test p=0.5125
summary(fit_2)
qqnorm(Res_2, main=NA)
qqline(Res_2, col='black')
plot(as.vector(fitted(fit_2)),as.vector(residuals(fit_2)),xlab="Fitted", ylab="Residuals")
abline(h=0)


forecast<- predict(fit_2,n.ahead = 12)
forecast
U = forecast$pred + 1.96*forecast$se
L = forecast$pred - 1.96*forecast$se
ts.plot(tourist, xlim=c(2017,2025), ylim=c(0,11000000),main= NA, ylab = "Overnight Stays in Portugal", xlab = "Year")
lines(forecast$pred, col="red", type="l")
lines(U, col="blue")
lines(L, col="green")
legend("topleft", legend = c("Predicted","Upper","Lower"), col = c("red","blue","green"), lty = 1, horiz =TRUE, cex = 0.7)



