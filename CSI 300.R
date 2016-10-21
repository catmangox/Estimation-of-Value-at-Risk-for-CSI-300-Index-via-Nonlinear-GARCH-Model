#Installing required packages and loading them as library in R
#Time series analysis and computational finance
library("tseries");
;#An S3 class with methods for totally ordered indexed observations. It is particularly aimed at irregular time series of numeric vectors/matrices and factors. zoo's key design goals are independence of a particular index/date/time class and consistency with ts and base R by providing methods to extend standard generics.
library("zoo");
#Methods and tools for displaying and analysing univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modelling.
library("forecast");
#R companion to Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley)
library("FinTS");
# ARFIMA, in-mean, external regressors and various GARCH flavors, with methods for fit, forecast, simulation, inference and plotting.
library("rugarch");
library("ggplot2");
library("psych");
library("Quandl");

#get the daily adjusted close data of CSI 300(from 2006-06-21 to 2016-06-21) from quadl then return the zool class object named 'csi.data'
csi.data=Quandl(c("YAHOO/SS_000300.4"), api_key="sqUGVk1vqe37KNAyZ-1Z", start_date="2006-06-21", end_date="2016-06-21",type = "zoo");
plot(csi.data, main = "CSI 300 Index Closing Prices on SSE", ylab = "Price (USD)", xlab = "Date");
#show the volatility of CSI 300 Index and plot it.
csi.ret<- diff(log(csi.data)) * 100;
plot(csi.ret, main = "Daily Compound Returns", xlab = "Date", ylab = "Return in percent");
qplot(csi.ret,,xlim=c(-5,5), geom="histogram") ;
describe(csi.ret);
#find the best model by suing ARIMA model
fit1 <- auto.arima(csi.data,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc',trace =TRUE );
plot(forecast(fit1,h=2000));
str(fit1);
#If p-value of Ljung-Box test is smaller than 5% level of significance then there exist the ARCH effect which shows the green light to proceed ahead to GARCH.
Box.test(fit1$residuals^2,lag=12, type="Ljung-Box");
res_garch11_spec<- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 1, 0)));
res_garch11_fit<- ugarchfit(spec = res_garch11_spec, data = csi.ret);
ctrl = list(tol = 1e-7, delta = 1e-9)
res_garch11_roll <- ugarchroll(res_garch11_spec, csi.ret, n.start = 120, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
report(res_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99);
res_garch12_spec<- ugarchspec(variance.model = list(garchOrder = c(1, 2)), mean.model = list(armaOrder = c(0, 1, 0)));
res_garch12_fit<- ugarchfit(spec = res_garch12_spec, data = csi.ret);
res_garch21_spec<- ugarchspec(variance.model = list(garchOrder = c(2, 1)), mean.model = list(armaOrder = c(0, 1, 0)));
res_garch21_fit<- ugarchfit(spec = res_garch21_spec, data = csi.ret);
res_garch12_roll <- ugarchroll(res_garch12_spec, csi.ret, n.start = 120, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
report(res_garch12_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99);
res_garch21_roll <- ugarchroll(res_garch21_spec, csi.ret, n.start = 120, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl, fit.control = list(scale = 1))
report(res_garch21_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99);
res_garch11_fit;
res_garch12_fit;
res_garch21_fit;
res_garch11_fit;
res_garch11_fcst <- ugarchforecast(res_garch11_fit, n.ahead = 12);
res_garch11_fcst

plot(res_garch11_fit);
plot(res_garch12_fit);
plot(res_garch21_fit)