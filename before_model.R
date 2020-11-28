data = read.csv('C:\\Users\\user\\git\\econometrics-regression\\data\\final_merge.csv')

library(tidyverse)
library(lubridate)

data%>%select('기온', '강수량', '풍속', '최다풍향', '습도', 
                '기압', '안개시간') %>% plot()

  
library(corrplot)

a = data%>%select('기온', '강수량', '풍속', '최다풍향',  '안개시간') 
corrplot(a, method="number")

glimpse(data)
data20 = data[year(ymd(data$date)) == 2020,]


# corr 높은것 정리
# 교통량은 inout으로 통일
# 서울 확진자는 seoul_covid_confirmed로 통일
# 중국도 china_covid_confirmed로 통일
# 기온 강수량 음의 상관관계인데 일단 보류
# 중국 미세먼지/초미세먼지는 서울 y 따라감

lm25_1920 = lm(y_pm2.5 ~  
            china_pm2.5 +  seoul_covid_confirmed +  china_stock_index +  
            기온 +  강수량 +  풍속 +  최다풍향 +  습도 +  기압 +  안개시간 +  
            transport_inandout +  china_covid_confirmed +  kospi_index +  soeul_oil_consumption,
          data = data)

lm25_20 = lm(y_pm2.5 ~  
               china_pm2.5 +  seoul_covid_confirmed +  china_stock_index +  
               기온 +  강수량 +  풍속 +  최다풍향 +  습도 +  기압 +  안개시간 +  
               transport_inandout +  china_covid_confirmed +  kospi_index +  soeul_oil_consumption,
             data = data20)

lm10_1920 = lm(y_pm10 ~  
                 china_pm2.5 +  seoul_covid_confirmed +  china_stock_index +  
                 기온 +  강수량 +  풍속 +  최다풍향 +  습도 +  기압 +  안개시간 +  
                 transport_inandout +  china_covid_confirmed +  kospi_index +  soeul_oil_consumption,
               data = data)


lm10_20 = lm(y_pm10 ~  
                 china_pm2.5 +  seoul_covid_confirmed +  china_stock_index +  
                 기온 +  강수량 +  풍속 +  최다풍향 +  습도 +  기압 +  안개시간 +  
                 transport_inandout +  china_covid_confirmed +  kospi_index +  soeul_oil_consumption,
               data = data20)

summary(lm25)
summary(lm25_2020)

summary(lm10)






step_lm25 = step(lm25, direction = 'both')
summary(step_lm25)



step_lm10 = step(lm10, direction = 'both')
summary(step_lm10)













library(tidyverse)


data
seoul_covid_confirmed
china_stock_index
기온
강수량
풍속
최다풍향
습도
기압
안개시간
transport_inandout
china_covid_confirmed
kospi_index
soeul_oil_consumption