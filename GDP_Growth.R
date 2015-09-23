######################################################
# GDP Growth 
# Reference : http://databank.worldbank.org/data/reports.aspx?Code=NY.GDP.MKTP.KD.ZG&id=af3ce82b&report_name=Popular_indicators&populartype=series&ispopular=y#
# Target Country : Hungary, Egypt, Arab Rep, Iraq, Jordan, Lebanon, Turkey, Italy, France, United Kingdom, United States, Sweden
######################################################
gdp_wide = read.csv('Research/GDP/Popular indicators_Data.csv')
head(gdp_wide)
names(gdp_wide)

# 아래에 필요없는 빈칸들이 있다. 이를 삭제
dplyr::select(gdp_wide, Country.Name)
# 빈칸으로 입력된 Row들 삭제
gdp_wide[!gdp_wide$Series.Code == "", ]

# dplyr::select(gdp_wide, Country.Name)
# Column 이름 변경
# Notice : Column Name이 숫자로 할 경우, Column 이름을 불러올 때,
# data.frame의 컬럼 이름인지, 아니면 숫자인지 인식을 못하는 경우가 있는 것 같음
# 이를 보완하기 위해 컬럼 이름 앞에 x 값을 넣음
# Challenge : 이렇게 길게 명령어 안적고 쉬운 방법 없을까.
library(dplyr) 
gdp_wide = gdp_wide %>%
  dplyr::rename(Series.Name = 癤풱eries.Name)
gdp_wide = gdp_wide %>%
  dplyr::rename('x2000' = X2000..YR2000.)
gdp_wide = gdp_wide %>%
  dplyr::rename('x2001' = X2001..YR2001.)
gdp_wide = gdp_wide %>%
  dplyr::rename('x2002' = X2002..YR2002.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2003' = X2003..YR2003.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2004' = X2004..YR2004.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2005' = X2005..YR2005.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2006' = X2006..YR2006.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2007' = X2007..YR2007.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2008' = X2008..YR2008.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2009' = X2009..YR2009.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2010' = X2010..YR2010.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2011' = X2011..YR2011.) 
gdp_wide = gdp_wide %>%
  dplyr::rename('x2012' = X2012..YR2012.)
names(gdp_wide)

# 불필요한 Column 삭제
gdp_wide$Country.Code = NULL

# 타입을 변경해줘야함
dplyr::filter(gdp_wide, x2000 == '..')

# Help : NA 값이나 다름없는 문자열 '..'을 삭제하려고 하는데,
# 엉뚱하게 다른 값들이 변경됨

############################
# Test Code
# 이상함!!!!
head(gdp_wide$x2000)
head(ifelse(gdp_wide$x2000 == '..', 0, gdp_wide$x2000))
############################


gdp_wide$x2000 = ifelse(gdp_wide$x2000 == '..', 0, gdp_wide$x2000)
gdp_wide$x2001 = ifelse(gdp_wide$x2001 == '..', 0, gdp_wide$x2001)
gdp_wide$x2002 = ifelse(gdp_wide$x2002 == '..', 0, gdp_wide$x2002)
gdp_wide$x2003 = ifelse(gdp_wide$x2003 == '..', 0, gdp_wide$x2003)
gdp_wide$x2004 = ifelse(gdp_wide$x2004 == '..', 0, gdp_wide$x2004)
gdp_wide$x2005 = ifelse(gdp_wide$x2005 == '..', 0, gdp_wide$x2005)
gdp_wide$x2006 = ifelse(gdp_wide$x2006 == '..', 0, gdp_wide$x2006)
gdp_wide$x2007 = ifelse(gdp_wide$x2007 == '..', 0, gdp_wide$x2007)
gdp_wide$x2008 = ifelse(gdp_wide$x2008 == '..', 0, gdp_wide$x2008)
gdp_wide$x2009 = ifelse(gdp_wide$x2009 == '..', 0, gdp_wide$x2009)
gdp_wide$x2010 = ifelse(gdp_wide$x2010 == '..', 0, gdp_wide$x2010)
gdp_wide$x2011 = ifelse(gdp_wide$x2011 == '..', 0, gdp_wide$x2011)
gdp_wide$x2012 = ifelse(gdp_wide$x2012 == '..', 0, gdp_wide$x2012)

# 변환 : Wide -> Tidy Data
gdp_long = tidyr::gather(gdp_wide, Year, GDP_annual_growth, x2000:x2012)
head(gdp_long)

library(stringr)
# Column에서 Year 앞에 붙은 x를 삭제
gdp_long$Year = stringr::str_replace(gdp_long$Year, 'x', '')
# 숫자로 변경
gdp_long$Year = as.numeric(gdp_long$Year)

# GDP Growth 만 뽑아오기 위해 해당 코드로 간추림
gdp_growth = gdp_long %>%
  dplyr::filter(gdp_long$Series.Code == 'NY.GDP.MKTP.KD.ZG')

# 그래프 실행
library(ggplot2)
ggplot(gdp_growth,
       aes(x = Year, y = GDP_annual_growth, col = Country.Name)
       ) + geom_point() + geom_line()