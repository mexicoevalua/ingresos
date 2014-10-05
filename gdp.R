# Incorporar GDP
# International Monetary Fund, World Economic Outlook Database, April 2014
# Datos en miles de millones de pesos, transformar a millones de pesos
# http://www.imf.org/external/pubs/ft/weo/2014/01/weodata/weorept.aspx?sy=1980&ey=2019&scsm=1&ssd=1&sort=country&ds=.&br=1&pr1.x=94&pr1.y=10&c=273&s=NGDP_R%2CNGDP%2CNGDP_D%2CNID_NGDP&grp=0&a=

gdp  <- read.csv("gdp1980-2019.csv")
gdp$gdpConstantMmdp  <- gdp$gdpConstantMmdp * 1000
gdp$gdpCurrentMmdp  <- gdp$gdpCurrentMmdp * 1000
gdp  <- gdp[,-2] # quitar gdp precios constantes
names(gdp) <- c("year", "gdpCorriente","invPctGdp")
head(gdp)
