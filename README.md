# SwedenMap (alpha version)

###  Create maps of Sweden populated with data from Kolada.se using swemaps and rkolada

Inspired by NYTimes TheUpshot - The Best and Worst New York Neighborhoods for Subway Service, Rat Control and 42 Other Joys of Urban Life

https://www.nytimes.com/interactive/2017/06/30/upshot/the-best-and-worst-new-york-neighborhoods.html

This code uses data from the Kommun- och landstingdatabasen (www.kolada.se)

The major part of this scritpt uses code by https://github.com/reinholdsson/ (https://github.com/reinholdsson/swemaps, https://github.com/reinholdsson/rkolada)

#### Install
```
devtools::install_github('jespermaag/SwedenMaps')
```
#### Pick values from KPI_id_names_short files
```
N00908	Förvärvsarbetande invånare 16-19 år, andel (%)
N00909	Förvärvsarbetande invånare 20-24 år, andel (%)
N00910	Förvärvsarbetande invånare 25-34 år, andel (%)
N00911	Förvärvsarbetande invånare 35-49 år, andel (%)
N00912	Förvärvsarbetande invånare 50-59 år, andel (%)
N00913	Förvärvsarbetande invånare 60-64 år, andel (%)
N00914	Förvärvsarbetande invånare 20-64 år, andel (%)
```

#### Run
```
SwedenMap(znorm=TRUE,gif=TRUE,year='2015',value=FALSE, name='Förvarsarbetande',kpiID='N00908,N00909,N00910,N00911,N00912,N00913,N00914')
```


