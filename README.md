# Investopedia Homeownership By Generation Analysis

This repository contains the code and data behind the [Investopedia article](https://www.investopedia.com/millennial-homeownership-still-lagging-behind-previous-generations-7510642) comparing homeownership rates and home affordability across generations. The `data` folder contains supplementary data used in the analysis and visualizations in the article while the `visualizations` folder contains CSV files that created the three [Datawrapper](https://www.datawrapper.de/) visualizations in the article. Analysis results can be reproduced using the code in the R file `home_ownership_costs_generation_ipums.R` and downloading Current Population Survey microdata from [IPUMS](https://cps.ipums.org/cps/) with the variable selection outlined below. Please take care to adhere to the IPUMS [terms of use](https://www.ipums.org/about/terms).

Variables selected for IPUMS CPS extract: 
* YEAR
* SERIAL
* MONTH
* CPSID
* ASECFLAG
* HFLAG
* ASECWTH
* CPI99
* OWNERSHP
* HHINCOME
* PERNUM
* CPSIDP
* ASECWT
* RELATE
* AGE

Samples selected for IPUMS CPS extract:
* IPUMS-CPS ASEC 1982 - 2022