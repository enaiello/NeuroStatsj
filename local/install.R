#install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
#install.packages('jmvtools', repos=c('https://repo.jamovi.org'))
#devtools::install_github('jamovi/jmvcore')
library(jmvcore)
jmvtools::version()
jmvtools::check()
#jmvtools::create()

source("local/functions.R")

#jmvtools::install()
installme("NeuroStatsj")

