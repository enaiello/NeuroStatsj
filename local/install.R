library(jmvcore)
#devtools::check_win_release() 
#home<-"C:\\Program Files/jamovi 2.2.5.0/"
jmvtools::version()
jmvtools::check()
home<-"flatpak"
jmvtools::check(home=home)

jmvScaffold::copy_files()
#jmvScaffold::install_module_full("GAMLj3")


jmvScaffold::install_module("NeuroStatsj")

#jmvScaffold::format_jamovi()  
