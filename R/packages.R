### Install all packages ####
pkg <- c("foreign", "ggplot2", "MASS", "reshape2", "ArfimaMLM", "sn", "moments", 
    "dplyr", "data.table", "colorout", "knitr", "tidyr", "plyr", "stringr", "zoo", 
    "sandwich", "lmtest", "devtools")
inst <- pkg %in% installed.packages()
if (length(pkg[!inst]) > 0) install.packages(pkg[!inst])
# lapply(pkg,library,character.only=TRUE)
rm(inst, pkg)

### Function for Plotting ### write function to plot + export pdf etc.? 
