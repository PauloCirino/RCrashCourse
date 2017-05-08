#### Loading Packages
##### Always make functions as 'idiot-proof' as you can
loadOrInstallPackage <- function(package){
    package[1]
    ## Tries Loading the package
    tryResult <- try(expr = require(package = package, 
                                    character.only = TRUE ),
                     silent = TRUE)
    
    ## Person Doesnt Have The package
    if(!tryResult){
        try(expr = install.packages(package), silent = TRUE)
        try(expr = require(package, 
                           character.only = TRUE,
                           quietly = TRUE),
            silent = TRUE)
    }
    TRUE
}

packagesVet <- c('dplyr', 'ggplot2', 'DT', 'sparklyr')

#### Applies loadOrInstallPackage() function to all packages in packagesVet
aux <- sapply(X = packagesVet, FUN = loadOrInstallPackage)

sparkConnection <- try( spark_connect(master = "local",
                                      version = '1.6.2',
                                      hadoop_version = '2.6'),
                        silent = TRUE)
if( class(sparkConnection) == 'try-error'){
    sparkAvailableVersions <- sparklyr::spark_install(version = '1.6.2',
                                                      hadoop_version = '2.6', 
                                                      reset = TRUE,
                                                      verbose = TRUE)
    sparkConnection <- spark_connect(master = "local",
                                     version = '1.6.2',
                                     hadoop_version = '2.6')
}

