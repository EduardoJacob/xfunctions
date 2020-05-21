#' @title Automatically install/load Packages
#' 
#' @description 
#' Receives a list of packages to Install/Load \cr
#' Adds the received packages to a list of default packages to allways load \cr
#' Installs missing packages \cr
#' Load Packages \cr
#' Inspect Packages for datasets \cr
#' Creates 3 data frames on Global Environment: \cr
#' Loaded.Packages \cr
#' Loaded.Datasets \cr
#' Loaded.Vignettes
#' @param package 0, 1 or more comma separated packages
#' @keywords XLibrary
#' @export
#' @examples
#' XLibrary()
#' XLibrary("benchmarkme")
#' XLibrary("package1","package2")


XLibrary = function(...) {
  cat("\nXLibrary: Automatically Install/Load Packages \n")
  
  # cat("Initialy loaded Packages. :",.packages(),"\n")
  # Load passed packages and concatenate default packages
  packages = unlist(list(...),use.names=FALSE)
  packages = c(packages,"tidyverse","DataExplorer","esquisse","ggplot2","cranlogs","tibble")
  packages = sort(unique(packages))
  
  cat("List of packages to Install/Load:",packages,"\n")
  
  installed.packages = as.data.frame(installed.packages(),stringsAsFactors = F)
  installed.packages = installed.packages$Package
  cat("There are:",length(installed.packages),"packages installed \n")
  
  new_packages = packages[!(packages %in% installed.packages)]
  # XShowObject(new_packages)
  
  if ( length(new_packages) == 0 ) {
    cat("No new Packages to Install\n")
  } else {
    cat("List of missing packages to Install:",new_packages,"\n")
    # Install new Packages
    cat("\n")
    for (package in new_packages) {
      cat("Installing package",package,"\n")
      install.packages(package,dependencies = T)
    }
  }
  
  # Load Packages
  packages_version = NULL
  # cat("\n")
  for (package in packages) {
    Info = unlist(packageDescription(package))
    packages_version = c(packages_version,Info["Version"])
    # cat("Loading package",package,Info["Version"],"\n")
    library(package,character.only = T)
  }
  # Downloads per month
  packages_downloads = NULL
  # cat("\n")
  for (package in packages) {
    downloads = sum( cranlogs::cran_downloads(when = "last-month", packages = package)$count )
    packages_downloads = c(packages_downloads,downloads)
    # cat("Downloads/month",package,downloads,"\n")
  }
  # Inspect Packages for Datasets
  # cat("\n")
  Datasets = NULL
  packages_datasets = NULL
  for (package in packages) {
    d = data(package = package)$results
    # cat("Inspect package",package,nrow(d),"datasets found\n")
    Datasets = rbind(Datasets,d)
    packages_datasets = c(packages_datasets,nrow(d))
  }
  Loaded.Vignettes <<- as.data.frame(vignette(package=packages)$results)
  # Get number of Vignettes per Package
  packages_vignettes = NULL
  for (package in packages) {
    df = as.data.frame(vignette(package=package)$results) 
    packages_vignettes = c(packages_vignettes,nrow(df))
  }
  
  
  # Print Packages
  cat("\n")
  df = data.frame(Package=packages,Version=packages_version,
      Downloads.Month=packages_downloads,Datasets=packages_datasets,Vignettes=packages_vignettes)
  print( df, row.names = FALSE) 
  
  
  # Create Loaded.Packages on Global Environment
  Package = sort(.packages())
  Version = vector()
  Title = vector()
  New.Version = vector()
  old.packages = as.data.frame(old.packages(repos = "http://cran.us.r-project.org"),stringsAsFactors = F) 
  for (package in Package) {
    Info = unlist(packageDescription(package))
    Version = c(Version,Info["Version"])
    Title = c(Title,Info["Title"])
    ReposVer = ""
    if ( package %in% old.packages$Package ) {
      ReposVer = subset(old.packages,Package==package,select = ReposVer)[1,1]
    }
    New.Version = c(New.Version,ReposVer)
  }
  Loaded.Packages <<- data.frame(Package,Version,New.Version,Title,stringsAsFactors=F)
  
  
  
  Datasets = data.frame(Datasets,stringsAsFactors = F)
  # Get Dataset Dimensions and Class of "dataset" object
  cat("\n")
  Class = vector()
  Dimensions = vector()
  Item.data = vector()
  Item.name = vector()
  for (i in 1:nrow(Datasets)) {
    Package = Datasets[i,1]
    d = unname(Datasets[i,3])
    
    # Split d into: dataset_name (data_name)
    # example: package=caret absorp (tecator)
    d = gsub(")","",d,fixed=T)
    d = gsub("(","",d,fixed=T)
    d = unlist(strsplit(d," "))
    if ( length(d) == 1 ) {
      dataset_name = d
      data_name = d
    } else {
      dataset_name = d[1]
      data_name = d[2]
    }
    
    data(list=data_name,package=Package,envir = environment())
    d = get(dataset_name)
    ddim = paste(dim(d),collapse = "x")
    if (ddim == "") ddim = length(d)
    cclass = paste(class(d),collapse = " ")
    
    Item.data = c(Item.data,data_name)
    Item.name = c(Item.name,dataset_name)
    Dimensions = c(Dimensions,ddim)
    Class = c(Class,cclass)
  }
  
  Datasets = cbind(Package = Datasets$Package,
                   Item = Datasets$Item,
                   Item.data,
                   Item.name,
                   Title = Datasets$Title,
                   Class,
                   Dimensions,
                   LibPath = Datasets$LibPath)
  
  # Create Loaded.Datasets on Global Environment
  Loaded.Datasets <<- data.frame(Datasets,stringsAsFactors=F)
  
} # XLibrary

