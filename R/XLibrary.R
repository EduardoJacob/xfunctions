#' Automatically install/load Packages
#' 
#' Receives a list of packages to Install/Load \cr
#' Adds the received packages to a list of default packages to allways load \cr
#' Installs missing packages \cr
#' Load Packages \cr
#' Inspect Packages for datasets \cr
#' Creates 3 data frames on Global Environment: \cr
#' Loaded.Packages \cr
#' Loaded.Datasets \cr
#' Loaded.Vignettes
#' 
#' @param ... 0, 1 or more comma separated packages
#' @export
#' @examples
#' \dontrun{
#' XLibrary()
#' XLibrary("benchmarkme")
#' XLibrary("package1","package2")
#' }
XLibrary = function(...) {
  cat("\nXLibrary: Automatically Install/Load Packages \n")
  
  # cat("Initialy loaded Packages. :",.packages(),"\n")
  # Load passed packages and concatenate default packages
  packages = unlist(list(...),use.names=FALSE)
  #packages = c(packages,"tidyverse","DataExplorer","ggplot2","cranlogs","tibble","esquisse") 
  #packages = "xfunctions"
  packages = c(packages,"tidyverse","ggplot2") 
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
  Version = vector()
  # cat("\n")
  for (package in packages) {
    Info = unlist(utils::packageDescription(package))
    Version = c(Version,Info["Version"])
    # cat("Loading package",package,Info["Version"],"\n")
    library(package,character.only = T)
  }
  # Downloads per month
  # packages_downloads = NULL
  # cat("\n")
  # for (package in packages) {
    # package = "zoo"
    # downloads = 0
    # try(
    #  { downloads = sum( cranlogs::cran_downloads(when = "last-month", packages = package)$count ) },
    #  silent = T
    #)
    
    # packages_downloads = c(packages_downloads,downloads)
    # cat("Downloads/month",package,downloads,"\n")
  #}
  # Inspect Packages for Datasets
  # cat("\n")
  Datasets = NULL
  packages_datasets = NULL
  for (package in packages) {
    d = utils::data(package = package)$results
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
  #df = data.frame(Package=packages,Version=Version,
  #                Downloads.Month=packages_downloads,Datasets=packages_datasets,Vignettes=packages_vignettes)
  df = data.frame(Package=packages,Version=Version,
                  Datasets=packages_datasets,Vignettes=packages_vignettes)
  print( df, row.names = FALSE) 
  
  
  # Create Loaded.Packages on Global Environment: .packages()
  Package = sort(.packages())
  Version = vector()
  Description = vector()
  Built = vector()
  New.Version = vector()
  Location = vector()
  Source = vector()
  Repo = vector()
  # old.packages = as.data.frame(old.packages(repos = "http://cran.us.r-project.org"),stringsAsFactors = F) 
  old.packages = as.data.frame(old.packages(),stringsAsFactors = F) 
  for (package in Package) {
    Info = packageDescription(package)
    
    Version = c(Version,Info[["Version"]])
    Description = c(Description,Info[["Title"]])
    Built = c(Built,unlist(strsplit(Info[["Built"]], ";"))[1])
    Location = c(Location,find.package(package))
    
    NewVersion = ""
    NewSource = XLibraryGetSource(package)
    NewRepo = "" # GitHub Repo
    
    if ( package %in% old.packages$Package ) NewVersion = subset(old.packages,Package==package,select = ReposVer)[1,1]
    
    if ( NewSource == "GitHub" ) {
      NewRepo = paste0(Info[["GithubUsername"]],"/",Info[["GithubRepo"]])
      
      con = url(paste0("https://raw.githubusercontent.com/",NewRepo,"/HEAD/DESCRIPTION"))
      github_info = read.dcf(con, all = TRUE) 
      close(con)
      if ( Info[["Version"]] != github_info$Version ) NewVersion = github_info$Version 
    } 
    
    Source = c(Source,NewSource)
    New.Version = c(New.Version,NewVersion)
    Repo = c(Repo,NewRepo)
  }
  Loaded.Packages <<- data.frame(Package,Description,Source,Version,New.Version,Built,Repo,Location,stringsAsFactors=F)
  
  
  
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


XLibraryGetSource = function(package) {
  desc = packageDescription(package) 
  
  if ( isTRUE( desc[["Priority"]] == "base") ) return("Base")
  if ( isTRUE( desc[["Repository"]] == "CRAN") ) return("CRAN")
  if ( isTRUE( desc[["RemoteType"]] == "github") ) return("GitHub")
  
  return("LOCAL")
}








