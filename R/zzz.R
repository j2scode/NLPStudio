.onLoad <- function(libname, pkgname) {

  # Instantiate Singleton Classes
  if (file.exists("./NLPStudio/.State.Rdata")) {
    welcomeMessage <- "#                      Welcome back to the NLPStudio (Beta)!                              #"
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioState <<- StateServer$new()$getInstance()
    nlpStudioState$loadState()
   } else {
    welcomeMessage <- "#                         Welcome to the NLPStudio (Beta)!                                #"
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioState <<- StateServer$new()$getInstance()
    nlpStudioState$saveState("nlpStudio", nlpStudio)
    nlpStudioState$saveState()
    nlpStudioState$loadState()
  }

  packageStartupMessage(paste0("#=========================================================================================#"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(welcomeMessage)
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("# Thank you for installing the NLPStudio package.  You are encouraged to review the       #"))
  packageStartupMessage(paste0("# the vignettes at https://www.blahblah.com before starting. With that, please be aware   #"))
  packageStartupMessage(paste0("# of several pre-instantiated objects that are required for manipulating the NLPStudio    #"))
  packageStartupMessage(paste0("# and for saving and restoring current or historial state of an object, and they are      #"))
  packageStartupMessage(paste0("# as follows:                                                                             #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#    nlpStudio - This is an object of the NLPStudio class and contains data and methods   #"))
  packageStartupMessage(paste0("#                for instantiating labs, which are the environments in which your data    #"))
  packageStartupMessage(paste0("#                analyses and workflows will reside.                                      #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#    nlpStudioState - This is an object of the StateServer class which is responsible    #"))
  packageStartupMessage(paste0("#                for saving and restoring the current state of objects within NLPStudio.  #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#    nlpSnapshots <- An object of the Snap0 class which is responsible for saving and     #"))
  packageStartupMessage(paste0("#                and restoring snapshots of objects within NLPStudio.                     #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("# In addition, there are a few labs that have been created to contain orphaned documents  #"))
  packageStartupMessage(paste0("# and collections and they are as follows:                                                #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#    orphanLab - An object of the Lab class that contains orphaned document collections   #"))
  packageStartupMessage(paste0("#    orphanCollection - An object of the DocumentCollection class that contains orphaned  #"))
  packageStartupMessage(paste0("#                documents.                                                               #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("# Well, that's it..for now.  I hope tha you find this package an enhancement to your      #"))
  packageStartupMessage(paste0("#    productivity and enjoyment.                                                          #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#                                       Data Science Salon                                #"))
  packageStartupMessage(paste0("#                                       https://github.com/DataScienceSalon/NLPStudio     #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#=========================================================================================#"))

}

