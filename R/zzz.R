.onAttach <- function( lib, pkg ) {
  packageStartupMessage(
    paste0( "\nRecommendated citation for the 'CropScapeR' package is:\n",
            "Bowen Chen (2020). ",
            "CropScapeR: Access Cropland Data Layer Data via the 'CropScape' Web Service. ",
            "R package version 1.1.2. \n\n",
            "For more information, visit the package website at: https://github.com/cbw1243/CropScapeR. ",
            "If you have any question/suggestion/comment ",
            "regarding the 'CropScapeR' package, ",
            "please send emails to Bowen Chen (bwchen0719@gmail.com)."),
    domain = NULL,  appendLF = TRUE )
}
