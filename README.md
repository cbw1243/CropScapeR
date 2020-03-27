## CropScapeR: Access Cropland Data Layer data via the CropScape web service

The [Cropland Data Layer (CDL)](https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php) is a data product produced by the National Agricultural Statistics Service of U.S. Department of Agriculture. It provides geo-referenced, high accuracy, 30 or 56 m resolution, crop-specific cropland land cover information for up to 48 contiguous states in the U.S. from 1997 to the present. This data product has been extensively used in the agriculture research [1]. 

[CropScape](https://nassgeodata.gmu.edu/CropScape/) is an interactive Web CDL exploring system, and it was developed to query, visualize, disseminate, and analyze CDL data geospatially through standard geospatial Web services in a publicly accessible online environment[2]. The development of the `CropScapeR` package is to allow R users to easily utilize the geospatial processing services provided by CropScape, so that users can effectively and efficiently access and analyze the CDL data. 

## Key functions     
We implement four geospatial processing services provided by [CropScape](https://nassgeodata.gmu.edu/CropScape/) in `R`: 

1. `GetCDLValue`/`GetCDLFile`         
The `GetCDLValue` service finds the pixel value at a given location (defined by a coordinate), and the `GetCDLFile` service fetches irregularly shaped CDL data[2]. The shape could be a county boundary (defined by county FIPS code), a triangle area (defined by three coordinates), or a rectangle/box (defined by four corner points). The `GetCDLValue` and `GetCDLFile` services are implemented by a single R function: `GetCDLData`. The `GetCDLData` function takes an Area of Interest (AOI) and a crop year value as inputs and return the CDL raster data. The raster data can be saved as TIF file or a data table. Examples are provided in the help file of `GetCDLData`.    
2. `GetCDLImage`         
The `GetCDLImage` service generates the preview images of the customized CDL data and the Keyhole Markup Language (KML) file with links to actual images that can be displayed in Google Earth[2]. This service is implemented by the `GetCDLImage` function.

3. `GetCDLStat`          
The `GetCDLImage` service generates statistical information (for example, value, category name, and acreage) of the CDL data of an AOI[2]. This service is implemented by the `GetCDLStat` function.

4. `GetCDLComp`            
The `GetCDLComp` service performs cropland change analysis by comparing the pixels of the cropland area defined by AOI between two given years[2]. This service is implemented by the `GetCDLComp` function.

Examples using the `R` functions are provided in the function help files. CropScape provides several other geoprocessing services, and they would be implemented in R in future developments. 

## Package installation   
To install the package, run the following codes in `R`:
```
install.packages("devtools") # Run this if the devtools package is not installed.     
devtools::install_github("cbw1243/CropScapeR")  
```
Note the `CropScapeR` package depends on the `rgdal` and `sp` packages to process the raster files. 

## Development   
The package is initially released on March 26, 2020 at GitHub to collect users feedbacks. The package will be submitted to CRAN. If you have any suggestion, please contact the author.

Note that the package could be updated at any time at the current stage. To enjoy the latest version, install again before using it.

## Acknowledgement      
The development of this package was supported by USDA-NRCS Agreement No. NR193A750016C001 through the Cooperative Ecosystem Studies Units network. Any opinions, findings, conclusions, or recommendations expressed are those of the author(s) and do not necessarily reflect the view of the U.S. Department of Agriculture. 

## Contact   
[Bowen Chen](https://sites.google.com/view/bwchen), PhD (bwchen@illinois.edu)

## Reference   

[1] Boryan, Claire, Zhengwei Yang, Rick Mueller, and Mike Craig. 2011. Monitoring US Agriculture: The US Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer Program. *Geocarto International* 26 (5): 341–58. https://doi.org/10.1080/10106049.2011.562309.

[2] Han, Weiguo, Zhengwei Yang, Liping Di, and Richard Mueller. 2012. CropScape: A Web Service Based Application for Exploring and Disseminating US Conterminous Geospatial Cropland Data Products for Decision Support. *Computers and Electronics in Agriculture* 84 (June): 111–23. https://doi.org/10.1016/j.compag.2012.03.005.


