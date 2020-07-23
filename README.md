![CRAN/METACRAN](https://img.shields.io/cran/v/CropScapeR?color=brightgreen) ![](http://cranlogs.r-pkg.org/badges/grand-total/CropScapeR?color=brightgreen) 


## CropScapeR: Access Cropland Data Layer data via the CropScape Web Service

The [Cropland Data Layer (CDL)](https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php) is a data product produced by the National Agricultural Statistics Service of U.S. Department of Agriculture. It provides geo-referenced, high accuracy, 30 or 56 meter resolution, crop-specific cropland land cover information for up to 48 contiguous states in the U.S. from 1997 to the present. This data product has been extensively used in agricultural research [1]. 

[CropScape](https://nassgeodata.gmu.edu/CropScape/) is an interactive Web CDL exploring system, and it was developed to query, visualize, disseminate, and analyze CDL data geospatially through standard geospatial Web services in a publicly accessible online environment[2]. The development of the `CropScapeR` package is to allow R users to easily utilize the geospatial processing services provided by CropScape, so that they can effectively and efficiently access and analyze the CDL data. 

## Key functions     
We implement four geospatial processing services provided by [CropScape](https://nassgeodata.gmu.edu/CropScape/) in `R`: 

1. `GetCDLValue`/`GetCDLFile`         
The `GetCDLValue` service finds the pixel value at a given location (defined by a coordinate), and the `GetCDLFile` service fetches irregularly shaped CDL data[2]. The `GetCDLValue` and `GetCDLFile` services are implemented in `R` by one function: `GetCDLData`. The `GetCDLData` function takes an Area of Interest (AOI) and a year value as inputs and return the requested CDL raster data.  This function does the data request in two steps. First, the function sends data requests to the CropScape online server using the `GET` function from the `httr` package. Second, the function reads the requested data into `R` using the `raster` function from the `raster` package. By default, the data returned from the CropScape are in the raster-based GeoTIFF file format. Users can choose to save the raw data in TIF format into their local drives.

2. `GetCDLImage`         
The `GetCDLImage` service generates the preview images of the customized CDL data and the Keyhole Markup Language (KML) file with links to actual images that can be displayed in Google Earth[2]. This service is implemented by the `GetCDLImage` function.

3. `GetCDLStat`          
The `GetCDLImage` service generates statistical information (for example, value, category name, and acreage) of the CDL data of an AOI[2]. This service is implemented by the `GetCDLStat` function.

4. `GetCDLComp`            
The `GetCDLComp` service performs cropland change analysis by comparing the pixels of the cropland area defined by AOI between two given years[2]. This service is implemented by the `GetCDLComp` function.

## Package usage  
### Key parameters  
The four functions introduced above take three necessary inputs to work: `aoi`, `year`, `type`. API key is not needed. 

* `aoi`: Area of Interest. An AOI can be a county (defined by county FIPS code), a rectangle/box (defined by four corner points), a polygon (defined by multiple points), a point (defined a single coordinate), or a custom area (defined by an ESRI shapefile provided by users). 
* `year`: a year value.   
* `type`: Type of the AOI. 'f' for county, 'b' for box area, 'ps' for polygon, 'p' for a single coordinate, 's' for ESRI shapefile.

### Examples  
#### Get data for a county 
```
# Champaign county in Illinois, FIPS code: 17019
data <- GetCDLData(aoi = 17019, year = 2018, type = 'f')
```
#### Get data for a box area    
The AOI should be a numeric vector with 4 elements. The format to define the box is (min x, min y, max x, max y).  
```
data <- GetCDLData(aoi = c(130783,2203171,153923,2217961), year = 2018, type = 'b')
```
Note that the default coordinate system is the Albers projection system. Users can use longitude/latitude by specifying a coordinate system: 
```
data <- GetCDLData(aoi = c(130783,2203171,153923,2217961), year = '2018', type = 'b')
```
If users have a shapefile, users can extract the coordinates of bounding box of the shapefile and then make data request:
```
# Read the shapefile into R
shape_file <- sf::st_read("Your shapefile. File name ended with .shp")
# Convert into a Spatial object
shape_file_s <- as(shape_file, "Spatial") 
# Get bounding box of this shapefile 
box <- as.vector(sp::bbox(shape_file_s))
# Extract data 
data <- GetCDLData(aoi = bbox, year = '2018', type = 'b')
```
The above example assumes that the shapefile has the Albers projection system. If not, make sure that you specify the correct system (same to the shapefile) in `crs`. 

#### Get data for a polygon     
The AOI should be a numeric vector with at least 6 elements. The format to define the polygon is (x1, y2, x2, y2, ..., xn, yn).  
```
# A  triangle area defined by 3 coordinates 
data <- GetCDLData(aoi = c(175207,2219600,175207,2235525,213693,2219600), year = '2018', type = 'ps')
```

#### Get data for a point     
The AOI should be a numeric vector with 2 elements. The format to define the point is (x, y).  
```
data <- GetCDLData(aoi = c(-94.6754,42.1197), year = 2018, type = 'p', crs = '+init=epsg:4326')
```

#### Get data for a custom area defined by a shapefile   
The CropScape server takes shapefile as an AOI to make data request. Yet, it requires users to provide a URL of a compressed ESRI shapefile. The .shp, .shx, .dbf, and .prj files must all be compressed with no subdirectories in a single ZIP file. In cases that the compressed shapefile is saved in the local disk, this shapefile needs to be published to a website URL (so CropScape can read the shapefile). 

There are many ways to generate a URL for a file saved in the computer. Here is an example. Assume that you save the zipped shapefile in Dropbox. First, create a link to the file by right clicking on the file and then selecting 'Copy Dropbox link'. In my case, I get this link: https://www.dropbox.com/s/cvcxjpyakxfyfpm/York_SF_watershed.zip?dl=0
Second, remove '?dl=0' from the link content to make sure that the link ends with 'zip'. Then copy to `R` and make data request:
```
link <- 'https://www.dropbox.com/s/cvcxjpyakxfyfpm/York_SF_watershed.zip'
data <- GetCDLData(aoi = link, year = 2018, type = 's')
```
Because the zipped shapefile is directly sent to CropScape, the `GetCDLData` function cannot convert the coordinate system before sending the request. Therefore, users must ensure that the zipping shapefile has the Albers projection system.  

The usage of the other three functions (`GetCDLImage`, `GetCDLStat`, `GetCDLComp`) are similar to the usage of `GetCDLData`. So, we do not provide more examples here. 

## Technical notes   
###  SSL certificate   
The `CropScapeR` package was developed under the Windows system. Some unanticipated technical issues might occur when using the `CropScapeR` package in a Mac operating system. A notable one is the SSL certificate problem. SSL refers to the Secure Sockets Layer, and SSL certificate displays important information for verifying the owner of a website and encrypting web traffic with SSL/TLS for securing connecttion. Several Mac users have reported errors called 'SSL certificate problem: SSL certificate expired'. As the name suggests, this is because CropScape server has an expired certificate, which affects the Mac users. Windows users should not expect this issue. 

With an invalid SSL certificate, the `GetCDLData` function would fail because: (1) it cannot send httr GET request any more; and (2) it cannot read the requested raster TIF data via the `raster` function any more. Here is a two-step workaround of the certificate issue. At step 1, specify in `R` that you want to skip the certificate validation when making the httr GET request. At step 2, download the raster TIF data into your local drive using the `download.file` function with `wget`, and then read the downloaded raster file using the `raster` function. The second step is automatically processed inside the `GetCDLData` function. So you just have to do the first step manually. Below is an example to get the CDL data for the Champaign county in 2018 on a Mac computer.  

```
# Skip the SSL check
httr::set_config(httr::config(ssl_verifypeer = 0L))
# Automatically generate a temporary path to save the data
tif_file <- tempfile(fileext = '.tif')
# Download the raster TIF file into specified path, also read into R 
data <- GetCDLData(aoi = 17019, year = 2018, type = 'f', save_path = tif_file)
```

## Package installation 
The `CropScapeR` package is accepted by `CRAN`, it can be directly installed in `R`
```
install.packages("CropScapeR")
```
To install development version of the package, run the following codes in `R`:
```
install.packages("devtools") # Run this if the devtools package is not installed.     
devtools::install_github("cbw1243/CropScapeR")  
```
The development version provides the most recent updates of the package. 

Note: `CropScapeR` package depends on the `rgdal` and `sp` packages to process the raster files. 

## Contact   
[Bowen Chen](https://www.bwchen.com), PhD (bwchen@illinois.edu)

## Acknowledgement      
The development of this package was supported by USDA-NRCS Agreement No. NR193A750016C001 through the Cooperative Ecosystem Studies Units network. Any opinions, findings, conclusions, or recommendations expressed are those of the author(s) and do not necessarily reflect the view of the U.S. Department of Agriculture. 

Dr. [Benjamin Gramig](https://www.bengramig.com/) is a contributor of this package. I thank Dr. [Taro Mieno](http://taromieno.netlify.com/) for providing useful suggestions. 


## Reference   

[1] Boryan, Claire, Zhengwei Yang, Rick Mueller, and Mike Craig. 2011. Monitoring US Agriculture: The US Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer Program. *Geocarto International* 26 (5): 341–58. https://doi.org/10.1080/10106049.2011.562309.

[2] Han, Weiguo, Zhengwei Yang, Liping Di, and Richard Mueller. 2012. CropScape: A Web Service Based Application for Exploring and Disseminating US Conterminous Geospatial Cropland Data Products for Decision Support. *Computers and Electronics in Agriculture* 84 (June): 111–23. https://doi.org/10.1016/j.compag.2012.03.005.


