# photomoss
Photomoss is a full protocol that involves image retrieval, pre-processing, calculation of spectral indices and binary segmentation. Whole process is supported by a combination of cheap modified drone cameras and built-for-purpose ImageJ macros and the R package, PhotomossR. It offers a research tool to work with Biological Soil Covers, including biological soil crusts.

## Protocol:
![workflowPHOTOMOSS](https://github.com/user-attachments/assets/c9094858-47e9-476a-a88a-1b919738ac82)

**2. Image pre-processing:**

A ImageJ imj macro repository providing tools to paired images alignment and histogram matching within a picture series. We use ImageJ because it is open-source, very accessible and familiar in biological sciences, fitting with our main goal to develop an free and accessible protocol.
Find tutorial here***!! (Nagore)


**3. What is PhotomossR?**

PhotomossR is a developement from mosscoder/crustcover package (https://github.com/mosscoder/crustCover). PhotomossR is the analitical part of the Photomoss protocol, an open source protocol focused on meassure of Biological Soil Covers areas in field or lab experiments. To achieve this duty, it uses the same principles as (Fischer2012) that take advantage of Near InfraRed (NIR) and visible RGB images. With the color channels of this images, we can calculate several spectral indexes. In contrast with crustcover that measures seven index, PhotomossR can use a great set of 19 spectral indexes. As crust cover, PhotomossR core function can calculate moss area using a given spectral index and implementigg a custom threshold value, but in addition, it can apply an automatic segmentation following a set of 12 different segmentation methods if needed. Other additional functionalities of PhotomossR in comparison with crustcover are the semiautomatization of analysis over the images, and a segmentation accuracy test functionality, to test the segmentation accuracy comparing the calculated surfaces with a the baseline provided by a binary mask done with ImageJ.

**Installing PhotomossR**
```{.r }
if(require(devtools) != T){
  install.packages('devtools')
  require(devtools)
}
```
```{.r }
install_github("MMolBUs/PhotomossR")
library(PhotomossR)
```

**Running photomoss**

```{.r }
wd.path <- ''
setwd(wd.path)
````
chart2
```{.r }
# We create the chart object (a list of polygons) with the chart2 function.
# To do this we click over the color cells chart in the image.
# Important note: follow the order as indicated in the figure below.
chart <- chart2(wd.path) 
```
![image](https://github.com/user-attachments/assets/7c348dac-f727-4793-a908-ba2d940b9966)

roi2polygon.2 and extractPIX.from.Poly.
```{.r }
# Creates a readable polygon files from the ImageJ .roi files.
# Then we crop the pixels that fell inside the polygons and obtain a list polygon dataframe (obs.areas object)

roi_paths <- list.files(path = "./rois", pattern=".roi$", full.names = T, recursive = T)
obs.areas <- roi2polygon(roi.folder = "./rois", pic.folder = "./vis")
```
ccspectral.df
```{.r }
# The basic result of this function is a dataframe with the areas in number of pixels of background and moss area for each sample.
# If argument descrip = T the descriptive statistics of the different areas. que?
# The resulting data.frame is saved in a new folder in your working directory.
ccspectral.df(tif.path, # your working directory
              chart, # object chart obtained wuith the function chart2
              obs.areas, # Polygon files obtained with roi2polygon.2 function
              pdf = F,
              calculate.thresh = T,
              descrip = T,
              manual.mask.test = F,
              index. = c("NDVI", "SR"),
              threshold.method = c("Huang"),
              threshold.vector = NULL,
              descriptors. = c("median", "mean", "sd", "min", "max", "diff.range")
              )
```
Arguments:

- *_wd.path*: the path of the working directory where are the vis, nir, mask, rois folders and names.csv file.

- *chart*: polygon list obtained with chart.2 function.

- *pic.format*: Picture file format. It could be "jpg" for .jpg, .JPG and .jpeg; or "tif", for .tif format. Default = "tif".

- *obs.areas*: list of polygons data.frame obtained with roi2polygon function.

- *pdf*: logical, to present the results in image and histogram of moss areas. Default = F.

- *calculate.thresh*: logical, to Calculate autothreshold. Default = F.

- *descrip*: logical, to calculate statistical descriptors of index value in the classified areas. Default = F.

- *manual.mask.test*: logical, if you want to test the accuracy of image segmentation comparing with handmade drawn moss area. Default = F.

- *index.*: character with what index you want to calculate. By default, all options: index. = c("NDVI", "SR", "MSAVI", "EVI", "CI", "BSCI", "BI","NORR", "NORG", "NORB", "EXR", "EXG","EXB", "EXGR", "CIVE", "VEG", "HUE", "SAT", "VAL") Index are more detailed bellow.

- *threshold.method*: character, if calculate.thresh= T. The autosegmentation method to separate moss from background. The argument can be ONE of the following values: "Huang", "IJDefault", "IsoData", "Li", "Mean", "MinErrorI", "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle". Autothereshold methods are more detailed bellow.

- *threshold.vector*: numeric, if calculate.thresh= F the index value to segment the image to separate moss from background. Must have the same length than index. argument, and must respect index. argument default order.

- *descriptors.*: character, if descrip= T the statistic descriptors of index values in the classified areas. Default: descriptors. = c("median","mean","sd","min", "max","diff.range")

****
## Workflow of photomossR code
The following **diagram** describes the workflow in which the scripts are organized to create de package

```mermaid
flowchart LR
    A([calcs]) --> B{cc.spectral}
    C{chart2} -->|chart| B
 O[chart.from.tif?] --> C

   D[calculate.raster.thresh.fun.ccsdf] --> A
   E[autothreshold.value.fun.ccsdf] --> D
   F[cell.extract.color.cal.fun.ccsdf] --> A
   M[cell.count.sf.class.fun.ccsdf] --> A
   G[indexcalculation.fun.ccsdf] --> A
   H[raster.jpg.ccspectral.ccsdf] --> A
   I[raster.tif.ccspectral.ccsdf] --> A
   J[descriptor.calculation.fun] --> A
   K[plotpdf] --> A
   L[TSS.IoU] --> A
   
   N[change.labels.order] --> AA
   AA[UNKNOWN]

   Q[roi2polygon] --> P
   P[extractPIX.from.POLY] --> |obs.areas = Polygon| B
   
   B --> BB[example]
   ```
