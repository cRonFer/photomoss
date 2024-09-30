## photomoss

# Workflow
![workflowPHOTOMOSS](https://github.com/user-attachments/assets/ad42c860-b3f0-4ab9-b6e3-3655efd879ee)

# Workflow of photomossR code
The following **diagram** describes the workflow in which the scripts are organized to create de package

```mermaid
flowchart LR
    A([calcs]) --> B{cc.spectral}
    C{chart2} -->|chart| B
 O[chart.from.tif?] --> C

   D[calculate.raster.thresh.fun.ccsdf] --> A
   E[autothreshold.value.fun.ccsdf] --> D
   F[cell.extract.color.cal.fun.ccsdf] --> A
   G[indexcalculation.fun.ccsdf] --> A
   H[raster.jpg.ccspectral.ccsdf] --> A
   I[raster.tif.ccspectral.ccsdf] --> A
   J[descriptor.calculation.fun] --> A
   K[plotpdf] --> A
   L[TSS.IoU] --> A
   
   M[cell.count.sf.class.fun.ccsdf] --> AA
   N[change.labels.order] --> AA
   AA[UNKNOWN]

   Q[roi2polygon] --> P
   P[extractPIX.from.POLY] --> |obs.areas = Polygon| B
   
   B --> BB[example]
   ```
