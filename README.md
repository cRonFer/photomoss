# photomoss

# Workflow
The following **diagram** describes the workflow in which the scripts are organized to create de package

```mermaid
flowchart LR
    A([calcs]) --> B{cc.spectral}
    C{chart2}

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
   O[chart.from.tif?] --> C
   P[extractPIX.from.POLY] --> AA
   Q[roi2polygon] --> P
   AA[UNKNOWN]
   B --> BB[example]
   C --> BB[example]
   ```
