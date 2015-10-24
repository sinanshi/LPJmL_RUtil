# LPJmL_RUtil
## Get start with LPJmL_RUtil
```bash
git clone https://github.com/sinanshi/LPJmL_RUtil
sudo R CMD install LPJmL_RUtil
```
```R
library(lpjutil)
```
## Notes
* How to generate documentation
 ```
 library(roxygen2)
 roxygenize("LPJmL_RUtil/")
 R CMD Rd2pdf LPJmL_RUtil
```
