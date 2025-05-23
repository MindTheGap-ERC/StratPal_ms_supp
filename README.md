# StratPal_ms_supp

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14669896.svg)](https://doi.org/10.5281/zenodo.14669896)

Supplementary code for the manuscript "StratPal: An R package for creating stratigraphic paleobiology modeling pipelines"

## Usage

Open the file "StratPal_ms_supp.Rproj" in the RStudion IDE. This will set all paths correctly, and install the `renv` package (if not already installed). Then, run

```R
renv::restore()
```

to install all required packages and their dependencies. Then you can inspect the code under `code/examples.R` or generate the figures via

```R
source("code/examples.R")
```

This will generate all figures and save them in the folder `figs/`.

## Citation

To cite this repository, please use

* Hohmann, N. (2025). Supplementary code for the manuscript "StratPal: An R package for creating stratigraphic paleobiology modeling pipelines" (v1.0.1). Zenodo. https://doi.org/10.5281/zenodo.14669896

## License

Apache 2.0, see LICENSE file for full text

## Copyright

Copyright 2023 Netherlands eScience Center and Utrecht University

## Funding information

Funded by the European Union (ERC, MindTheGap, StG project no 101041077). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Research Council. Neither the European Union nor the granting authority can be held responsible for them.
![European Union and European Research Council logos](https://erc.europa.eu/sites/default/files/2023-06/LOGO_ERC-FLAG_FP.png)
