## fvp: An R package for Athlete Force-Velocity-Power Profiling

The fvp package was written to extend the functionality of its predecessory, *midsprint*. Users can use this package to model and athlete's sprint- and jump-based force-velocity profile. These models include:    
     
* Sprint abilities modelled over time and distance
* In-game speed-acceleration model
* Sprint test models using distance and time splits
* Jump-based force-velocity model
    
Once a player's abilities are modelled, the user can opt to return a data set that encompasses their modelled force-velocity-power profile.
    
Since this package is built to provide practitioners modelled observations, the package does not support plotting functions like those in *midsprint*. As such, *midsprint* will be updated to include these reporting functions with *fvp* providing the back-end analyses. 

## Installing the Package

To install the package, copy-and-paste the following code into your R console. The package is very small and should download quickly.

```
devtools::install_github("aaronzpearson/fvp")
library(fvp)
```

The plotting examples rely on two other packages to return aesthetically pleasing plots. If you don't have these packages installed on your computer, you can download them copy-and-pasting the following into your R console. You do not need to install these packages for the package to work.

```
install.packages("ggplot2")
install.packages("patchwork")

library(ggplot2)
library(patchwork)
```


## Package Functionality

**Note** The models have been validated using values in metric (m/s, m/s/s, etc.). To convert your current values to metric, use the `convert.to.metric()` function. This function can be applied to multiple variables effectively using a function like `apply()` from base R or `mutate()` from the *dplyr* package.

### Function Families
    
To provide practitioners the ability to produce multiple analyses, functions are grouped by *family*. As such, each family of functions begins with the same prefix. Expanding on the models outlined above, the prefixes are:
    
* `gps`: Sprint abilities modelled over time and distance
* `sa`: In-game speed-acceleration model
* `scout`: Sprint test models using distance and time splits
* `fv`: Jump-based force-velocity model
* `fvp`: Modelled force-velocity-power profile from sprint models

### Function Naming Conventions

For consistency, the function names (after the prefix) follow the following naming convention:
    
* `.data`: Cleaned and formatted speed and acceleration observations
* `.data.player`: A player's anthropomorphic data and weather conditions
* `.data.testing`: Supplemental testing information like load and athlete testing results
* `.player.profile`: Models a player's abilities and returns a summarized data frame 
* `.player.profile.game `: Unique to the *gps* family, returns a player's observed sprint abilities
* `.player.splits`: Speed, acceleration, and time at distinct distances
* `.results.model`: Data set containing modelled observations
* `.results.observed`: Data set containing observations that are used to model a player's abilities
* `.results.game`: Unique to the *gps* family, returns a player's modelled in-game abilities
* `.results.fitted`: Unique to the *sa* family, returns the data that were maintained to fit the linear model

*See fvp-vignette.pdf for sample code and model outputs*
