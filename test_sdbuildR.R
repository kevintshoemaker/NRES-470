
# install.packages("sdbuildR")

library(sdbuildR)
library(textutils)


# load xmile model -------

sfm <- xmile("SIR")
plot(sfm)
sim <- simulate(sfm)
plot(sim)


# load model from insightMaker!  -----------

URL <- "https://insightmaker.com/insight/1znQvT6mXmNXQJrQzsCwkX/New-2026"
sfm <- insightmaker_to_sfm(URL = URL)
sim <- simulate(sfm)
plot(sim)


# build model from scratch -----

sfm <- xmile() |>
  header(name = "Population growth") |>
  build("X", "stock", eqn = "100", label = "Population size") |>
  build("change", "flow",
        eqn = "r * (1 - X/K)", to = "X",
        label = "Births and Deaths"
  ) |>
  build("r", "constant", eqn = "5", label = "Growth rate") |>
  build("K", "constant", eqn = "10000", label = "Carrying capacity") |>
  sim_specs(stop = 10000, time_units = "days")
sim=simulate(sfm)
plot(sim)



summary(sfm)
as.data.frame(sfm)


# set up Julia ----------

   # install juliaup and use this to load latest stable release ... 
library(sdbuildR)
library(JuliaConnectoR)

# juliaSetupOk()
# stopJulia()
startJuliaServer()

# C:\\Users\\kshoemaker\\.julia\\juliaup\\julia-1.12.4+0.x64.w64.mingw32\\bin\\julia.exe

# usethis::edit_r_environ()  # add Julia to PATH

install_julia_env()  # set up Julia environment for sdbuildR

julia_status()

## activate Julia session for sdbuildR -----

use_julia()

use_julia(stop = TRUE)  # stop session














