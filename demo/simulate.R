# Destinie 2
# Copyright © 2005-2018, Institut national de la statistique et des etudes economiques
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

####################################
#echantillon de depart
######################################
library(jsonlite)
library(openxlsx)
library(stringr)

lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}

args = commandArgs(trailingOnly = FALSE)

with_input_path = TRUE
prefixIndex = which(args == "--file")
if (length(prefixIndex) && prefixIndex < length(args)) {
  input_path = args[prefixIndex+1]
} else {
  input_path = "server/example.xlsx"
}
sourcepath = input_path


#detach(package:destinie, unload=T)
prefixIndex = which(args == "--library")
if (str_length(lib) > 0 || (length(prefixIndex) && prefixIndex < length(args))) {
  print('load separated lib...')
  from_args=length(prefixIndex) && prefixIndex < length(args)
  separate_lib=ifelse(from_args, args[prefixIndex+1], lib)
  print(separate_lib)
  .libPaths(separate_lib)
}

## Regimes
# 1 ACTUEL
# 2 ACTUEL_MODIF
# 3 DELEVOYE
# 4 COMM_PM
regime = 1
regimes = c(ACTUEL=1, ACTUEL_MODIF=2, DELEVOYE=3, COMM_PM=4)
prefixIndex = which(args == "--regime")
if (length(prefixIndex) && prefixIndex < length(args)) {
  regime=regimes[args[prefixIndex+1]]
  if(is.na(regime)) {
    regime=1
  }
}
library(destinie)

age_exo=-1
prefixIndex = which(args == "--age-exo")
if (length(prefixIndex) && prefixIndex < length(args)) {
  age_exo=as.integer(args[prefixIndex+1])
  comportement=3
  if (age_exo>0) {
    simul$ech$age_exo=age_exo
  }
}

simulate(sourcepath, regime, age_exo)
