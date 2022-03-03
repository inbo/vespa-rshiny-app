rm(list=ls())
library('googleCloudRunner')

r <- './test_preprocessing.R'
ba <- cr_build_yaml(
  steps = cr_buildstep_r(r, name= "verse"),
  artifacts = cr_build_yaml_artifact(c('Individual.csv', 'Nest.csv', 'Management.csv')))

build <- cr_build(ba)
built <- cr_build_wait(build)

cr_build_artifacts(built, overwrite=TRUE)
# 2019-12-22 12:36:10 -- Saved artifact.csv to artifact.csv (1.7 Kb)

read.csv("Individual.csv")
read.csv("Nest.csv")
read.csv("Management.csv")
#                     X  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#1            Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#2        Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#3           Datsun 710 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#4       Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
# ... etc ...