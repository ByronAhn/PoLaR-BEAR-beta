# PoLaR-BEAR-beta

## To-do list

* @BTA
  + priority: comment up code 
    * indicate the purpose / argument / outputs of each function
    * clarify what rPraat and PraatR are each doing in the scripts
  + add: using output of extract-info from PoLaR Praat plugin
  + work on adding algorithms that deal in pseudo labels / utterance properties (see CSV list below)
* @Simmons students:
  + Read over existing R code so you can know what functions do what
  + get NMV's python code - translate it into R
  + **_MAKE SURE_** that you are not duplicating the functionality of any of the existing R code

### Intended CSV outputs:

* make sure there are CSVs per…
  + …pitch accent (pseudo label)
    * especially including slopes:
      + slopes domain = find the higest F0, and the lowest F0 preceding it
      + overall rise = calculate slope of lowest to highest that define the domain
      + steepest rise = calculate slopes between neighbor points, and use the highest value; submit both values to analysis
  + …range
  + …utterance
