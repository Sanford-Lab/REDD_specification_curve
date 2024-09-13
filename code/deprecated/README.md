# Readme

All the code in functions/ folder should ONLY be functions and be named with a "_function" 
at the end to denote this.

## Three Step Process

[1] cleaning / processing
  two functions to convert to shp and then another to clean and process the data once
  we pull it from Google Earth Engine
  
[2] methods
  right now only contains matching... but idea is that it contains all the methods we want
  to use (synthetic controls, etc.)

[3] visualization
  code for specification curves.

The idea is that we can just call all these functions to do our task
  So have one folder with all the projects and methods and whatnot in a list.
  Call process (step 1), and then plot (step 2). Should have minimal code, so its easy to 
  test.
  
TODO:
  not sure what plot_projects file does
  synthetical control_function is currently empty ?
  how should i abstract/move spec_chart_functions as a function.
  