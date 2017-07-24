# http://bioinfo.umassmed.edu/bootstrappers/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
} #load / install+load installr

# using the package:
updateR()