makediags <- function(casedata, rowname = "Rows", colname = "Cols"){
#
# Function to create a factor identifying diagonal cells of a square
# contingency table that is in case data form
#
  numrow <- max(casedata[rowname])
  diagfactor <- rep(0, length(casedata[rowname]))
  for(i in 1:numrow)
  diagfactor <- diagfactor + as.numeric(casedata[rowname] == i & casedata[colname] == i) * i
  diagfactor <- as.factor(diagfactor)
  contrasts(diagfactor) <- contr.treatment(numrow + 1)
  diagfactor}
#
makeoffdiags <- function(casedata, rowname = "Rows", colname = "Cols"){
#
# Function to create a factor identifying symmetric off-diagonal cells of a square
# contingency table that is in case data form. Factor is coded based on indicator variables.
#
  numrow <- max(casedata[rowname])
  offdiagfactor <- rep(0, length(casedata[rowname]))
  ind <- 1
  for(i in 1:(numrow - 1)) {
    for(j in (i + 1):numrow) {
      offdiagfactor <- offdiagfactor + (as.numeric(casedata[rowname] == i & casedata[colname] == j) + 
        as.numeric(casedata[rowname] == j & casedata[colname] == i)) * ind
      ind <- ind + 1}}
	offdiagfactor <- as.factor(offdiagfactor)
	contrasts(offdiagfactor) <- contr.treatment(ind)
	offdiagfactor}
#

table2case <- function(intable, rowname="Rows", colname="Cols"){
#
# Convert two-way contingency table in table to a case data form data frame
#
# intable - input contingency table as matrix
#
# outdata - output data frame of table in glm format
# 
  rows <- rep(c(1:nrow(intable)),ncol(intable))
  cols <- rep(c(1:ncol(intable)),each=nrow(intable))
  count <- as.vector(intable)
  outdata <- data.frame(cbind(rows,cols,count))
  names(outdata) <- c(rowname,colname,"Count")
 outdata}