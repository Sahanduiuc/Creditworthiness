# Creditworthiness
R and KnitR code to produce results found in "Credit Scoring via Logistic Regression" (http://www.utstat.utoronto.ca/~ali/papers/creditworthinessProject.pdf)

The main file to run is logisticRegression.Rnw which produces the .tex and .pdf files that make up the document. This requires KnitR which is a package that allows R code to be weaved with TeX code to produce output efficiently. For convenience, the code to produce the results alone (without the .tex and .pdf files) is isolated in main_script.R

The logisticRegression.bib file is needed to produce the references in the PDF file. 

The data for this analysis is found in the file german.data.txt which is freely available from the UC Irvine Machine Learning Repository (https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data)
