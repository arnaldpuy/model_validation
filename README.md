
# Turbulent code in water models

[Arnald Puy](https://www.arnaldpuy.com/), Mariano Méndez, Nanxin Wei, Seth N. Linga, 
Ethan Bacon, Joshua Caulcott-Copper, Edward Ahmad, Euan Willis, Carmen Aguiló, Fernando G. Tinetti

This study examines the semantics of "validation" and "verification" in 19 large-scale
water models used at the science-policy interface and examines their code through software
quality assessment metrics.

## Abstract

*Water models used in scientific research simulate various aspects of the water cycle 
from the basin to the global level and inform our approach to water challenges and climate change. 
Although their outputs are compared against external data as part of the model quality check, 
the standard of their underlying source code remains unknown. Here we benchmark 19 
state-of-the-art water models against several software quality metrics and find that 
most are untestable: cyclomatic complexities are beyond evaluable limits and code readability 
is nonexistent or extremely difficult. Our results suggest that scientific water models lag 
behind in almost all standards, best practices and large-scale software development guidelines.*

## Replication

### Data

This repository contains the data and files produced in this study.

#### Generated data

The files below are necessary to replicate our study. 

* `validation_work_students.xlsx`
* `dt.papers.close.reading.xlsx`

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the primary data section are stored for a successful 
compilation.

