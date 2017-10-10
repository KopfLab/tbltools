# RAT generation

## Software

To generate a PDF for an iRAT/tRAT, you need the following installed:

 - R and a good RMarkdown editor ([RStudio](https://www.rstudio.com/) is strongly recommended)
 - The [tbltools R package](https://github.com/KopfLab/tbltools)
 
## Setup / Check the demo works

The easiest way to get started is to work off the files in this demo folder. You can either download them individually or [download the whole tbltools package](https://github.com/KopfLab/tbltools/archive/master.zip) as a zip file, which will include the `demo_RAT` folder (you don't need the rest of the download).

 - Open the [`RAT_demo.Rmd`](RAT_demo.Rmd) in RStudio
 - Select `Knit to PDF` from the **Knit** menu right above the file. This will generate a PDF based on the information in the `RAT_demo.xlsx` file and the formatting instructions and surrounding text provided in the `RAT_demo.Rmd`. If you do this for the first time, RStudio may ask you to install some additional packages and software including [pandoc](https://pandoc.org/). If you are not working in RStudio, you will have to manually knit the document and call pandoc to generate the PDF. If the demo file does not generate a PDF successfully, please check in the error messages to see if you are missing any specific software before [filing a bug report](https://github.com/KopfLab/tbltools/issues).
 
## Your own iRAT/tRAT

At this point, design your RAT any way you like by modifying:

 - The text in the `RAT_demo.Rmd` file to include whichever instructions you would like to show on your test. All standard [Markdown](http://rmarkdown.rstudio.com/authoring_basics.html) formatting is supported. 
 - The commands in the **questions** chunk of the `RAT_demo.Rmd` file. You can run the whole chunk locally in RStudio to see the Markdown it generates for the questions and modify any of the parameters to change how the questions are sorted, grouped and arranged. For details on the functions and paramegers, consult the tbltools documentation by typing `?create_RAT_from_excel`, `?arrange_RAT_questions`, and `?generate_RAT_choices` from your R console. 
 - The **questions** tab in the `RAT_demo.xlsx` Excel file. This is where you can modify your questions and answers. The only columns that are absolutely necessary are **question**, **answer** and **correct** but **include** is useful to change which questions to include in the test and **group**, **order** and **layout** can be useful for additional formatting. The questions and answers themselves are also in [Markdown](http://rmarkdown.rstudio.com/authoring_basics.html) format and support the inclusion of images as well as equations (in latex format) as seen in the first few example questions.
 - The answer **key** tab in the `RAT_demo.xlsx` Excel file. This should reflect the correct answers on the IF-AT for the test. The tbltools functions will automatically ensure that the correct option is placed with the right answer when generating the questions. If any questions don't have enough answer options to fit their location on the test (e.g. a TRUE/FALSE question but the correct answer is D) you will get an error message from tbltools. The easiest way to fix this is by providing the exact position of the question using the **order** column in the Excel spreadsheet (as illustrated in the demo excel sheet, you can fix the position of some questions and leave the rest randomly placed on the test or within their questions group).
 
That's it! Just run the **questions** chunk to make sure your test still generates correctly and then you can `Knit to PDF` to make a printable PDF for administering the iRAT/tRAT.
