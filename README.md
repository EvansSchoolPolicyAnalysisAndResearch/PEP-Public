# Policy Explorer Platform (Template Repository)

## Setup for first-time users
- If you don't already have them, download [R](https://cran.rstudio.com/) and [R Studio](https://posit.co/download/rstudio-desktop/). [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is strongly recommended for Windows users.
- Either download the latest release of this repository or clone it using your preferred git client.
- Open the .Rproj file in the main directory (it should automatically load in R Studio).
- If you are a regular R user, type `renv::hydrate()` into the console. It can decrease setup time by associating packages you're already using with the project and download any additional ones as needed.
- If you encounter a problem or are running this from a fresh installation of R `renv::restore()` will install the exact versions of the packages used to create the app.
- Open app.R and press the "Run App" button that appears in the upper right hand corner of the file viewer pane or type `shiny::runApp()` into the console.
- Follow the instructions in the user guide for setting up the app with your own data, or explore examples using [the demo](https://github.com/EvansSchoolPolicyAnalysisAndResearch/PEP-Demo).

## What is PEP?
Originally developed to facilitate policy analysis and program implementation based on data from recurring agricultural surveys, PEP has now evolved into a general-purpose policy analysis framework for other survey-based, administrative, or RCT datasets. Elements of the user interface and data management are controlled through spreadsheets to enable users to navigate large datasets without having to write their own code.

## Why build PEP?

The challenge of successfully delivering evidence-grounded insights to decisionmakers has been addressed by a huge variety of software packages, each with different strengths, limitations, and costs. None, however, are directly grounded in the policy analysis process. PEP aims to serve policy analysts by embedding data analysis within the larger policy analysis context, by providing a framework that enables users to organize contextual information to ground data products in scholarly literature and/or reports and consideration of community needs via stakeholder mapping. PEP can leverage R's vast library of community-maintained packages to benefit participants in every step of the policy analysis process:

**For decisionmakers**, PEP provides a more interactive experience than a standard policy analysis report, enabling comparisons between alternatives and a way to swiftly iterate on options or proposals.  
**For policy analysis professionals**, PEP provides a framework for organizing ideas and data, summarizing data according to established practices, and exporting regression-ready datasets for follow-up statistical analyses.  
**For educators**, PEP can be used as a teaching tool for policy analysis, providing a way to construct and customize examples without needing to write your own code and giving students a structured format to work through their own analyses.  
**For advanced R users in policy and the social sciences**, PEP can provide a customizable and extensible foundation for building custom applications for organizations that need low-cost and low-complexity solutions.  

## Our Roadmap

This project is under active development, and we intend to continue releasing features and improving the user experience over the next several months. Our priorities include:
- More detailed options for mapping
- Embedded tools and step-by-step guidance for first time setup
- UX enhancements and custom configuration options

## About the Team

PEP was built and is maintained by EPAR postdoctoral scholars Vanisha Sharma and Joaquin Mayorga and EPAR research scientist Andrew Tomes.

## Talk to us!

Please feel free to submit an issue (requires free GitHub account) or email us @ uw.eparx@gmail.com with feedback or suggestions.


## License
[![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
