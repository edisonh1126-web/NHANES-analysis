# NHANES BMI & SBP Analysis (2021–2023)

This repository contains scripts and a reproducible report that analyze the association between BMI and mean systolic blood pressure (SBP) among adults (>=20 years) using NHANES 2021–2023.

## Contents
- `NHANES_analysis_report.Rmd` — Complete reproducible R Markdown. Knit to PDF for the final report.
- `Lab session week 5 (Class).R` — Week 5 scripts (BMI & SBP cleaning and visualization).
- `Lab session week 6 (Class).R` — Week 6 scripts (education, race, BP trial reshaping).
- `outputs/` — Generated figures and report (may be excluded from repo if large).

## Quick start (how to reproduce)
1. Download the NHANES XPT files and place them in a local folder. Set `data_dir` at the top of `NHANES_analysis_report.Rmd` to that folder.
   - Required files: `DEMO_L.XPT`, `BMX_L.XPT`, `BPXO_L.XPT`.
2. Open the RMarkdown in RStudio and click Knit to produce the PDF report.

Or from PowerShell / command line:

```powershell
# from project root
Rscript -e "rmarkdown::render('C:/Users/Edison/Downloads/NHANES_analysis_report.Rmd', output_format='pdf_document')"
```

## Data
The required NHANES files (DEMO_L.XPT, BMX_L.XPT, BPXO_L.XPT) are included in this repository for convenience to reproduce the analysis. If you prefer to obtain them yourself, download the appropriate 2021–2023 files from the NHANES website and set `data_dir` at the top of `NHANES_analysis_report.Rmd`.

If you plan to host large data files in a Git repository, consider using Git LFS or an external storage location to avoid inflating the repo size.

## Git / GitHub (quick instructions)
1. Initialize repo and commit files:
```powershell
cd 'C:/Users/Edison/Downloads'
git init
---


\- `Lab session week 5 (Class).R` — week 5 code.

\- `Lab session week 6 (Class).R` — week 6 code.

\- `outputs/` — saved figures and report.



How to reproduce:

1\. Put NHANES XPT files (DEMO\_L.XPT, BMX\_L.XPT, BPXO\_L.XPT) in a local folder and set `data\_dir` at top of the Rmd.

2\. Install packages and run Knit in RStudio, or run:

