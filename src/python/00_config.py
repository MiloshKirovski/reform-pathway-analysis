from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[2]

DATA_ORIGINAL  = PROJECT_ROOT / "data" / "original"
DATA_PROCESSED = PROJECT_ROOT / "data" / "processed"
PAPER_TABLES   = PROJECT_ROOT / "paper_outputs" / "tables"
PAPER_FIGURES  = PROJECT_ROOT / "paper_outputs" / "figures"

for _d in [DATA_PROCESSED, PAPER_TABLES, PAPER_FIGURES]:
    _d.mkdir(parents=True, exist_ok=True)

PWT_XLSX = DATA_ORIGINAL / "pwt110.xlsx"

EU_2004_DONORS = [
    "Estonia", "Czechia", "Slovenia", "Poland", "Hungary",
    "Slovakia", "Latvia", "Lithuania", "Malta", "Cyprus",
]

BALKANS = [
    "North Macedonia", "Albania", "Serbia",
    "Bosnia and Herzegovina", "Montenegro",
]

TREATED_UNIT   = "North Macedonia"
TREAT_YEAR     = 2004
YEARS_PRE      = list(range(1990, TREAT_YEAR))
YEARS_POST     = list(range(TREAT_YEAR, 2014))
YEARS_ALL      = YEARS_PRE + YEARS_POST

DONORS_NMK = EU_2004_DONORS

RIDGE_ALPHAS_LOG = (-4, 4, 60)
SCM_MAXITER      = 20_000
SCM_FTOL         = 1e-12

BLOCK1 = list(range(2004, 2009))
BLOCK2 = list(range(2009, 2014))

DONORS_NON_EU_BSTS = [
    "Albania", "Bosnia and Herzegovina", "Serbia", "Montenegro",
    "Moldova", "Ukraine", "Belarus", "Georgia", "Armenia",
]
DONORS_EU2004_BSTS = EU_2004_DONORS + ["Bulgaria", "Romania"]
YEARS_BSTS = list(range(1990, 2015))