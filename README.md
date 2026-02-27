# Budget Spreadsheet Builder

This project includes a small Python CLI app that takes a bank CSV export and writes a budgeting workbook using your existing Excel file as the baseline structure.

## What it does

- Reads your transaction CSV.
- Normalizes date/description/amount fields.
- Auto-categorizes transactions using keyword rules.
- Creates three sheets in the output workbook:
  - `Transactions`
  - `Category Summary`
  - `Monthly Summary`
- Preserves any other sheets that already exist in your baseline workbook template.

## Setup

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Usage

```bash
python budget_builder.py \
  --csv /path/to/bank_export.csv \
  --template /path/to/your_baseline.xlsx \
  --output /path/to/budget_output.xlsx
```

### Optional flags

- `--date-col` (default: `Date`)
- `--description-col` (default: `Description`)
- `--amount-col` (default: `Amount`)
- `--debit-positive` (use this if your bank exports debits as positive and credits as negative)

## Expected CSV columns

By default, these columns are expected:

- `Date`
- `Description`
- `Amount`

If your export uses different names, pass `--date-col`, `--description-col`, and `--amount-col`.

## Customizing categories

Edit `DEFAULT_CATEGORY_MAP` in `budget_builder.py` to match your own spending patterns.
