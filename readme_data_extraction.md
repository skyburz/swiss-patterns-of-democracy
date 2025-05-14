# Variable Documentation Data Extraction

## Files Created

We've extracted data from the PDF documentation file "Kantonale DM_1979–2023_Codebuch.pdf" and created several files:

1. `data/pdf_extracted_text.txt` - Raw text extracted from the PDF
2. `data/variable_documentation.csv` - Initial attempt at parsing the table
3. `data/full_pdf_text.txt` - Complete text from the PDF in a single file
4. `data/potential_variables.txt` - Lines that might contain variable information
5. `data/variables_from_pdf.csv` - Attempted structured extraction of variables

## How to Use the Data

The PDF seems to have a structured table with columns for:
- **Indikator**: Variable name/identifier
- **Quelle(n)**: Sources for the data
- **Bemerkungen**: Notes or descriptions

### Options for Working with the Data

1. **Use the CSV files directly**: Review `data/variable_documentation.csv` which contains our best attempt at parsing the table structure.

2. **Manual extraction**: If you need more precise data, you can:
   - Review `data/full_pdf_text.txt` and manually extract the information you need
   - Use `data/potential_variables.txt` as a starting point to identify variable names
   - Open the PDF directly in a PDF reader for the most accurate view

3. **External Tools**: Consider using specialized PDF table extraction tools like:
   - [Tabula](https://tabula.technology/) - Free tool for extracting tables from PDFs
   - Adobe Acrobat Pro's export features
   - Online PDF table extraction services

## Next Steps

To get a clean, well-structured version of the variable documentation:

1. Review the extracted files to see if they meet your needs
2. If needed, use Tabula or similar tool to specifically extract the table
3. Clean the data using R or Excel to format it appropriately
4. Consider creating a structured data dictionary for your project

## R Scripts Created

- `extract_pdf_table.R` - Initial extraction of PDF text and attempt to parse tables
- `clean_variable_documentation.R` - Attempts to clean the extracted table data
- `final_clean.R` - Alternative approach with different parsing logic

You can modify these scripts further if needed for additional processing. 