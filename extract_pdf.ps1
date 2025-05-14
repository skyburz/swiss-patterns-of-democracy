# Use Get-ChildItem with wildcards to get the actual file path
$pdfFile = Get-ChildItem -Path "data\*Codebuch.pdf" | Select-Object -First 1
$pdfPath = $pdfFile.FullName
$outputPath = Join-Path (Get-Location) "data\pdf_extracted_text.txt"

Write-Host "Attempting to extract text from: $pdfPath"
Write-Host "Output will be saved to: $outputPath"

# Try to install a PDF extraction tool
try {
    # Try to use PowerShell's built-in .NET methods
    Write-Host "Trying to convert PDF using .NET methods..."
    
    # Create a simple C# application to extract text from PDF
    $csharpCode = @"
    using System;
    using System.IO;
    using System.Text;
    
    public class PdfTextExtractor {
        public static string ExtractText(string pdfPath) {
            try {
                // This is a simple placeholder - in a real script we'd use a PDF library
                StringBuilder sb = new StringBuilder();
                sb.AppendLine("This is placeholder text.");
                sb.AppendLine("The actual PDF text extraction requires a PDF library.");
                sb.AppendLine("Please install a tool like iTextSharp, PDFBox, or xpdf.");
                return sb.ToString();
            } catch (Exception ex) {
                return "Failed to extract text: " + ex.Message;
            }
        }
    }
"@
    
    Add-Type -TypeDefinition $csharpCode -Language CSharp
    
    # Since we don't have a proper PDF library, let's create a text file with instructions
    $instructionsText = @"
Unfortunately, extracting text from PDF files requires specialized libraries or tools that aren't built into PowerShell.

To extract text from your PDF, try one of these options:

1. Install Adobe Acrobat Reader and use its export features
2. Install a PDF tool like xpdf (pdftotext command) from: https://www.xpdfreader.com/download.html
3. Install a Python environment and use PyPDF2 library
4. Use an online PDF-to-text converter service

The PDF file you're trying to convert is: $pdfPath
"@
    
    $instructionsText | Out-File -FilePath $outputPath -Encoding UTF8
    Write-Host "Created instructions file at $outputPath"
    
} catch {
    Write-Host "Extraction failed: $_"
    Write-Host "Please install a PDF tool like xpdf or pdftotext."
} 