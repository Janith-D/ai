<#
run_checks.ps1

PowerShell wrapper to run local verification steps:
- verifies `swipl` is available on PATH
- runs `python .\quick_check.py`
- runs SWI-Prolog plunit tests

Usage: .\run_checks.ps1
#>

# Ensure script exits with failure if any command fails
$ErrorActionPreference = 'Stop'

function Error-Exit($msg, $code=1) {
    Write-Error $msg
    exit $code
}

# Check for swipl on PATH
$swiplCmd = Get-Command swipl -ErrorAction SilentlyContinue
if (-not $swiplCmd) {
    Error-Exit "SWI-Prolog 'swipl' not found on PATH. Please install SWI-Prolog (https://www.swi-prolog.org) or add it to PATH." 2
}

Write-Host "Found SWI-Prolog: $($swiplCmd.Source)"

Write-Host "Running Python quick_check.py..."
& python .\quick_check.py
if ($LASTEXITCODE -ne 0) {
    Error-Exit "python quick_check.py failed with exit code $LASTEXITCODE" $LASTEXITCODE
}

Write-Host "Running Prolog tests (plunit)..."
& swipl -s .\tests\kb_generated_tests.pl -g "run_tests,halt.";
if ($LASTEXITCODE -ne 0) {
    Error-Exit "Prolog tests failed with exit code $LASTEXITCODE" $LASTEXITCODE
}

Write-Host "All checks completed successfully."
exit 0
