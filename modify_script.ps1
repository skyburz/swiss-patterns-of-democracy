$content = Get-Content -Path "R/elections_module.R" -Raw; $content | Set-Content -Path "modify_script.txt" -Encoding utf8
