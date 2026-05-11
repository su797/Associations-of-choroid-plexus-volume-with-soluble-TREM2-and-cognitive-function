$ErrorActionPreference = 'Stop'

$projectRoot = 'I:\researchR\project\ChpSTREM2AD'
$metaPath = Join-Path $projectRoot 'document\ADNI_list_with_sTREM2.xlsx'
$baselinePath = Join-Path $projectRoot 'data\raw\Data_all.csv'
$outLong = Join-Path $projectRoot 'data\clean\ChpSTREM2AD_longitudinal_cognition_dataset.csv'
$outSummaryCsv = Join-Path $projectRoot 'data\clean\ChpSTREM2AD_longitudinal_subject_summary.csv'
$outNote = Join-Path $projectRoot 'document\隸ｴ譏蚕06_郤ｵ蜷第黄螻募庄陦梧ｧ隸・ｼｰ.md'

function Parse-NullableDouble {
  param([string]$Value)
  if ([string]::IsNullOrWhiteSpace($Value)) { return $null }
  $v = $Value.Trim()
  if ($v -eq '>1700') { return 1700.0 }
  try { return [double]$v } catch { return $null }
}

function Parse-NullableDate {
  param([string]$Value)
  if ([string]::IsNullOrWhiteSpace($Value)) { return $null }
  try { return [datetime]$Value } catch { return $null }
}

# Baseline analytic sample (current 735 selected records)
$baseline = Import-Csv -LiteralPath $baselinePath | ForEach-Object {
  [pscustomobject]@{
    RID = [string]$_.RID
    PTID = [string]$_.PTID
    baseline_VISCODE = [string]$_.VISCODE
    baseline_date = Parse-NullableDate ([string]$_.EXAMDATE)
    baseline_DX = [string]$_.DX
    ChP_ICV_bl = Parse-NullableDouble ([string]$_.'ChP/ICV')
    ChP_SUM_bl = Parse-NullableDouble ([string]$_.'choroid-plexus_SUM')
    sTREM2_bl = Parse-NullableDouble ([string]$_.MSD_STREM2CORRECTED)
    ABETA_bl = Parse-NullableDouble ([string]$_.ABETA)
    TAU_bl = Parse-NullableDouble ([string]$_.TAU)
    PTAU_bl = Parse-NullableDouble ([string]$_.PTAU)
    MMSE_bl = Parse-NullableDouble ([string]$_.MMSE)
    MOCA_bl = Parse-NullableDouble ([string]$_.MOCA)
    ADAS13_bl = Parse-NullableDouble ([string]$_.ADAS13)
    CDRSB_bl = Parse-NullableDouble ([string]$_.CDRSB)
    Age_bl = Parse-NullableDouble ([string]$_.AGE)
    Sex = [string]$_.PTGENDER
    Education = Parse-NullableDouble ([string]$_.PTEDUCAT)
    APOE4 = [string]$_.APOE4
  }
} | Where-Object { $_.RID -ne '' -and $_.baseline_date -ne $null }

# Longitudinal metadata from ADNI_list_with_sTREM2.xlsx
$connStr = "Provider=Microsoft.ACE.OLEDB.12.0;Data Source=$metaPath;Extended Properties='Excel 12.0 Xml;HDR=YES;IMEX=1';"
$conn = New-Object System.Data.OleDb.OleDbConnection($connStr)
$conn.Open()
$query = @"
SELECT RID, PTID, VISCODE, EXAMDATE, DX, MMSE, MOCA, ADAS13, CDRSB, mPACCdigit, mPACCtrailsB
FROM [Sheet1$]
"@
$cmd = $conn.CreateCommand()
$cmd.CommandText = $query
$adapter = New-Object System.Data.OleDb.OleDbDataAdapter($cmd)
$dt = New-Object System.Data.DataTable
[void]$adapter.Fill($dt)
$conn.Close()

$metaRows = $dt.Rows | ForEach-Object {
  [pscustomobject]@{
    RID = [string]$_.RID
    PTID = [string]$_.PTID
    followup_VISCODE = [string]$_.VISCODE
    followup_date = Parse-NullableDate ([string]$_.EXAMDATE)
    followup_DX = [string]$_.DX
    MMSE_fu = Parse-NullableDouble ([string]$_.MMSE)
    MOCA_fu = Parse-NullableDouble ([string]$_.MOCA)
    ADAS13_fu = Parse-NullableDouble ([string]$_.ADAS13)
    CDRSB_fu = Parse-NullableDouble ([string]$_.CDRSB)
    mPACCdigit_fu = Parse-NullableDouble ([string]$_.mPACCdigit)
    mPACCtrailsB_fu = Parse-NullableDouble ([string]$_.mPACCtrailsB)
  }
} | Where-Object { $_.RID -ne '' -and $_.followup_date -ne $null }

$metaByRid = @{}
foreach ($row in $metaRows) {
  if (-not $metaByRid.ContainsKey($row.RID)) {
    $metaByRid[$row.RID] = New-Object System.Collections.ArrayList
  }
  [void]$metaByRid[$row.RID].Add($row)
}
foreach ($rid in @($metaByRid.Keys)) {
  $metaByRid[$rid] = $metaByRid[$rid] | Sort-Object followup_date
}

$longRows = New-Object System.Collections.Generic.List[object]
$subjectSummary = New-Object System.Collections.Generic.List[object]

foreach ($b in $baseline) {
  if (-not $metaByRid.ContainsKey($b.RID)) { continue }
  $fu = $metaByRid[$b.RID] | Where-Object { $_.followup_date -gt $b.baseline_date }

  foreach ($f in $fu) {
    $days = ($f.followup_date - $b.baseline_date).Days
    $longRows.Add([pscustomobject]@{
      RID = $b.RID
      PTID = $b.PTID
      baseline_VISCODE = $b.baseline_VISCODE
      baseline_date = $b.baseline_date.ToString('yyyy-MM-dd')
      baseline_DX = $b.baseline_DX
      followup_VISCODE = $f.followup_VISCODE
      followup_date = $f.followup_date.ToString('yyyy-MM-dd')
      followup_DX = $f.followup_DX
      days_from_baseline = $days
      years_from_baseline = [math]::Round($days / 365.25, 3)
      ChP_ICV_bl = $b.ChP_ICV_bl
      ChP_SUM_bl = $b.ChP_SUM_bl
      sTREM2_bl = $b.sTREM2_bl
      ABETA_bl = $b.ABETA_bl
      TAU_bl = $b.TAU_bl
      PTAU_bl = $b.PTAU_bl
      MMSE_bl = $b.MMSE_bl
      MOCA_bl = $b.MOCA_bl
      ADAS13_bl = $b.ADAS13_bl
      CDRSB_bl = $b.CDRSB_bl
      Age_bl = $b.Age_bl
      Sex = $b.Sex
      Education = $b.Education
      APOE4 = $b.APOE4
      MMSE_fu = $f.MMSE_fu
      MOCA_fu = $f.MOCA_fu
      ADAS13_fu = $f.ADAS13_fu
      CDRSB_fu = $f.CDRSB_fu
      mPACCdigit_fu = $f.mPACCdigit_fu
      mPACCtrailsB_fu = $f.mPACCtrailsB_fu
    }) | Out-Null
  }

  $subjectSummary.Add([pscustomobject]@{
    RID = $b.RID
    baseline_DX = $b.baseline_DX
    any_followup = [int](($fu | Measure-Object).Count -gt 0)
    MMSE_followup = [int](($fu | Where-Object { $_.MMSE_fu -ne $null } | Measure-Object).Count -gt 0)
    MOCA_followup = [int](($fu | Where-Object { $_.MOCA_fu -ne $null } | Measure-Object).Count -gt 0)
    ADAS13_followup = [int](($fu | Where-Object { $_.ADAS13_fu -ne $null } | Measure-Object).Count -gt 0)
    CDRSB_followup = [int](($fu | Where-Object { $_.CDRSB_fu -ne $null } | Measure-Object).Count -gt 0)
    mPACCdigit_followup = [int](($fu | Where-Object { $_.mPACCdigit_fu -ne $null } | Measure-Object).Count -gt 0)
    mPACCtrailsB_followup = [int](($fu | Where-Object { $_.mPACCtrailsB_fu -ne $null } | Measure-Object).Count -gt 0)
    n_followup_visits = ($fu | Measure-Object).Count
    max_years_followup = if (($fu | Measure-Object).Count -gt 0) {
      [math]::Round(((($fu | Sort-Object followup_date | Select-Object -Last 1).followup_date - $b.baseline_date).Days) / 365.25, 3)
    } else { 0 }
  }) | Out-Null
}

$longRows | Export-Csv -LiteralPath $outLong -NoTypeInformation -Encoding UTF8
$subjectSummary | Export-Csv -LiteralPath $outSummaryCsv -NoTypeInformation -Encoding UTF8

$nBase = ($baseline | Measure-Object).Count
$nAny = ($subjectSummary | Where-Object { $_.any_followup -eq 1 } | Measure-Object).Count
$nRows = ($longRows | Measure-Object).Count
$visitCounts = $subjectSummary | Where-Object { $_.any_followup -eq 1 } | Select-Object -ExpandProperty n_followup_visits | Sort-Object
$medVisits = if ($visitCounts.Count -gt 0) { $visitCounts[[math]::Floor(($visitCounts.Count - 1) / 2)] } else { 0 }
$years = $subjectSummary | Where-Object { $_.any_followup -eq 1 } | Select-Object -ExpandProperty max_years_followup | Sort-Object
$medYears = if ($years.Count -gt 0) { $years[[math]::Floor(($years.Count - 1) / 2)] } else { 0 }
$dxLines = $baseline | Group-Object baseline_DX | ForEach-Object { "- $($_.Name): $($_.Count) baseline participants" }

$mmseN = ($subjectSummary | Where-Object { $_.MMSE_followup -eq 1 } | Measure-Object).Count
$mocaN = ($subjectSummary | Where-Object { $_.MOCA_followup -eq 1 } | Measure-Object).Count
$adasN = ($subjectSummary | Where-Object { $_.ADAS13_followup -eq 1 } | Measure-Object).Count
$cdrN = ($subjectSummary | Where-Object { $_.CDRSB_followup -eq 1 } | Measure-Object).Count
$mpaccdN = ($subjectSummary | Where-Object { $_.mPACCdigit_followup -eq 1 } | Measure-Object).Count
$mpacctN = ($subjectSummary | Where-Object { $_.mPACCtrailsB_followup -eq 1 } | Measure-Object).Count

Get-Item $outLong, $outSummaryCsv | Select-Object FullName,Length,LastWriteTime

