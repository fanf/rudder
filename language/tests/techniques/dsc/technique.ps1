# generated by rudderc
# @name technique_dsc
# @version 1.0

function Technique-Dsc {
  [CmdletBinding()]
  param (
    [Parameter(Mandatory=$True)]
    [String]$ReportId,
    [Parameter(Mandatory=$True)]
    [String]$TechniqueName,
    [Switch]$AuditOnly
  )

  $ReportIdBase = $reportId.Substring(0,$reportId.Length-1)
  $LocalClasses = New-ClassContext
  $ResourcesDir = $PSScriptRoot + "\resources"
  $ReportId = $ReportIdBase+"58fc35d7-7277-49d9-a6f5-a3ecb715d694"
  _rudder_common_report_na -ComponentName "Directory check exists" -ComponentKey "tmp" -Message "Not applicable" -ReportId $ReportId -TechniqueName $TechniqueName -Report:$true -AuditOnly:$AuditOnly
  $ReportId = $ReportIdBase+"c00d3129-f612-4087-84ea-c1ae9b98f9d8"
  $LocalClasses = Merge-ClassContext $LocalClasses $(Command-Execution -Command "dsc" -ComponentName "Command execution" -ReportId $ReportId -TechniqueName $TechniqueName -Report:$true -AuditOnly:$AuditOnly).get_item("classes")
}
