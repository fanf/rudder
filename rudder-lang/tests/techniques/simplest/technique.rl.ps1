# generated by rudderc
# @name simplest
# @version 1.0

function Simplest-Technique {
  [CmdletBinding()]
  param (
    [Parameter(Mandatory=$False)] [Switch] $AuditOnly,
    [Parameter(Mandatory=$True)]  [String] $ReportId,
    [Parameter(Mandatory=$True)]  [String] $TechniqueName
  )

  $LocalClasses = New-ClassContext
  $ResourcesDir = $PSScriptRoot + "\resources"
  $Condition = "debian_family"
  if (Evaluate-Class $Condition $LocalClasses $SystemClasses) {
    $LocalClasses = Merge-ClassContext $LocalClasses $(File-Absent -ComponentName "File-Absent" -ReportId $ReportId -AuditOnly $AuditOnly -TechniqueName $TechniqueName).get_item("classes")
  }
  else {
    _rudder_common_report_na -ComponentName "File-Absent" -ComponentKey "Path" -Message "Not-Applicable" -ReportId $ReportId -AuditOnly $AuditOnly -TechniqueName $TechniqueName
  }
}
