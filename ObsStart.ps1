<#
    Script simplifying starting Erlang applications.
    Should be placed in root directory of Erlang application.
#>

$ScriptRoot = $PSScriptRoot

$AppName = (Split-Path $ScriptRoot -Leaf)

$EbinPath = Join-Path -Path $ScriptRoot -ChildPath "_build\default\lib\$AppName\ebin"

Set-Location -Path $ScriptRoot

rebar3 compile

Write-Host "Rebar3 compiles erlang app"

erl -pa $EbinPath -sname observer -hidden -setcookie MyCookie -run observer -eval "application:ensure_all_started($AppName)."