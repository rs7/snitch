# Запуск приложения из консоли для тестирования

## cmd  
`(if exist log rmdir /S /Q log) && cls && rebar3 shell --apps likes`

## sh
`rm -rf && clear && rebar3 shell --apps likes`
