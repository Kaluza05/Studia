## Budowanie projektu
Aby zbudować projekt (z poziomu gui_version i terminal_version osobno):
    dune build

## Uruchamianie programu terminalowego:

Aby uruchomić program terminalowy:
    dune exec terminal_version/src/game_queue_working.exe

Następnie w osobnym terminalu:
    nc localhost 1234

## Uruchamianie programu gui:

Aby uruchomić program gui:
    dune exec gui_version/src/server.exe

Następnie w osobnym terminalu:
    dune exec gui_version/src/client.exe