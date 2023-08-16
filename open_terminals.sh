#!/bin/bash

# Open terminal windows using your preferred terminal emulator
gnome-terminal -- bash -c "epmd -daemon; make example2; exec bash"
gnome-terminal -- bash -c "erlc plane.erl; erlc tower.erl; erlc controller.erl; erl -name controller@127.0.0.1 -setcookie COOKIE -eval 'controller:start_controller().' ; exec bash"
gnome-terminal -- bash -c "erl -name a@127.0.0.1 -setcookie COOKIE; exec bash"
gnome-terminal -- bash -c "erl -name b@127.0.0.1 -setcookie COOKIE; exec bash"
gnome-terminal -- bash -c "erl -name c@127.0.0.1 -setcookie COOKIE; exec bash"
gnome-terminal -- bash -c "erl -name d@127.0.0.1 -setcookie COOKIE; exec bash"

