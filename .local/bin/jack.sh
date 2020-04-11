#!/bin/sh

# start jack
jack_control start


# loop client creation
nohup /usr/bin/alsa_out -j ploop -dploop -q 1 &
nohup /usr/bin/alsa_in  -j cloop -dcloop -q 1 &

# give it some time before connecting to system ports
sleep 1

# cloop ports -> jack output ports
jack_connect cloop:capture_1 system:playback_1
jack_connect cloop:capture_2 system:playback_2


# system microphone to "ploop" ports
jack_connect system:capture_1 ploop:playback_1
jack_connect system:capture_1 ploop:playback_2
