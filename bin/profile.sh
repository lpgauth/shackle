erl -pa _build/profile/lib/*/ebin \
    -pa _build/profile/lib/*/test \
    +K true \
    +scl false \
    +spp true \
    -noshell \
    -eval 'shackle_profile:run()' \
    -eval 'init:stop()'
