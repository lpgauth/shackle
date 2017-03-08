erl -pa _build/test/lib/*/ebin \
    -pa _build/test/lib/*/test \
    +K true \
    +scl false \
    +spp true \
    -noshell \
    -eval 'shackle_bench:run()' \
    -eval 'init:stop()'
