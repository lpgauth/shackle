erl -pa _build/bench/lib/*/ebin \
    -pa _build/bench/lib/*/test \
    +K true \
    +scl false \
    +spp true \
    -noshell \
    -eval 'shackle_bench:run()' \
    -eval 'init:stop()'
