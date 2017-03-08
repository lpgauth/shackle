erl -pa _build/test/lib/*/ebin \
    -pa _build/test/lib/*/test \
    +K true \
    +scl false \
    +spp true \
    -noshell \
    -eval 'arithmetic_tcp_server:start()'
