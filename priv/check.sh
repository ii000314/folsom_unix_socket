MONITORING_SOCKET=/tmp/monitoring.sock
TIMEOUT_SECONDS=1

test -S $MONITORING_SOCKET && echo -n $@ | nc -U $MONITORING_SOCKET -w $TIMEOUT_SECONDS || echo 0