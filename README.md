folsom_unix_socket
==================

folsom_unix_socket


Main project goal: easy integration with zabbix

Require netcat for work.

1. Setup your monitoring socket path in priv/check.sh
2. Setup your monitoring socket path in application vaiable *monitoring_socket*

Note: default path is /tmp/monitoring.sock

Basic setup zabbix callback
---------------------------

```
>> cat ~/monitoring/check.sh
# resetup via [{folsom_unix, [{monitoring_socker, "/custom/path"}]}]

MONITORING_SOCKET=/tmp/monitoring.sock
test -S $MONITORING_SOCKET && echo -n $@ | nc -U $MONITORING_SOCKET || echo 0

```
```
>> cat /etc/zabbix/conf.d/myapp.conf

UserParameter=myapp.running,/home/myapp/monitoring/check.sh running | head -n 1

```

Basic testing
-------------

Initial: `rebar get-deps`

If hacking package:

```
>> rebar compile && erl -pa ebin -pa deps/*/ebin -s dev
```

When testing integration with your application:

```
# in second console run
>> echo -n "running" | nc -U /tmp/monitoring.sock
```