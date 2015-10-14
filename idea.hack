# Try to connect to webservers at host 192.168.0.2,
# at ports 80 through 9000
-test1-
    protocol: [HTTP/1]
    address: [192.168.0.2]
    ports: [80..9000]

# Try to connect with SSH and FTP, to all ports,
# on hosts 10.0.0.2 through 10.0.0.10
-test2-
    protocol: [SSH, FTP]
    address: [10.0.0.2..10.0.0.10]
    ports: *