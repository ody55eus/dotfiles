(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (guix gexp))

(services
 (list
  (service iptables-service-type
           (iptables-configuration
            (ipv4-rules (plain-file "iptables.rules" "*filter
          :INPUT ACCEPT
          :FORWARD ACCEPT
          :OUTPUT ACCEPT
          -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
          -A INPUT -p tcp --dport 22 -j ACCEPT
          -A INPUT -j REJECT --reject-with icmp-port-unreachable
          COMMIT
          "))
            (ipv6-rules (plain-file "ip6tables.rules" "*filter
          :INPUT ACCEPT
          :FORWARD ACCEPT
          :OUTPUT ACCEPT
          -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
          -A INPUT -p tcp --dport 22 -j ACCEPT
          -A INPUT -j REJECT --reject-with icmp6-port-unreachable
          COMMIT
          "))))
