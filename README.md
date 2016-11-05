# DistBackupService
A distributed, peer-to-peer file backup service, written in Erlang! Tufts
University Comp 50: Concurrent Programming course final project.

# Components
  * Client: Client interacts with users and send backup/retrieve requests over
    a TCP port. Each node has a running client.
  * Server: Server listens on a TCP port and answers receive/send requests.
    Each node has a running server.
  * Monitor: Monitor is a centralized, master server that keeps a list of and
    manages running servers. Each cluster has ONE monitor.

# Requirements
  * Erlang: Erlang/OTP 18 or up.

# Authors
Xuanrui (Ray) Qi, Brinley Macnamara & Benjamin Holen

# License
MIT License. See "LICENSE" file for details.
