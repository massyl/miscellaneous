## How to increase unlimt on Linux Mint
If your run out of file handle limits and want to increase the value `ulimit -n` in linux Mint follow steps bellow

### Edit /etc/security/limits.conf and add

 - USER soft nofile 100000
 - USER hard nofile 100000

 Those two lines increase soft and hard limit for user USER. if you want these changes to be effectif for all users, replace `USER` by
 `*`

 note that the value `100000` must be adjusted as needed

### Edit /etc/pam.d/common-session
  - Add this line to the end of the file

  `session	required			pam_limits.so`


### Resart your system

For me, restarting the system was necessary, to have those changes take effect
