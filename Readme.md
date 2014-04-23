# IPToolKit (for R)

A library of functions for working with, and obtaining information about, IP addresses

## WARNING

This has only been tested on Linux so far.  Some functions may not work on MAC/Windows (if you try it please let me know)

IP Location data from IP2Location is required for IP geolocation available from <a href="http://lite.ip2location.com">http://lite.ip2location.com</a>

## TO DO

. Add IPv6 Support
. Convert nslookup to use native C code rather than use system()
. enable nslookup to take a vector of ip address and return results for all
. Add a function to sort a vector of IP addresses
. Add support for othr geolocation datasets (e.g. MaxMind)
. . Consider enabling an API instead of a dataset
