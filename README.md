# The purpose of this branch is to trigger a sysroot update.

At the moment the trigger cannot be run on Travis CI server because it does not have device-mapper driver so the kpartx fails to create loop device. However, the instructions in the :sysroot task has been verified to work correctly on Linux system with proper device-mapper driver support.

## History:
- 2014-08-27 - trigger sysroot update for Raspbian Debian Wheezy 2014-06-20
