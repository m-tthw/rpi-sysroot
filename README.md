> ## DEPRECATED WARNING
> This repository will be removed in the near future.
> New sysroot will be built in docker build stages as part of building DBE image.

# The purpose of this branch is to trigger a sysroot update.

At the moment the trigger cannot be run on Travis CI server because it does not have device-mapper driver so the kpartx fails to create loop device. However, the instructions in the :sysroot task has been verified to work correctly on Linux system with proper device-mapper driver support.

## History:
- 2017-12-06 - trigger sysroot update for Raspbian Stretch-lite 2017-11-29
- 2016-06-22 - trigger sysroot update for Raspbian Jessie-lite 2016-05-27
- 2016-02-22 - trigger sysroot update for Raspbian Jessie-lite 2016-02-09
- 2015-02-27 - trigger sysroot update for Raspbian Wheezy 2015-02-16
- 2014-11-26 - trigger sysroot update for Raspbian Wheezy 2014-09-09
- 2014-08-27 - trigger sysroot update for Raspbian Wheezy 2014-06-20
