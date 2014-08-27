#
# Copyright (c) 2008-2014 the Urho3D project.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

# Usage: NOT intended to be used manually (if you insist then try: rake travis)
desc 'Create a new sysroot from Raspbian image'
task :travis do
  system 'wget http://downloads.raspberrypi.org/raspbian_latest -O raspbian.zip' or abort 'Failed to download latest Raspbian image'
  system 'unzip raspbian.zip' or abort 'Failed to unzip Raspbian image'
  system 'sudo kpartx -a -v *.img && sudo mount -o loop /dev/mapper/loop0p2 /mnt' or abort 'Failed to create and mount loop device'
  system 'git clone --depth=1 https://github.com/urho3d/rpi-sysroot.git' or abort 'Failed to clone existing sysroot'
  system 'mv rpi-sysroot/README.md /tmp && mv rpi-sysroot/.git /tmp' or abort 'Failed to temporarily move the extra files before rsync'
  system 'sudo rsync -a --delete /mnt/ rpi-sysroot && sudo chown -R $USER: rpi-sysroot && cp /usr/bin/qemu-arm-static rpi-sysroot/usr/bin && ruby -i -pe "gsub(/^/, %q{#})" rpi-sysroot/etc/ld.so.preload' or abort 'Failed to rsync new sysroot'
  system 'mv /tmp/README.md rpi-sysroot && mv /tmp/.git rpi-sysroot' or abort 'Failed to move the extra files back after rsync'
  system "bash -c 'basename {*,}.img' |tr -d '\n' |ruby -i -le 'version = STDIN.read; puts ARGF.read.gsub(/(?<=\().*?(?=\))/m, version)' rpi-sysroot/README.md" or abort 'Failed to update image version'
  system 'for f in dev proc sys; do sudo mount --bind /$f rpi-sysroot/$f; done && sudo chroot rpi-sysroot /bin/bash -c "apt-get install libraspberrypi0 libraspberrypi-dev libasound2-dev libudev-dev" && for f in dev proc sys; do sudo umount rpi-sysroot/$f; done' or abort 'Failed to install prerequisite software packages in new sysroot'
  system 'sudo umount /mnt && sudo kpartx -d *.img' or abort 'Failed to unmount loop device'
  system 'ln -snf ../../../lib/arm-linux-gnueabihf/libdl.so.2 rpi-sysroot/usr/lib/arm-linux-gnueabihf/libdl.so' or abort 'Failed to fix symbolic link for dl library'
  system "cd rpi-sysroot && git config user.name '#{ENV['GIT_NAME']}' && git config user.email '#{ENV['GIT_EMAIL']}' && git remote set-url --push origin https://#{ENV['GH_TOKEN']}@github.com/urho3d/rpi-sysroot.git && git add -A . && ( git commit -q -m \"Travis CI: Raspberry Pi sysroot update at #{Time.now.utc}.\" || true) && git push -q >/dev/null 2>&1" or abort 'Failed to push new sysroot'
end
