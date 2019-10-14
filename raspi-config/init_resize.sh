#!/bin/sh

reboot_pi () {
  umount /boot
  mount / -o remount,ro
  sync
  echo b > /proc/sysrq-trigger
  sleep 5
  exit 0
}

check_commands () {
  if ! command -v whiptail > /dev/null; then
      echo "whiptail not found"
      sleep 5
      return 1
  fi
  for COMMAND in grep cut sed parted fdisk findmnt partprobe; do
    if ! command -v $COMMAND > /dev/null; then
      FAIL_REASON="$COMMAND not found"
      return 1
    fi
  done
  return 0
}

get_variables () {
  ROOT_PART_DEV=$(findmnt / -o source -n)
  ROOT_PART_NAME=$(echo "$ROOT_PART_DEV" | cut -d "/" -f 3)
  ROOT_DEV_NAME=$(echo /sys/block/*/"${ROOT_PART_NAME}" | cut -d "/" -f 4)
  ROOT_DEV="/dev/${ROOT_DEV_NAME}"
  ROOT_PART_NUM=$(cat "/sys/block/${ROOT_DEV_NAME}/${ROOT_PART_NAME}/partition")

  BOOT_PART_DEV=$(findmnt /boot -o source -n)
  BOOT_PART_NAME=$(echo "$BOOT_PART_DEV" | cut -d "/" -f 3)
  BOOT_DEV_NAME=$(echo /sys/block/*/"${BOOT_PART_NAME}" | cut -d "/" -f 4)
  BOOT_PART_NUM=$(cat "/sys/block/${BOOT_DEV_NAME}/${BOOT_PART_NAME}/partition")

  BAK_ROOT_PART_NUM=$((ROOT_PART_NUM + 1))
  BAK_BOOT_PART_DEV="${ROOT_DEV}p${BAK_ROOT_PART_NUM}"
  DATA_PART_NUM=$((BAK_ROOT_PART_NUM + 1))
  DATA_PART_DEV="${ROOT_DEV}p${DATA_PART_NUM}"

  OLD_DISKID=$(fdisk -l "$ROOT_DEV" | sed -n 's/Disk identifier: 0x\([^ ]*\)/\1/p')

  ROOT_DEV_SIZE=$(cat "/sys/block/${ROOT_DEV_NAME}/size")
  TARGET_END=$((ROOT_DEV_SIZE - 1))

  BOOT_DEV_SIZE=$(cat "/sys/block/${BOOT_DEV_NAME}/${BOOT_PART_NAME}/size")
  BOOT_START=$(cat "/sys/block/${BOOT_DEV_NAME}/${BOOT_PART_NAME}/start")
  BOOT_END=$((BOOT_DEV_SIZE + BOOT_START - 1))
  ROOT_START=$((BOOT_END + 1))
  ROOT_END=$((ROOT_START + 16777216))
  BAK_ROOT_START=$((ROOT_END + 1))
  BAK_ROOT_END=$((BAK_ROOT_START + 16777216))
  DATA_START=$((BAK_ROOT_END + 1))
  DATA_END=$TARGET_END

  PARTITION_TABLE=$(parted -m "$ROOT_DEV" unit s print | tr -d 's')

  LAST_PART_NUM=$(echo "$PARTITION_TABLE" | tail -n 1 | cut -d ":" -f 1)

  ROOT_PART_LINE=$(echo "$PARTITION_TABLE" | grep -e "^${ROOT_PART_NUM}:")
  ROOT_PART_START=$(echo "$ROOT_PART_LINE" | cut -d ":" -f 2)
  ROOT_PART_END=$(echo "$ROOT_PART_LINE" | cut -d ":" -f 3)
}

fix_partuuid() {
  DISKID="$(fdisk -l "$ROOT_DEV" | sed -n 's/Disk identifier: 0x\([^ ]*\)/\1/p')"

  sed -i "s/${OLD_DISKID}/${DISKID}/g" /etc/fstab
  sed -i "s/${OLD_DISKID}/${DISKID}/" /boot/cmdline.txt

  echo "#PARTUUID=${DISKID}-0${BAK_ROOT_PART_NUM}        /bak_root       ext4    defaults,noatime  0       1" >> "/etc/fstab"
  echo "PARTUUID=${DISKID}-0${DATA_PART_NUM}        /data           ext4    defaults,noatime  0       1" >> "/etc/fstab"

  #sed -i '/\/boot/s/^/#/g' "/etc/fstab"
}

check_variables () {
  if [ "$BOOT_DEV_NAME" != "$ROOT_DEV_NAME" ]; then
      FAIL_REASON="Boot and root partitions are on different devices"
      return 1
  fi

  if [ "$ROOT_PART_NUM" -ne "$LAST_PART_NUM" ]; then
    FAIL_REASON="Root partition should be last partition"
    return 1
  fi

  if [ "$ROOT_PART_END" -gt "$TARGET_END" ]; then
    FAIL_REASON="Root partition runs past the end of device"
    return 1
  fi

  if [ ! -b "$ROOT_DEV" ] || [ ! -b "$ROOT_PART_DEV" ] || [ ! -b "$BOOT_PART_DEV" ] ; then
    FAIL_REASON="Could not determine partitions"
    return 1
  fi
}

create_partions () {
  echo "d # delete partition
    ${ROOT_PART_NUM} # partion number 2
    n # new partition
    p # primary partition
    ${ROOT_PART_NUM} # partion number 2
    ${ROOT_START} # start immediately after preceding partition
    ${ROOT_END} # 8G system/root partition
    n # new partition
    p # primary partition
    ${BAK_ROOT_PART_NUM} # partion number 3
    ${BAK_ROOT_START} # start immediately after preceding partition
    ${BAK_ROOT_END} # 8G system/root partition
    n # new partition
    p # primary partition
    ${DATA_PART_NUM} # partion number 4
    ${DATA_START} # start immediately after preceding partition
    ${DATA_END}  # extend partition to end of disk
    p # print the in-memory partition table
    w # write the partition table" | sed -e 's/\s*\([\+0-9a-zA-Z]*\).*/\1/' | fdisk "$ROOT_DEV"
}

create_extend_fs () {
  resize2fs "/dev/${ROOT_PART_NAME}"
  mkfs.ext4  -F "${BAK_BOOT_PART_DEV}"
  mkfs.ext4  -F "${DATA_PART_DEV}"
}

create_mounting_points () {
  mkdir "/bak_root"
  mkdir "/data"

  chmod 770 "/bak_root"
  chmod 770 "/data"
}

main () {
  get_variables

  if ! check_variables; then
    return 1
  fi

  create_partions
  create_extend_fs
  fix_partuuid
  create_mounting_points

  partprobe "$ROOT_DEV"

  return 0
}

mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t tmpfs tmp /run
mkdir -p /run/systemd

mount /boot
mount / -o remount,rw
sync

echo 1 > /proc/sys/kernel/sysrq

if ! check_commands; then
  reboot_pi
fi

if main; then
  whiptail --msgbox "Resized root filesystem. Please reboot system." 20 60
else
  whiptail --msgbox "Could not modify partition map.\n${FAIL_REASON}" 20 60
fi

reboot_pi
