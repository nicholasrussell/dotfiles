# Installing Guix

## Reference
[System Crafters (David Wilson) - Installing Guix as a Complete GNU/Linux System](https://www.youtube.com/watch?v=oSy-TmoxG_Y)
[(Show Notes)](https://systemcrafters.net/craft-your-system-with-guix/full-system-install/)

## Build the Base Image

1. Download using `$DOTFILES/bin/download-guix`
1. Create base image
   - `qemu-img create -f qcow2 guix-base.img 40G`
1. Boot the installer
   - Windows: `& 'C:\Program Files\qemu\qemu-system-x86_64.exe' -drive file=guix-base.img,index=0,if=none,id=nvm -device nvme,serial=deadbeef,drive=nvm -m 32G -smp 10 -netdev user,id=vmnic,hostfwd=tcp::6022-:22 -device virtio-net,netdev=vmnic -accel whpx,kernel-irqchip=off -device VGA,vgamem_mb=32768 -boot d -cdrom guix-system-install-1.4.0.x86_64-linux.iso`
1. Go through graphical install
   - Enable substitutes
   - For root and user passwords, use a temporary root pasword and leave user password blank (we will overwrite later)
   - Finish install and reboot (restart qemu without boot and cdrom options)
1. Log in to user account
1. Place channel from [nonguix](https://gitlab.com/nonguix/nonguix) in `./guix/base-channels.scm` (see also: [SystemCrafters](https://github.com/SystemCrafters/guix-installer/blob/master/guix/base-channels.scm))
1. Place installer from [SystemCrafters](https://github.com/SystemCrafters/guix-installer/blob/master/guix/installer.scm) in `./guix/installer.scm`
1. Run `guix time-machine -C './guix/base-channels.scm' -- describe -f channels > './guix/channels.scm'`
1. Run `export image=$(guix time-machine -C './guix/channels.scm' -- system image -t iso9660 './guix/installer.scm')`
1. Run `mv $image ./guix-installer.iso`
1. Configure sshd to allow local connections and scp guix-installer.iso to local machine

## Steps

1. Get modified Guix base image
2. Boot the installer
   - qemu:
     - `qemu-img create -f qcow2 guix.img 80G`
     - `& 'C:\Program Files\qemu\qemu-system-x86_64.exe' -drive file=guix.img,index=0,if=none,id=nvm -device nvme,serial=deadbeef,drive=nvm -m 32G -smp 10 -netdev user,id=vmnic,hostfwd=tcp::6022-:22 -device virtio-net,netdev=vmnic -accel whpx,kernel-irqchip=off -device VGA,vgamem_mb=32768 -boot d -cdrom guix-installer.iso`
3. Select locale then choose "Graphical install ..."
4. Fill out the graphical installer
   - Enable substitutes
   - For root and user passwords, use a temporary root pasword and leave user password blank (we will overwrite later)
   - Desktop environments: GNOME, i3, Emacs EXWM
   - Network services: Mozilla NSS
   - Once you get to where the installer outputs the configuration file, stop
5. Modify install
   - Press Ctrl + Alt + F3 to drop into new TTY
   - `emacs /mnt/etc/config.scm`
     - use-modules: `(use-modules (gnu) (nongnu packages linux))`
     - operating system (before `locale`):
         ```
         (kernel linux)
         (firmware (list linux-firmware))
         ```
   - `herd start cow-store /mnt`
   - `cp /etc/channels.scm /mnt/etc/`
   - `chmod +w /mnt/etc/channels.scm`
   - `guix time-machine -C /mnt/etc/channels.scm -- system init /mnt/etc/config.scm /mnt`
     - If ever hanging, `Ctrl+C` then re-run command
   - `reboot`
6. Once fresh install boots up, change passwords
   - `Ctrl+Alt+F5` and log in as root
   - `passwd` to change root password
   - `passwd {user}` to change user password
   - `exit`
   - `Ctrl+Alt+F7`
7. Log in
8. Persist configuration
   - `mkdir -p ~/.config/guix`
   - `cp /etc/channels.scm ~/.config/guix/`
   - `cp /etc/config.scm ~/.config/guix/system.scm`
   - `chmod +w ~/.config/guix/channels.scm`
9. Un-pin commits
   - Edit `~/.config/guix/channels.scm` and remove the lines containing (commit ...)
10. Update system
    - `guix pull`
    - `sudo -E guix system reconfigure ~/.config/guix/system.scm`
    - `reboot`

## Virtualization Notes

### QEMU

#### On Windows

- Prior to running, ensure "Windows Hypervisor Platform" feature is turned on
- Run with e.g. `& 'C:\Program Files\qemu\qemu-system-x86_64.exe' -drive file=guix.img,index=0,if=none,id=nvm -device nvme,serial=deadbeef,drive=nvm -m 32G -smp 20 -device virtio-net,netdev=vmnic -netdev user,id=vmnic,hostfwd=tcp::6022-:22 -accel whpx,kernel-irqchip=off -device VGA,vgamem_mb=32768`
  - Once `guix install spice-vdagent` has been installed, add `-spice port=6900,disable-ticketing=on -chardev qemu-vdagent,id=ch1,name=vdagent,clipboard=on -device virtio-serial-pci -device virtserialport,chardev=ch1,id=ch1,name=com.redhat.spice.0`
