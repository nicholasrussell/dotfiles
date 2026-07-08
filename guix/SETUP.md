# Installing Guix

## Steps

1. Download iso from [https://guix.gnu.org/en/download/](https://guix.gnu.org/en/download/)
2. Select locale then choose "Graphical install ..."
3. Fill out the graphical installer
   - Enable substitutes
   - For root and user passwords, use a temporary root password and leave user password blank (we will overwrite later)
   - Desktop environments: xfce
   - Once you get to where the installer outputs the configuration file, stop
5. Modify install
   - Press Ctrl + Alt + F3 to drop into new TTY
   - `wget https://substitutes.nonguix.org/signing-key.pub`
   - `mv signing-key.pub /etc/nonguix-signing-key.pub`
   - `chown root:root /etc/nonguix-signing-key.pub`
   - `cp /etc/nonguix-signing-key.pub /mnt/etc/nonguix-signing-key.pub`
   - `emacs /etc/channels.scm`
       ```scheme
       (cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
       ```
   - `cp /etc/channels.scm /mnt/etc/channels.scm`
   - `chmod +w /mnt/etc/channels.scm`
   - `emacs /mnt/etc/config.scm`
     - use-modules:
         ```scheme
         (use-modules (gnu)
                      (gnu services desktop)
                      (nongnu packages linux)
                      (nongnu system linux-initrd))
         ```
     - operating system (before `locale`):
         ```scheme
         (kernel linux)
         (initrd microcode-initrd)
         (firmware (list linux-firmware))
         ```
     - services:
         ```scheme
         (services
           (modify-services
             (append (list (service xfce-desktop-service-type)
                           (set-xorg-configuration (xorg-configuration (keyboard-layout keyboard-layout))))
                     %desktop-services)
             (guix-service-type config =>
                                (guix-configuration
                                  (inherit config)
                                  (substitute-urls
                                    (append (list "https://substitutes.nonguix.org")
                                            %default-substitute-urls))
                                  (authorized-keys
                                    (append (list (local-file "./nonguix-signing-key.pub"))
                                            %default-authorized-guix-keys))))))
         ```
   - `herd start cow-store /mnt`
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

## Steps 2
1. Install Guix
2. Log in to user
3. Create `~/.config/guix/channels.scm`
```scheme
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
```
4. `guix pull`
5. `GUIX_PROFILE="/home/<user>/.config/guix/current"`
6. `. "$GUIX_PROFILE/etc/profile"`
7. `unset GUIX_PROFILE`
8. `hash guix`
9. `guix describe` and make sure nonguix is there
10.
```
mkdir substitutes-keys
wget https://substitutes.nonguix.org/signing-key.pub
mv signing-key.pub substitutes-keys/nonguix-signing-key.pub
sudo guix archive --authorize < substitutes-keys/nonguix-signing-key.pub
```

## Virtualization Notes

### QEMU

#### On Windows

- Prior to running, ensure "Windows Hypervisor Platform" feature is turned on
- Run with e.g. `& 'C:\Program Files\qemu\qemu-system-x86_64.exe' -drive file=guix.img,index=0,if=none,id=nvm -device nvme,serial=deadbeef,drive=nvm -m 32G -smp 20 -device virtio-net,netdev=vmnic -netdev user,id=vmnic,hostfwd=tcp::6022-:22 -accel whpx,kernel-irqchip=off -device VGA,vgamem_mb=32768`
  - Once `guix install spice-vdagent` has been installed, add `-spice port=6900,disable-ticketing=on -chardev qemu-vdagent,id=ch1,name=vdagent,clipboard=on -device virtio-serial-pci -device virtserialport,chardev=ch1,id=ch1,name=com.redhat.spice.0`


```
GUIX_PROFILE="/home/nrussell/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
```
