# Overview

Reminder on how to import GPG keys

## Export

```bash
gpg -a --export >mypubkeys.asc
gpg -a --export-secret-keys >myprivatekeys.asc
gpg --export-ownertrust >otrust.txt
```

## Import

```bash
gpg --import myprivatekeys.asc
gpg --import mypubkeys.asc
gpg -K
gpg -k
gpg --import-ownertrust otrust.txt
```

## Transfer Key

```bash
gpg --export-key SOMEKEYID | ssh othermachine gpg --import
gpg --export-secret-key SOMEKEYID | ssh othermachine gpg --import
```

## Transfer All

```bash
scp -rp ~/.gnupg othermachine:
```
