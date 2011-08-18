#!/bin/sh -ex

ver=$1

[ -n "$ver" ] || { echo "needs version" >&2 ; exit 1; }
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/debian-el_${ver}_all.deb
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/devscripts-el_${ver}_all.deb
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/dpkg-dev-el_${ver}_all.deb
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/emacs-goodies-el_${ver}_all.deb
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/gnus-bonus-el_${ver}_all.deb
wget http://ftp.us.debian.org/debian/pool/main/e/emacs-goodies-el/vm-bonus-el_${ver}_all.deb

for i in *deb
do
	dpkg-deb --extract $i .
	rm $i
done
