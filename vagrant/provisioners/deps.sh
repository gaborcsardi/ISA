#! /bin/sh

apt-get update
apt-get -y upgrade
apt-get -y install r-base libxml2-dev texlive-base \
    texlive-latex-recommended texlive-fonts-extra texlive-latex-extra \
    texlive-fonts-recommended libopenmpi1.6-dev openmpi1.6 qpdf \
    libcurl4-openssl-dev curl emacs
apt-get clean

