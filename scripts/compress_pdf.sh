#!/bin/bash
cd plots
rm -r compressed
mkdir compressed
for file in *.pdf
do
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile="compressed/${file%%.*}_comp.pdf" $file
#	gs -sDEVICE=pngalpha -dNOPAUSE -dQUIET -dBATCH -dUseCropBox -sOutputFile="compressed/${file%%.*}_comp.png" $file
done
