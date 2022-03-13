mkdir 3rd

cd 3rd

git clone --depth=1 https://github.com/skalogryz/richmemo.git
git clone https://github.com/blikblum/multilog.git
cd multilog
git reset --hard 6d95af3
cd..
git clone --depth=1 http://git.code.sf.net/p/lazarus-ccr/dcpcrypt
git clone https://github.com/Kagamma/internettools.git
cd internettools
git checkout fmd
cd ..
svn checkout https://svn.code.sf.net/p/synalist/code/trunk synalist-code

lazbuild --add-package-link dcpcrypt/dcpcrypt.lpk
lazbuild --add-package-link internettools/internettools.lpk
lazbuild --add-package-link richmemo/richmemopackage.lpk 
lazbuild --build-ide= --add-package laz.virtualtreeview_package 
lazbuild --add-package-link richmemo/ide/richmemo_design.lpk
lazbuild --build-ide= --add-package multilog/multiloglaz.lpk 

cd ..


